# ==============================================================================
# R/helpers_bucking_core.R
#
# Purpose : The per-tree bucking core shared by both backend runners
#           (scripts/app_backend/01_run_sampled_plot_bucking.R): candidate
#           geometry, pricing and the DP for a set of trees, for one run or
#           several runs at once (Performance phase, 2026-10-13).
#
# Design  :
#   - Price-independent work once: the candidates (positions, price volumes,
#     price-diameter classes) are generated once per tree for the union of
#     the runs' products. Each run keeps the candidates of its own products,
#     then prices them and runs the DP. A comparison (default + scenario)
#     therefore costs one candidate pass, not two. Product definitions are
#     shared between runs (only membership and prices differ); runs whose
#     shared products are defined differently get their own pass.
#   - Plain vectors, no per-tree data.frames: .candidate_columns() (helpers_
#     candidate_generation.R) gives the usable candidates as column vectors;
#     .price_vectors() (helpers_valuation.R) prices them; .dp_select() is the
#     backend DP (.optimize_dp_fast() is its data.frame wrapper). Result rows
#     are built only for the selected logs.
#   - Results equal those of the per-tree data.frame loop this replaces
#     (audit 22; main baseline outputs/performance/baseline_main_3fa0a9d.rds).
#   - No RNG and no global state: a tree's result depends only on the tree,
#     so trees can be processed in any chunking or on any worker.
# ==============================================================================



# ------------------------------------------------------------------------------
# DP (backend optimizer, v3)
# ------------------------------------------------------------------------------

# Backward-induction DP over candidate logs given as vectors.
#
# Tie-breaking (Phase 4a, R/helpers_dp_optimizer.R): totals within
# DP_TIE_TOL_NOK are equal; then fewer logs, higher priority (lower
# ProductPriority), longer log, ProductKey; skipping a position (unallocated)
# wins a tie only when it needs fewer logs. Audits 16 and 19 assert that
# this agrees with optimize_one_tree_dp().
#
# @param sp_vec,ep_vec Integer start and stop positions (cm).
# @param val_vec  Value (NOK) per candidate.
# @param prio_vec ProductPriority per candidate (no NA).
# @param pk_vec   Rank of the candidate's ProductKey among the sorted keys.
# @return list(sel_idx = selected candidates in stem order, best = best
#   total value).
.dp_select <- function(sp_vec, ep_vec, val_vec, prio_vec, pk_vec) {
  n_dp    <- length(sp_vec)
  len_vec <- ep_vec - sp_vec

  all_pos  <- sort(unique(c(sp_vec, ep_vec)))
  sentinel <- max(all_pos) + 1L
  all_pos  <- c(all_pos, sentinel)
  m        <- length(all_pos)
  max_pos    <- max(all_pos)
  pos_to_idx <- integer(max_pos + 1L)
  pos_to_idx[all_pos + 1L] <- seq_len(m)

  # Stop-position index per candidate, and the candidates grouped by the
  # index of their start position (original order within a group).
  stop_idx <- pos_to_idx[ep_vec + 1L]
  valid    <- stop_idx > 0L & ep_vec <= max_pos
  use      <- if (all(valid)) seq_len(n_dp) else which(valid)
  by_start <- split(use, factor(pos_to_idx[sp_vec[use] + 1L], levels = seq_len(m)))

  dp_val <- numeric(m)
  dp_n   <- integer(m)
  dp_log <- integer(m)
  tol    <- DP_TIE_TOL_NOK

  for (i in seq(m - 1L, 1L)) {
    cands       <- by_start[[i]]
    default_val <- dp_val[i + 1L]
    if (length(cands) > 0L) {
      sv       <- stop_idx[cands]
      total_v  <- val_vec[cands] + dp_val[sv]
      best_tot <- max(default_val, total_v)
      tied     <- which(total_v >= best_tot - tol)
      if (length(tied) > 0L) {
        k <- if (length(tied) == 1L) tied else {
          ct <- cands[tied]
          tied[order(1L + dp_n[sv[tied]], prio_vec[ct], -len_vec[ct], pk_vec[ct])[1L]]
        }
        n_k <- 1L + dp_n[sv[k]]
        # Skip wins only if tied with the best and needing fewer logs.
        if (!(default_val >= best_tot - tol && dp_n[i + 1L] < n_k)) {
          dp_val[i] <- total_v[k]
          dp_n[i]   <- n_k
          dp_log[i] <- cands[k]
          next
        }
      }
    }
    dp_val[i] <- default_val
    dp_n[i]   <- dp_n[i + 1L]
    # dp_log[i] stays 0 (skip)
  }

  # Traceback
  sel_idx <- integer(0)
  i <- 1L
  while (i < m) {
    ci <- dp_log[i]
    if (ci == 0L) { i <- i + 1L
    } else {
      sel_idx <- c(sel_idx, ci)
      sp <- ep_vec[ci]
      if (sp > max_pos) break
      si <- pos_to_idx[sp + 1L]
      if (si == 0L) break
      i <- si
    }
  }
  list(sel_idx = sel_idx, best = dp_val[1L])
}


# ------------------------------------------------------------------------------
# Runs
# ------------------------------------------------------------------------------

# Prepare the runs bucked together: per run its product keys, price lookup
# and own candidate plan (for its error messages), and the candidate plans
# shared by runs whose common products are defined identically.
#
# @param runs List of list(pd, lc, lookup): ProductData, LengthClasses and
#   build_price_lookup() result per run.
# @return list(runs, groups): groups = list of list(plan, runs = run
#   indices), one candidate pass per group.
.prepare_runs <- function(runs) {
  runs <- lapply(runs, function(r) {
    r$keys     <- as.character(r$pd$ProductKey)
    r$opts     <- normalize_grade_opts(r$opts)             # stem-grade options (Phase 6)
    r$own_plan <- .candidate_plan(r$pd, r$lc, grade_opts = r$opts)
    r
  })
  same_def <- function(a, b) {
    # Different stem-grade options give different candidates, whatever the products.
    if (!identical(a$opts, b$opts)) return(FALSE)
    # Permitted and reference grades change a product's candidates (also for stems
    # without grades), whichever way the column is missing on one side.
    perm <- function(x, k, col) if (col %in% names(x$pd))
      norm_permitted_grades(x$pd[[col]][match(k, x$pd$ProductKey)]) else NA_character_
    butt <- function(x, k) if ("ButtCondition" %in% names(x$pd))
      as.character(x$pd$ButtCondition[match(k, x$pd$ProductKey)]) else NA_character_
    for (k in intersect(a$keys, b$keys)) {
      if (!identical(butt(a, k), butt(b, k))) return(FALSE)
      for (col in c("PermittedGrades", "ReferenceGrades"))
        if (!identical(perm(a, k, col), perm(b, k, col))) return(FALSE)
      cols <- setdiff(intersect(names(a$pd), names(b$pd)), c("PermittedGrades", "ReferenceGrades", "ButtCondition"))
      ra <- a$pd[a$pd$ProductKey == k, cols, drop = FALSE]
      rb <- b$pd[b$pd$ProductKey == k, cols, drop = FALSE]
      la <- a$lc[a$lc$ProductKey == k, , drop = FALSE]
      lb <- b$lc[b$lc$ProductKey == k, , drop = FALSE]
      rownames(ra) <- rownames(rb) <- rownames(la) <- rownames(lb) <- NULL
      lcols <- intersect(names(la), names(lb))
      if (!identical(ra, rb) || !identical(la[lcols], lb[lcols])) return(FALSE)
      # Price-volume lengths (hpr imports; the column may be in one run only).
      plen <- function(l) { p <- if ("PriceLength_cm" %in% names(l)) as.numeric(l$PriceLength_cm)
                                  else rep(NA_real_, nrow(l))
                            ifelse(is.na(p), as.numeric(l$LogLength), p) }
      if (!identical(plen(la), plen(lb))) return(FALSE)
    }
    TRUE
  }
  # Greedy grouping: a run joins the first group it is compatible with.
  groups <- list()
  for (i in seq_along(runs)) {
    g <- Position(function(gr) all(vapply(gr$runs, function(j) same_def(runs[[i]], runs[[j]]),
                                          logical(1))), groups)
    if (is.na(g)) groups[[length(groups) + 1L]] <- list(runs = i)
    else groups[[g]]$runs <- c(groups[[g]]$runs, i)
  }
  groups <- lapply(groups, function(gr) {
    pd <- runs[[gr$runs[1L]]]$pd; lc <- runs[[gr$runs[1L]]]$lc
    for (j in gr$runs[-1L]) {
      new <- !runs[[j]]$pd$ProductKey %in% pd$ProductKey
      if (any(new)) {
        pd <- .bind_rows_fill(pd, runs[[j]]$pd[new, , drop = FALSE])
        lc <- .bind_rows_fill(lc, runs[[j]]$lc[runs[[j]]$lc$ProductKey %in%
                                                 runs[[j]]$pd$ProductKey[new], , drop = FALSE])
      }
    }
    gr$plan <- .candidate_plan(pd, lc, grade_opts = runs[[gr$runs[1L]]]$opts)
    gr
  })
  list(runs = runs, groups = groups)
}


# ------------------------------------------------------------------------------
# Trees
# ------------------------------------------------------------------------------

# A data.frame from equal-length column vectors, without data.frame()'s
# checks (the per-tree loop builds many small ones).
.cols_to_df <- function(cols)
  structure(cols, class = "data.frame",
            row.names = c(NA_integer_, -length(cols[[1L]])))

# Row-bind per-tree column lists (tree rows or selected logs, NULL where
# none) into one data.frame; NULL if all are NULL.
.bind_cols_lists <- function(x) {
  x <- x[!vapply(x, is.null, logical(1))]
  if (length(x) == 0L) return(NULL)
  .cols_to_df(setNames(lapply(names(x[[1L]]), function(nm)
    do.call(c, lapply(x, `[[`, nm))), names(x[[1L]])))
}

# A tree's result row: the columns of tree_summary, as a list of length-1
# vectors (.bind_cols_lists() makes the table).
.tree_row <- function(tmeta, tid, sg, n_sel = 0L, value = 0, vol = 0, com = 0,
                      nok = NA_real_, status, msg) {
  list(
    plot_id              = as.character(tmeta$plot_id),
    treid                = tid,
    species_group        = sg,
    dbh_cm               = as.numeric(tmeta$dbh_cm),
    height_m             = as.numeric(tmeta$height_m),
    n_logs_selected      = n_sel,
    total_value_nok      = value,
    total_volume_m3      = vol,
    commercial_volume_m3 = com,
    nok_per_m3           = nok,
    optimizer_status     = status,
    optimizer_message    = msg,
    height_source        = as.character(or_default(tmeta$height_source, NA_character_))
  )
}

# Price one run's candidates of one tree and optimise. geo: usable_only
# .candidate_columns() result (union products of the group); run: an entry
# of .prepare_runs()$runs. Returns list(row, sel, unpriced, t_price, t_dp).
.price_and_buck_tree <- function(geo, run, pv, tmeta, tid) {
  sg  <- pv$species_group
  out <- list(row = NULL, sel = NULL, unpriced = NULL, t_price = 0, t_dp = 0)
  if (sum(geo$n_all[names(geo$n_all) %in% run$keys]) == 0L) {
    out$row <- .tree_row(tmeta, tid, sg, status = "no_candidates",
                         msg = "Candidate generation returned 0 rows")
    return(out)
  }
  keys   <- vapply(geo$products, `[[`, "", "pk")
  in_run <- keys %in% run$keys
  cl     <- geo$cols
  r_idx  <- which(in_run[cl$prod])
  if (length(r_idx) == 0L) {
    out$row <- .tree_row(tmeta, tid, sg, status = "no_candidates",
                         msg = "No eligible candidates after eligibility filter")
    return(out)
  }

  # Pricing (helpers_valuation.R).
  t0    <- proc.time()[["elapsed"]]
  prod  <- cl$prod[r_idx]
  sp    <- cl$StartPos[r_idx]
  ep    <- cl$StopPos[r_idx]
  vol   <- cl$price_volume_m3[r_idx]
  lc_dm <- as.integer((ep - sp) / 10L)
  pr    <- .price_vectors(keys, prod, lc_dm, cl$PriceDiameterClass_cm[r_idx], run$lookup)
  value <- vol * pr$price
  out$t_price <- proc.time()[["elapsed"]] - t0

  # Unpriced candidates, compactly (ProductKey x LengthClass_dm counts).
  na_p <- is.na(pr$price)
  if (any(na_p)) {
    up_tab <- as.data.frame(
      table(ProductKey     = keys[prod[na_p]],
            LengthClass_dm = as.character(lc_dm[na_p])),
      stringsAsFactors = FALSE)
    up_tab <- up_tab[up_tab$Freq > 0L, , drop = FALSE]
    names(up_tab)[names(up_tab) == "Freq"] <- "n_candidates"
    up_tab$LengthClass_dm <- as.integer(up_tab$LengthClass_dm)
    out$unpriced <- up_tab
  }

  # DP: only priced candidates enter.
  ok <- which(!na_p & !is.na(value))
  if (length(ok) == 0L) {
    out$row <- .tree_row(tmeta, tid, sg, status = "no_priced_candidates",
                         msg = "No priced candidates; all candidates unpriced")
    return(out)
  }
  t0 <- proc.time()[["elapsed"]]
  prio_p <- vapply(geo$products, `[[`, 0, "priority")
  prio_p[is.na(prio_p)] <- 99
  # Rank of each candidate's ProductKey among the keys present (tie key).
  p_ok  <- prod[ok]
  rank  <- match(keys, sort(unique(keys[unique(p_ok)])))
  res <- tryCatch(
    .dp_select(sp[ok], ep[ok], value[ok], prio_p[p_ok], rank[p_ok]),
    error = function(e) {
      warning(sprintf("Tree %s: optimizer error: %s", tid, conditionMessage(e)))
      list(error = conditionMessage(e))
    })
  out$t_dp <- proc.time()[["elapsed"]] - t0
  if (!is.null(res$error)) {
    out$row <- .tree_row(tmeta, tid, sg, status = "error", msg = res$error)
    return(out)
  }
  n_sel <- length(res$sel_idx)
  if (n_sel == 0L) {
    out$row <- .tree_row(tmeta, tid, sg, value = res$best, status = "zero_logs",
                         msg = "DP ran but selected 0 logs")
    return(out)
  }

  # Selected logs (the columns of selected_logs_all, in its order).
  j   <- ok[res$sel_idx]           # into the run's rows
  det <- .candidate_details(geo, r_idx[j])
  sel <- list(
    plot_id                        = rep(as.character(tmeta$plot_id), n_sel),
    treid                          = rep(as.character(pv$treid), n_sel),
    species_group                  = rep(sg, n_sel),
    ProductKey                     = det$ProductKey,
    ProductType                    = det$ProductType,
    StartPos                       = det$StartPos,
    StopPos                        = det$StopPos,
    LogLength                      = det$LogLength,
    LengthClass_dm                 = det$LengthClass_dm,
    price_volume_m3                = det$price_volume_m3,
    PriceNOKperM3                  = pr$price[j],
    value_nok                      = value[j],
    PriceDiameterClass_cm_uncapped = as.integer(det$PriceDiameterClass_cm),
    PriceDiameterClass_cm_used     = pr$class_used[j],
    TopDiameter_ob                 = det$TopDiameter_ob,
    TopDiameter_ub                 = det$TopDiameter_ub)
  # Commercial volume under bark (the NOK/m3 denominator) per selected log.
  sel$commercial_volume_m3 <- commercial_volume_ub(pv$diameterPosition,
                                                   pv$DiameterValueUB, .cols_to_df(sel))
  com_vol <- sum(sel$commercial_volume_m3)
  out$row <- .tree_row(tmeta, tid, sg, n_sel = n_sel, value = res$best,
                       vol = sum(sel$price_volume_m3, na.rm = TRUE), com = com_vol,
                       nok = if (com_vol > 0) res$best / com_vol else NA_real_,
                       status = "ok", msg = sprintf("Selected %d logs", n_sel))
  out$sel <- sel
  out
}

# Buck a set of trees for one or more runs (the per-tree loop of both
# runners; also the unit of work of a parallel chunk).
#
# @param pvs       Named list of profile vectors with under-bark profiles.
# @param tree_data One row per tree (plot_id, treid, dbh_cm, height_m,
#   height_source).
# @param prep      .prepare_runs() result.
# @param stump_cm,step_cm Stump height and start-position step (cm).
# @return list with, per run, tree rows, selected logs and unpriced tables
#   (lists in tree order; NULL where none), and stage times in seconds
#   (t_cand shared by the runs; t_price, t_dp per run).
.buck_trees <- function(pvs, tree_data, prep, stump_cm, step_cm) {
  tids   <- names(pvs)
  n      <- length(tids)
  n_runs <- length(prep$runs)
  per_run <- lapply(seq_len(n_runs), function(r)
    list(rows = vector("list", n), sel = vector("list", n),
         unpriced = vector("list", n), t_price = 0, t_dp = 0))
  t_cand <- 0
  t_idx  <- match(tids, as.character(tree_data$treid))
  hs     <- tree_data$height_source

  for (i in seq_len(n)) {
    if (is.na(t_idx[i])) next   # should not happen
    tid <- tids[i]; pv <- pvs[[tid]]; ti <- t_idx[i]
    tmeta <- list(plot_id = tree_data$plot_id[ti], dbh_cm = tree_data$dbh_cm[ti],
                  height_m = tree_data$height_m[ti],
                  height_source = if (is.null(hs)) NULL else hs[ti])
    for (gr in prep$groups) {
      t0  <- proc.time()[["elapsed"]]
      no_products <- list(error = sprintf(
        "no products configured for species_group '%s'.", pv$species_group))
      geo <- tryCatch(
        .candidate_columns(pv, or_default(gr$plan[[pv$species_group]], no_products),
                           stump_cm, step_cm, usable_only = TRUE),
        error = function(e) e)
      t_cand <- t_cand + proc.time()[["elapsed"]] - t0
      for (r in gr$runs) {
        run <- prep$runs[[r]]
        g   <- geo
        # A failed union pass, or a run without products for this species:
        # the run's own pass gives its own error (as a separate run would).
        own <- run$own_plan[[pv$species_group]]
        if (inherits(g, "error") || is.null(own) || !is.null(own$error)) {
          t0 <- proc.time()[["elapsed"]]
          g <- tryCatch(
            .candidate_columns(pv, or_default(own, no_products),
                               stump_cm, step_cm, usable_only = TRUE),
            error = function(e) e)
          t_cand <- t_cand + proc.time()[["elapsed"]] - t0
        }
        if (inherits(g, "error")) {
          warning(sprintf("Tree %s: candidate generation error: %s",
                          tid, conditionMessage(g)))
          per_run[[r]]$rows[[i]] <- .tree_row(tmeta, tid, pv$species_group,
                                              status = "no_candidates",
                                              msg = "Candidate generation returned 0 rows")
          next
        }
        res <- .price_and_buck_tree(g, run, pv, tmeta, tid)
        per_run[[r]]$rows[[i]]     <- res$row
        per_run[[r]]$sel[i]        <- list(res$sel)
        per_run[[r]]$unpriced[i]   <- list(res$unpriced)
        per_run[[r]]$t_price <- per_run[[r]]$t_price + res$t_price
        per_run[[r]]$t_dp    <- per_run[[r]]$t_dp + res$t_dp
      }
    }
  }
  list(runs = per_run, t_cand = t_cand)
}
