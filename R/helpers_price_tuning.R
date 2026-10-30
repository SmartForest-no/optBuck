# ==============================================================================
# R/helpers_price_tuning.R
#
# Purpose : Price-matrix tuning (roadmap 7, session 2; no UI). Steers one
#           sawlog product's prices so that the share of logs (or volume) per
#           length class within each diameter class approaches a target, the
#           fordelningsgrad (FG).
#
# Terms   : "grid" = one product's length classes x diameter classes. For an
#           .hpr-imported product the diameter classes are the file's own
#           classes (each Apter whole-cm row maps to the file class it was
#           priced from, file_class_of_cm()); for other products every Apter
#           diameter row is its own class. All matrices are length x diameter
#           on that grid; NA = unavailable (no price) or no target.
#
# `products` : list(pd, lc, pm): ProductData, LengthClasses and the price
#           matrix (pm_minimal() layout, long). Prices are in pm.
#
# Needs   : the backend (swap_job_prices(), subset_job_stems(), buck_job(),
#           finish_job()) for tune_prices() only; the rest is pure.
# ==============================================================================

# ---- FG ----------------------------------------------------------------------------

#' FG of one diameter class: S, W in percent over the length classes.
fg_class <- function(S, W) 100 - 0.5 * sum(abs(S - W))

#' Real target: the Distribution values of the available cells, normalised to
#' 100 % within each diameter class (columns). A class whose values sum to 0
#' has no target: its column is NA. (Unavailable cells count as 0.)
real_target <- function(dist, avail) {
  w <- dist * avail; w[is.na(w)] <- 0
  s <- colSums(w)
  sweep(w, 2L, ifelse(s > 0, s, NA_real_), "/") * 100
}

#' The Apter diameter row (whole cm) -> the file's diameter class (lower limit
#' mm): the largest class <= c * 10 mm, as the import maps prices (the smallest
#' class below it).
file_class_of_cm <- function(cm, d_file) {
  d_file <- sort(unique(d_file))
  vapply(cm, function(c) { u <- d_file[d_file <= c * 10]; if (length(u)) max(u) else min(d_file) }, 0)
}

# ---- the grid ------------------------------------------------------------------------

# The product's matrix rows (a length class) of the price matrix.
.tg_rows <- function(pm, product_id)
  pm[pm$ProductKey == product_id & !is.na(pm$LengthClass_dm), , drop = FALSE]

#' The distribution info of one .hpr product, for tuning_grid(hpr =).
#'
#' @param dst A read_hpr_distribution() result (scripts/roadmap7_helpers.R).
#' @param key The product's ProductKey in the file (assortment HprProductKey).
hpr_tuning_info <- function(dst, key)
  list(cells = dst$cells[[key]],
       category = dst$products$DistributionCategory[match(key, dst$products$key)])

#' A product's tuning grid.
#'
#' @param products list(pd, lc, pm).
#' @param product_id ProductKey of a matrix-priced product.
#' @param hpr NULL, or list(cells (d_mm, l_cm, dist), category) from
#'   hpr_tuning_info(): the file's diameter classes, its Distribution and its
#'   DistributionCategory. File length classes map to Apter's by the import's
#'   rule (rounded up to whole dm).
#' @return list(product_id, name, lengths (dm), diam_classes, price (length x
#'   diameter, NA = unavailable), species, cm_map (named by Apter cm row: the
#'   grid column it belongs to), basis_default ("volume" when the file's
#'   DistributionCategory is "Volume of logs", else "number"), distribution
#'   (NULL, or the file's Distribution on the grid)).
tuning_grid <- function(products, product_id, hpr = NULL) {
  rows <- .tg_rows(products$pm, product_id)
  if (nrow(rows) == 0L) stop("No matrix-priced product '", product_id, "' in the price matrix.")
  lengths <- sort(unique(rows$LengthClass_dm))
  cm <- sort(unique(rows$PriceDiameterClass_cm))
  d_file <- if (is.null(hpr)) NULL else sort(unique(hpr$cells$d_mm))
  cls <- if (is.null(d_file)) as.numeric(cm) else file_class_of_cm(cm, d_file)
  cm_map <- setNames(cls, as.character(cm))
  diam <- sort(unique(cls))
  price <- matrix(NA_real_, length(lengths), length(diam), dimnames = list(lengths, diam))
  ok <- !is.na(rows$PriceNOKperM3)
  r <- rows[ok, , drop = FALSE]
  ij <- cbind(match(r$LengthClass_dm, lengths), match(cm_map[as.character(r$PriceDiameterClass_cm)], diam))
  first <- !duplicated(ij)                                  # rows are in cm order: the first priced row wins
  price[ij[first, , drop = FALSE]] <- r$PriceNOKperM3[first]
  dist <- NULL
  if (!is.null(hpr)) {
    cells <- hpr$cells
    dist <- matrix(0, length(lengths), length(diam), dimnames = list(lengths, diam))
    k <- cbind(match(ceiling(cells$l_cm / 10), lengths), match(cells$d_mm, diam))
    keep <- stats::complete.cases(k) & !is.na(cells$dist)
    dist[k[keep, , drop = FALSE]] <- cells$dist[keep]
  }
  sp <- products$pd$species_group[match(product_id, products$pd$ProductKey)]
  cat_ <- if (is.null(hpr)) NA_character_ else as.character(hpr$category)
  list(product_id = product_id,
       name = products$pd$ProductName[match(product_id, products$pd$ProductKey)],
       lengths = lengths, diam_classes = diam, price = price, species = sp, cm_map = cm_map,
       basis_default = if (!is.na(cat_) && grepl("volume", cat_, ignore.case = TRUE)) "volume" else "number",
       distribution = dist)
}

#' Products with every Apter cell of the product set from its grid cell.
#' Unavailable cells (NA in the grid or in the price matrix) stay as they are.
apply_grid_prices <- function(products, grid, price) {
  pm <- products$pm
  i <- which(pm$ProductKey == grid$product_id & !is.na(pm$LengthClass_dm) & !is.na(pm$PriceNOKperM3))
  r <- match(pm$LengthClass_dm[i], grid$lengths)
  c <- match(grid$cm_map[as.character(pm$PriceDiameterClass_cm[i])], grid$diam_classes)
  new <- price[cbind(r, c)]
  set <- !is.na(new)
  pm$PriceNOKperM3[i[set]] <- new[set]
  products$pm <- pm
  products
}

# ---- target ---------------------------------------------------------------------------

#' Target W in percent per diameter class (columns sum to 100 over the
#' available cells); NA where no target.
#'
#' @param type "real": the Distribution of the available cells normalised to
#'   100 per class (classes summing to 0 get NA); "neutral": equal share per
#'   available length class; "long": weights 1..k from the shortest to the
#'   longest available length class, normalised.
#' @param distribution Length x diameter matrix for "real" (default: the grid's
#'   own `distribution`, the file's).
tuning_target <- function(grid, type = c("real", "neutral", "long"), distribution = NULL) {
  type <- match.arg(type)
  avail <- !is.na(grid$price)
  w <- switch(type,
    real = {
      d <- or_default(distribution, grid$distribution)
      if (is.null(d)) stop("The real target needs a distribution.")
      real_target(d, avail)
    },
    neutral = { x <- avail * 1; sweep(x, 2L, ifelse(colSums(x) > 0, colSums(x), NA_real_), "/") * 100 },
    long = { x <- apply(avail, 2L, cumsum) * avail
             if (!is.matrix(x)) x <- matrix(x, nrow(avail), ncol(avail))
             sweep(x, 2L, ifelse(colSums(x) > 0, colSums(x), NA_real_), "/") * 100 })
  w[!avail] <- NA_real_
  dimnames(w) <- dimnames(grid$price)
  w
}

# ---- tally and FG ---------------------------------------------------------------------

#' The product's logs of a run on its grid: counts and price volume (the volume
#' the prices apply to). A log goes to the cell of the classes it was priced
#' with: LengthClass_dm (a price length class of the product) and
#' PriceDiameterClass_cm_used (via cm_map). Stops if a log has no cell or if the
#' summed counts or volumes differ from the product's totals in the result.
#'
#' @param result finish_job() output.
#' @return list(n, v) matrices on the grid.
tally_grid <- function(result, product_id, grid) {
  sl <- result$selected_logs_all
  sl <- sl[as.character(sl$ProductKey) == product_id, , drop = FALSE]
  r <- match(sl$LengthClass_dm, grid$lengths)
  c <- match(grid$cm_map[as.character(sl$PriceDiameterClass_cm_used)], grid$diam_classes)
  n <- v <- matrix(0, length(grid$lengths), length(grid$diam_classes), dimnames = dimnames(grid$price))
  ok <- !is.na(r) & !is.na(c)
  if (any(ok)) {
    s <- rowsum(cbind(1, sl$price_volume_m3[ok]), paste(r[ok], c[ok]), reorder = FALSE)
    key <- do.call(rbind, strsplit(rownames(s), " ", fixed = TRUE))
    idx <- cbind(as.integer(key[, 1L]), as.integer(key[, 2L]))
    n[idx] <- s[, 1L]; v[idx] <- s[, 2L]
  }
  tot_n <- nrow(sl); tot_v <- sum(sl$price_volume_m3)
  if (sum(n) != tot_n || abs(sum(v) - tot_v) > 1e-6 * max(1, abs(tot_v)))
    stop(sprintf("tally_grid(%s): the grid does not hold all logs: counts %s of %s (difference %s), price volume %.6f of %.6f m3 (difference %.6f).",
                 product_id, sum(n), tot_n, tot_n - sum(n), sum(v), tot_v, tot_v - sum(v)), call. = FALSE)
  list(n = n, v = v)
}

#' Fordelningsgrad on a tally.
#'
#' Shares S = 100 * x / column total, x = tally$n ("number") or tally$v
#' ("volume"). FG_j = 100 - 0.5 * sum_i |S_ij - W_ij| over the cells with W not
#' NA; overall = mean of FG_j weighted by the column totals. Classes with total
#' 0 or no target are left out (NA).
#' @return list(overall, by_class (named), S, weights (column totals)).
fg_grid <- function(tally, W, basis = c("number", "volume")) {
  basis <- match.arg(basis)
  x <- if (basis == "number") tally$n else tally$v
  tot <- colSums(x)
  S <- sweep(x, 2L, ifelse(tot > 0, tot, 1), "/") * 100
  valid <- tot > 0 & colSums(!is.na(W)) > 0
  fg <- vapply(seq_along(tot), function(j) {
    if (!valid[j]) return(NA_real_)
    h <- !is.na(W[, j]); 100 - 0.5 * sum(abs(S[h, j] - W[h, j])) }, 0)
  names(fg) <- colnames(x)
  overall <- if (any(valid)) sum(tot[valid] * fg[valid]) / sum(tot[valid]) else NA_real_
  list(overall = overall, by_class = fg, S = S, weights = tot)
}

# ---- the price update --------------------------------------------------------------

#' The next price matrix. Steered cells: W not NA, price available, class
#' eligible; every other cell is P1.
#'
#' Per steered cell: g_r = 0.95 for r < 3, else 0.95^(r - 2); D = S - W
#' (percentage points); if r >= 2 and |S - S_prev| < 0.5 the cell stagnated and
#' keeps P, else P_new = P - g_r * D. Calibration, only when r + 1 > 10, per
#' eligible class j: cbar_new = sum V*P_new / sum V (latest run's volume V),
#' cbar_1 = sum V1*P1 / sum V1 (sums over the available cells; a class with a
#' zero volume sum is skipped); the steered cells of the class are shifted by
#' -(cbar_new - cbar_1). Floor: P_new = max(P_new, floor), floor <= P1 per
#' cell, so a price is never raised above its original by the floor.
#'
#' @param P current prices, P1 original prices, S/S_prev current/previous
#'   shares (S_prev NULL at r = 1), W target, r iteration just evaluated,
#'   eligible logical per diameter class, floor matrix, V/V1 volume tallies of
#'   the latest and the first run.
update_prices <- function(P, P1, S, S_prev, W, r, eligible, floor, V, V1) {
  avail <- !is.na(P1)
  steer <- !is.na(W) & avail & matrix(eligible, nrow(P1), ncol(P1), byrow = TRUE)
  g <- if (r < 3) 0.95 else 0.95^(r - 2)
  Pn <- P1
  new <- P - g * (S - W)
  if (r >= 2 && !is.null(S_prev)) new[abs(S - S_prev) < 0.5] <- P[abs(S - S_prev) < 0.5]
  Pn[steer] <- new[steer]
  if (r + 1 > 10) {
    for (j in which(eligible)) {
      a <- avail[, j]
      if (!any(a) || sum(V[a, j]) <= 0 || sum(V1[a, j]) <= 0) next
      cbar_new <- sum(V[a, j] * Pn[a, j]) / sum(V[a, j])
      cbar_1 <- sum(V1[a, j] * P1[a, j]) / sum(V1[a, j])
      s <- steer[, j]
      Pn[s, j] <- Pn[s, j] - (cbar_new - cbar_1)
    }
  }
  Pn[steer] <- pmax(Pn[steer], floor[steer])
  Pn
}

# ---- tuning --------------------------------------------------------------------------

# The floor of a species: the highest pulpwood price of the species + 10 NOK.
# Returns list(value, product (name), price) -- value 0 when the species has no priced pulpwood.
.tg_floor_info <- function(products, species) {
  pd <- products$pd
  keys <- as.character(pd$ProductKey[pd$ProductType == "pulpwood" & pd$species_group == species])
  pm <- products$pm[products$pm$ProductKey %in% keys & !is.na(products$pm$PriceNOKperM3), , drop = FALSE]
  if (nrow(pm) == 0L) return(list(value = 0, product = NA_character_, price = NA_real_))
  i <- which.max(pm$PriceNOKperM3)
  list(value = pm$PriceNOKperM3[i] + 10, product = pd$ProductName[match(pm$ProductKey[i], pd$ProductKey)],
       price = pm$PriceNOKperM3[i])
}

#' Tune one or several sawlog products' prices towards target distributions.
#'
#' r = 1 bucks a sample of the job's stems with the original prices; each
#' further iteration updates the prices (update_prices()), swaps them into the
#' job and bucks the same sample again. Stops at max_iter, or when r >= 4 and
#' the best FG up to r exceeds the best up to r - 3 by less than 0.1 (only if
#' `early_stop`; with FALSE it always runs max_iter iterations). Returns the
#' iteration with the highest FG (on `basis`).
#'
#' Several products (restructure R3.1) are tuned together: every iteration
#' updates each product's grid with the rules above (eligibility, stagnation,
#' calibration, floor, all per product), applies them all, bucks the sample
#' once and tallies each product. The overall FG, used to choose the best
#' iteration and for the stop rule, is the mean of the products' FG weighted
#' by each product's total logs (basis "number") or price volume (basis
#' "volume") in iteration 1. With one product everything is as it was before:
#' same trace, prices and FG.
#'
#' @param job A hpr job (prepare_hpr_stems_job()).
#' @param products list(pd, lc, pm) the job was prepared with.
#' @param product_id One ProductKey, or several.
#' @param target_type "real", "neutral" or "long": one for all, or one per product.
#' @param hpr Passed to tuning_grid() (the file's classes and Distribution): one
#'   hpr_tuning_info() for one product, a list named by ProductKey for several.
#' @param distribution NULL, or the matrix for the "real" target (a list named
#'   by ProductKey for several products).
#' @param basis "number" or "volume" (one for all, or one per product); NULL:
#'   "volume" when target_type is "real" and the product's DistributionCategory
#'   is "Volume of logs", else "number".
#' @param early_stop TRUE (default): the stop rule above; FALSE: always max_iter
#'   iterations.
#' @param on_iter NULL, or function(r, max_iter, fg) called after each iteration
#'   (fg = that iteration's overall FG), e.g. for a progress display.
#' @param min_logs A diameter class is steered only if its first run has at
#'   least this many logs (counts, whatever the basis).
#' @return One product: list(best_price, best_iter, trace, eligible,
#'   fg_class_first, fg_class_best, settings, grid, W, floor (value, product,
#'   price, matrix), steered (cells), cells_at_floor_best, P_first, prices (by
#'   iteration), logs_first, by_product). Several: the same, with best_price,
#'   eligible, fg_class_*, grid, W, floor, steered, cells_at_floor_best,
#'   P_first, prices and logs_first as lists named by ProductKey, the trace with
#'   one `fg_<ProductKey>` column per product, and by_product (one list per
#'   product with its own, single-product-shaped results and `fg` by iteration).
tune_prices <- function(job, products, product_id, target_type, distribution = NULL,
                        n = 1000, seed = 1, max_iter = 30, min_logs = 20, basis = NULL,
                        hpr = NULL, workers = apter_workers(), early_stop = TRUE, on_iter = NULL) {
  ids <- as.character(product_id); multi <- length(ids) > 1L
  if (anyDuplicated(ids)) stop("tune_prices(): a product is listed twice.", call. = FALSE)
  st <- lapply(seq_along(ids), function(k) {
    id <- ids[k]
    ty <- if (length(target_type) > 1L) target_type[[k]] else target_type
    grid <- tuning_grid(products, id, if (multi) hpr[[id]] else hpr)
    W <- tuning_target(grid, ty, if (multi && is.list(distribution)) distribution[[id]] else distribution)
    bs <- if (is.null(basis)) NULL else if (length(basis) > 1L) basis[[k]] else basis
    if (is.null(bs)) bs <- if (ty == "real" && grid$basis_default == "volume") "volume" else "number"
    P1 <- grid$price
    fl <- .tg_floor_info(products, grid$species)
    list(id = id, type = ty, grid = grid, W = W, basis = bs, P1 = P1, P = P1, fl = fl, floor_m = pmin(P1, fl$value),
         S_prev = NULL, V1 = NULL, eligible = NULL, logs_first = NA_integer_, fg1 = NULL,
         fg_hist = numeric(0), fgs = list(), prices = list(), weight = NA_real_)
  })
  N <- length(job$pvs)
  set.seed(seed); idx <- sample(N, min(n, N))
  job_s <- subset_job_stems(job, idx)

  cur <- products
  trace <- list()
  fg_hist <- numeric(0)
  for (r in seq_len(max_iter)) {
    t0 <- proc.time()[["elapsed"]]
    if (r > 1L) job_s <- swap_job_prices(job_s, cur)
    res <- finish_job(job_s, buck_job(job_s, workers))
    sl <- res$selected_logs_all
    val_total <- sum(res$tree_summary$total_value_nok)
    v_cur_sum <- 0; v_one_sum <- 0; prod_vol <- 0; mx_inc <- 0; mx_dec <- 0
    tals <- vector("list", length(st)); fbs <- fns <- fvs <- vector("list", length(st))
    for (k in seq_along(st)) {
      s <- st[[k]]
      tal <- tally_grid(res, s$id, s$grid)
      f_b <- fg_grid(tal, s$W, s$basis); f_n <- fg_grid(tal, s$W, "number"); f_v <- fg_grid(tal, s$W, "volume")
      val_prod <- sum(sl$value_nok[as.character(sl$ProductKey) == s$id])
      a <- !is.na(s$P1)
      v_cur <- sum(tal$v[a] * s$P[a]); v_one <- sum(tal$v[a] * s$P1[a])
      if (r == 1L) {
        if (abs(val_prod - v_one) > 0.5)
          stop(sprintf("value check failed on iteration 1%s: the product's value in the result is %.3f NOK, sum(V*P1) is %.3f NOK (difference %.3f).",
                       if (multi) sprintf(" (%s)", s$id) else "", val_prod, v_one, val_prod - v_one), call. = FALSE)
        s$V1 <- tal$v; s$logs_first <- sum(tal$n)
        s$eligible <- colSums(tal$n) >= min_logs & colSums(!is.na(s$W)) > 0
        s$fg1 <- f_b$by_class
        s$weight <- if (s$basis == "volume") sum(tal$v) else sum(tal$n)
      }
      v_cur_sum <- v_cur_sum + v_cur; v_one_sum <- v_one_sum + v_one; prod_vol <- prod_vol + sum(tal$v)
      mx_inc <- max(mx_inc, max(0, s$P[a] - s$P1[a])); mx_dec <- max(mx_dec, max(0, s$P1[a] - s$P[a]))
      s$prices[[r]] <- s$P; s$fgs[[r]] <- f_b$by_class
      s$fg_hist <- c(s$fg_hist, f_b$overall)
      st[[k]] <- s; tals[[k]] <- tal; fbs[[k]] <- f_b; fns[[k]] <- f_n; fvs[[k]] <- f_v
    }
    wmean <- function(f) {                           # weighted by the iteration-1 logs or volume
      x <- vapply(f, function(z) z$overall, 0); w <- vapply(st, function(s) s$weight, 0)
      ok <- !is.na(x) & w > 0
      if (any(ok)) sum(w[ok] * x[ok]) / sum(w[ok]) else NA_real_
    }
    fg_all <- if (multi) wmean(fbs) else fbs[[1L]]$overall
    fg_n_all <- if (multi) wmean(fns) else fns[[1L]]$overall
    fg_v_all <- if (multi) wmean(fvs) else fvs[[1L]]$overall
    row <- data.frame(
      r = r, fg = fg_all, fg_number = fg_n_all, fg_volume = fg_v_all,
      value_orig = val_total - v_cur_sum + v_one_sum, value_total = val_total,
      product_volume_m3 = prod_vol, sawlog_volume_m3 = sum(sl$price_volume_m3[as.character(sl$ProductType) == "sawlog"]),
      max_increase = mx_inc, max_decrease = mx_dec, elapsed_s = NA_real_)
    if (multi)
      for (k in seq_along(st)) row[[paste0("fg_", st[[k]]$id)]] <- fbs[[k]]$overall
    trace[[r]] <- row
    fg_hist <- c(fg_hist, fg_all)
    if (is.function(on_iter)) on_iter(r, max_iter, fg_all)
    best_r <- which.max(fg_hist)
    if (early_stop && r >= 4L && max(fg_hist) - max(fg_hist[seq_len(r - 3L)]) < 0.1) { trace[[r]]$elapsed_s <- proc.time()[["elapsed"]] - t0; break }
    if (r == max_iter) { trace[[r]]$elapsed_s <- proc.time()[["elapsed"]] - t0; break }
    cur <- products
    for (k in seq_along(st)) {
      s <- st[[k]]
      Pn <- update_prices(s$P, s$P1, fbs[[k]]$S, s$S_prev, s$W, r, s$eligible, s$floor_m, tals[[k]]$v, s$V1)
      s$S_prev <- fbs[[k]]$S
      s$P <- Pn
      cur <- apply_grid_prices(cur, s$grid, s$P)
      st[[k]] <- s
    }
    trace[[r]]$elapsed_s <- proc.time()[["elapsed"]] - t0
  }
  trace <- do.call(rbind, trace)
  best <- which.max(trace$fg)
  by <- lapply(st, function(s) {
    Pb <- s$prices[[best]]
    steer <- !is.na(s$W) & !is.na(s$P1) & matrix(s$eligible, nrow(s$P1), ncol(s$P1), byrow = TRUE)
    at_floor <- steer & !is.na(Pb) & s$floor_m < s$P1 - 1e-9 & Pb <= s$floor_m + 1e-9
    list(best_price = Pb, eligible = setNames(s$eligible, colnames(s$P1)),
         fg_class_first = s$fg1, fg_class_best = s$fgs[[best]], grid = s$grid, W = s$W,
         floor = c(s$fl, list(matrix = s$floor_m)), steered = sum(steer),
         cells_at_floor_best = sum(at_floor), P_first = s$P1, prices = s$prices,
         logs_first = s$logs_first, fg = s$fg_hist, basis = s$basis, target_type = s$type)
  })
  names(by) <- ids
  settings <- list(product_id = product_id, target_type = target_type, basis = if (multi) vapply(st, function(s) s$basis, "") else st[[1L]]$basis,
                   n = n, sample_size = length(idx), stems = N, seed = seed, max_iter = max_iter, min_logs = min_logs, early_stop = early_stop)
  if (!multi) {
    b <- by[[1L]]
    return(list(best_price = b$best_price, best_iter = best, trace = trace, eligible = b$eligible,
                fg_class_first = b$fg_class_first, fg_class_best = b$fg_class_best, settings = settings,
                grid = b$grid, W = b$W, floor = b$floor, steered = b$steered,
                cells_at_floor_best = b$cells_at_floor_best, P_first = b$P_first, prices = b$prices,
                logs_first = b$logs_first, by_product = by))
  }
  field <- function(f) setNames(lapply(by, `[[`, f), ids)
  list(best_price = field("best_price"), best_iter = best, trace = trace, eligible = field("eligible"),
       fg_class_first = field("fg_class_first"), fg_class_best = field("fg_class_best"), settings = settings,
       grid = field("grid"), W = field("W"), floor = field("floor"), steered = field("steered"),
       cells_at_floor_best = field("cells_at_floor_best"), P_first = field("P_first"), prices = field("prices"),
       logs_first = field("logs_first"), by_product = by)
}
# ---- tuning in the app ----------------------------------------------------------------
# The app has no .hpr file at hand, only the imported scenario (price matrix with
# the hidden Distribution column, defs with the hidden DistributionCategory).

#' The number of stems to tune on: all of them when the product has at most
#' `target_logs` logs, else the share that gives about `target_logs` logs.
#' Never more than n_stems, never less than 1.
tuning_sample_size <- function(n_stems, product_logs, target_logs = 1000) {
  n <- if (product_logs <= target_logs) n_stems else ceiling(n_stems * target_logs / product_logs)
  max(1L, min(as.integer(n_stems), as.integer(n)))
}

#' The `hpr` argument of tuning_grid()/tune_prices() from a scenario alone.
#'
#' The diameter class of a whole-cm row is its hidden FileDiameterClass_mm (the
#' lower limit of the .hpr/.pin class it was spread from); a row without one
#' (not from a file, or a row added in the app) is its own class, whole cm x 10.
#' Rows are never merged. The Distribution of a class is that of its first row.
#'
#' @param defs Assortment table (DistributionCategory is read from it, NA if absent).
#' @param pm Price matrix of the scenario (long, hidden Distribution and
#'   FileDiameterClass_mm columns optional).
#' @return list(cells (d_mm, l_cm, dist), category), as hpr_tuning_info().
tuning_hpr_info <- function(defs, pm, product_id) {
  rows <- .tg_rows(pm, product_id)
  if (nrow(rows) == 0L) stop("No matrix-priced product '", product_id, "' in the price matrix.")
  lengths <- sort(unique(rows$LengthClass_dm))
  cm <- sort(unique(rows$PriceDiameterClass_cm))
  col <- function(n) if (n %in% names(rows)) rows[[n]] else rep(NA_real_, nrow(rows))
  fc <- col(PM_FILE_CLASS_COL); dist_all <- col(PM_DISTRIBUTION_COL)
  cls <- vapply(cm, function(c) { v <- fc[rows$PriceDiameterClass_cm == c & !is.na(fc)]
                                  if (length(v)) v[1L] else c * 10 }, 0)
  first <- !duplicated(cls)                                   # one row per class (the first, in cm order)
  cells <- do.call(rbind, lapply(which(first), function(i) {
    r <- rows[rows$PriceDiameterClass_cm == cm[i], , drop = FALSE]
    data.frame(d_mm = cls[i], l_cm = lengths * 10, dist = dist_all[rows$PriceDiameterClass_cm == cm[i]][match(lengths, r$LengthClass_dm)])
  }))
  key <- match(product_id, assortment_table_keys(defs))
  cat_ <- if (!is.na(key) && "DistributionCategory" %in% names(defs)) as.character(defs$DistributionCategory[key]) else NA_character_
  list(cells = cells, category = cat_)
}

#' The products of a scenario that can be tuned: sawlog type, matrix-priced, more
#' than one priced length class and more than one diameter class (the derived
#' file classes with a price).
#' @return data.frame(product_id, name, species, has_file_target); has_file_target =
#'   the product has a Distribution value > 0 in pm.
tuning_products <- function(defs, pm) {
  keys <- assortment_table_keys(defs)
  out <- list()
  for (i in seq_along(keys)) {
    if (!identical(as.character(defs$Type[i]), "sawlog") || !identical(as.character(defs$PriceType[i]), "matrix")) next
    rows <- .tg_rows(pm, keys[i])
    ok <- rows[!is.na(rows$PriceNOKperM3), , drop = FALSE]
    if (nrow(ok) == 0L || length(unique(ok$LengthClass_dm)) < 2L) next
    cls <- unique(tuning_hpr_info(defs, pm, keys[i])$cells$d_mm)
    cls_ok <- unique(file_class_of_cm(ok$PriceDiameterClass_cm, cls))
    if (length(cls_ok) < 2L) next
    d <- if (PM_DISTRIBUTION_COL %in% names(rows)) rows[[PM_DISTRIBUTION_COL]] else NA_real_
    out[[length(out) + 1L]] <- data.frame(product_id = keys[i], name = as.character(defs$Name[i]),
                                          species = as.character(defs$Species[i]),
                                          has_file_target = any(d > 0, na.rm = TRUE), stringsAsFactors = FALSE)
  }
  if (length(out)) do.call(rbind, out)
  else data.frame(product_id = character(0), name = character(0), species = character(0), has_file_target = logical(0))
}

#' The matrix of one tuned product in one of the tuning tab's four views
#' (restructure R3.4): diameter classes (file class, mm) in rows, length
#' classes (cm) in columns, like the price grid.
#'
#' @param by One element of tune_prices()$by_product extended with the run's
#'   shares (see tuning_run()): `W` (target), `S_orig`, `S_tuned` (shares in
#'   percent, length x class), `fg_orig`, `fg_tuned` (FG per class),
#'   `P1`, `Pb` (prices before and after), `eligible`, `grid`.
#' @param view "target", "orig", "tuned" or "change".
#' @return list(m = classes x lengths numeric (NA: no value), fg = FG per class
#'   or NULL, digits): shares and prices rounded to whole numbers.
tuning_matrix_view <- function(by, view = c("target", "orig", "tuned", "change")) {
  view <- match.arg(view)
  g <- by$grid
  lens <- g$lengths * 10; cls <- g$diam_classes
  m <- switch(view,
    target = by$W, orig = by$S_orig, tuned = by$S_tuned,
    change = { steer <- !is.na(by$W) & !is.na(by$P1) & matrix(by$eligible, nrow(by$P1), ncol(by$P1), byrow = TRUE)
               d <- by$Pb - by$P1; d[!steer] <- NA_real_; d })
  m <- round(t(m))
  m[t(is.na(g$price))] <- NA_real_                           # a cell without a price has no value
  dimnames(m) <- list(cls, lens)
  fg <- switch(view, orig = by$fg_orig, tuned = by$fg_tuned, NULL)
  list(m = m, fg = if (is.null(fg)) NULL else unname(fg[as.character(cls)]), classes = cls, lengths_cm = lens)
}
