# ==============================================================================
# R/helpers_check.R  (roadmap 8, step 4, 2026-11-04)
#
# Purpose : The bucking check (tab 5): how a harvester bucked the stems of an .hpr file
#           compared with the best possible bucking under the file's own instruction
#           (products, prices, permitted grades). One engine: the optimum comes from the
#           shared bucking core (prepare_hpr_stems_job() / buck_job()); the recorded logs
#           are valued with exactly the same price volume, price class and price lookup
#           (value_recorded_logs()), so the two differ only by the cutting pattern.
#
# Rules   : * Instruction = the file's own: hpr_to_scenario() of the file, grades on
#             (use_grades), no forced cut at grade changes, recorded waste logs are defect
#             zones where only waste may be cut (reject_zones; the old optBuck's grade -1).
#           * Waste = the file's unclassified product (found by definition, not by number).
#           * Recorded logs are valued even when outside the product's limits (counted).
#           * Manual cut = CuttingReason other than "Automatic" and "Spinning".
#           * Bucking yield index (apteringsgrad) = sum recorded value / sum best possible
#             value over the checked stems.
# Backend text is English (the app translates messages, tr_msg()).
# ==============================================================================

# A stem whose recorded logs (waste included) cover less than this share of its measured length is
# left out of the check (incomplete data, e.g. one 3 dm waste piece on 6 m of profile).
CHECK_MIN_COVER <- 0.5

# Value the recorded logs like candidates of the optimiser.
#
# For each recorded log: top diameter at the product's DiameterTopPosition (under bark if the
# product says so), price volume by the product's method, price class floor(top/10), price from
# the run's price lookup, value. Diameters at off-grid positions are interpolated (as the engine
# does for the Mid method); solid volumes at off-grid positions by interpolating the cumulative
# 10 cm cylinder volume.
#
# @param job     A prepared hpr stems job (prepare_hpr_stems_job()).
# @param logs    read_hpr_logs()$logs, restricted to the stems of the job.
# @param prodmap data.frame(HprProductKey, Species, key) mapping a file product + species to the
#   assortment key of the job's products.
# @param rec_prices NULL, or hpr_rec_prices(): the file's own price grid by length class (cm) and diameter row
#   (decided 2026-11-04: a recorded log in a "manual only" cell or in a length class the import drops is valued
#   with the file's price; the optimiser still cannot choose such cells). NULL: the job's price lookup.
# @return data.frame, one row per log of `logs` (same order): ProductKey (assortment key, NA for
#   waste or unknown), is_waste, unknown_product, StartPos, StopPos, LogLength, price_volume_m3,
#   PriceDiameterClass_cm, PriceNOKperM3, value_nok, commercial_volume_m3, TopUb_calc,
#   TopOb_calc, outside (outside the product's limits).
value_recorded_logs <- function(job, logs, prodmap, hpr_products = NULL, rec_prices = NULL) {
  run <- job$prep$runs[[1L]]; lookup <- run$lookup
  n <- nrow(logs)
  out <- data.frame(akey = rep(NA_character_, n), unknown_product = rep(FALSE, n),
                    price_volume_m3 = NA_real_, PriceDiameterClass_cm = NA_integer_,
                    PriceNOKperM3 = NA_real_, value_nok = 0, commercial_volume_m3 = 0, class_dm = NA_real_, class_cm = NA_real_,
                    TopUb_calc = NA_real_, TopOb_calc = NA_real_, outside = FALSE,
                    stringsAsFactors = FALSE)
  by_stem <- split(seq_len(n), logs$stem_key)
  for (sk in names(by_stem)) {
    pv <- job$pvs[[sk]]; if (is.null(pv)) next
    ii <- by_stem[[sk]]
    sp <- pv$species_group
    plan <- run$own_plan[[sp]]; if (is.null(plan) || !is.null(plan$error)) next
    pk_all <- vapply(plan$products, `[[`, "", "pk")
    pos <- as.numeric(pv$diameterPosition)
    f_ob <- stats::approxfun(pos, as.numeric(pv$DiameterValue), rule = 2)
    f_ub <- stats::approxfun(pos, as.numeric(pv$DiameterValueUB), rule = 2)
    cum_of <- function(d) {                       # cumulative 10 cm cylinder volume at any position
      tops <- seq(10, floor(max(pos) / 10) * 10, by = 10)
      cv <- c(0, cumsum(pi * (stats::approx(pos, d, xout = tops, rule = 2)$y / 2)^2 * 10 / 1e8))
      stats::approxfun(c(0, tops), cv, rule = 2)
    }
    cum_ub <- cum_of(as.numeric(pv$DiameterValueUB)); cum_ob <- cum_of(as.numeric(pv$DiameterValue))
    for (k in ii) {
      lk <- logs[k, ]
      out$TopUb_calc[k] <- f_ub(lk$StopPos); out$TopOb_calc[k] <- f_ob(lk$StopPos)
      if (isTRUE(lk$waste)) next
      m <- prodmap$key[prodmap$HprProductKey == lk$ProductKey & prodmap$Species == sp]
      if (length(m) == 0L || !m[1L] %in% pk_all) { out$unknown_product[k] <- TRUE; next }
      p <- plan$products[[match(m[1L], pk_all)]]
      out$akey[k] <- p$pk
      ser <- if (isTRUE(p$ub)) f_ub else f_ob
      top <- ser(lk$StopPos - p$top_offset); butt <- ser(lk$StartPos)
      # The log's length class: the file's own class (largest lower limit <= the length; margins are
      # 0 in the owner's files), mapped to optBuck's whole-dm column by the import's rule (rounded up).
      # Without the file's classes the cut length decides.
      hp <- if (!is.null(hpr_products)) hpr_products[[lk$ProductKey]]
      lcl <- if (!is.null(hp) && length(hp$len_classes)) hp$len_classes[hp$len_classes <= lk$LogLength] else numeric(0)
      if (!is.null(hp) && length(hp$len_classes)) {
        cls_dm <- if (length(lcl)) ceiling(max(lcl) / 10) else NA_real_
        allowed_dm <- p$lengths / 10
        if (!is.na(cls_dm) && !cls_dm %in% allowed_dm) { below <- allowed_dm[allowed_dm <= cls_dm]; cls_dm <- if (length(below)) max(below) else NA_real_ }
        len_out <- !length(lcl) || (!is.na(hp$len_class_max) && lk$LogLength > hp$len_class_max)   # outside the FILE's length classes
      } else { cls_dm <- floor(lk$LogLength / 10); len_out <- lk$LogLength < p$min_len || lk$LogLength > p$max_len }
      out$class_dm[k] <- cls_dm
      out$class_cm[k] <- if (!is.null(hp) && length(hp$len_classes)) (if (length(lcl)) max(lcl) else NA_real_) else floor(lk$LogLength / 10) * 10
      mid <- if (identical(p$method, "Mid")) ser((lk$StartPos + lk$StopPos) / 2)
      cum <- if (isTRUE(p$ub)) cum_ub else cum_ob
      pvol <- tryCatch(price_volume_m3(method = p$method, top_d_mm = top,
                                       length_cm = if (is.null(p$price_len) || is.na(cls_dm)) lk$LogLength else
                                         (if (length(lcl)) max(lcl) else NA_real_) %|NA|% lk$LogLength,   # class-priced: the class lower limit
                                       start_pos = lk$StartPos, stop_pos = lk$StopPos,
                                       solid_vol = function(a, b) cum(b) - cum(a),
                                       adjustment = p$adjustment, mid_d_mm = mid), error = function(e) NA_real_)
      out$price_volume_m3[k] <- pvol
      out$PriceDiameterClass_cm[k] <- as.integer(floor(top / 10))
      out$outside[k] <- len_out || top < p$min_top || top > p$max_top || butt < p$min_butt || butt > p$max_butt
      out$commercial_volume_m3[k] <- cum_ub(lk$StopPos) - cum_ub(lk$StartPos)
    }
  }
  has <- !is.na(out$akey)
  if (any(has)) {
    df <- data.frame(ProductKey = out$akey[has], LengthClass_dm = as.integer(out$class_dm[has]),
                     PriceDiameterClass_cm_uncapped = out$PriceDiameterClass_cm[has], stringsAsFactors = FALSE)
    pr <- if (is.null(rec_prices)) lookup_prices(df, lookup) else {
      d <- pmin(out$PriceDiameterClass_cm[has], rec_prices$cap[out$akey[has]])
      unname(rec_prices$price[paste(out$akey[has], out$class_cm[has], d)])
    }
    out$PriceNOKperM3[has] <- pr
    v <- out$price_volume_m3[has] * pr
    out$value_nok[has] <- ifelse(is.na(v), 0, v)
  }
  out
}

# The file's own price grid for valuing recorded logs (decided 2026-11-04): every priced cell of the product,
# also cells marked "Only logs bucked manually allowed" and file length classes the import drops (no whole-dm
# length inside the class), by the file's length class lower limit (cm) and whole-cm diameter row (the import's
# row rule). Forbidden cells and positions outside the matrix have no price. The optimiser never gets the extra cells.
hpr_rec_prices <- function(h, defs) {
  akeys <- assortment_table_keys(defs)
  g <- do.call(rbind, lapply(seq_len(nrow(defs)), function(i) {
    p <- h$products[[defs$HprProductKey[i]]]; if (is.null(p)) return(NULL)
    cells <- p$cells[!is.na(p$cells$d_mm) & !is.na(p$cells$l_cm), , drop = FALSE]
    ok <- cells[!is.na(cells$price) & !cells$criteria %in% HPR_FORBIDDEN, , drop = FALSE]
    if (nrow(ok) == 0L) return(NULL)
    D <- sort(unique(ok$d_mm)); D_all <- sort(unique(cells$d_mm)); L <- sort(unique(ok$l_cm))
    rows_cm <- seq(floor(min(D) / 10), floor(max(D) / 10))
    cls <- vapply(rows_cm, function(cc) { u <- D_all[D_all <= cc * 10 & D_all >= min(D)]; if (length(u)) max(u) else min(D) }, 0)
    gg <- expand.grid(i = seq_along(rows_cm), j = seq_along(L), KEEP.OUT.ATTRS = FALSE)
    data.frame(key = akeys[i], L_cm = L[gg$j], d_cm = rows_cm[gg$i],
               price = ok$price[match(paste(cls[gg$i], L[gg$j]), paste(ok$d_mm, ok$l_cm))], stringsAsFactors = FALSE)
  }))
  if (is.null(g)) g <- data.frame(key = character(0), L_cm = numeric(0), d_cm = numeric(0), price = numeric(0))
  list(price = setNames(g$price, paste(g$key, g$L_cm, g$d_cm)), cap = tapply(g$d_cm, g$key, max))
}

`%|NA|%` <- function(a, b) if (length(a) == 0L || is.na(a)) b else a

# Share (0-1) helper that returns NA for an empty denominator.
.share <- function(a, b) if (is.finite(b) && b > 0) a / b else NA_real_

# A product counts as "used" in the bucking check when it has at least this share of the recorded
# (non-waste) logs of a species (0.3.1; was: at least one log).
CHECK_USED_MIN_SHARE <- 0.01

# The ProductKeys the harvester used in a file: for each species, the products with at least
# `min_share` of that species' recorded non-waste logs; a product used for any species counts.
#
# @param logs       Recorded logs (read_hpr_logs()$logs: waste, ProductKey, species_key).
# @param species_map Named character: species group key -> "spruce", "pine" or "none".
# @param min_share  Smallest share (0-1) of a species' logs.
# @return Character vector of ProductKeys.
check_used_products <- function(logs, species_map,
                                min_share = getOption("optbuck.check_used_min_share", CHECK_USED_MIN_SHARE)) {
  lg <- logs[!logs$waste, , drop = FALSE]
  if (nrow(lg) == 0L) return(character(0))
  sp <- unname(species_map[as.character(lg$species_key)])
  sp[is.na(sp)] <- "none"
  tab <- unclass(table(species = sp, key = as.character(lg$ProductKey)))
  share <- tab / pmax(rowSums(tab), 1)
  colnames(share)[colSums(share >= min_share - 1e-12) > 0L]
}

# The bucking check of one .hpr file.
#
# @param path        .hpr (or .hpr.gz / .zip with an .hpr).
# @param species_map Named character (species group key -> "spruce"/"pine"/"none"); default: guessed.
# @param filename    Name for messages.
# @param reject_zones TRUE: recorded waste logs are defect zones (the decision of 2026-11-04).
# @param used_only  TRUE (default; the tab has no box): the best possible bucking may use only the products
#   the harvester used (check_used_products(): at least 1 % of a species' recorded logs); FALSE: every product of the file's instruction.
# @param manual_ok  TRUE (default): the best possible bucking may also use cells marked "Only logs bucked
#   manually allowed" (with the file's price): the operator was allowed to cut them. FALSE: as the simulator.
# @param stage       "all" (default: prepare, buck, finish in this process); "prepare" returns a `check_prep`
#   object (or a final result when the file cannot be checked) without bucking; "finish" takes `prep` and the
#   chunk results `b` of buck_job() (the app runs the stages as background jobs: app/helpers_jobs.R).
# @param progress    NULL or function(step) called with "read", "opt", "val", "tab".
# @return list(ok, errors, warnings, info, no_profile, file, overview, stems, logs, key_figures,
#   assortments, length_dist, manual, defect, top, excluded, flagged, control, broken).
bucking_check <- function(path, species_map = NULL, filename = basename(path), reject_zones = TRUE,
                          used_only = TRUE, manual_ok = TRUE, progress = NULL, root = get_root_dir(),
                          stage = "all", prep = NULL, b = NULL) {
  prog <- function(step) if (is.function(progress)) progress(step)
  fail <- function(e, w = character(0)) list(ok = FALSE, errors = e, warnings = w, info = character(0))
  if (identical(stage, "finish")) {
    .progress <- progress; .b <- b; list2env(unclass(prep), environment())   # everything prepare computed
    progress <- .progress; b <- .b; stage <- "finish"
    prog <- function(step) if (is.function(progress)) progress(step)
  } else {
  prog("read")
  h <- read_hpr(path, filename); if (!isTRUE(h$ok)) return(fail(h$errors, h$warnings))
  smap <- if (is.null(species_map)) setNames(guess_hpr_species(h$species$name, h$species$user_id), h$species$key) else species_map
  rl <- read_hpr_logs(path, filename, defs = h); if (!isTRUE(rl$ok)) return(fail(rl$errors))
  # Every product with a recorded log is in the scenario, so each recorded log is valued; the best possible
  # bucking may use only the products the harvester used (check_used_products(): >= 1 % of a species' logs).
  keep <- if (used_only) unique(rl$logs$ProductKey[!rl$logs$waste]) else NULL
  sc <- hpr_to_scenario(h, smap, keep = keep, manual_ok = manual_ok); if (!isTRUE(sc$ok)) return(fail(sc$errors, sc$warnings))
  pm <- pm_minimal(sc$pm); defs <- sc$defs; pr <- products_from_defs(defs, pm); pd <- pr$pd
  used <- if (used_only) check_used_products(rl$logs, smap) else NULL
  drop_unused <- !is.null(used) && any(!is.na(defs$HprProductKey) & !defs$HprProductKey %in% used)
  lg <- rl$logs
  warnings <- c(h$warnings, sc$warnings, rl$warnings); info <- c(sc$info, rl$info)
  akeys <- assortment_table_keys(defs)
  prodmap <- data.frame(HprProductKey = defs$HprProductKey, Species = defs$Species, key = akeys, stringsAsFactors = FALSE)
  ptype <- setNames(as.character(defs$Type), akeys); pname <- setNames(as.character(defs$Name), akeys)
  lg$species <- unname(smap[lg$species_key])
  # Recorded product of each log: the assortment key (file product + stem species).
  mk <- match(paste(lg$ProductKey, lg$species), paste(prodmap$HprProductKey, prodmap$Species))
  lg$akey <- ifelse(lg$waste, NA_character_, prodmap$key[mk])
  lg$ptype <- ifelse(lg$waste, "waste", unname(ptype[lg$akey]))
  lg$pname <- ifelse(lg$waste, "Unclassified", unname(pname[lg$akey]))

  # ---- the overview (needs no stem profiles) -------------------------------------------------------
  pg <- hpr_permitted_grades(h)
  saw_keys <- akeys[defs$Type %in% "sawlog"]
  saw_grades <- lapply(split(seq_len(nrow(defs)), defs$Species), function(i) {   # grades some sawlog product permits
    d <- defs[i, , drop = FALSE]; d <- d[d$Type %in% "sawlog", , drop = FALSE]
    if (nrow(d) == 0L) return(numeric(0))
    g <- lapply(d$PermittedGrades, permitted_grades_set)
    if (any(lengths(g) == 0L)) NA else sort(unique(unlist(g)))     # NA = a sawlog product permits all grades
  })
  grade_flag <- function(sk, g) {                                   # TRUE when no sawlog product of the species permits grade g
    sp <- unname(smap[sk]); sg <- saw_grades[[sp]]
    if (is.null(sg) || (length(sg) == 1L && is.na(sg))) FALSE else !g %in% sg
  }
  pg$not_sawlog <- if (nrow(pg)) mapply(function(sk, g) if (is.na(g) || is.na(sk)) NA else grade_flag(sk, g), pg$species_key, pg$grade) else logical(0)
  prods <- do.call(rbind, c(list(data.frame(ProductKey = character(0), ProductName = character(0), Species = character(0),
         Type = character(0), MinTop_cm = numeric(0), MaxTop_cm = numeric(0), MinLength_cm = numeric(0), MaxLength_cm = numeric(0),
         Condition = character(0), Grades = character(0), stringsAsFactors = FALSE)),
    lapply(seq_len(nrow(defs)), function(i) data.frame(ProductKey = defs$HprProductKey[i], ProductName = defs$Name[i],
         Species = defs$Species[i], Type = defs$Type[i], MinTop_cm = defs$MinTopDiameter_cm[i], MaxTop_cm = defs$MaxTopDiameter_cm[i],
         MinLength_cm = defs$MinLength_cm[i], MaxLength_cm = defs$MaxLength_cm[i],
         Condition = defs$ButtCondition[i], Grades = defs$PermittedGrades[i], stringsAsFactors = FALSE))))
  overview <- list(species = h$species[, c("key", "name", "user_id")], species_map = smap, products = prods,
                   permitted_grades = pg, grade_names = h$grade_names, unclassified_keys = h$unclassified_keys)

  n_stems_file <- length(unique(lg$stem_key))
  base <- list(ok = TRUE, errors = character(0), warnings = warnings, info = info, file = filename,
               overview = overview, n_logs = nrow(lg), n_stems_file = n_stems_file, no_profile = FALSE,
               defs = defs, ptype = ptype, pname = pname, used_only = used_only,
               n_products = length(unique(defs$HprProductKey[!defs$Type %in% "waste"])))

  # ---- stems ----------------------------------------------------------------------------------------------
  st <- read_hpr_stems(path, filename, defs = h)
  if (!isTRUE(st$ok) || all(st$stems$n_points == 0L)) {
    base$no_profile <- TRUE
    base$logs <- lg
    base$assortments <- check_assortments(NULL, NULL, ptype, lg_recorded = lg)
    base$manual <- check_manual(lg, NULL)
    base$excluded <- data.frame(treid = unique(lg$stem_key), reason = "no stem profile (StemDiameters)", stringsAsFactors = FALSE)
    return(base)
  }
  reject <- if (reject_zones) { w <- lg[lg$waste, , drop = FALSE]
    data.frame(stem_key = w$stem_key, start = w$StartPos, length = w$LogLength, stringsAsFactors = FALSE) }
  # The products the best possible bucking may use (waste rows always).
  pm_o <- pm; pr_o <- pr
  if (drop_unused) {
    defs_o <- defs[is.na(defs$HprProductKey) | defs$HprProductKey %in% used, , drop = FALSE]
    pm_o <- pm[pm$ProductKey %in% assortment_table_keys(defs_o), , drop = FALSE]
    pr_o <- products_from_defs(defs_o, pm_o)
  }
  price_path <- tempfile(fileext = ".csv"); pm_export(pm_o, price_path); on.exit(unlink(price_path), add = TRUE)
  opts <- list(use_grades = TRUE, cut_at_changes = FALSE, reject_zones = reject_zones)
  job <- prepare_hpr_stems_job(st, smap, price_matrix_path = price_path, root = root, products = pr_o$pd,
                               length_classes = pr_o$lc, grade_opts = opts, reject = reject)
  # The recorded logs are valued with all the file's products (same profiles, other runs).
  job_rec <- if (drop_unused) swap_job_prices(job, list(pd = pr$pd, lc = pr$lc, pm = pm), root = root) else job
  if (identical(stage, "prepare")) return(structure(as.list(environment()), class = "check_prep"))
  }
  prog("opt")
  res <- finish_job(job, if (is.null(b)) buck_job(job) else b)
  opt <- res$selected_logs_all; tsum <- res$tree_summary
  prog("val")
  in_job <- names(job$pvs)
  lgj <- lg[lg$stem_key %in% in_job, , drop = FALSE]
  vr <- value_recorded_logs(job_rec, lgj, prodmap, hpr_products = h$products, rec_prices = hpr_rec_prices(h, defs))
  rec <- cbind(lgj, vr)
  rec$species_group <- vapply(rec$stem_key, function(k) job$pvs[[k]]$species_group, "")
  prog("tab")

  # ---- stem table, exclusions, flags ------------------------------------------------------------------
  pd_o <- pr_o$pd                                  # the products the best possible bucking uses
  min_saw_top <- vapply(split(seq_len(nrow(pd_o)), pd_o$species_group), function(i) {
    d <- pd_o[i, , drop = FALSE]; d <- d[d$ProductType %in% "sawlog", , drop = FALSE]
    if (nrow(d)) min(d$MinTopDiameter) else NA_real_ }, 0)
  sk <- in_job
  stems <- data.frame(StemKey = sk, species_group = vapply(sk, function(k) job$pvs[[k]]$species_group, ""),
                      stringsAsFactors = FALSE)
  last_ub <- vapply(sk, function(k) { pv <- job$pvs[[k]]; as.numeric(utils::tail(pv$DiameterValueUB, 1L)) }, 0)
  top_end <- vapply(sk, function(k) max(job$pvs[[k]]$diameterPosition), 0)
  stems$last_ub_mm <- last_ub
  stems$measured_cm <- top_end
  stems$logged_cm <- as.numeric(tapply(rec$LogLength, factor(rec$stem_key, levels = sk), sum))
  stems$logged_cm[is.na(stems$logged_cm)] <- 0
  stems$n_logs_rec <- as.integer(table(factor(rec$stem_key, levels = sk)))
  above <- tapply(rec$StopPos, factor(rec$stem_key, levels = sk), function(x) if (length(x)) max(x) else NA_real_)
  stems$log_above_profile <- !is.na(above) & unname(above) > top_end + 1e-9
  mst <- min_saw_top[stems$species_group]
  stems$possible_broken_top <- !is.na(mst) & last_ub >= mst
  n_broken <- sum(stems$possible_broken_top)
  broken <- list(n = n_broken, n_stems = nrow(stems), share = .share(n_broken, nrow(stems)),
                 excluded = FALSE)   # never excluded on this rule, only flagged (decided 2026-11-04)
  stems$reason <- NA_character_
  stems$reason[stems$n_logs_rec == 0L] <- "no recorded logs"
  stems$reason[is.na(stems$reason) & stems$logged_cm < CHECK_MIN_COVER * stems$measured_cm] <- "recorded logs cover less than half of the measured stem"
  stems$reason[is.na(stems$reason) & stems$log_above_profile] <- "a recorded log ends above the measured profile"
  oks <- setNames(tsum$optimizer_status, as.character(tsum$treid))
  stems$opt_status <- unname(oks[stems$StemKey])
  stems$value_rec <- as.numeric(tapply(rec$value_nok, factor(rec$stem_key, levels = sk), sum))
  stems$value_opt <- as.numeric(tapply(opt$value_nok, factor(as.character(opt$treid), levels = sk), sum))
  stems$value_opt[is.na(stems$value_opt)] <- 0
  stems$reason[is.na(stems$reason) & !stems$opt_status %in% "ok"] <- "no best bucking found (no priced candidates)"
  stems$use <- is.na(stems$reason)
  stems$com_rec <- as.numeric(tapply(rec$commercial_volume_m3, factor(rec$stem_key, levels = sk), sum))
  stems$com_opt <- as.numeric(tapply(opt$commercial_volume_m3, factor(as.character(opt$treid), levels = sk), sum))
  stems$com_opt[is.na(stems$com_opt)] <- 0
  man <- tapply(rec$manual %in% TRUE, factor(rec$stem_key, levels = sk), sum)
  stems$n_manual <- as.integer(man); stems$has_manual <- stems$n_manual > 0L
  stems$yield <- ifelse(stems$use & stems$value_opt > 0, stems$value_rec / stems$value_opt, NA_real_)
  stems$rec_gt_opt <- stems$use & stems$value_opt > 0 & stems$value_rec > stems$value_opt + 1e-9
  stems$outside <- as.logical(tapply(rec$outside, factor(rec$stem_key, levels = sk), any)) %in% TRUE
  use_keys <- stems$StemKey[stems$use]

  excluded <- rbind(
    data.frame(treid = as.character(job$meta$excluded$treid), reason = job$meta$excluded$excluded_reason, stringsAsFactors = FALSE),
    data.frame(treid = stems$StemKey[!stems$use], reason = stems$reason[!stems$use], stringsAsFactors = FALSE))
  flagged <- list(
    broken_top = stems$StemKey[stems$possible_broken_top],
    outside = stems$StemKey[stems$use & stems$outside], rec_gt_opt = stems$StemKey[stems$rec_gt_opt])

  recu <- rec[rec$stem_key %in% use_keys, , drop = FALSE]
  optu <- opt[as.character(opt$treid) %in% use_keys, , drop = FALSE]
  optu$ptype <- unname(ptype[as.character(optu$ProductKey)])
  optu$pname <- unname(pname[as.character(optu$ProductKey)])
  optu$stem_key <- as.character(optu$treid)

  # key figures per species and total
  kf1 <- function(s, label = NULL, drop_broken = FALSE) {
    ii <- if (is.null(s)) rep(TRUE, nrow(stems)) else stems$species_group == s
    ii <- ii & stems$use & stems$value_opt > 0
    if (drop_broken) ii <- ii & !stems$possible_broken_top
    v_r <- sum(stems$value_rec[ii]); v_o <- sum(stems$value_opt[ii])
    data.frame(scope = if (!is.null(label)) label else if (is.null(s)) "All species" else s, stems = sum(ii), value_rec = v_r, value_opt = v_o,
               com_rec = sum(stems$com_rec[ii]), com_opt = sum(stems$com_opt[ii]),
               value_per_m3_rec = .share(v_r, sum(stems$com_rec[ii])), value_per_m3_opt = .share(v_o, sum(stems$com_opt[ii])),
               lost = v_o - v_r, yield = .share(v_r, v_o), stringsAsFactors = FALSE)
  }
  key_figures <- do.call(rbind, c(list(kf1(NULL)), lapply(sort(unique(stems$species_group)), kf1),
                                   list(kf1(NULL, label = "without_broken", drop_broken = TRUE))))   # last row: yield index without the flagged stems

  list_out <- c(base, list(
    stems = stems, logs = list(recorded = rec, optimal = opt), key_figures = key_figures, broken = broken,
    excluded = excluded, flagged = flagged,
    n_incomplete = sum(stems$reason %in% "recorded logs cover less than half of the measured stem"),
    n_left_out = sum(!grepl("^multi-tree|set to Not used$", excluded$reason)),   # incomplete or deviating data (not other species, not multi-tree)
    job = job, prodmap = prodmap, min_saw_top = min_saw_top,
    assortments = check_assortments(recu, optu, ptype),
    manual = check_manual(recu, stems),
    top = check_top(job, recu, optu),
    defect = check_defect(job, recu, pd, defs, smap, saw_grades),
    length_dist = check_length_dist(recu, optu, defs, pm, pr, ptype),
    control = check_control(recu)))
  list_out
}

# ---- tables ---------------------------------------------------------------------------------------------------

# Per assortment: logs, settlement (price) volume, commercial volume and value, recorded vs best
# possible; sawlog share of commercial volume.
check_assortments <- function(rec, opt, ptype, lg_recorded = rec) {
  agg <- function(key, type, name, pv, com, val) {
    if (length(key) == 0L) return(NULL)
    f <- factor(paste(key, name, sep = "\r"))
    data.frame(ProductName = sub("^.*\r", "", levels(f)), key = sub("\r.*$", "", levels(f)),
               logs = as.integer(table(f)), price_m3 = as.numeric(tapply(pv, f, sum, na.rm = TRUE)),
               commercial_m3 = as.numeric(tapply(com, f, sum, na.rm = TRUE)),
               value = as.numeric(tapply(val, f, sum, na.rm = TRUE)),
               type = as.character(tapply(type, f, function(x) x[1L])), stringsAsFactors = FALSE)
  }
  if (is.null(opt)) {            # logs-only file: recorded counts and the harvester's own volumes
    r <- agg(ifelse(is.na(lg_recorded$akey), "waste", lg_recorded$akey), lg_recorded$ptype, lg_recorded$pname,
             lg_recorded$m3_price, lg_recorded$m3sub, rep(0, nrow(lg_recorded)))
    return(list(recorded = r, optimal = NULL, saw_share_rec = NA_real_, saw_share_opt = NA_real_))
  }
  r <- agg(ifelse(is.na(rec$akey), "waste", rec$akey), rec$ptype, rec$pname, rec$price_volume_m3, rec$commercial_volume_m3, rec$value_nok)
  o <- agg(as.character(opt$ProductKey), opt$ptype, opt$pname, opt$price_volume_m3, opt$commercial_volume_m3, opt$value_nok)
  sh <- function(d) { d <- d[!d$type %in% "waste", , drop = FALSE]; .share(sum(d$commercial_m3[d$type %in% "sawlog"]), sum(d$commercial_m3)) }
  list(recorded = r, optimal = o, saw_share_rec = sh(r), saw_share_opt = sh(o))
}

# Manual cuts: share of all logs, of sawlogs, of butt logs that are sawlogs; yield index of stems
# with and without manual cuts.
check_manual <- function(lg, stems) {
  m <- lg$manual %in% TRUE; known <- !is.na(lg$manual)
  saw <- lg$ptype %in% "sawlog"; butt <- lg$LogKey == 1
  out <- list(all = .share(sum(m), sum(known)), sawlogs = .share(sum(m & saw), sum(known & saw)),
              butt_sawlogs = .share(sum(m & saw & butt), sum(known & saw & butt)),
              n_all = sum(known), n_sawlogs = sum(known & saw), n_butt_sawlogs = sum(known & saw & butt),
              yield_with = NA_real_, yield_without = NA_real_)
  if (!is.null(stems)) {
    u <- stems$use & stems$value_opt > 0
    out$yield_with <- .share(sum(stems$value_rec[u & stems$has_manual]), sum(stems$value_opt[u & stems$has_manual]))
    out$yield_without <- .share(sum(stems$value_rec[u & !stems$has_manual]), sum(stems$value_opt[u & !stems$has_manual]))
    out$n_with <- sum(u & stems$has_manual); out$n_without <- sum(u & !stems$has_manual)
  }
  out
}

# Top diameter under bark (cm) of the last log and of the last sawlog (the topmost non-waste log of
# each stem), mean per species, recorded vs best possible; both from the stem profile.
check_top <- function(job, rec, opt) {
  last <- function(d, key, sp, ptype, top) {
    d <- data.frame(k = d[[key]], sp = d[[sp]], ptype = d[[ptype]], stop = d$StopPos, top = top, stringsAsFactors = FALSE)
    d <- d[!d$ptype %in% "waste" & !is.na(d$ptype), , drop = FALSE]
    pick <- function(x) { x <- x[order(x$k, -x$stop), , drop = FALSE]; x[!duplicated(x$k), , drop = FALSE] }
    list(any = pick(d), saw = pick(d[d$ptype == "sawlog", , drop = FALSE]))
  }
  ub_at <- function(k, pos) mapply(function(kk, pp) stats::approx(job$pvs[[kk]]$diameterPosition, job$pvs[[kk]]$DiameterValueUB, xout = pp, rule = 2)$y, k, pos)
  rr <- last(rec, "stem_key", "species_group", "ptype", rec$TopUb_calc)
  oo <- opt; oo$top <- if (nrow(opt)) ub_at(opt$treid, opt$StopPos) else numeric(0)
  oo <- last(oo, "stem_key", "species_group", "ptype", oo$top)
  mean_by <- function(x) if (nrow(x) == 0L) data.frame(sp = character(0), mean_cm = numeric(0), n = integer(0)) else {
    s <- tapply(x$top / 10, x$sp, mean); data.frame(sp = names(s), mean_cm = as.numeric(s), n = as.integer(table(x$sp)[names(s)]), stringsAsFactors = FALSE) }
  list(last_rec = mean_by(rr$any), last_opt = mean_by(oo$any), sawlast_rec = mean_by(rr$saw), sawlast_opt = mean_by(oo$saw))
}

# Defect wood: volume (solid, under bark, 10 cm cells) of cells whose recorded grade no sawlog
# product of the species permits: of the total volume up to the top of the last recorded log, of the
# volume in sawlog dimensions (under-bark diameter at least the smallest sawlog minimum top
# diameter), of the butt log in sawlog dimensions; plus the volume cut as waste by the operator.
check_defect <- function(job, rec, pd, defs, smap, saw_grades) {
  min_top <- vapply(split(seq_len(nrow(pd)), pd$species_group), function(i) {
    d <- pd[i, , drop = FALSE]; d <- d[d$ProductType %in% "sawlog", , drop = FALSE]
    if (nrow(d)) min(d$MinTopDiameter) else NA_real_ }, 0)
  acc <- c(total = 0, defect_total = 0, dim = 0, defect_dim = 0, butt_dim = 0, defect_butt_dim = 0, waste = 0)
  for (k in unique(rec$stem_key)) {
    pv <- job$pvs[[k]]; if (is.null(pv) || is.null(pv$grade)) next
    sp <- pv$species_group; r <- rec[rec$stem_key == k, , drop = FALSE]
    pos <- pv$diameterPosition; g <- seq(0, floor(max(pos) / 10) * 10 - 10, by = 10)
    if (!length(g)) next
    d_top <- stats::approx(pos, as.numeric(pv$DiameterValueUB), xout = g + 10, rule = 2)$y
    vol <- pi * (d_top / 2)^2 * 10 / 1e8
    grade <- as.numeric(pv$grade)[pmax(findInterval(g, pos), 1L)]
    sg <- saw_grades[[sp]]
    bad <- if (is.null(sg) || (length(sg) == 1L && is.na(sg))) rep(FALSE, length(g)) else (!is.na(grade) & !grade %in% sg)
    top_end <- max(r$StopPos); in_stem <- g + 10 <= top_end
    mt <- min_top[sp]; dimc <- if (is.na(mt)) rep(FALSE, length(g)) else d_top >= mt
    first <- r[which.min(r$LogKey), ]; in_butt <- g >= first$StartPos & g + 10 <= first$StopPos
    wz <- hpr_reject_cells(g, r$StartPos[r$waste], r$LogLength[r$waste])
    acc <- acc + c(sum(vol[in_stem]), sum(vol[in_stem & bad]), sum(vol[in_stem & dimc]), sum(vol[in_stem & dimc & bad]),
                   sum(vol[in_butt & dimc]), sum(vol[in_butt & dimc & bad]), sum(vol[in_stem & wz]))
  }
  list(volume_m3 = acc[["total"]], share_total = .share(acc[["defect_total"]], acc[["total"]]),
       share_dim = .share(acc[["defect_dim"]], acc[["dim"]]), share_butt_dim = .share(acc[["defect_butt_dim"]], acc[["butt_dim"]]),
       share_waste = .share(acc[["waste"]], acc[["total"]]))
}

# Length distribution of the sawlog assortments (logs per length class, dm), recorded vs best
# possible, and the degree of fit (FG) of the recorded and the best possible outcome where the
# product has a target (the file's Distribution).
check_length_dist <- function(rec, opt, defs, pm, pr, ptype) {
  keys <- names(ptype)[ptype %in% "sawlog"]
  rows <- lapply(keys, function(k) {
    r <- rec[!is.na(rec$akey) & rec$akey == k, , drop = FALSE]; o <- opt[as.character(opt$ProductKey) == k, , drop = FALSE]
    cls <- sort(unique(c(r$class_dm, o$LengthClass_dm)))
    if (!length(cls)) return(NULL)
    d <- data.frame(key = k, length_dm = cls,
                    recorded = as.integer(table(factor(r$class_dm, levels = cls))),
                    best = as.integer(table(factor(o$LengthClass_dm, levels = cls))), stringsAsFactors = FALSE)
    d
  })
  tab <- do.call(rbind, rows)
  fg <- do.call(rbind, lapply(keys, function(k) {
    i <- match(k, assortment_table_keys(defs))
    if (is.na(i) || !identical(defs$PriceType[i], "matrix") || all(is.na(defs$MAXDeviation[i]) & is.na(defs$DistributionAllowed[i]))) return(NULL)
    tryCatch({
      hpr <- tuning_hpr_info(defs, pm, k); if (is.null(hpr) || is.null(hpr$cells) || !any(hpr$cells$dist > 0, na.rm = TRUE)) return(NULL)
      grid <- tuning_grid(list(pd = pr$pd, lc = pr$lc, pm = pm), k, hpr); W <- tuning_target(grid, "real")
      basis <- grid$basis_default
      r <- rec[!is.na(rec$akey) & rec$akey == k, , drop = FALSE]
      rr <- data.frame(ProductKey = r$akey, LengthClass_dm = as.integer(r$class_dm),
                       PriceDiameterClass_cm_used = pmin(r$PriceDiameterClass_cm, max(as.integer(names(grid$cm_map)))),
                       price_volume_m3 = r$price_volume_m3, stringsAsFactors = FALSE)
      rr <- rr[!is.na(rr$price_volume_m3), , drop = FALSE]
      f_rec <- fg_grid(tally_grid(list(selected_logs_all = rr), k, grid), W, basis)$overall
      oo <- opt[as.character(opt$ProductKey) == k, , drop = FALSE]
      f_opt <- if (nrow(oo)) fg_grid(tally_grid(list(selected_logs_all = oo), k, grid), W, basis)$overall else NA_real_
      data.frame(key = k, basis = basis, fg_recorded = f_rec, fg_best = f_opt,
                 allowed = defs$DistributionAllowed[i], maxdev = defs$MAXDeviation[i], stringsAsFactors = FALSE)
    }, error = function(e) NULL)
  }))
  list(tally = tab, fg = fg)
}

# Control against the harvester's own volumes: optBuck's price volume of the recorded logs vs the
# file's `m3 (price)`, and the recorded logs outside their product's limits.
check_control <- function(rec) {
  r <- rec[!rec$waste & !is.na(rec$price_volume_m3) & !is.na(rec$m3_price) & rec$m3_price > 0, , drop = FALSE]
  d <- (r$price_volume_m3 - r$m3_price) / r$m3_price
  unp <- !rec$waste & !is.na(rec$akey) & is.na(rec$PriceNOKperM3)
  nw <- !rec$waste
  by_prod <- if (any(unp)) { f <- factor(rec$akey[unp])
    tot <- tapply(ifelse(is.na(rec$m3_price[nw]), 0, rec$m3_price[nw]), factor(rec$akey[nw], levels = levels(f)), sum)
    data.frame(key = levels(f), logs = as.integer(table(f)), m3 = as.numeric(tapply(ifelse(is.na(rec$m3_price[unp]), 0, rec$m3_price[unp]), f, sum)),
               share_pct = 100 * as.numeric(tapply(ifelse(is.na(rec$m3_price[unp]), 0, rec$m3_price[unp]), f, sum)) / as.numeric(tot), stringsAsFactors = FALSE) } else NULL
  list(unpriced_by_product = by_prod, unpriced_share_pct = 100 * .share(sum(ifelse(is.na(rec$m3_price[unp]), 0, rec$m3_price[unp])), sum(ifelse(is.na(rec$m3_price[nw]), 0, rec$m3_price[nw]))),
       n = nrow(r), harvester_m3 = sum(r$m3_price), optbuck_m3 = sum(r$price_volume_m3),
       diff_pct = 100 * (sum(r$price_volume_m3) / sum(r$m3_price) - 1), share_above_2pct = 100 * mean(abs(d) > 0.02),
       mean_abs_pct = 100 * mean(abs(d)), n_outside = sum(rec$outside, na.rm = TRUE), n_unknown = sum(rec$unknown_product),
       n_unpriced = sum(!rec$waste & !is.na(rec$akey) & is.na(rec$PriceNOKperM3)))   # no price cell for the log's class: valued 0
}

# The products a scenario takes over from an .hpr that is loaded in tab 1 (0.3.2): the file's default
# selection (hpr_product_table()$default: mapped species, priced cells) limited to the products the
# harvester used, i.e. at least 1 % of a species' recorded non-waste logs (check_used_products()).
# A file without readable logs (or with no used product) keeps the default selection.
#
# @param path,filename The file.  @param hpr read_hpr() result.  @param species_map Group key -> species.
# @return list(keep (ProductKeys), used (TRUE when the logs decided)).
hpr_default_keep <- function(path, filename, hpr, species_map) {
  tab <- hpr_product_table(hpr, species_map)
  base <- as.character(tab$key[tab$default])
  rl <- tryCatch(read_hpr_logs(path, filename, defs = hpr), error = function(e) NULL)
  if (is.null(rl) || !isTRUE(rl$ok) || nrow(rl$logs) == 0L) return(list(keep = base, used = FALSE))
  keep <- base[base %in% check_used_products(rl$logs, species_map)]
  if (length(keep) == 0L) list(keep = base, used = FALSE) else list(keep = keep, used = TRUE)
}
