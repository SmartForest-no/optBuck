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

#' Tune one sawlog product's prices towards a target distribution.
#'
#' r = 1 bucks a sample of the job's stems with the original prices; each
#' further iteration updates the prices (update_prices()), swaps them into the
#' job and bucks the same sample again. Stops at max_iter, or when r >= 4 and
#' the best FG up to r exceeds the best up to r - 3 by less than 0.1 (only if
#' `early_stop`; with FALSE it always runs max_iter iterations). Returns the
#' iteration with the highest FG (on `basis`).
#'
#' @param job A hpr job (prepare_hpr_stems_job()).
#' @param products list(pd, lc, pm) the job was prepared with.
#' @param hpr Passed to tuning_grid() (the file's classes and Distribution).
#' @param basis "number" or "volume"; NULL: "volume" when target_type is "real"
#'   and the product's DistributionCategory is "Volume of logs", else "number".
#' @param early_stop TRUE (default): the stop rule above; FALSE: always max_iter
#'   iterations.
#' @param min_logs A diameter class is steered only if its first run has at
#'   least this many logs (counts, whatever the basis).
#' @return list(best_price, best_iter, trace, eligible, fg_class_first,
#'   fg_class_best, settings, grid, W, floor (value, product, price, matrix),
#'   steered (cells), cells_at_floor_best, P_first, prices (by iteration)).
tune_prices <- function(job, products, product_id, target_type, distribution = NULL,
                        n = 1000, seed = 1, max_iter = 30, min_logs = 20, basis = NULL,
                        hpr = NULL, workers = apter_workers(), early_stop = TRUE) {
  grid <- tuning_grid(products, product_id, hpr)
  W <- tuning_target(grid, target_type, distribution)
  if (is.null(basis)) basis <- if (target_type == "real" && grid$basis_default == "volume") "volume" else "number"
  P1 <- grid$price
  N <- length(job$pvs)
  set.seed(seed); idx <- sample(N, min(n, N))
  job_s <- subset_job_stems(job, idx)
  fl <- .tg_floor_info(products, grid$species)
  floor_m <- pmin(P1, fl$value)

  cur <- products; P <- P1
  prices <- list(); trace <- list(); S_prev <- NULL
  V1 <- NULL; eligible <- NULL; logs_first <- NA_integer_; fg1 <- NULL; fg_hist <- numeric(0); fgs <- list()
  for (r in seq_len(max_iter)) {
    t0 <- proc.time()[["elapsed"]]
    if (r > 1L) job_s <- swap_job_prices(job_s, cur)
    res <- finish_job(job_s, buck_job(job_s, workers))
    tal <- tally_grid(res, product_id, grid)
    f_b <- fg_grid(tal, W, basis); f_n <- fg_grid(tal, W, "number"); f_v <- fg_grid(tal, W, "volume")
    sl <- res$selected_logs_all
    val_total <- sum(res$tree_summary$total_value_nok)
    val_prod <- sum(sl$value_nok[as.character(sl$ProductKey) == product_id])
    a <- !is.na(P1)
    v_cur <- sum(tal$v[a] * P[a]); v_one <- sum(tal$v[a] * P1[a])
    if (r == 1L) {
      if (abs(val_prod - v_one) > 0.5)
        stop(sprintf("value check failed on iteration 1: the product's value in the result is %.3f NOK, sum(V*P1) is %.3f NOK (difference %.3f).",
                     val_prod, v_one, val_prod - v_one), call. = FALSE)
      V1 <- tal$v; logs_first <- sum(tal$n)
      eligible <- colSums(tal$n) >= min_logs & colSums(!is.na(W)) > 0
      fg1 <- f_b$by_class
    }
    trace[[r]] <- data.frame(
      r = r, fg = f_b$overall, fg_number = f_n$overall, fg_volume = f_v$overall,
      value_orig = val_total - v_cur + v_one, value_total = val_total,
      product_volume_m3 = sum(tal$v), sawlog_volume_m3 = sum(sl$price_volume_m3[as.character(sl$ProductType) == "sawlog"]),
      max_increase = max(0, P[a] - P1[a]), max_decrease = max(0, P1[a] - P[a]),
      elapsed_s = NA_real_)
    prices[[r]] <- P; fgs[[r]] <- f_b$by_class
    fg_hist <- c(fg_hist, f_b$overall)
    best_r <- which.max(fg_hist)
    if (early_stop && r >= 4L && max(fg_hist) - max(fg_hist[seq_len(r - 3L)]) < 0.1) { trace[[r]]$elapsed_s <- proc.time()[["elapsed"]] - t0; break }
    if (r == max_iter) { trace[[r]]$elapsed_s <- proc.time()[["elapsed"]] - t0; break }
    Pn <- update_prices(P, P1, f_b$S, S_prev, W, r, eligible, floor_m, tal$v, V1)
    S_prev <- f_b$S
    P <- Pn
    cur <- apply_grid_prices(products, grid, P)
    trace[[r]]$elapsed_s <- proc.time()[["elapsed"]] - t0
  }
  trace <- do.call(rbind, trace)
  best <- which.max(trace$fg)
  Pb <- prices[[best]]
  steer <- !is.na(W) & !is.na(P1) & matrix(eligible, nrow(P1), ncol(P1), byrow = TRUE)
  at_floor <- steer & !is.na(Pb) & floor_m < P1 - 1e-9 & Pb <= floor_m + 1e-9
  list(best_price = Pb, best_iter = best, trace = trace, eligible = setNames(eligible, colnames(P1)),
       fg_class_first = fg1, fg_class_best = fgs[[best]],
       settings = list(product_id = product_id, target_type = target_type, basis = basis, n = n,
                       sample_size = length(idx), stems = N, seed = seed, max_iter = max_iter, min_logs = min_logs, early_stop = early_stop),
       grid = grid, W = W, floor = c(fl, list(matrix = floor_m)), steered = sum(steer),
       cells_at_floor_best = sum(at_floor), P_first = P1, prices = prices, logs_first = logs_first)
}
