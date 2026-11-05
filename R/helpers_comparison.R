# ==============================================================================
# R/helpers_comparison.R
#
# Purpose : Default vs. scenario results (Phase 4a): which trees really changed
#           their cutting pattern, and NOK/m3 per scope and assortment under
#           both price matrices.
#
# Changed tree (docs/plans/PLAN.md, Phase 4a): the selected log sets differ
# AND the difference matters, i.e. at least one of
#   - at scenario prices, the scenario pattern is worth more than the default
#     pattern by more than `tol` (or the default pattern is not priced there),
#   - at default prices, the default pattern is worth more than the scenario
#     pattern by more than `tol` (or the scenario pattern is not priced there),
#   - the commercial volume by assortment (ProductType) differs by > 1e-9 m3.
# Differences that are only equal-value ties are counted separately and are
# not shown as changes. With the deterministic tie-breaking in the optimizers
# (R/helpers_dp_optimizer.R) such ties should be rare.
#
# Needs: lookup_prices()/price_logs() (helpers_valuation.R), DP_TIE_TOL_NOK
#        (helpers_dp_optimizer.R).
# ==============================================================================



.tree_key <- function(df) paste(df$plot_id_github, df$treid, sep = "|")

# Per-tree comparison of two runs on the same trees.
#
# @param r1,r2   Backend results (default and scenario) on the same sample.
# @param pm1,pm2 Their price matrices (PriceMatrix_long).
# @param tol     Value tolerance (NOK), default DP_TIE_TOL_NOK.
# @return data.frame, one row per tree: plot_id_github, treid,
#   species_group, n_logs_default, n_logs_scenario, value_default,
#   value_scenario (each under its own prices), gain_at_scenario,
#   gain_at_default, max_volume_diff_m3, pattern_differs, changed, tie_only.
compare_cutting_patterns <- function(r1, r2, pm1, pm2,
                                     tol = if (exists("DP_TIE_TOL_NOK")) DP_TIE_TOL_NOK else 1e-6) {
  # Vectorised (2026-10-15): every log is priced once per matrix and summed
  # per tree with split() + sum(), the same sums in the same order as the
  # former per-tree loop, so the results are identical.
  t1 <- r1$tree_summary; t2 <- r2$tree_summary
  keys <- intersect(.tree_key(t1), .tree_key(t2))
  if (length(keys) == 0L) return(data.frame())
  l1 <- r1$selected_logs_all; l2 <- r2$selected_logs_all
  k1 <- .tree_key(l1); k2 <- .tree_key(l2)
  lk1 <- build_price_lookup(pm1); lk2 <- build_price_lookup(pm2)

  # Value of each tree's pattern at given prices: NA if a log is not priced
  # there, 0 without logs.
  val <- function(l, k, lk) {
    out <- setNames(rep(0, length(keys)), keys)
    if (nrow(l) == 0L) return(out)
    p <- lookup_prices(l, lk)
    v <- vapply(split(l$price_volume_m3 * p, factor(k, levels = unique(k))), sum, numeric(1))
    v <- v[names(v) %in% keys]
    out[names(v)] <- v
    out
  }
  v_a1 <- val(l1, k1, lk1); v_b2 <- val(l2, k2, lk2)
  v_a2 <- val(l1, k1, lk2); v_b1 <- val(l2, k2, lk1)
  gain2 <- ifelse(is.na(v_a2), Inf, v_b2 - v_a2)   # scenario pattern advantage at scenario prices
  gain1 <- ifelse(is.na(v_b1), Inf, v_a1 - v_b1)   # default pattern advantage at default prices

  # Log sets and log counts per tree.
  set_of <- function(l, k) {
    s <- vapply(split(paste(l$StartPos, l$StopPos, l$ProductKey), factor(k, levels = unique(k))),
                function(x) paste(sort(x), collapse = "
"), character(1))
    out <- setNames(rep("", length(keys)), keys); s <- s[names(s) %in% keys]; out[names(s)] <- s
    out
  }
  n_of <- function(k) { n <- table(factor(k, levels = keys)); setNames(as.integer(n), keys) }
  differs <- set_of(l1, k1) != set_of(l2, k2)

  # Largest difference in commercial volume by assortment class per tree.
  vol <- function(l, k) {
    if (nrow(l) == 0L) return(numeric(0))
    g <- paste(k, l$ProductType, sep = "
")
    vapply(split(l$commercial_volume_m3, factor(g, levels = unique(g))), sum, numeric(1))
  }
  va <- vol(l1, k1); vb <- vol(l2, k2)
  g <- union(names(va), names(vb))
  d <- abs(ifelse(is.na(va[g]), 0, va[g]) - ifelse(is.na(vb[g]), 0, vb[g]))
  dk <- sub("
[^
]*$", "", g)
  dvol <- setNames(rep(0, length(keys)), keys)
  if (length(g)) {
    m <- vapply(split(d, factor(dk, levels = unique(dk))), max, numeric(1))
    m <- m[names(m) %in% keys]; dvol[names(m)] <- m
  }

  matters <- gain2 > tol | gain1 > tol | dvol > 1e-9
  tr <- t1[match(keys, .tree_key(t1)), , drop = FALSE]
  data.frame(plot_id_github = tr$plot_id_github, treid = tr$treid,
             species_group = tr$species_group,
             n_logs_default = n_of(k1), n_logs_scenario = n_of(k2),
             value_default = unname(v_a1), value_scenario = unname(v_b2),
             gain_at_scenario = unname(gain2), gain_at_default = unname(gain1),
             max_volume_diff_m3 = unname(dvol), pattern_differs = unname(differs),
             changed = unname(differs & matters), tie_only = unname(differs & !matters),
             stringsAsFactors = FALSE, row.names = NULL)
}

# NOK/m3 per scope and assortment under both price matrices.
#
# @param by "assortment" (the runs' per_m3_by_assortment: types in the
#   Total scope, assortments per species) or "type" (Sawlog, Pulpwood,
#   Other in every scope; waste left out), for scenarios with different
#   assortments.
# @return data.frame: scope, assortment, nok_per_m3_default,
#   nok_per_m3_scenario, delta_nok_per_m3, volume_share_default,
#   volume_share_scenario, delta_volume_share. Scope totals have assortment
#   "All" and NOK/m3 = scope value / scope commercial volume.
compare_per_m3 <- function(r1, r2, by = c("assortment", "type")) {
  by <- match.arg(by)
  if (by == "type") { r1 <- .per_m3_by_type(r1); r2 <- .per_m3_by_type(r2) }
  tot <- function(r) {
    s <- r$per_m3_summary
    data.frame(scope = s$scope, assortment = "All", nok = s$nok_per_m3,
               share = ifelse(s$commercial_volume_m3 > 0, 1, NA_real_),
               stringsAsFactors = FALSE)
  }
  asg <- function(r) {
    a <- r$per_m3_by_assortment
    data.frame(scope = a$scope, assortment = a$assortment,
               nok = a$nok_per_m3_contribution, share = a$volume_share,
               stringsAsFactors = FALSE)
  }
  x1 <- rbind(tot(r1), asg(r1)); x2 <- rbind(tot(r2), asg(r2))
  m <- merge(x1, x2, by = c("scope", "assortment"), all = TRUE,
             suffixes = c("_default", "_scenario"))
  for (c in c("nok_default", "nok_scenario", "share_default", "share_scenario"))
    m[[c]][is.na(m[[c]]) & m$assortment != "All"] <- 0
  scope_order <- c("Total", sort(setdiff(unique(m$scope), "Total")))
  m <- m[order(match(m$scope, scope_order), m$assortment != "All",
               match(m$assortment, ASSORTMENT_TYPE_GROUPS), m$assortment), ]
  data.frame(scope = m$scope, assortment = m$assortment,
             nok_per_m3_default = m$nok_default, nok_per_m3_scenario = m$nok_scenario,
             delta_nok_per_m3 = m$nok_scenario - m$nok_default,
             volume_share_default = m$share_default,
             volume_share_scenario = m$share_scenario,
             delta_volume_share = m$share_scenario - m$share_default,
             stringsAsFactors = FALSE, row.names = NULL)
}


# ---- scenarios with different assortments (2026-10-22) -----------------------------
# Per-assortment B - A only makes sense for assortments both scenarios have.
# When their assortments differ, the comparison is also shown by assortment
# type: sawlog, pulpwood and other (custom commercial types). Waste is left
# out (value 0, as in the log tally).

# Type groups in display order.
ASSORTMENT_TYPE_GROUPS <- c("Sawlog", "Pulpwood", "Other")

# The type group of each ProductType: "Sawlog", "Pulpwood", "Other", or NA
# for waste.
type_group <- function(product_type) {
  t <- as.character(product_type)
  out <- ifelse(t %in% "sawlog", "Sawlog", ifelse(t %in% "pulpwood", "Pulpwood", "Other"))
  out[t %in% "waste" | is.na(t)] <- NA_character_
  out
}

# A run whose per_m3_by_assortment is by type group in every scope.
.per_m3_by_type <- function(r) {
  l <- r$selected_logs_all; g <- type_group(l$ProductType)
  l <- l[!is.na(g), , drop = FALSE]; g <- g[!is.na(g)]
  l$ProductType <- g; l$ProductKey <- g
  r$per_m3_by_assortment <- summarise_per_m3(l, r$tree_summary)$by_assortment
  r
}

# Logs, value, price volume, commercial volume and share of commercial
# volume per type group in A (r1) and B (r2). Groups without logs in
# either scenario are left out.
#
# @return data.frame: type, logs_a, logs_b, value_a, value_b, price_vol_a,
#   price_vol_b, comm_vol_a, comm_vol_b, share_a, share_b.
compare_by_type <- function(r1, r2) {
  per <- function(r) {
    l <- r$selected_logs_all; g <- type_group(l$ProductType); k <- !is.na(g)
    f <- factor(g[k], levels = ASSORTMENT_TYPE_GROUPS)
    s <- function(x) as.numeric(tapply(x[k], f, sum, na.rm = TRUE, default = 0))
    tot <- sum(l$commercial_volume_m3, na.rm = TRUE)
    list(logs = as.integer(table(f)), value = s(l$value_nok), price_vol = s(l$price_volume_m3),
         comm_vol = s(l$commercial_volume_m3),
         share = if (tot > 0) s(l$commercial_volume_m3) / tot else rep(NA_real_, length(ASSORTMENT_TYPE_GROUPS)))
  }
  a <- per(r1); b <- per(r2)
  out <- data.frame(type = ASSORTMENT_TYPE_GROUPS, logs_a = a$logs, logs_b = b$logs,
                    value_a = a$value, value_b = b$value,
                    price_vol_a = a$price_vol, price_vol_b = b$price_vol,
                    comm_vol_a = a$comm_vol, comm_vol_b = b$comm_vol,
                    share_a = a$share, share_b = b$share, stringsAsFactors = FALSE)
  out[out$logs_a > 0L | out$logs_b > 0L, , drop = FALSE]
}
