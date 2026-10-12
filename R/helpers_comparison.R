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
.log_set  <- function(l) sort(paste(l$StartPos, l$StopPos, l$ProductKey))

#' Per-tree comparison of two runs on the same trees.
#'
#' @param r1,r2   Backend results (default and scenario) on the same sample.
#' @param pm1,pm2 Their price matrices (PriceMatrix_long).
#' @param tol     Value tolerance (NOK), default DP_TIE_TOL_NOK.
#' @return data.frame, one row per tree: plot_id_github, treid,
#'   species_group, n_logs_default, n_logs_scenario, value_default,
#'   value_scenario (each under its own prices), gain_at_scenario,
#'   gain_at_default, max_volume_diff_m3, pattern_differs, changed, tie_only.
compare_cutting_patterns <- function(r1, r2, pm1, pm2,
                                     tol = if (exists("DP_TIE_TOL_NOK")) DP_TIE_TOL_NOK else 1e-6) {
  t1 <- r1$tree_summary; t2 <- r2$tree_summary
  keys <- intersect(.tree_key(t1), .tree_key(t2))
  l1 <- split(r1$selected_logs_all, .tree_key(r1$selected_logs_all))
  l2 <- split(r2$selected_logs_all, .tree_key(r2$selected_logs_all))
  lk1 <- build_price_lookup(pm1); lk2 <- build_price_lookup(pm2)
  empty <- r1$selected_logs_all[0, , drop = FALSE]

  val <- function(l, lk) {               # value of a pattern at given prices
    if (nrow(l) == 0L) return(0)
    p <- lookup_prices(l, lk)
    if (anyNA(p)) return(NA_real_)       # pattern not priced at these prices
    sum(l$price_volume_m3 * p)
  }
  vol_by_type <- function(l) tapply(l$commercial_volume_m3, l$ProductType, sum)

  rows <- lapply(keys, function(k) {
    a <- l1[[k]] %||% empty; b <- l2[[k]] %||% empty
    differs <- !identical(.log_set(a), .log_set(b))
    v_a1 <- val(a, lk1); v_b2 <- val(b, lk2)
    v_a2 <- val(a, lk2); v_b1 <- val(b, lk1)
    gain2 <- if (is.na(v_a2)) Inf else v_b2 - v_a2   # scenario pattern advantage at scenario prices
    gain1 <- if (is.na(v_b1)) Inf else v_a1 - v_b1   # default pattern advantage at default prices
    va <- vol_by_type(a); vb <- vol_by_type(b)
    types <- union(names(va), names(vb))
    dvol  <- 0
    if (length(types) > 0L) {
      va2 <- setNames(rep(0, length(types)), types); va2[names(va)] <- va
      vb2 <- setNames(rep(0, length(types)), types); vb2[names(vb)] <- vb
      dvol <- max(abs(va2 - vb2))
    }
    matters <- gain2 > tol || gain1 > tol || dvol > 1e-9
    tr <- t1[.tree_key(t1) == k, ][1L, ]
    data.frame(plot_id_github = tr$plot_id_github, treid = tr$treid,
               species_group = tr$species_group,
               n_logs_default = nrow(a), n_logs_scenario = nrow(b),
               value_default = v_a1, value_scenario = v_b2,
               gain_at_scenario = gain2, gain_at_default = gain1,
               max_volume_diff_m3 = dvol, pattern_differs = differs,
               changed = differs && matters, tie_only = differs && !matters,
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) return(data.frame())
  out
}

#' NOK/m3 per scope and assortment under both price matrices.
#'
#' @return data.frame: scope, assortment, nok_per_m3_default,
#'   nok_per_m3_scenario, delta_nok_per_m3, volume_share_default,
#'   volume_share_scenario, delta_volume_share. Scope totals have assortment
#'   "All" and NOK/m3 = scope value / scope commercial volume.
compare_per_m3 <- function(r1, r2) {
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
  m <- m[order(match(m$scope, scope_order), m$assortment != "All", m$assortment), ]
  data.frame(scope = m$scope, assortment = m$assortment,
             nok_per_m3_default = m$nok_default, nok_per_m3_scenario = m$nok_scenario,
             delta_nok_per_m3 = m$nok_scenario - m$nok_default,
             volume_share_default = m$share_default,
             volume_share_scenario = m$share_scenario,
             delta_volume_share = m$share_scenario - m$share_default,
             stringsAsFactors = FALSE, row.names = NULL)
}
