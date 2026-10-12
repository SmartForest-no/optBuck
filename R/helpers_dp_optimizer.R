# ==============================================================================
# R/helpers_dp_optimizer.R
#
# Purpose : Dynamic-programming (backward-induction) log-bucking optimiser
#           extracted from the accepted prototype scripts 06 and 11.
#
# Algorithm (identical to accepted scripts):
#   1. Filter out candidates with missing price / value.
#   2. Build a sorted unique-position state space from all StartPos/StopPos.
#   3. Backward induction: dp_val[i] = best total value achievable from
#      position all_pos[i] to the end.
#   4. Traceback: follow dp_log chain to recover selected logs.
#
# Tie-breaking (2026-10-12, Phase 4a): deterministic and documented.
#   At each position, every option (skip this position, or start one of the
#   candidates here) has a total = its own value + the best total from where
#   it ends. Options whose total is within DP_TIE_TOL_NOK of the best total
#   count as equal; among those the choice is, in order:
#     1. fewer logs in the rest of the stem,
#     2. higher-priority product (ProductPriority, from ProductData$Priority:
#        sawlog 1, pulpwood 2, waste 99; 99 if absent),
#     3. the longer log,
#     4. ProductKey (alphabetical; only matters for equal-priority products),
#   and leaving the position unallocated ranks after any log at equal log
#   count. The rule is independent of input row order and of prices (prices
#   only decide which options tie). The stored value is the chosen option's
#   own total, so the reported value is always the sum of the selected logs;
#   at most DP_TIE_TOL_NOK is given up per position.
#   The app's fast optimizer (.optimize_dp_fast in
#   scripts/app_backend/01_run_sampled_plot_bucking.R) implements the same
#   rule; audits 16 (checks 14-15) and 19 assert they agree exactly.
#
# Usage:
#   source("R/helpers_dp_optimizer.R")
#   result <- optimize_one_tree_dp(valued_candidates)
#   result$selected_logs   # data.frame of selected logs
#   result$dp_best_value   # total value (NOK)
# ==============================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}

#' Totals within this many NOK of the best count as a tie (see header).
DP_TIE_TOL_NOK <- 1e-6


# ------------------------------------------------------------------------------
# Main DP function
# ------------------------------------------------------------------------------

#' Run the backward-induction DP optimiser for one tree.
#'
#' The input data frame must have been priced (PriceNOKperM3 and value_nok
#' present and non-NA).  Rows with NA value_nok are silently dropped.
#'
#' @param valued_candidates data.frame with priced, eligible candidates.
#'   Required columns: StartPos (int), StopPos (int), value_nok (numeric),
#'   and all other columns to be passed through to the selected-log output.
#' @return Named list:
#'   \item{selected_logs}{data.frame of selected (non-overlapping) logs.}
#'   \item{dp_best_value}{numeric — optimal total value (NOK).}
#'   \item{n_selected}{integer — number of selected logs.}
#'   \item{n_dp_candidates}{integer — rows entering the DP.}
#'   \item{optimizer_status}{character — "ok", "zero_logs", or "no_candidates".}
#'   \item{optimizer_message}{character — human-readable status.}
optimize_one_tree_dp <- function(valued_candidates) {
  # Remove rows with missing value.
  dp_in <- valued_candidates[
    !is.na(valued_candidates$value_nok) &
    !is.na(valued_candidates$price_volume_m3), , drop = FALSE]
  dp_in$StartPos <- as.integer(round(dp_in$StartPos))
  dp_in$StopPos  <- as.integer(round(dp_in$StopPos))
  n_dp <- nrow(dp_in)

  if (n_dp == 0L) {
    return(list(
      selected_logs     = data.frame(),
      dp_best_value     = 0,
      n_selected        = 0L,
      n_dp_candidates   = 0L,
      optimizer_status  = "no_candidates",
      optimizer_message = "No priced candidates; DP not run"
    ))
  }

  # Tie-break keys (see header): product priority and alphabetical product rank.
  prio <- if ("ProductPriority" %in% names(dp_in))
    as.numeric(dp_in$ProductPriority) else rep(99, n_dp)
  prio[is.na(prio)] <- 99
  pk_rank <- match(as.character(dp_in$ProductKey),
                   sort(unique(as.character(dp_in$ProductKey))))
  if (length(pk_rank) == 0L || anyNA(pk_rank)) pk_rank <- rep(0L, n_dp)
  log_len <- dp_in$StopPos - dp_in$StartPos

  # State space: sorted unique positions across all starts and stops.
  all_pos  <- sort(unique(c(dp_in$StartPos, dp_in$StopPos)))
  sentinel <- max(all_pos) + 1L
  all_pos  <- c(all_pos, sentinel)
  m        <- length(all_pos)

  dp_val <- numeric(m)      # total value of the chosen solution from all_pos[i]
  dp_n   <- integer(m)      # number of logs in that solution
  dp_log <- integer(m)      # index of the candidate chosen at all_pos[i] (0 = none)

  # Pre-build a position-to-index lookup (positions are in cm, values up to ~5000).
  max_pos      <- max(all_pos)
  pos_to_idx   <- integer(max_pos + 1L)
  for (i in seq_len(m)) pos_to_idx[all_pos[i] + 1L] <- i

  # TRUE if candidate a (n_a logs) beats candidate b (n_b logs) on the
  # tie-break keys: fewer logs, higher priority, longer log, ProductKey.
  key_better <- function(n_a, a, n_b, b) {
    if (n_a != n_b)                 return(n_a < n_b)
    if (prio[a] != prio[b])         return(prio[a] < prio[b])
    if (log_len[a] != log_len[b])   return(log_len[a] > log_len[b])
    pk_rank[a] < pk_rank[b]
  }

  # Backward induction
  for (i in seq(m - 1L, 1L)) {
    cur_pos   <- all_pos[i]
    # All candidates starting exactly at cur_pos, with their totals.
    cand_idx  <- which(dp_in$StartPos == cur_pos)
    tot <- n_tot <- numeric(0); ok_c <- integer(0)
    for (ci in cand_idx) {
      sp    <- dp_in$StopPos[ci]
      if (sp > max_pos) next
      si    <- pos_to_idx[sp + 1L]
      if (si == 0L) next
      ok_c  <- c(ok_c, ci)
      tot   <- c(tot, dp_in$value_nok[ci] + dp_val[si])
      n_tot <- c(n_tot, 1L + dp_n[si])
    }

    skip_val <- dp_val[i + 1L]
    best_tot <- max(c(skip_val, tot))

    # Best candidate among those tied with the best total.
    best_k <- 0L
    for (k in seq_along(ok_c)) {
      if (tot[k] < best_tot - DP_TIE_TOL_NOK) next
      if (best_k == 0L ||
          key_better(n_tot[k], ok_c[k], n_tot[best_k], ok_c[best_k]))
        best_k <- k
    }

    # Skip if it is tied with the best and needs fewer logs than that candidate
    # (at equal log count a log ranks before leaving the position unallocated).
    skip_ok <- skip_val >= best_tot - DP_TIE_TOL_NOK
    if (best_k == 0L || (skip_ok && dp_n[i + 1L] < n_tot[best_k])) {
      dp_val[i] <- skip_val
      dp_n[i]   <- dp_n[i + 1L]
      dp_log[i] <- 0L
    } else {
      dp_val[i] <- tot[best_k]
      dp_n[i]   <- n_tot[best_k]
      dp_log[i] <- ok_c[best_k]
    }
  }

  # Traceback
  sel_idx <- integer(0)
  i       <- 1L
  while (i < m) {
    ci <- dp_log[i]
    if (ci == 0L) {
      i <- i + 1L
    } else {
      sel_idx <- c(sel_idx, ci)
      sp  <- dp_in$StopPos[ci]
      if (sp > max_pos) break
      si  <- pos_to_idx[sp + 1L]
      if (si == 0L) break
      i   <- si
    }
  }

  n_sel <- length(sel_idx)

  if (n_sel == 0L) {
    return(list(
      selected_logs     = data.frame(),
      dp_best_value     = dp_val[1L],
      n_selected        = 0L,
      n_dp_candidates   = n_dp,
      optimizer_status  = "zero_logs",
      optimizer_message = "DP ran but selected 0 logs"
    ))
  }

  sel <- dp_in[sel_idx, , drop = FALSE]
  rownames(sel) <- NULL

  list(
    selected_logs     = sel,
    dp_best_value     = dp_val[1L],
    n_selected        = n_sel,
    n_dp_candidates   = n_dp,
    optimizer_status  = "ok",
    optimizer_message = sprintf("Selected %d logs", n_sel)
  )
}


# ------------------------------------------------------------------------------
# Post-optimisation checks
# ------------------------------------------------------------------------------

#' Check for overlapping logs in the selected-log output.
#'
#' Overlap: two consecutive logs from the same tree where StopPos of the
#' first exceeds StartPos of the next.  (Touching logs — StopPos == next
#' StartPos — are allowed.)
#'
#' @param selected_logs data.frame with columns treid, StartPos, StopPos.
#' @return Integer count of overlapping consecutive pairs.
check_selected_logs_no_overlap <- function(selected_logs) {
  if (is.null(selected_logs) || nrow(selected_logs) < 2L) return(0L)

  sel_s <- selected_logs[order(selected_logs$treid, selected_logs$StartPos), ]
  n     <- nrow(sel_s)

  same_tree  <- sel_s$treid[-1L] == sel_s$treid[-n]
  n_overlap  <- sum(same_tree & (sel_s$StopPos[-n] > sel_s$StartPos[-1L]),
                    na.rm = TRUE)
  as.integer(n_overlap)
}


# ------------------------------------------------------------------------------
# Summary helper
# ------------------------------------------------------------------------------

#' Summarise the optimiser result for one tree.
#'
#' @param selected_logs  data.frame from optimize_one_tree_dp()$selected_logs.
#' @param tree_height_m  Numeric; used to compute stem coverage (optional).
#' @param stump_height_cm Integer; used to compute stem coverage (optional).
#' @return Named list with summary statistics.
summarize_optimizer_result <- function(selected_logs,
                                        tree_height_m    = NA_real_,
                                        stump_height_cm  = 10L) {
  if (is.null(selected_logs) || nrow(selected_logs) == 0L) {
    return(list(
      n_selected            = 0L,
      total_value_nok       = 0,
      total_volume_m3       = 0,
      sawlog_volume_m3      = 0,
      pulpwood_volume_m3    = 0,
      waste_volume_m3       = 0,
      sawlog_value_nok      = 0,
      pulpwood_value_nok    = 0,
      waste_value_nok       = 0,
      stem_coverage_pct     = NA_real_
    ))
  }

  sl <- selected_logs

  is_sawlog  <- grepl("sawlog",  sl$ProductType, ignore.case = TRUE)
  is_pulp    <- grepl("pulp",    sl$ProductType, ignore.case = TRUE)
  is_waste   <- grepl("waste|solid", sl$ProductType, ignore.case = TRUE)

  safe_sum <- function(x) if (length(x) == 0L) 0 else sum(x, na.rm = TRUE)

  # Covered stem length (from first StartPos to last StopPos)
  covered_cm  <- max(sl$StopPos, na.rm = TRUE) -
                 min(sl$StartPos, na.rm = TRUE)
  usable_cm   <- if (!is.na(tree_height_m))
                   as.integer(round(tree_height_m * 100)) - stump_height_cm
                 else
                   NA_real_
  coverage    <- if (!is.na(usable_cm) && usable_cm > 0)
                   100 * covered_cm / usable_cm
                 else
                   NA_real_

  list(
    n_selected            = nrow(sl),
    total_value_nok       = safe_sum(sl$value_nok),
    total_volume_m3       = safe_sum(sl$price_volume_m3),
    sawlog_volume_m3      = safe_sum(sl$price_volume_m3[is_sawlog]),
    pulpwood_volume_m3    = safe_sum(sl$price_volume_m3[is_pulp]),
    waste_volume_m3       = safe_sum(sl$price_volume_m3[is_waste]),
    sawlog_value_nok      = safe_sum(sl$value_nok[is_sawlog]),
    pulpwood_value_nok    = safe_sum(sl$value_nok[is_pulp]),
    waste_value_nok       = safe_sum(sl$value_nok[is_waste]),
    stem_coverage_pct     = coverage
  )
}
