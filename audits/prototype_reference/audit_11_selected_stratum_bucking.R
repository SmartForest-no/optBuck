# ==============================================================================
# audits/audit_11_selected_stratum_bucking.R
#
# Purpose : Independent audit of R/11_run_bucking_on_selected_stratum.R
#           outputs.
#
# Checks  :
#  1.  Required output files exist
#  2.  At least 1 000 trees were processed (unless stratum has fewer)
#  3.  Failed tree count is zero or clearly reported
#  4.  Each successful tree has generated candidates (unless too_short)
#  5.  Each successful tree has more than one unique StartPos (unless too_short)
#  6.  No overlapping selected logs within any tree
#  7.  LogLength == StopPos - StartPos
#  8.  No missing PriceNOKperM3 or value_nok in selected logs
#  9.  ProductKey matches species_group (spruce -> spruce_*, pine -> pine_*)
# 10.  Tree-level total_selected_value_nok == sum(value_nok) from selected logs
# 11.  Tree-level total_selected_volume_m3 == sum(price_volume_m3) from logs
# 12.  No unpriced 31 dm sawlog was selected
# 13.  Missing prices are only the known 31 dm sawlog gap; any other is FAIL
# 14.  No row inflation from price join
# 15.  At least some trees have more than one selected log
# 16.  Zero-log trees are listed and explained (small DBH / no priced cands)
# 17.  PriceDiameterClass_cm_used <= MAX_PRICE_DIAMETER_CLASS_CM in selected
# 18.  Capped rows: PriceDiameterClass_cm_used == MAX where uncapped > MAX
# 19.  No selected log has missing price because of diameter class > MAX
# 20.  No unpriced sawlog candidate has PriceDiameterClass_cm_used > MAX
#       (i.e. the cap was actually applied)
#
# Verdict :
#   PASS                   -- all checks pass, no missing prices
#   ACCEPTED WITH REVIEW   -- only known 31 dm gap, explained zero-log trees,
#                             or documented price-diameter capping
#   FAIL                   -- overlaps, value mismatch, required files missing,
#                             species/product mismatch, missing prices outside
#                             the known 31 dm sawlog gap
#
# Outputs :
#   outputs/selected_stratum_bucking/audit_11_selected_stratum_bucking.csv
#   outputs/selected_stratum_bucking/audit_11_selected_stratum_bucking.txt
# ==============================================================================


# ---- setup ----

library(readr)

root_dir <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
out_dir  <- file.path(root_dir, "outputs", "selected_stratum_bucking")

# Must match R/11_run_bucking_on_selected_stratum.R
MAX_PRICE_DIAMETER_CLASS_CM <- 50L

VALID_PKS <- c("spruce_sawlog", "spruce_pulpwood", "spruce_waste",
               "pine_sawlog",   "pine_pulpwood",   "pine_waste")

audit_txt <- file.path(out_dir, "audit_11_selected_stratum_bucking.txt")
audit_csv <- file.path(out_dir, "audit_11_selected_stratum_bucking.csv")

cat("", file = audit_txt)

alog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = audit_txt, append = TRUE)
}

alog(
  "=============================================================",
  "  Audit: R/11_run_bucking_on_selected_stratum.R outputs",
  sprintf("  Run: %s", Sys.time()),
  sprintf("  MAX_PRICE_DIAMETER_CLASS_CM  : %d cm", MAX_PRICE_DIAMETER_CLASS_CM),
  "============================================================="
)

results <- data.frame(check = character(0), result = character(0),
                      detail = character(0), stringsAsFactors = FALSE)

record <- function(check, result, detail = "") {
  tag <- sprintf("  [%-6s] %s", result, check)
  if (nchar(detail) > 0L) {
    tag <- paste0(tag, " | ", detail)
  }
  alog(tag)
  results <<- rbind(results,
    data.frame(check = check, result = result, detail = detail,
               stringsAsFactors = FALSE))
}

PASS   <- "PASS"
REVIEW <- "REVIEW"
FAIL   <- "FAIL"


# ---- 1. Required output files ----

alog("", "--- 1. Required output files ---")

required_files <- c(
  "selected_stratum_selected_logs.csv",
  "selected_stratum_tree_summary.csv",
  "selected_stratum_candidate_diagnostics.csv",
  "selected_stratum_unpriced_summary.csv",
  "selected_stratum_checkpoint_status.csv",
  "selected_stratum_run_summary.txt"
)

for (fn in required_files) {
  fp <- file.path(out_dir, fn)
  if (file.exists(fp)) {
    record(sprintf("File exists: %s", fn), PASS)
  } else {
    record(sprintf("File exists: %s", fn), FAIL, "File not found")
  }
}


# ---- 2. Read output files ----

alog("", "--- 2. Reading files ---")

read_csv_safe <- function(path) {
  if (!file.exists(path)) {
    return(data.frame())
  }
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE),
    error = function(e) data.frame()
  )
}

sel   <- as.data.frame(read_csv_safe(
  file.path(out_dir, "selected_stratum_selected_logs.csv")))
ts    <- as.data.frame(read_csv_safe(
  file.path(out_dir, "selected_stratum_tree_summary.csv")))
diag  <- as.data.frame(read_csv_safe(
  file.path(out_dir, "selected_stratum_candidate_diagnostics.csv")))
unpr  <- as.data.frame(read_csv_safe(
  file.path(out_dir, "selected_stratum_unpriced_summary.csv")))
chk   <- as.data.frame(read_csv_safe(
  file.path(out_dir, "selected_stratum_checkpoint_status.csv")))

alog(sprintf("  Tree summary        : %d rows", nrow(ts)))
alog(sprintf("  Selected logs       : %d rows", nrow(sel)))
alog(sprintf("  Diagnostics         : %d rows", nrow(diag)))
alog(sprintf("  Unpriced summary    : %d rows", nrow(unpr)))
alog(sprintf("  Checkpoint status   : %d rows", nrow(chk)))

# Helper: retrieve a scalar value from the diagnostics key-value table.
diag_val <- function(key) {
  if (nrow(diag) == 0L || !("item" %in% names(diag) &&
                             "value" %in% names(diag))) {
    return(NA_character_)
  }
  v <- diag$value[diag$item == key]
  if (length(v) == 0L) NA_character_ else v[1]
}

`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}

n_trees_valid     <- as.integer(diag_val("n_trees_valid"))     %||% nrow(ts)
n_trees_fail      <- as.integer(diag_val("n_trees_optimizer_fail")) %||% NA_integer_
n_trees_zero      <- as.integer(diag_val("n_trees_zero_selected_logs")) %||% NA_integer_
n_prof_fail       <- as.integer(diag_val("n_profile_failures")) %||% NA_integer_
n_dm31_total      <- as.integer(diag_val("n_unpriced_31dm_sawlog_total")) %||% NA_integer_
n_cap_total       <- as.integer(diag_val("n_price_diameter_capped_total")) %||% NA_integer_
n_plots           <- as.integer(diag_val("n_plots_represented")) %||% NA_integer_


# ---- 3. Tree count ----

alog("", "--- 3. Tree count ---")

if (nrow(ts) > 0L) {
  n_ts <- nrow(ts)
  if (n_ts >= 1000L) {
    record("At least 1000 trees processed", PASS,
           sprintf("n = %d", n_ts))
  } else {
    # Check if input stratum had fewer trees.
    n_valid_diag <- n_trees_valid %||% 0L
    if (!is.na(n_valid_diag) && n_valid_diag < 1000L) {
      record("At least 1000 trees processed", REVIEW,
             sprintf("n = %d; stratum only had %d valid trees", n_ts, n_valid_diag))
    } else {
      record("At least 1000 trees processed", FAIL,
             sprintf("n = %d; expected >= 1000", n_ts))
    }
  }
  record(sprintf("n_plots from diagnostics: %s", n_plots %||% "NA"), PASS,
         sprintf("%d plots represented", n_plots %||% NA_integer_))
} else {
  record("Tree summary has rows", FAIL, "tree_summary is empty")
}


# ---- 4. Failed tree count ----

alog("", "--- 4. Failed trees ---")

if (!is.na(n_trees_fail)) {
  n_ts <- nrow(ts)
  pct  <- if (n_ts > 0L) n_trees_fail / n_ts * 100 else 0
  if (n_trees_fail == 0L) {
    record("Optimizer error count", PASS,
           sprintf("0 / %d (%.1f%%)", n_ts, pct))
  } else {
    # Fail-rows should carry their error messages; report as REVIEW.
    record("Optimizer error count", REVIEW,
           sprintf("%d / %d (%.1f%%) optimizer errors", n_trees_fail, n_ts, pct))
  }
} else {
  record("Failed tree count from diagnostics", REVIEW,
         "n_trees_optimizer_fail not found in diagnostics")
}

if (!is.na(n_prof_fail)) {
  if (n_prof_fail == 0L) {
    record("Profile failure count", PASS,
           "0 profile failures")
  } else {
    record("Profile failure count", REVIEW,
           sprintf("%d profile failure(s)", n_prof_fail))
  }
}


# ---- 5. Candidates generated ----

alog("", "--- 5. Candidates generated per tree ---")

if (nrow(ts) > 0L && "n_generated_candidates" %in% names(ts) &&
    "optimizer_status" %in% names(ts)) {
  ok_ts     <- ts[!is.na(ts$optimizer_status) &
                    ts$optimizer_status %in% c("ok", "zero_logs",
                                               "no_priced_candidates"), ]
  zero_gen  <- ok_ts[!is.na(ok_ts$n_generated_candidates) &
                       ok_ts$n_generated_candidates == 0L, ]
  record(
    "Each successful tree has generated candidates",
    if (nrow(zero_gen) == 0L) PASS else FAIL,
    sprintf("%d trees with 0 generated candidates (excluding too-short/no-product)",
            nrow(zero_gen)))
} else {
  record("Candidates generated check", REVIEW,
         "n_generated_candidates or optimizer_status missing from tree_summary")
}


# ---- 6. Unique StartPos coverage ----

alog("", "--- 6. Unique StartPos coverage ---")

if (nrow(ts) > 0L && "n_unique_StartPos_generated" %in% names(ts) &&
    "optimizer_status" %in% names(ts)) {
  ok_ts2 <- ts[!is.na(ts$optimizer_status) &
                 ts$optimizer_status %in% c("ok", "zero_logs",
                                            "no_priced_candidates"), ]
  only1  <- ok_ts2[!is.na(ok_ts2$n_unique_StartPos_generated) &
                     ok_ts2$n_unique_StartPos_generated <= 1L, ]
  record(
    "Non-trivial trees have > 1 unique StartPos",
    if (nrow(only1) == 0L) PASS else FAIL,
    sprintf("%d tree(s) with only 1 unique StartPos (non-trivial status)",
            nrow(only1)))
} else {
  record("Unique StartPos check", REVIEW,
         "n_unique_StartPos_generated missing from tree_summary")
}


# ---- 7. No overlapping logs ----

alog("", "--- 7. No overlapping selected logs ---")

if (nrow(sel) > 0L && all(c("treid", "StartPos", "StopPos") %in% names(sel))) {
  sel_s     <- sel[order(sel$treid, sel$StartPos), ]
  same_tree <- sel_s$treid[-1L] == sel_s$treid[-nrow(sel_s)]
  n_overlap <- sum(same_tree & (sel_s$StopPos[-nrow(sel_s)] > sel_s$StartPos[-1L]))
  record("No overlapping selected logs", if (n_overlap == 0L) PASS else FAIL,
         sprintf("%d overlapping pair(s)", n_overlap))
} else if (nrow(sel) == 0L) {
  record("No overlapping selected logs", PASS, "0 selected logs")
} else {
  record("No overlapping selected logs", REVIEW,
         "Required columns missing from selected_logs")
}


# ---- 8. LogLength == StopPos - StartPos ----

alog("", "--- 8. LogLength == StopPos - StartPos ---")

if (nrow(sel) > 0L &&
    all(c("LogLength", "StartPos", "StopPos") %in% names(sel))) {
  wrong <- sum(sel$LogLength != (sel$StopPos - sel$StartPos), na.rm = TRUE)
  record("LogLength == StopPos - StartPos",
         if (wrong == 0L) PASS else FAIL,
         sprintf("%d rows | %d logs with wrong LogLength", nrow(sel), wrong))
} else {
  record("LogLength == StopPos - StartPos", PASS, "0 selected logs")
}


# ---- 9. No missing prices in selected logs ----

alog("", "--- 9. No missing prices or values in selected logs ---")

if (nrow(sel) > 0L) {
  na_price <- if ("PriceNOKperM3" %in% names(sel)) sum(is.na(sel$PriceNOKperM3)) else NA_integer_
  na_val   <- if ("value_nok"     %in% names(sel)) sum(is.na(sel$value_nok))     else NA_integer_
  record("No missing PriceNOKperM3 (selected)",
         if (!is.na(na_price) && na_price == 0L) PASS else FAIL,
         sprintf("%d NA", na_price %||% NA_integer_))
  record("No missing value_nok (selected)",
         if (!is.na(na_val) && na_val == 0L) PASS else FAIL,
         sprintf("%d NA", na_val %||% NA_integer_))
} else {
  record("No missing prices in selected logs", PASS, "0 selected logs")
}


# ---- 10. ProductKey matches species_group ----

alog("", "--- 10. ProductKey matches species_group ---")

if (nrow(sel) > 0L &&
    all(c("ProductKey", "species_group") %in% names(sel))) {
  mismatched <- sel[
    !is.na(sel$species_group) & !is.na(sel$ProductKey) &
    !(startsWith(sel$ProductKey, sel$species_group)), ]
  record("ProductKey matches species_group for all selected logs",
         if (nrow(mismatched) == 0L) PASS else FAIL,
         sprintf("%d mismatched row(s)", nrow(mismatched)))
  invalid_pk <- sel[!is.na(sel$ProductKey) &
                      !(sel$ProductKey %in% VALID_PKS), ]
  record("All selected ProductKey values are valid",
         if (nrow(invalid_pk) == 0L) PASS else FAIL,
         sprintf("Valid: %s", paste(sort(unique(sel$ProductKey)), collapse = ", ")))
} else if (nrow(sel) == 0L) {
  record("ProductKey matches species_group", PASS, "0 selected logs")
} else {
  record("ProductKey / species_group check", REVIEW,
         "Required columns missing")
}


# ---- 11. Per-tree value consistency ----

alog("", "--- 11. Per-tree value consistency ---")

if (nrow(sel) > 0L && nrow(ts) > 0L &&
    all(c("treid", "value_nok") %in% names(sel)) &&
    all(c("treid", "total_selected_value_nok") %in% names(ts))) {
  sel_val <- tapply(sel$value_nok, sel$treid, sum, na.rm = TRUE)
  ts_val  <- setNames(ts$total_selected_value_nok, as.character(ts$treid))
  common  <- intersect(names(sel_val), names(ts_val))
  diffs   <- abs(sel_val[common] - ts_val[common])
  n_bad   <- sum(!is.na(diffs) & diffs > 0.01)
  record("Per-tree value consistent (summary vs selected logs)",
         if (n_bad == 0L) PASS else FAIL,
         sprintf("%d tree(s) with |diff| > 0.01 NOK", n_bad))
} else {
  record("Per-tree value consistency", REVIEW,
         "Cannot check (0 selected logs or missing columns)")
}


# ---- 12. Per-tree volume consistency ----

alog("", "--- 12. Per-tree volume consistency ---")

if (nrow(sel) > 0L && nrow(ts) > 0L &&
    all(c("treid", "price_volume_m3") %in% names(sel)) &&
    all(c("treid", "total_selected_volume_m3") %in% names(ts))) {
  sel_vol <- tapply(sel$price_volume_m3, sel$treid, sum, na.rm = TRUE)
  ts_vol  <- setNames(ts$total_selected_volume_m3, as.character(ts$treid))
  common  <- intersect(names(sel_vol), names(ts_vol))
  diffs   <- abs(sel_vol[common] - ts_vol[common])
  n_bad   <- sum(!is.na(diffs) & diffs > 1e-4)
  record("Per-tree volume consistent (summary vs selected logs)",
         if (n_bad == 0L) PASS else FAIL,
         sprintf("%d tree(s) with |diff| > 1e-4 m3", n_bad))
} else {
  record("Per-tree volume consistency", REVIEW,
         "Cannot check (0 selected logs or missing columns)")
}


# ---- 13. No unpriced 31 dm sawlog selected ----

alog("", "--- 13. No unpriced 31 dm sawlog selected ---")

if (nrow(sel) > 0L &&
    all(c("ProductKey", "LengthClass_dm", "PriceNOKperM3") %in% names(sel))) {
  unpr_31_sel <- sel[
    grepl("sawlog", sel$ProductKey) &
      !is.na(sel$LengthClass_dm) & sel$LengthClass_dm == 31L &
      is.na(sel$PriceNOKperM3), ]
  record("No unpriced 31 dm sawlog selected",
         if (nrow(unpr_31_sel) == 0L) PASS else FAIL,
         sprintf("%d unpriced 31 dm sawlog row(s) in selected", nrow(unpr_31_sel)))
} else {
  record("No unpriced 31 dm sawlog selected", PASS, "0 selected logs")
}


# ---- 14. Missing prices only from known 31 dm gap ----

alog("", "--- 14. Missing prices only from known 31 dm gap ---")

# The unpriced_summary stores aggregated (ProductKey, LengthClass_dm, n) counts.
# We report 31 dm gap as REVIEW and any other unpriced entry as FAIL.

if (nrow(unpr) > 0L && all(c("ProductKey", "LengthClass_dm", "n") %in% names(unpr))) {
  is_31_sawlog <- grepl("sawlog", unpr$ProductKey) & unpr$LengthClass_dm == 31L
  n_dm31_unpr  <- sum(unpr$n[is_31_sawlog], na.rm = TRUE)
  other_unpr   <- unpr[!is_31_sawlog, ]
  n_other_unpr <- if (nrow(other_unpr) > 0L) sum(other_unpr$n, na.rm = TRUE) else 0L

  if (n_dm31_unpr > 0L) {
    record("REVIEW: 31 dm sawlog price gap",
           REVIEW,
           sprintf("%d unpriced 31 dm sawlog candidate(s) | Known issue: price matrix starts at 34 dm",
                   n_dm31_unpr))
  }
  record("No missing prices outside known 31 dm sawlog gap",
         if (n_other_unpr == 0L) PASS else FAIL,
         if (n_other_unpr == 0L) "All unpriced candidates are the expected 31 dm sawlog gap"
         else sprintf("%d unpriced candidates outside known gap (ProductKey/LengthClass_dm: %s)",
                      n_other_unpr,
                      paste(other_unpr$ProductKey, other_unpr$LengthClass_dm, sep = "/",
                            collapse = ", ")))
} else {
  record("Unpriced summary check", PASS, "No unpriced candidates")
}


# ---- 15. No row inflation from price join ----

alog("", "--- 15. No row inflation from price join ---")

if (nrow(sel) > 0L && nrow(ts) > 0L &&
    "n_selected_logs" %in% names(ts) &&
    "n_priced_candidates" %in% names(ts)) {
  sel_per_tree    <- tapply(rep(1L, nrow(sel)), sel$treid, sum)
  ts_n_sel        <- setNames(ts$n_selected_logs, as.character(ts$treid))
  ts_n_priced     <- setNames(ts$n_priced_candidates, as.character(ts$treid))
  common          <- intersect(names(sel_per_tree), names(ts_n_sel))
  inflated        <- sum(
    !is.na(sel_per_tree[common]) &
    !is.na(ts_n_priced[common]) &
    sel_per_tree[common] > ts_n_priced[common],
    na.rm = TRUE)
  record("No row inflation from price join",
         if (inflated == 0L) PASS else FAIL,
         sprintf("selected <= priced for all trees | %d tree(s) with inflation",
                 inflated))
} else {
  record("No row inflation from price join", REVIEW,
         "Cannot check (0 selected logs or missing columns)")
}


# ---- 16. Trees with multiple selected logs ----

alog("", "--- 16. Trees with multiple selected logs ---")

if (nrow(ts) > 0L && "n_selected_logs" %in% names(ts)) {
  n_multi <- sum(!is.na(ts$n_selected_logs) & ts$n_selected_logs > 1L)
  n_any   <- sum(!is.na(ts$n_selected_logs) & ts$n_selected_logs >= 1L)
  record("At least some trees have > 1 selected log",
         if (n_multi > 0L) PASS else FAIL,
         sprintf("%d tree(s) with >1 log; %d trees with >=1 log",
                 n_multi, n_any))
} else {
  record("Trees with multiple logs", REVIEW, "n_selected_logs missing")
}


# ---- 17. Zero-log trees explained ----

alog("", "--- 17. Trees with zero selected logs ---")

if (nrow(ts) > 0L && "n_selected_logs" %in% names(ts)) {
  n_with  <- sum(!is.na(ts$n_selected_logs) & ts$n_selected_logs >= 1L)
  n_zero  <- if (!is.na(n_trees_zero)) n_trees_zero else
             sum(!is.na(ts$n_selected_logs) & ts$n_selected_logs == 0L &
                   (is.na(ts$error) | ts$error %in% c(NA_character_, "")))
  n_ts    <- nrow(ts)
  record("At least some trees have selected logs",
         if (n_with > 0L) PASS else FAIL,
         sprintf("%d / %d trees have >= 1 selected log", n_with, n_ts))
  if (n_zero > 0L) {
    dbh_zero_max <- if ("dbh_cm" %in% names(ts)) {
      zero_rows  <- ts[!is.na(ts$n_selected_logs) & ts$n_selected_logs == 0L &
                         (is.na(ts$error) | ts$error %in% c(NA_character_, "")), ]
      round(max(zero_rows$dbh_cm, na.rm = TRUE), 1)
    } else NA_real_
    statuses <- if ("optimizer_status" %in% names(ts)) {
      zero_rows <- ts[!is.na(ts$n_selected_logs) & ts$n_selected_logs == 0L &
                        (is.na(ts$error) | ts$error %in% c(NA_character_, "")), ]
      paste(unique(zero_rows$optimizer_status), collapse = ", ")
    } else "unknown"
    record(
      sprintf("REVIEW: %d tree(s) with 0 selected logs (%.1f%%)",
              n_zero, n_zero / max(n_ts, 1L) * 100),
      REVIEW,
      sprintf("max DBH of zero-log trees = %s cm; status(es): %s",
              dbh_zero_max, statuses))
  }
} else {
  record("Zero-log trees check", REVIEW, "n_selected_logs missing")
}


# ---- 18. PriceDiameterClass_cm_used <= MAX ----

alog("", "--- 18. PriceDiameterClass_cm_used <= MAX ---")

if (nrow(sel) > 0L &&
    "PriceDiameterClass_cm_used" %in% names(sel) &&
    "PriceDiameterClass_cm_uncapped" %in% names(sel)) {
  above_max <- sel[!is.na(sel$PriceDiameterClass_cm_used) &
                     sel$PriceDiameterClass_cm_used > MAX_PRICE_DIAMETER_CLASS_CM, ]
  record(
    sprintf("PriceDiameterClass_cm_used <= %d cm in all selected logs",
            MAX_PRICE_DIAMETER_CLASS_CM),
    if (nrow(above_max) == 0L) PASS else FAIL,
    sprintf("%d selected log(s) with PriceDiameterClass_cm_used > %d",
            nrow(above_max), MAX_PRICE_DIAMETER_CLASS_CM))

  # Document how many selected logs were affected by capping.
  capped_sel <- sel[!is.na(sel$PriceDiameterClass_cm_uncapped) &
                      !is.na(sel$PriceDiameterClass_cm_used) &
                      sel$PriceDiameterClass_cm_uncapped > sel$PriceDiameterClass_cm_used, ]
  if (nrow(capped_sel) > 0L) {
    record(
      sprintf("Price-diameter capping documented in selected logs (cap = %d cm)",
              MAX_PRICE_DIAMETER_CLASS_CM),
      REVIEW,
      sprintf("%d selected log(s) had PriceDiameterClass_cm capped (uncapped > used)",
              nrow(capped_sel)))
  }
} else if (nrow(sel) == 0L) {
  record(sprintf("PriceDiameterClass_cm_used <= %d cm", MAX_PRICE_DIAMETER_CLASS_CM),
         PASS, "0 selected logs")
} else {
  record(sprintf("PriceDiameterClass_cm_used <= %d cm check", MAX_PRICE_DIAMETER_CLASS_CM),
         REVIEW, "Column PriceDiameterClass_cm_used missing from selected_logs")
}


# ---- 19. Uncapped/used consistency ----

alog("", "--- 19. Uncapped/used consistency ---")

if (nrow(sel) > 0L &&
    all(c("PriceDiameterClass_cm_uncapped",
          "PriceDiameterClass_cm_used") %in% names(sel))) {
  # Where uncapped > MAX, used must equal MAX.
  capped_rows   <- sel[!is.na(sel$PriceDiameterClass_cm_uncapped) &
                         sel$PriceDiameterClass_cm_uncapped > MAX_PRICE_DIAMETER_CLASS_CM, ]
  bad_capped    <- if (nrow(capped_rows) > 0L) {
    sum(capped_rows$PriceDiameterClass_cm_used != MAX_PRICE_DIAMETER_CLASS_CM,
        na.rm = TRUE)
  } else 0L

  # Where uncapped <= MAX, used must equal uncapped.
  uncapped_rows <- sel[!is.na(sel$PriceDiameterClass_cm_uncapped) &
                         sel$PriceDiameterClass_cm_uncapped <= MAX_PRICE_DIAMETER_CLASS_CM, ]
  bad_uncapped  <- if (nrow(uncapped_rows) > 0L) {
    sum(uncapped_rows$PriceDiameterClass_cm_used != uncapped_rows$PriceDiameterClass_cm_uncapped,
        na.rm = TRUE)
  } else 0L

  record(
    "Capped rows: PriceDiameterClass_cm_used == MAX where uncapped > MAX",
    if (bad_capped == 0L) PASS else FAIL,
    sprintf("%d row(s) with uncapped > MAX but used != MAX", bad_capped))
  record(
    "Uncapped rows: PriceDiameterClass_cm_used == uncapped where uncapped <= MAX",
    if (bad_uncapped == 0L) PASS else FAIL,
    sprintf("%d row(s) where uncapped <= MAX but used != uncapped", bad_uncapped))
} else {
  record("Uncapped/used consistency", REVIEW,
         "PriceDiameterClass columns missing from selected_logs")
}


# ---- 20. No unpriced sawlog due to diameter > MAX (cap must have worked) ----

alog("", "--- 20. No unpriced sawlog due to diameter > MAX ---")

# The unpriced_summary only has (ProductKey, LengthClass_dm, n).
# We cannot directly check PriceDiameterClass_cm_used in the summary.
# However, selected logs CAN be checked: if any selected sawlog has
# PriceDiameterClass_cm_used > MAX it means the cap failed.
# For the unpriced summary we can only detect sawlog entries at unexpected
# LengthClass_dm values -- treat any non-31 sawlog unpriced row as FAIL.

if (nrow(unpr) > 0L &&
    all(c("ProductKey", "LengthClass_dm", "n") %in% names(unpr))) {
  # Sawlog entries that are NOT the known 31 dm gap => cap or pricing issue.
  bad_sawlog <- unpr[grepl("sawlog", unpr$ProductKey) &
                       unpr$LengthClass_dm != 31L, ]
  record(
    sprintf(
      "No unpriced sawlog candidate outside known 31 dm gap (PriceDiameterClass_cm_used <= %d implied)",
      MAX_PRICE_DIAMETER_CLASS_CM),
    if (nrow(bad_sawlog) == 0L) PASS else FAIL,
    sprintf("%d unpriced sawlog entry(ies) at unexpected LengthClass_dm: %s",
            nrow(bad_sawlog),
            if (nrow(bad_sawlog) > 0L)
              paste(bad_sawlog$LengthClass_dm, collapse = ", ")
            else "none"))
} else {
  record("No unexpected unpriced sawlog entries", PASS,
         "No unpriced candidates or summary not available")
}

# Also check cap in selected logs directly (redundant with check 18 but explicit).
if (nrow(sel) > 0L && "PriceDiameterClass_cm_used" %in% names(sel) &&
    "ProductKey" %in% names(sel)) {
  sel_saw_badcap <- sel[
    grepl("sawlog", sel$ProductKey) &
      !is.na(sel$PriceDiameterClass_cm_used) &
      sel$PriceDiameterClass_cm_used > MAX_PRICE_DIAMETER_CLASS_CM, ]
  record(
    sprintf("No selected sawlog has PriceDiameterClass_cm_used > %d (cap worked)",
            MAX_PRICE_DIAMETER_CLASS_CM),
    if (nrow(sel_saw_badcap) == 0L) PASS else FAIL,
    sprintf("%d selected sawlog(s) with PriceDiameterClass_cm_used > %d",
            nrow(sel_saw_badcap), MAX_PRICE_DIAMETER_CLASS_CM))
}


# ---- 21. Checkpoint consistency ----

alog("", "--- 21. Checkpoint and runtime consistency ---")

if (nrow(chk) > 0L && "n_trees_processed" %in% names(chk)) {
  n_chk_done  <- chk$n_trees_processed[nrow(chk)]
  n_chk_total <- if ("n_trees_total" %in% names(chk)) chk$n_trees_total[nrow(chk)] else NA_integer_
  elapsed_chk <- if ("elapsed_seconds" %in% names(chk)) chk$elapsed_seconds[nrow(chk)] else NA_real_

  matches <- !is.na(n_chk_total) && n_chk_done == n_chk_total
  record("Checkpoint processed == total trees",
         if (matches) PASS else REVIEW,
         sprintf("%d / %d processed at last checkpoint (%.1f s)",
                 n_chk_done, n_chk_total %||% NA_integer_,
                 elapsed_chk %||% NA_real_))

  # Runtime sanity: per-tree time should be positive and reasonable.
  if (!is.na(elapsed_chk) && n_chk_done > 0L) {
    spt <- elapsed_chk / n_chk_done
    record("Runtime per tree is positive and reasonable",
           if (spt > 0 && spt < 10) PASS else REVIEW,
           sprintf("%.4f s/tree  (%.1f s total, %d trees)",
                   spt, elapsed_chk, n_chk_done))
  }
} else {
  record("Checkpoint file check", REVIEW, "Checkpoint file missing or empty")
}


# ---- 22. Saving audit outputs ----

alog("", "--- 22. Saving audit outputs ---")

readr::write_csv(results, audit_csv)
alog(sprintf("  Saved: %s", basename(audit_csv)))

# ---- final verdict ----

n_pass   <- sum(results$result == PASS)
n_review <- sum(results$result == REVIEW)
n_fail   <- sum(results$result == FAIL)

verdict <- if (n_fail > 0L) {
  "FAIL"
} else if (n_review > 0L) {
  "ACCEPTED WITH REVIEW ITEMS"
} else {
  "PASS"
}

alog(
  "",
  "=============================================================",
  "  AUDIT FINAL SUMMARY",
  "=============================================================",
  sprintf(" result  n"),
  sprintf("   PASS %d", n_pass),
  sprintf(" REVIEW %d", n_review),
  sprintf("   FAIL %d", n_fail),
  "",
  sprintf("  Script 11 outputs: %s", verdict),
  "============================================================="
)

if (n_review > 0L) {
  alog("", "REVIEW items:")
  for (j in seq_len(nrow(results))) {
    if (results$result[j] == REVIEW) {
      tag <- sprintf("  [REVIEW] %s", results$check[j])
      if (nchar(results$detail[j]) > 0L) {
        tag <- paste0(tag, " | ", results$detail[j])
      }
      alog(tag)
    }
  }
}

if (n_fail > 0L) {
  alog("", "FAIL items:")
  for (j in seq_len(nrow(results))) {
    if (results$result[j] == FAIL) {
      tag <- sprintf("  [FAIL] %s", results$check[j])
      if (nchar(results$detail[j]) > 0L) {
        tag <- paste0(tag, " | ", results$detail[j])
      }
      alog(tag)
    }
  }
}

alog(
  sprintf("\nAudit log: %s", audit_txt),
  sprintf("[%s] Script 11 audit complete.", verdict)
)
