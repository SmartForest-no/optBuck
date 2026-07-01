# ==============================================================================
# audits/audit_10_mixed_species_example_bucking.R
#
# Purpose : Independent audit of R/10_run_mixed_species_example_bucking.R
#           outputs.
#
# Checks  :
#  1.  Required output files exist
#  2.  Both spruce and pine are represented
#  3.  At least 100 trees processed (preferably ~500)
#  4.  Failed tree count is zero or clearly reported
#  5.  Each successful tree has generated candidates
#  6.  Each successful tree has more than one unique StartPos (unless too short)
#  7.  No overlapping selected logs within any tree
#  8.  LogLength == StopPos - StartPos
#  9.  No missing PriceNOKperM3 or value_nok in selected logs
# 10.  ProductKey matches species_group (spruce -> spruce_*, pine -> pine_*)
# 11.  Tree-level total_selected_value_nok == sum(value_nok) per tree
# 12.  Tree-level total_selected_volume_m3 == sum(price_volume_m3) per tree
# 13.  No unpriced 31 dm sawlog selected
# 14.  Missing prices are only the known 31 dm sawlog gap
# 15.  No row inflation from price join
# 16.  At least some trees have more than one selected log
# 17.  Trees with zero selected logs are listed and explained
# 18.  PriceDiameterClass_cm_used <= MAX_PRICE_DIAMETER_CLASS_CM
# 19.  Capped rows: PriceDiameterClass_cm_used == MAX where uncapped > MAX
# 20.  No selected log has missing price from diameter class > MAX
# 21.  No unpriced sawlog candidate has PriceDiameterClass_cm_uncapped > MAX
#
# Verdict :
#   PASS                   -- all checks pass, no missing prices
#   ACCEPTED WITH REVIEW   -- only known 31 dm gap, explained zero-log trees,
#                             or documented price-diameter capping
#   FAIL                   -- overlaps, value mismatch, required files missing,
#                             species/product mismatch, other missing prices
#
# Outputs :
#   outputs/mixed_species_example_bucking/audit_10_mixed_species_example_bucking.csv
#   outputs/mixed_species_example_bucking/audit_10_mixed_species_example_bucking.txt
# ==============================================================================


# ---- setup ----

library(readr)

root_dir <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
out_dir  <- file.path(root_dir, "outputs", "mixed_species_example_bucking")

# Must match R/10_run_mixed_species_example_bucking.R
MAX_PRICE_DIAMETER_CLASS_CM <- 50L

VALID_PKS <- c("spruce_sawlog", "spruce_pulpwood", "spruce_waste",
               "pine_sawlog",   "pine_pulpwood",   "pine_waste")

audit_log <- file.path(out_dir, "audit_10_mixed_species_example_bucking.txt")
audit_csv <- file.path(out_dir, "audit_10_mixed_species_example_bucking.csv")

cat("", file = audit_log)

alog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = audit_log, append = TRUE)
}

alog(
  "=============================================================",
  "  Audit: R/10_run_mixed_species_example_bucking.R outputs",
  sprintf("  Run: %s", Sys.time()),
  sprintf("  MAX_PRICE_DIAMETER_CLASS_CM  : %d cm", MAX_PRICE_DIAMETER_CLASS_CM),
  "============================================================="
)

results <- data.frame(check = character(0), result = character(0),
                      detail = character(0), stringsAsFactors = FALSE)

record <- function(check, result, detail = "") {
  tag <- sprintf("  [%-6s] %s", result, check)
  if (nchar(detail) > 0L) tag <- paste0(tag, " | ", detail)
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
  "mixed_species_selected_trees.csv",
  "mixed_species_stem_profiles_summary.csv",
  "mixed_species_selected_logs.csv",
  "mixed_species_tree_summary.csv",
  "mixed_species_candidate_diagnostics.csv",
  "mixed_species_unpriced_candidates.csv",
  "mixed_species_run_summary.txt"
)

all_ok <- TRUE
for (fn in required_files) {
  fp <- file.path(out_dir, fn)
  if (file.exists(fp)) {
    record(sprintf("File exists: %s", fn), PASS)
  } else {
    record(sprintf("File exists: %s", fn), FAIL, "File not found")
    all_ok <- FALSE
  }
}

if (!all_ok) {
  alog("FATAL: Required output files are missing. Cannot continue.")
  writeLines(capture.output(print(results)), audit_log)
  stop("Required output files missing. Run script 10 first.")
}


# ---- 2. Read files ----

alog("", "--- 2. Reading files ---")

sel   <- readr::read_csv(file.path(out_dir, "mixed_species_selected_logs.csv"),
                         show_col_types = FALSE)
ts    <- readr::read_csv(file.path(out_dir, "mixed_species_tree_summary.csv"),
                         show_col_types = FALSE)
diag  <- readr::read_csv(file.path(out_dir, "mixed_species_candidate_diagnostics.csv"),
                          show_col_types = FALSE)
unpr  <- readr::read_csv(file.path(out_dir, "mixed_species_unpriced_candidates.csv"),
                          show_col_types = FALSE)
stree <- readr::read_csv(file.path(out_dir, "mixed_species_selected_trees.csv"),
                          show_col_types = FALSE)
psum  <- readr::read_csv(file.path(out_dir, "mixed_species_stem_profiles_summary.csv"),
                          show_col_types = FALSE)

alog(sprintf("  Selected trees         : %d rows", nrow(stree)))
alog(sprintf("  Profile summary        : %d rows", nrow(psum)))
alog(sprintf("  Tree summary           : %d rows", nrow(ts)))
alog(sprintf("  Selected logs          : %d rows", nrow(sel)))
alog(sprintf("  Diagnostics            : %d rows", nrow(diag)))
alog(sprintf("  Unpriced candidates    : %d rows", nrow(unpr)))

get_diag <- function(key) {
  idx <- which(diag$item == key)
  if (length(idx) == 0L) return(NA_character_)
  diag$value[idx[1L]]
}


# ---- 3. Both species present ----

alog("", "--- 3. Both species in tree summary ---")

species_present <- unique(ts$species_group[!is.na(ts$species_group)])
has_spruce      <- "spruce" %in% species_present
has_pine        <- "pine"   %in% species_present

record(
  "Both spruce and pine present in tree summary",
  if (has_spruce && has_pine) PASS else REVIEW,
  sprintf("species present: %s", paste(sort(species_present), collapse = ", "))
)


# ---- 4. Tree count ----

alog("", "--- 4. Tree count ---")

n_ts <- nrow(ts)

record(
  sprintf("At least 100 trees processed | n = %d", n_ts),
  if (n_ts >= 100L) PASS else FAIL,
  sprintf("%d rows in tree_summary", n_ts)
)

if (n_ts >= 400L) {
  record("Preferably ~500 trees processed", PASS,
         sprintf("%d trees", n_ts))
} else {
  record("Preferably ~500 trees processed", REVIEW,
         sprintf("%d trees (< 400)", n_ts))
}


# ---- 5. Failed tree count ----

alog("", "--- 5. Failed trees ---")

n_fail_trees <- sum(!is.na(ts$error))
pct_fail     <- if (n_ts > 0L) round(n_fail_trees / n_ts * 100, 1) else NA

record(
  sprintf("Failed tree count | %d / %d (%.1f%%)",
          n_fail_trees, n_ts, pct_fail),
  if (n_fail_trees == 0L) PASS
  else if (n_fail_trees <= 5L) REVIEW
  else FAIL,
  if (n_fail_trees == 0L) "No optimizer errors" else
    paste("Errors:", paste(ts$error[!is.na(ts$error)], collapse = "; "))
)


# ---- 6. Candidates generated ----

alog("", "--- 6. Candidates generated ---")

ok_ts   <- ts[is.na(ts$error), ]
noncand <- ok_ts[!is.na(ok_ts$optimizer_status) &
                   !ok_ts$optimizer_status %in%
                     c("too_short", "no_products", "error"), ]
n_no_cands <- sum(is.na(noncand$n_generated_candidates) |
                    noncand$n_generated_candidates == 0L, na.rm = TRUE)

record(
  "Each successful tree has generated candidates",
  if (n_no_cands == 0L) PASS else REVIEW,
  sprintf("%d trees with 0 generated candidates (excluding too-short)",
          n_no_cands)
)


# ---- 7. Unique StartPos > 1 ----

alog("", "--- 7. Unique StartPos coverage ---")

non_short <- noncand[!is.na(noncand$n_unique_StartPos_generated) &
                       noncand$n_unique_StartPos_generated > 0L, ]
n_one_start <- sum(non_short$n_unique_StartPos_generated == 1L, na.rm = TRUE)

record(
  "Non-trivial trees have > 1 unique StartPos",
  if (n_one_start == 0L) PASS else REVIEW,
  sprintf("%d tree(s) with only 1 unique StartPos", n_one_start)
)


# ---- 8. No overlapping selected logs ----

alog("", "--- 8. No overlapping selected logs ---")

n_overlap <- 0L
if (nrow(sel) > 1L) {
  for (tid in unique(sel$treid)) {
    t_sel <- sel[sel$treid == tid, ]
    t_sel <- t_sel[order(t_sel$StartPos), ]
    if (nrow(t_sel) > 1L) {
      for (k in seq_len(nrow(t_sel) - 1L)) {
        if (t_sel$StopPos[k] > t_sel$StartPos[k + 1L]) {
          n_overlap <- n_overlap + 1L
        }
      }
    }
  }
}

record(
  "No overlapping selected logs",
  if (n_overlap == 0L) PASS else FAIL,
  sprintf("%d overlapping pair(s)", n_overlap)
)


# ---- 9. LogLength == StopPos - StartPos ----

alog("", "--- 9. LogLength == StopPos - StartPos ---")

if (nrow(sel) > 0L) {
  n_bad <- sum(sel$LogLength != (sel$StopPos - sel$StartPos), na.rm = TRUE)
  record(
    sprintf("LogLength == StopPos - StartPos | %d logs", nrow(sel)),
    if (n_bad == 0L) PASS else FAIL,
    sprintf("%d logs with wrong LogLength", n_bad)
  )
} else {
  record("LogLength == StopPos - StartPos", REVIEW, "No selected logs to check")
}


# ---- 10. No missing prices/values in selected logs ----

alog("", "--- 10. No missing prices or values in selected logs ---")

if (nrow(sel) > 0L) {
  n_na_price <- sum(is.na(sel$PriceNOKperM3))
  n_na_val   <- sum(is.na(sel$value_nok))
  record("No missing PriceNOKperM3 (selected)",
         if (n_na_price == 0L) PASS else FAIL,
         sprintf("%d NA", n_na_price))
  record("No missing value_nok (selected)",
         if (n_na_val == 0L) PASS else FAIL,
         sprintf("%d NA", n_na_val))
} else {
  record("No missing PriceNOKperM3 (selected)", REVIEW, "No selected logs")
  record("No missing value_nok (selected)",     REVIEW, "No selected logs")
}


# ---- 11. ProductKey matches species_group ----

alog("", "--- 11. ProductKey matches species_group ---")

if (nrow(sel) > 0L && "species_group" %in% names(sel) &&
    "ProductKey" %in% names(sel)) {

  # spruce trees must only use spruce_* products; pine must use pine_* only.
  mismatch_spruce <- sel[sel$species_group == "spruce" &
                           !grepl("^spruce_", sel$ProductKey), ]
  mismatch_pine   <- sel[sel$species_group == "pine" &
                           !grepl("^pine_",   sel$ProductKey), ]
  n_mismatch      <- nrow(mismatch_spruce) + nrow(mismatch_pine)

  record(
    "ProductKey matches species_group for all selected logs",
    if (n_mismatch == 0L) PASS else FAIL,
    sprintf("%d mismatched row(s)", n_mismatch)
  )

  invalid_pks <- unique(sel$ProductKey[!sel$ProductKey %in% VALID_PKS])
  record(
    "All selected ProductKey values are valid",
    if (length(invalid_pks) == 0L) PASS else FAIL,
    if (length(invalid_pks) == 0L) {
      paste("Valid:", paste(sort(unique(sel$ProductKey)), collapse = ", "))
    } else {
      paste("Invalid:", paste(invalid_pks, collapse = ", "))
    }
  )
} else {
  record("ProductKey matches species_group", REVIEW, "No selected logs or missing columns")
  record("All selected ProductKey values are valid", REVIEW, "No selected logs")
}


# ---- 12. Per-tree value consistency ----

alog("", "--- 12. Per-tree value consistency ---")

if (nrow(sel) > 0L && "total_selected_value_nok" %in% names(ts)) {
  val_by_tree <- tapply(sel$value_nok, sel$treid, sum, na.rm = TRUE)
  all_ok <- TRUE
  for (tid in names(val_by_tree)) {
    ts_val <- ts$total_selected_value_nok[ts$treid == tid]
    if (length(ts_val) == 1L && !is.na(ts_val)) {
      if (abs(val_by_tree[tid] - ts_val) > 0.01) all_ok <- FALSE
    }
  }
  record(
    "Per-tree value consistent (summary vs selected logs)",
    if (all_ok) PASS else FAIL
  )
} else {
  record("Per-tree value consistency", REVIEW, "No selected logs or column missing")
}


# ---- 13. Per-tree volume consistency ----

alog("", "--- 13. Per-tree volume consistency ---")

if (nrow(sel) > 0L && "total_selected_volume_m3" %in% names(ts)) {
  vol_by_tree <- tapply(sel$price_volume_m3, sel$treid, sum, na.rm = TRUE)
  all_ok <- TRUE
  for (tid in names(vol_by_tree)) {
    ts_vol <- ts$total_selected_volume_m3[ts$treid == tid]
    if (length(ts_vol) == 1L && !is.na(ts_vol)) {
      if (abs(vol_by_tree[tid] - ts_vol) > 1e-4) all_ok <- FALSE
    }
  }
  record(
    "Per-tree volume consistent (summary vs selected logs)",
    if (all_ok) PASS else FAIL
  )
} else {
  record("Per-tree volume consistency", REVIEW, "No selected logs or column missing")
}


# ---- 14. No unpriced 31 dm sawlog selected ----

alog("", "--- 14. No unpriced 31 dm sawlog selected ---")

if (nrow(sel) > 0L && "LengthClass_dm" %in% names(sel)) {
  sel_dm31 <- sel[!is.na(sel$LengthClass_dm) &
                    sel$LengthClass_dm == 31L &
                    grepl("sawlog", sel$ProductKey) &
                    (is.na(sel$PriceNOKperM3) | is.na(sel$value_nok)), ]
  record(
    "No unpriced 31 dm sawlog selected",
    if (nrow(sel_dm31) == 0L) PASS else FAIL,
    sprintf("%d unpriced 31 dm sawlog row(s) in selected", nrow(sel_dm31))
  )
} else {
  record("No unpriced 31 dm sawlog selected", PASS, "No selected logs to check")
}


# ---- 15. Missing prices only from 31 dm gap ----

alog("", "--- 15. Missing prices only from known 31 dm sawlog gap ---")

if (nrow(unpr) > 0L && "ProductKey" %in% names(unpr) &&
    "LengthClass_dm" %in% names(unpr)) {

  known_miss <- unpr[grepl("sawlog", unpr$ProductKey) &
                       !is.na(unpr$LengthClass_dm) &
                       unpr$LengthClass_dm == 31L, ]
  other_miss <- unpr[!(grepl("sawlog", unpr$ProductKey) &
                         !is.na(unpr$LengthClass_dm) &
                         unpr$LengthClass_dm == 31L), ]

  record(
    sprintf("REVIEW: 31 dm sawlog price gap | %d unpriced candidate(s)",
            nrow(known_miss)),
    REVIEW,
    "Known issue: sawlog price matrices start at 34 dm"
  )

  if (nrow(other_miss) == 0L) {
    record(
      "No missing prices outside known 31 dm gap",
      PASS,
      "All unpriced candidates are the expected 31 dm sawlog gap"
    )
  } else {
    miss_tbl <- as.data.frame(
      table(ProductKey = other_miss$ProductKey,
            LengthClass_dm = other_miss$LengthClass_dm),
      stringsAsFactors = FALSE)
    miss_tbl <- miss_tbl[miss_tbl$Freq > 0L, ]
    miss_str <- paste(
      apply(miss_tbl, 1,
            function(r) paste0(r["ProductKey"], "/dm", r["LengthClass_dm"],
                               "=", r["Freq"])),
      collapse = "; ")
    record(
      "No missing prices outside known 31 dm gap",
      FAIL,
      sprintf("%d unexpected unpriced candidates: %s", nrow(other_miss), miss_str)
    )
  }

} else if (nrow(unpr) == 0L) {
  record("Missing prices check (no unpriced candidates)", PASS,
         "Zero unpriced candidates")
} else {
  record("Missing prices check", REVIEW,
         "Unpriced candidates file exists but lacks required columns")
}


# ---- 16. No row inflation from price join ----

alog("", "--- 16. No row inflation from price join ---")

if ("n_priced_candidates" %in% names(ts) && nrow(sel) > 0L) {
  inflated <- character(0)
  for (tid in unique(sel$treid)) {
    n_sel_t    <- sum(sel$treid == tid)
    n_priced_t <- ts$n_priced_candidates[ts$treid == tid]
    if (length(n_priced_t) == 1L && !is.na(n_priced_t)) {
      if (n_sel_t > n_priced_t) {
        inflated <- c(inflated, tid)
      }
    }
  }
  record(
    "No row inflation from price join",
    if (length(inflated) == 0L) PASS else FAIL,
    if (length(inflated) == 0L) "selected <= priced for all trees" else
      paste("Inflated trees:", paste(inflated, collapse = ", "))
  )
} else {
  record("No row inflation check", REVIEW, "Column not available")
}


# ---- 17. At least some trees have multiple logs ----

alog("", "--- 17. Trees with multiple selected logs ---")

if (nrow(sel) > 0L) {
  logs_per_tree <- tapply(seq_len(nrow(sel)), sel$treid, length)
  n_multi_log   <- sum(logs_per_tree > 1L)
  record(
    sprintf("At least some trees have > 1 selected log | %d tree(s)", n_multi_log),
    if (n_multi_log > 0L) PASS else FAIL
  )
} else {
  record("Trees with multiple logs", FAIL, "No selected logs at all")
}


# ---- 18. Trees with zero selected logs ----

alog("", "--- 18. Trees with zero selected logs ---")

n_with_logs <- sum(!is.na(ts$n_selected_logs) & ts$n_selected_logs > 0L)
n_zero_logs <- sum(!is.na(ts$n_selected_logs) & ts$n_selected_logs == 0L)
pct_zero    <- if (nrow(ts) > 0L) round(n_zero_logs / nrow(ts) * 100, 1) else NA

record(
  sprintf("At least some trees have selected logs | %d / %d trees",
          n_with_logs, nrow(ts)),
  if (n_with_logs > 0L) PASS else FAIL
)

if (n_zero_logs > 0L) {
  zero_ts  <- ts[!is.na(ts$n_selected_logs) & ts$n_selected_logs == 0L, ]
  max_dbh  <- if ("dbh_cm" %in% names(zero_ts)) {
    max(zero_ts$dbh_cm, na.rm = TRUE)
  } else NA
  record(
    sprintf("REVIEW: %d tree(s) with 0 selected logs (%.1f%%)",
            n_zero_logs, pct_zero),
    REVIEW,
    sprintf("max DBH of zero-log trees = %.1f cm; status(es): %s",
            max_dbh,
            paste(unique(zero_ts$optimizer_status), collapse = ", "))
  )
}


# ---- 19. PriceDiameterClass_cm_used <= MAX ----

alog("", "--- 19. PriceDiameterClass_cm_used <= MAX_PRICE_DIAMETER_CLASS_CM ---")

if (nrow(sel) > 0L && "PriceDiameterClass_cm_used" %in% names(sel)) {
  n_above_max <- sum(!is.na(sel$PriceDiameterClass_cm_used) &
                       sel$PriceDiameterClass_cm_used > MAX_PRICE_DIAMETER_CLASS_CM)
  record(
    sprintf("PriceDiameterClass_cm_used <= %d cm in all selected logs",
            MAX_PRICE_DIAMETER_CLASS_CM),
    if (n_above_max == 0L) PASS else FAIL,
    sprintf("%d selected log(s) with PriceDiameterClass_cm_used > %d",
            n_above_max, MAX_PRICE_DIAMETER_CLASS_CM)
  )

  # Count and report how many selected logs had capping applied.
  n_capped_sel <- sum(!is.na(sel$PriceDiameterClass_cm_uncapped) &
                        !is.na(sel$PriceDiameterClass_cm_used) &
                        sel$PriceDiameterClass_cm_uncapped >
                          sel$PriceDiameterClass_cm_used)
  detail_cap <- sprintf("%d selected log(s) had PriceDiameterClass_cm capped "
                        , n_capped_sel)
  detail_cap <- paste0(detail_cap,
    sprintf("(uncapped > used; cap = %d cm)", MAX_PRICE_DIAMETER_CLASS_CM))

  if (n_capped_sel > 0L) {
    record(
      "Price-diameter capping documented in selected logs",
      REVIEW,
      detail_cap
    )
  } else {
    record(
      "Price-diameter capping documented in selected logs",
      PASS,
      "No selected log had its diameter class capped (all <= max)"
    )
  }
} else if (nrow(sel) == 0L) {
  record("PriceDiameterClass_cm_used <= MAX (no logs)", PASS, "No selected logs")
} else {
  record("PriceDiameterClass_cm_used <= MAX", FAIL,
         "Column PriceDiameterClass_cm_used missing from selected_logs.csv")
}


# ---- 20. Uncapped/used consistency ----

alog("", "--- 20. Uncapped/used consistency ---")

if (nrow(sel) > 0L && "PriceDiameterClass_cm_uncapped" %in% names(sel) &&
    "PriceDiameterClass_cm_used" %in% names(sel)) {

  # Where uncapped > MAX, used must be exactly MAX.
  over_max <- sel[!is.na(sel$PriceDiameterClass_cm_uncapped) &
                    sel$PriceDiameterClass_cm_uncapped > MAX_PRICE_DIAMETER_CLASS_CM, ]
  if (nrow(over_max) > 0L) {
    n_bad <- sum(over_max$PriceDiameterClass_cm_used != MAX_PRICE_DIAMETER_CLASS_CM,
                 na.rm = TRUE)
    record(
      "Capped rows: PriceDiameterClass_cm_used == MAX where uncapped > MAX",
      if (n_bad == 0L) PASS else FAIL,
      sprintf("%d row(s) with uncapped > MAX but used != MAX", n_bad)
    )
  } else {
    record(
      "Capped rows: uncapped/used consistency",
      PASS,
      "No selected log had uncapped diameter class > MAX"
    )
  }

  # Where uncapped <= MAX, used must equal uncapped.
  at_or_below <- sel[!is.na(sel$PriceDiameterClass_cm_uncapped) &
                       sel$PriceDiameterClass_cm_uncapped <= MAX_PRICE_DIAMETER_CLASS_CM, ]
  if (nrow(at_or_below) > 0L) {
    n_differ <- sum(at_or_below$PriceDiameterClass_cm_used !=
                      at_or_below$PriceDiameterClass_cm_uncapped, na.rm = TRUE)
    record(
      "Uncapped rows: PriceDiameterClass_cm_used == uncapped where uncapped <= MAX",
      if (n_differ == 0L) PASS else FAIL,
      sprintf("%d row(s) where uncapped <= MAX but used != uncapped", n_differ)
    )
  }
} else {
  record("Uncapped/used consistency", REVIEW,
         "Required columns missing from selected_logs.csv")
}


# ---- 21. No unpriced sawlog with diameter > MAX ----

alog("", "--- 21. No unpriced sawlog due to diameter > MAX ---")

if (nrow(unpr) > 0L && "PriceDiameterClass_cm_used" %in% names(unpr) &&
    "ProductKey" %in% names(unpr)) {

  # After capping, PriceDiameterClass_cm_used must be <= MAX for all sawlog
  # candidates (priced and unpriced alike).  Any unpriced sawlog that still has
  # PriceDiameterClass_cm_used > MAX means the cap was not applied correctly.
  # (Unpriced sawlogs with PriceDiameterClass_cm_uncapped > MAX but
  #  PriceDiameterClass_cm_used == MAX are unpriced for another reason, e.g.
  #  the known 31 dm length-class gap -- the cap DID work for them.)
  unpr_sawlog_bad_used <- unpr[
    grepl("sawlog", unpr$ProductKey) &
      !is.na(unpr$PriceDiameterClass_cm_used) &
      unpr$PriceDiameterClass_cm_used > MAX_PRICE_DIAMETER_CLASS_CM, ]

  record(
    sprintf(
      "No unpriced sawlog candidate has PriceDiameterClass_cm_used > %d",
      MAX_PRICE_DIAMETER_CLASS_CM),
    if (nrow(unpr_sawlog_bad_used) == 0L) PASS else FAIL,
    sprintf(
      "%d unpriced sawlog candidate(s) with PriceDiameterClass_cm_used > %d (cap failed)",
      nrow(unpr_sawlog_bad_used), MAX_PRICE_DIAMETER_CLASS_CM)
  )
} else if (nrow(unpr) == 0L) {
  record("No unpriced sawlog with PriceDiameterClass_cm_used > MAX", PASS,
         "No unpriced candidates")
} else {
  record("No unpriced sawlog with PriceDiameterClass_cm_used > MAX", REVIEW,
         "Column PriceDiameterClass_cm_used missing from unpriced_candidates.csv")
}


# ---- 22. Save audit results ----

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
  sprintf("  Script 10 outputs: %s", verdict),
  "============================================================="
)

if (n_review > 0L) {
  alog("", "REVIEW items:")
  for (j in which(results$result == REVIEW)) {
    alog(sprintf("  [REVIEW] %s | %s", results$check[j], results$detail[j]))
  }
}
if (n_fail > 0L) {
  alog("", "FAIL items:")
  for (j in which(results$result == FAIL)) {
    alog(sprintf("  [FAIL]   %s | %s", results$check[j], results$detail[j]))
  }
}

alog(sprintf("\nAudit log: %s", audit_log))
alog(sprintf("[%s] Script 10 audit complete.", verdict))
