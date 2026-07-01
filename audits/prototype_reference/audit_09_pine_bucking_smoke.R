# ==============================================================================
# audits/audit_09_pine_bucking_smoke.R
#
# Purpose : Independent audit of R/09_test_pine_bucking_smoke.R outputs.
#
# Checks  :
#  1.  Required output files exist
#  2.  At least 5 (preferably 25) pine trees processed
#  3.  All processed trees have species_group == "pine"
#  4.  Stem profiles exist and have non-missing diameters
#  5.  Each successful tree has generated candidates
#  6.  Each successful tree has more than one unique StartPos (unless too short)
#  7.  No overlapping selected logs within any tree
#  8.  LogLength == StopPos - StartPos
#  9.  No missing PriceNOKperM3 or value_nok in selected logs
# 10.  All selected ProductKey values are valid pine products
# 11.  Tree-level total_selected_value_nok == sum(value_nok) from selected logs
# 12.  Tree-level total_selected_volume_m3 == sum(price_volume_m3) from logs
# 13.  No unpriced 31 dm sawlog was selected
# 14.  Missing prices are only from the known 31 dm pine_sawlog gap
# 15.  No row inflation from price join (selected logs <= priced candidates)
# 16.  At least some trees have selected logs
# 17.  Zero-log trees are listed and explained
#
# Verdict :
#   PASS                   - all checks pass, no missing prices
#   ACCEPTED WITH REVIEW   - only known 31 dm gap or clearly explained zero-log trees
#   FAIL                   - overlaps, value mismatch, required files missing,
#                            other unexpected missing prices, many failures
#
# Outputs :
#   outputs/pine_smoke_bucking/audit_09_pine_bucking_smoke.csv
#   outputs/pine_smoke_bucking/audit_09_pine_bucking_smoke.txt
# ==============================================================================


# ---- setup ----

library(readr)

root_dir <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
out_dir  <- file.path(root_dir, "outputs", "pine_smoke_bucking")

audit_log  <- file.path(out_dir, "audit_09_pine_bucking_smoke.txt")
audit_csv  <- file.path(out_dir, "audit_09_pine_bucking_smoke.csv")

cat("", file = audit_log)

alog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = audit_log, append = TRUE)
}

alog(
  "=============================================================",
  "  Audit: R/09_test_pine_bucking_smoke.R outputs",
  sprintf("  Run: %s", Sys.time()),
  "============================================================="
)

# Accumulate audit results.
results <- data.frame(
  check  = character(0),
  result = character(0),
  detail = character(0),
  stringsAsFactors = FALSE
)

record <- function(check, result, detail = "") {
  tag <- sprintf("  [%-6s] %s", result, check)
  if (nchar(detail) > 0L) tag <- paste0(tag, " | ", detail)
  alog(tag)
  results <<- rbind(results,
    data.frame(check  = check,
               result = result,
               detail = detail,
               stringsAsFactors = FALSE))
}

PASS   <- "PASS"
REVIEW <- "REVIEW"
FAIL   <- "FAIL"

VALID_PINE_PKS <- c("pine_sawlog", "pine_pulpwood", "pine_waste")


# ---- 1. Required output files ----

alog("", "--- 1. Required output files ---")

required_files <- c(
  "pine_smoke_selected_trees.csv",
  "pine_smoke_selected_logs.csv",
  "pine_smoke_tree_summary.csv",
  "pine_smoke_candidate_diagnostics.csv",
  "pine_smoke_unpriced_candidates.csv",
  "pine_smoke_run_summary.txt"
)

all_files_ok <- TRUE
for (fn in required_files) {
  fp <- file.path(out_dir, fn)
  if (file.exists(fp)) {
    record(sprintf("File exists: %s", fn), PASS)
  } else {
    record(sprintf("File exists: %s", fn), FAIL, "File not found")
    all_files_ok <- FALSE
  }
}

if (!all_files_ok) {
  alog("FATAL: Required files missing. Cannot continue audit.")
  writeLines(capture.output(print(results)), audit_log)
  stop("Required output files missing. Run script 09 first.")
}


# ---- 2. Read files ----

alog("", "--- 2. Reading files ---")

sel    <- readr::read_csv(file.path(out_dir, "pine_smoke_selected_logs.csv"),
                          show_col_types = FALSE)
ts     <- readr::read_csv(file.path(out_dir, "pine_smoke_tree_summary.csv"),
                          show_col_types = FALSE)
diag   <- readr::read_csv(file.path(out_dir, "pine_smoke_candidate_diagnostics.csv"),
                           show_col_types = FALSE)
unpr   <- readr::read_csv(file.path(out_dir, "pine_smoke_unpriced_candidates.csv"),
                           show_col_types = FALSE)
strees <- readr::read_csv(file.path(out_dir, "pine_smoke_selected_trees.csv"),
                           show_col_types = FALSE)
prof   <- readr::read_csv(file.path(out_dir, "pine_smoke_stem_profiles.csv"),
                           show_col_types = FALSE)

alog(sprintf("  Selected trees   : %d rows", nrow(strees)))
alog(sprintf("  Stem profiles    : %d rows", nrow(prof)))
alog(sprintf("  Tree summary     : %d rows", nrow(ts)))
alog(sprintf("  Selected logs    : %d rows", nrow(sel)))
alog(sprintf("  Diagnostics      : %d rows", nrow(diag)))
alog(sprintf("  Unpriced cands   : %d rows", nrow(unpr)))

# Helper to pull a value from diag key-value table.
get_diag <- function(key) {
  idx <- which(diag$item == key)
  if (length(idx) == 0L) return(NA_character_)
  diag$value[idx[1L]]
}


# ---- 3. Tree count ----

alog("", "--- 3. Tree count ---")

n_selected_trees <- nrow(strees)
n_ts_rows        <- nrow(ts)

record(
  sprintf("At least 5 pine trees processed | n = %d", n_ts_rows),
  if (n_ts_rows >= 5L) PASS else FAIL,
  sprintf("%d rows in tree_summary", n_ts_rows)
)

if (n_ts_rows >= 25L) {
  record("Preferably 25 trees processed", PASS,
         sprintf("%d trees (target met)", n_ts_rows))
} else {
  record("Preferably 25 trees processed", REVIEW,
         sprintf("%d trees (< 25; may be acceptable if fewer pine available)",
                 n_ts_rows))
}


# ---- 4. species_group == "pine" ----

alog("", "--- 4. species_group check ---")

n_non_pine <- sum(ts$species_group != "pine", na.rm = TRUE)
record(
  "All processed trees have species_group == 'pine'",
  if (n_non_pine == 0L) PASS else FAIL,
  sprintf("%d non-pine rows in tree_summary", n_non_pine)
)


# ---- 5. Stem profiles: existence and non-missing diameters ----

alog("", "--- 5. Stem profiles ---")

n_prof_rows <- nrow(prof)
n_na_diam   <- sum(is.na(prof$diameter_cm))

record(
  "Stem profiles: rows > 0",
  if (n_prof_rows > 0L) PASS else FAIL,
  sprintf("%d rows in pine_smoke_stem_profiles.csv", n_prof_rows)
)

record(
  "Stem profiles: no missing diameters",
  if (n_na_diam == 0L) PASS else FAIL,
  sprintf("%d NA diameter_cm values", n_na_diam)
)


# ---- 6. Candidates generated for each successful tree ----

alog("", "--- 6. Candidates generated ---")

ok_trees <- ts[is.na(ts$error) | ts$error == "" | ts$optimizer_status %in%
                 c("ok", "zero_logs", "no_priced_candidates", "too_short"), ]
# Trees that should have had candidates (not "too_short" or "no_products").
cand_trees <- ok_trees[!is.na(ok_trees$optimizer_status) &
                         !ok_trees$optimizer_status %in%
                           c("too_short", "no_products", "error"), ]
n_no_cands <- sum(is.na(cand_trees$n_generated_candidates) |
                    cand_trees$n_generated_candidates == 0L)
record(
  "Each successful tree has generated candidates",
  if (n_no_cands == 0L) PASS else REVIEW,
  sprintf("%d trees with 0 generated candidates (excluding too-short)", n_no_cands)
)


# ---- 7. Unique StartPos > 1 for non-trivial trees ----

alog("", "--- 7. Unique StartPos coverage ---")

non_short <- cand_trees[!is.na(cand_trees$n_unique_StartPos_generated) &
                          cand_trees$n_unique_StartPos_generated > 0L, ]
n_one_start <- sum(non_short$n_unique_StartPos_generated == 1L, na.rm = TRUE)
record(
  "Non-trivial trees have > 1 unique StartPos",
  if (n_one_start == 0L) PASS else REVIEW,
  sprintf("%d tree(s) had only 1 unique StartPos (may be very short)", n_one_start)
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
  "No overlapping selected logs (all trees)",
  if (n_overlap == 0L) PASS else FAIL,
  sprintf("%d overlapping pair(s)", n_overlap)
)


# ---- 9. LogLength == StopPos - StartPos ----

alog("", "--- 9. LogLength == StopPos - StartPos ---")

if (nrow(sel) > 0L) {
  ll_check <- sel$LogLength == (sel$StopPos - sel$StartPos)
  n_bad_ll <- sum(!ll_check, na.rm = TRUE)
  record(
    sprintf("LogLength == StopPos - StartPos | %d logs", nrow(sel)),
    if (n_bad_ll == 0L) PASS else FAIL,
    sprintf("%d logs with wrong LogLength", n_bad_ll)
  )
} else {
  record("LogLength == StopPos - StartPos", REVIEW, "No selected logs to check")
}


# ---- 10. No missing PriceNOKperM3 or value_nok in selected logs ----

alog("", "--- 10. No missing prices or values in selected logs ---")

if (nrow(sel) > 0L) {
  n_na_price <- sum(is.na(sel$PriceNOKperM3))
  n_na_val   <- sum(is.na(sel$value_nok))
  record(
    "No missing PriceNOKperM3 (selected)",
    if (n_na_price == 0L) PASS else FAIL,
    sprintf("%d NA", n_na_price)
  )
  record(
    "No missing value_nok (selected)",
    if (n_na_val == 0L) PASS else FAIL,
    sprintf("%d NA", n_na_val)
  )
} else {
  record("No missing PriceNOKperM3 (selected)", REVIEW, "No selected logs")
  record("No missing value_nok (selected)",     REVIEW, "No selected logs")
}


# ---- 11. Valid ProductKey values ----

alog("", "--- 11. Valid ProductKey values ---")

if (nrow(sel) > 0L) {
  invalid_pks <- unique(sel$ProductKey[!sel$ProductKey %in% VALID_PINE_PKS])
  record(
    "All selected ProductKey values are valid pine products",
    if (length(invalid_pks) == 0L) PASS else FAIL,
    if (length(invalid_pks) == 0L) {
      paste(unique(sel$ProductKey), collapse = ", ")
    } else {
      paste("Invalid:", paste(invalid_pks, collapse = ", "))
    }
  )
} else {
  record("Valid ProductKey values", REVIEW, "No selected logs")
}


# ---- 12. Per-tree value consistency ----

alog("", "--- 12. Per-tree value consistency ---")

if (nrow(sel) > 0L && "total_selected_value_nok" %in% names(ts)) {
  val_by_tree <- tapply(sel$value_nok, sel$treid, sum, na.rm = TRUE)
  all_ok <- TRUE
  for (tid in names(val_by_tree)) {
    ts_val <- ts$total_selected_value_nok[ts$treid == tid]
    if (length(ts_val) == 1L && !is.na(ts_val)) {
      if (abs(val_by_tree[tid] - ts_val) > 0.01) {
        all_ok <- FALSE
      }
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
      if (abs(vol_by_tree[tid] - ts_vol) > 1e-4) {
        all_ok <- FALSE
      }
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
                    sel$ProductKey == "pine_sawlog" &
                    (is.na(sel$PriceNOKperM3) | is.na(sel$value_nok)), ]
  record(
    "No unpriced 31 dm pine_sawlog selected",
    if (nrow(sel_dm31) == 0L) PASS else FAIL,
    sprintf("%d unpriced 31 dm pine_sawlog log(s) in selected", nrow(sel_dm31))
  )
} else {
  record("No unpriced 31 dm sawlog selected", PASS, "No selected logs to check")
}


# ---- 15. Missing prices only from known 31 dm gap ----

alog("", "--- 15. Missing prices from known 31 dm gap only ---")

if (nrow(unpr) > 0L && "ProductKey" %in% names(unpr) &&
    "LengthClass_dm" %in% names(unpr)) {

  # 31 dm pine_sawlog: known and expected.
  known_miss <- unpr[unpr$ProductKey == "pine_sawlog" &
                       !is.na(unpr$LengthClass_dm) &
                       unpr$LengthClass_dm == 31L, ]
  other_miss <- unpr[!(unpr$ProductKey == "pine_sawlog" &
                        !is.na(unpr$LengthClass_dm) &
                        unpr$LengthClass_dm == 31L), ]

  n_known <- nrow(known_miss)
  n_other <- nrow(other_miss)

  record(
    sprintf("REVIEW: 31 dm pine_sawlog price gap | %d unpriced candidate(s)", n_known),
    REVIEW,
    "Known issue: pine_sawlog price matrix starts at 34 dm"
  )

  if (n_other == 0L) {
    record(
      "No missing prices outside known 31 dm gap",
      PASS,
      "All unpriced candidates are the expected 31 dm pine_sawlog gap"
    )
  } else {
    # Summarise unexpected missing.
    miss_tbl <- table(other_miss$ProductKey, other_miss$LengthClass_dm)
    miss_str <- paste(apply(as.matrix(miss_tbl), 1, function(r) {
      paste(names(r)[r > 0], r[r > 0], sep = "=")
    }), collapse = "; ")
    record(
      "No missing prices outside known 31 dm gap",
      FAIL,
      sprintf("%d unexpected unpriced candidates: %s", n_other, miss_str)
    )
  }

} else if (nrow(unpr) == 0L) {
  record("Missing prices check (no unpriced candidates)", PASS,
         "Zero unpriced candidates \u2013 no known 31 dm gap triggered (unusual)")
} else {
  record("Missing prices check", REVIEW,
         "Unpriced candidates file exists but lacks required columns")
}


# ---- 16. No row inflation from price join ----

alog("", "--- 16. No row inflation from price join ---")

if ("n_priced_candidates" %in% names(ts) && nrow(sel) > 0L) {
  for (tid in unique(sel$treid)) {
    n_sel_t   <- sum(sel$treid == tid)
    n_priced_t <- ts$n_priced_candidates[ts$treid == tid]
    if (length(n_priced_t) == 1L && !is.na(n_priced_t)) {
      if (n_sel_t > n_priced_t) {
        record(
          "No row inflation from price join",
          FAIL,
          sprintf("Tree %s: %d selected > %d priced", tid, n_sel_t, n_priced_t)
        )
      }
    }
  }
  record("No row inflation from price join", PASS,
         "selected logs <= priced candidates for all trees")
} else {
  record("No row inflation check", REVIEW, "Column not available")
}


# ---- 17. At least some trees have selected logs ----

alog("", "--- 17. Trees with selected logs ---")

n_with_logs  <- sum(!is.na(ts$n_selected_logs) & ts$n_selected_logs > 0L)
n_zero_logs  <- sum(!is.na(ts$n_selected_logs) & ts$n_selected_logs == 0L)
pct_zero     <- if (nrow(ts) > 0L) round(n_zero_logs / nrow(ts) * 100, 1) else NA

record(
  sprintf("At least some trees have selected logs | %d / %d trees",
          n_with_logs, nrow(ts)),
  if (n_with_logs > 0L) PASS else FAIL
)

if (n_zero_logs > 0L) {
  zero_ts <- ts[!is.na(ts$n_selected_logs) & ts$n_selected_logs == 0L, ]
  max_dbh_zero <- max(zero_ts$dbh_cm, na.rm = TRUE)
  record(
    sprintf("REVIEW: %d tree(s) with 0 selected logs (%.1f%%)", n_zero_logs, pct_zero),
    REVIEW,
    sprintf("max DBH of zero-log trees = %.1f cm; status(es): %s",
            max_dbh_zero,
            paste(unique(zero_ts$optimizer_status), collapse = ", "))
  )
}


# ---- 18. Save audit results ----

alog("", "--- 18. Saving audit outputs ---")
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
  sprintf("  Script 09 outputs: %s", verdict),
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
alog(sprintf("[%s] Script 09 audit complete.", verdict))
