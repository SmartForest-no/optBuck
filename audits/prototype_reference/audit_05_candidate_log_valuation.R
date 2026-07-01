# ==============================================================================
# audits/audit_05_candidate_log_valuation.R
#
# Purpose : Independently audit outputs from R/05_test_candidate_log_valuation.R.
#           Reads only – does not modify any project input or output files.
#           Writes audit outputs to outputs/valuation_tests/.
#
# Checks  : PASS / REVIEW / FAIL
# ==============================================================================

library(readr)
library(dplyr)

root_dir   <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
val_dir    <- file.path(root_dir, "outputs", "valuation_tests")
pm_dir     <- file.path(root_dir, "inputs",  "price_matrices")
inputs_dir <- file.path(root_dir, "inputs")

dir.create(val_dir, showWarnings = FALSE, recursive = TRUE)

audit_log_file <- file.path(val_dir, "audit_05_candidate_log_valuation.txt")
cat("", file = audit_log_file)

# --------------------------------------------------------------------------- #
# helpers                                                                      #
# --------------------------------------------------------------------------- #

checks <- list()

add_check <- function(label, result, msg = "") {
  checks[[length(checks) + 1L]] <<- list(label = label, result = result,
                                          msg   = msg)
  prefix <- switch(result,
    PASS   = "  [PASS]  ",
    REVIEW = "  [REVIEW]",
    FAIL   = "  [FAIL]  ",
    "  [????]  "
  )
  line <- sprintf("%s %s%s", prefix, label,
                  if (nchar(trimws(msg)) > 0) paste0(" | ", msg) else "")
  cat(line, "\n")
  cat(line, "\n", file = audit_log_file, append = TRUE)
}

alog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = audit_log_file, append = TRUE)
}

# Safe numeric helpers – avoid Inf/-Inf when all values are NA.
safe_min  <- function(x) { v <- x[!is.na(x)]; if (length(v)) min(v)  else NA_real_ }
safe_max  <- function(x) { v <- x[!is.na(x)]; if (length(v)) max(v)  else NA_real_ }
safe_mean <- function(x) { v <- x[!is.na(x)]; if (length(v)) mean(v) else NA_real_ }

alog(
  "=============================================================",
  "  Audit: R/05_test_candidate_log_valuation.R outputs",
  sprintf("  Run: %s", Sys.time()),
  "============================================================="
)

# --------------------------------------------------------------------------- #
# 1. Required output files exist                                               #
# --------------------------------------------------------------------------- #

alog("", "--- 1. Required output files ---")

required_files <- c(
  "candidate_logs_valued_test.csv",
  "candidate_logs_unmatched_price_keys.csv",
  "candidate_log_valuation_summary.txt",
  "price_join_diagnostic_by_product.csv"
)

all_files_ok <- TRUE
for (fname in required_files) {
  fpath <- file.path(val_dir, fname)
  if (file.exists(fpath)) {
    add_check(sprintf("File exists: %s", fname), "PASS")
  } else {
    add_check(sprintf("File exists: %s", fname), "FAIL", "not found")
    all_files_ok <- FALSE
  }
}

if (!all_files_ok)
  stop("Cannot continue audit: required output file(s) missing from outputs/valuation_tests/.")

# --------------------------------------------------------------------------- #
# 2. Read outputs and reference inputs                                         #
# --------------------------------------------------------------------------- #

alog("", "--- 2. Reading files ---")

valued    <- readr::read_csv(file.path(val_dir, "candidate_logs_valued_test.csv"),
                              show_col_types = FALSE)
unmatched <- readr::read_csv(file.path(val_dir, "candidate_logs_unmatched_price_keys.csv"),
                              show_col_types = FALSE)
diag_pk   <- readr::read_csv(file.path(val_dir, "price_join_diagnostic_by_product.csv"),
                              show_col_types = FALSE)
pm        <- readr::read_csv(file.path(pm_dir, "PriceMatrix_long_default_2021.csv"),
                              show_col_types = FALSE)

alog(sprintf("  valued rows     : %d", nrow(valued)))
alog(sprintf("  unmatched rows  : %d", nrow(unmatched)))
alog(sprintf("  diag_pk rows    : %d", nrow(diag_pk)))
alog(sprintf("  PriceMatrix rows: %d", nrow(pm)))

# --------------------------------------------------------------------------- #
# 3. Required columns in valued table                                          #
# --------------------------------------------------------------------------- #

alog("", "--- 3. Required columns in candidate_logs_valued_test ---")

required_cols <- c(
  "ProductKey", "LogLength", "TopDiameter", "price_volume_m3", "eligible",
  "PriceScenario", "LengthClass_dm", "PriceDiameterClass_cm",
  "PriceNOKperM3", "value_nok"
)
missing_cols <- setdiff(required_cols, names(valued))
if (length(missing_cols) == 0) {
  add_check("All required columns present", "PASS")
} else {
  add_check("All required columns present", "FAIL",
            paste("Missing:", paste(missing_cols, collapse = ", ")))
}

# --------------------------------------------------------------------------- #
# 4. All rows are eligible == TRUE                                             #
# --------------------------------------------------------------------------- #

alog("", "--- 4. All rows are eligible == TRUE ---")

n_not_eligible <- sum(!valued$eligible, na.rm = TRUE)
if (n_not_eligible == 0) {
  add_check("All rows eligible == TRUE", "PASS")
} else {
  add_check("All rows eligible == TRUE", "FAIL",
            sprintf("%d rows have eligible != TRUE", n_not_eligible))
}

# --------------------------------------------------------------------------- #
# 5. LengthClass_dm = LogLength / 10                                          #
# --------------------------------------------------------------------------- #

alog("", "--- 5. LengthClass_dm derivation ---")

lc_ok <- all(abs(valued$LengthClass_dm - valued$LogLength / 10) < 1e-9,
              na.rm = TRUE)
if (lc_ok) {
  add_check("LengthClass_dm == LogLength / 10", "PASS")
} else {
  n_bad <- sum(abs(valued$LengthClass_dm - valued$LogLength / 10) >= 1e-9,
               na.rm = TRUE)
  add_check("LengthClass_dm == LogLength / 10", "FAIL",
            sprintf("%d inconsistent rows", n_bad))
}

# --------------------------------------------------------------------------- #
# 6. PriceDiameterClass_cm = floor(TopDiameter / 10)                         #
# --------------------------------------------------------------------------- #

alog("", "--- 6. PriceDiameterClass_cm derivation ---")

dc_ok <- all(abs(valued$PriceDiameterClass_cm - floor(valued$TopDiameter / 10)) < 1e-9,
              na.rm = TRUE)
if (dc_ok) {
  add_check("PriceDiameterClass_cm == floor(TopDiameter / 10)", "PASS")
} else {
  n_bad <- sum(abs(valued$PriceDiameterClass_cm -
                     floor(valued$TopDiameter / 10)) >= 1e-9, na.rm = TRUE)
  add_check("PriceDiameterClass_cm == floor(TopDiameter / 10)", "FAIL",
            sprintf("%d inconsistent rows", n_bad))
}

# --------------------------------------------------------------------------- #
# 7. value_nok = price_volume_m3 * PriceNOKperM3                              #
# --------------------------------------------------------------------------- #

alog("", "--- 7. value_nok calculation ---")

priced_rows <- valued[!is.na(valued$PriceNOKperM3) & !is.na(valued$price_volume_m3), ]
if (nrow(priced_rows) > 0) {
  expected_val <- priced_rows$price_volume_m3 * priced_rows$PriceNOKperM3
  max_val_diff <- max(abs(priced_rows$value_nok - expected_val), na.rm = TRUE)
  if (max_val_diff < 1e-9) {
    add_check("value_nok == price_volume_m3 * PriceNOKperM3", "PASS",
              sprintf("%d priced rows checked, max diff = %g", nrow(priced_rows), max_val_diff))
  } else {
    n_bad_val <- sum(abs(priced_rows$value_nok - expected_val) >= 1e-9, na.rm = TRUE)
    add_check("value_nok == price_volume_m3 * PriceNOKperM3", "FAIL",
              sprintf("%d rows have incorrect value_nok", n_bad_val))
  }
} else {
  add_check("value_nok == price_volume_m3 * PriceNOKperM3", "REVIEW",
            "No rows with both PriceNOKperM3 and price_volume_m3 non-NA")
}

# --------------------------------------------------------------------------- #
# 8. Sawlog matched prices agree with PriceMatrix_long                        #
# --------------------------------------------------------------------------- #

alog("", "--- 8. Sawlog prices match PriceMatrix_long ---")

pm_matrix_ref <- pm[pm$PriceType == "matrix",
                    c("PriceScenario", "ProductKey", "LengthClass_dm",
                      "PriceDiameterClass_cm", "PriceNOKperM3")]
names(pm_matrix_ref)[5] <- "RefPriceNOKperM3"

sawlog_priced <- valued[!is.na(valued$PriceNOKperM3) &
                          valued$ProductKey %in%
                            pm$ProductKey[pm$PriceType == "matrix"], ]

if (nrow(sawlog_priced) > 0) {
  saw_check <- merge(
    sawlog_priced[, c("PriceScenario", "ProductKey", "LengthClass_dm",
                      "PriceDiameterClass_cm", "PriceNOKperM3")],
    pm_matrix_ref,
    by = c("PriceScenario", "ProductKey", "LengthClass_dm", "PriceDiameterClass_cm"),
    all.x = TRUE
  )
  n_missing_ref  <- sum(is.na(saw_check$RefPriceNOKperM3))
  n_price_mismatch <- sum(
    !is.na(saw_check$RefPriceNOKperM3) &
    abs(saw_check$PriceNOKperM3 - saw_check$RefPriceNOKperM3) >= 1e-6,
    na.rm = TRUE
  )
  if (n_missing_ref == 0 && n_price_mismatch == 0) {
    add_check("Sawlog prices match PriceMatrix_long", "PASS",
              sprintf("%d rows verified", nrow(saw_check)))
  } else {
    add_check("Sawlog prices match PriceMatrix_long", "FAIL",
              sprintf("%d missing in ref, %d price mismatches",
                      n_missing_ref, n_price_mismatch))
  }
} else {
  add_check("Sawlog prices match PriceMatrix_long", "REVIEW",
            "No priced sawlog rows to check")
}

# --------------------------------------------------------------------------- #
# 9. Flat (pulpwood/waste) prices agree with PriceMatrix_long                 #
# --------------------------------------------------------------------------- #

alog("", "--- 9. Flat prices match PriceMatrix_long ---")

pm_flat_ref <- pm[pm$PriceType == "flat",
                  c("PriceScenario", "ProductKey", "PriceNOKperM3")]
names(pm_flat_ref)[3] <- "RefPriceNOKperM3"

flat_priced <- valued[!is.na(valued$PriceNOKperM3) &
                        valued$ProductKey %in%
                          pm$ProductKey[pm$PriceType == "flat"], ]

if (nrow(flat_priced) > 0) {
  flat_check <- merge(
    flat_priced[, c("PriceScenario", "ProductKey", "PriceNOKperM3")],
    pm_flat_ref,
    by = c("PriceScenario", "ProductKey"),
    all.x = TRUE
  )
  n_flat_missing <- sum(is.na(flat_check$RefPriceNOKperM3))
  n_flat_mismatch <- sum(
    !is.na(flat_check$RefPriceNOKperM3) &
    abs(flat_check$PriceNOKperM3 - flat_check$RefPriceNOKperM3) >= 1e-6,
    na.rm = TRUE
  )
  if (n_flat_missing == 0 && n_flat_mismatch == 0) {
    add_check("Flat prices match PriceMatrix_long", "PASS",
              sprintf("%d rows verified", nrow(flat_check)))
  } else {
    add_check("Flat prices match PriceMatrix_long", "FAIL",
              sprintf("%d missing in ref, %d price mismatches",
                      n_flat_missing, n_flat_mismatch))
  }
} else {
  add_check("Flat prices match PriceMatrix_long", "REVIEW",
            "No priced flat rows to check")
}

# --------------------------------------------------------------------------- #
# 10. No row-count inflation from duplicate price joins                        #
# --------------------------------------------------------------------------- #

alog("", "--- 10. No row inflation from price join ---")

# The valued table should have the same number of rows as the eligible portion
# of the input candidate-log file.  Detect by checking that each (treid,
# ProductKey, StartPos, StopPos) combination appears exactly once.
if (all(c("treid", "StartPos", "StopPos") %in% names(valued))) {
  dup_keys <- paste(valued$treid, valued$ProductKey,
                    valued$StartPos, valued$StopPos, sep = "|")
  n_dupes <- sum(duplicated(dup_keys))
  if (n_dupes == 0) {
    add_check("No row inflation (no duplicate join keys)", "PASS")
  } else {
    add_check("No row inflation (no duplicate join keys)", "FAIL",
              sprintf("%d duplicate (treid, ProductKey, StartPos, StopPos) rows",
                      n_dupes))
  }
} else {
  add_check("No row inflation (treid/StartPos/StopPos columns present)", "REVIEW",
            "Columns treid, StartPos, StopPos not in valued table; skipping check")
}

# --------------------------------------------------------------------------- #
# 11. Missing prices are explained                                             #
# --------------------------------------------------------------------------- #

alog("", "--- 11. Missing prices explained ---")

n_missing_price <- sum(is.na(valued$PriceNOKperM3))

n_31dm_missing <- if ("LengthClass_dm" %in% names(valued)) {
  sum(is.na(valued$PriceNOKperM3) & !is.na(valued$LengthClass_dm) &
        valued$LengthClass_dm == 31, na.rm = TRUE)
} else 0L

n_other_missing <- n_missing_price - n_31dm_missing

if (n_missing_price == 0) {
  add_check("All eligible rows have prices", "PASS")
} else if (n_other_missing == 0) {
  add_check(
    "Missing prices explained",
    "REVIEW",
    sprintf(
      "%d missing price(s), all with LengthClass_dm == 31 (price matrix starts at 34 dm). Known issue.",
      n_31dm_missing
    )
  )
} else {
  add_check(
    "Missing prices explained",
    "FAIL",
    sprintf(
      "%d missing price(s) not explained by 31 dm issue (other unmatched = %d)",
      n_missing_price, n_other_missing
    )
  )
}

# --------------------------------------------------------------------------- #
# 12. Only expected ProductKey values                                          #
# --------------------------------------------------------------------------- #

alog("", "--- 12. ProductKey values ---")

expected_pks <- c("spruce_sawlog", "spruce_pulpwood",
                  "pine_sawlog",   "pine_pulpwood",
                  "spruce_waste",  "pine_waste")
unexpected_pks <- setdiff(unique(valued$ProductKey), expected_pks)
if (length(unexpected_pks) == 0) {
  add_check("Only expected ProductKey values", "PASS",
            paste(sort(unique(valued$ProductKey)), collapse = ", "))
} else {
  add_check("Only expected ProductKey values", "FAIL",
            paste("Unexpected:", paste(unexpected_pks, collapse = ", ")))
}

# --------------------------------------------------------------------------- #
# 13. Summary output                                                           #
# --------------------------------------------------------------------------- #

alog("", "--- 13. Audit summary tables ---")

# Price summary by ProductKey.
pk_audit <- do.call(rbind, lapply(sort(unique(valued$ProductKey)), function(pk) {
  rows <- valued[valued$ProductKey == pk, ]
  data.frame(
    ProductKey        = pk,
    n_rows            = nrow(rows),
    n_priced          = sum(!is.na(rows$PriceNOKperM3)),
    n_missing_price   = sum( is.na(rows$PriceNOKperM3)),
    n_31dm_unmatched  = sum( is.na(rows$PriceNOKperM3) &
                               !is.na(rows$LengthClass_dm) &
                               rows$LengthClass_dm == 31,
                             na.rm = TRUE),
    price_min         = safe_min(rows$PriceNOKperM3),
    price_max         = safe_max(rows$PriceNOKperM3),
    value_nok_total   = {v <- rows$value_nok[!is.na(rows$value_nok)];
                         if (length(v)) sum(v) else NA_real_},
    stringsAsFactors  = FALSE
  )
}))

alog(capture.output(print(pk_audit, row.names = FALSE)))

# --------------------------------------------------------------------------- #
# 14. Save audit outputs                                                       #
# --------------------------------------------------------------------------- #

alog("", "--- 14. Saving audit outputs ---")

checks_df <- do.call(rbind, lapply(checks, function(x)
  data.frame(label  = x$label,
             result = x$result,
             msg    = x$msg,
             stringsAsFactors = FALSE)
))
audit_csv <- file.path(val_dir, "audit_05_candidate_log_valuation.csv")
readr::write_csv(checks_df, audit_csv)
alog(sprintf("  Saved: %s", basename(audit_csv)))

# --------------------------------------------------------------------------- #
# 15. Final verdict                                                            #
# --------------------------------------------------------------------------- #

alog(
  "",
  "=============================================================",
  "  AUDIT FINAL SUMMARY",
  "============================================================="
)

n_pass   <- sum(checks_df$result == "PASS")
n_review <- sum(checks_df$result == "REVIEW")
n_fail   <- sum(checks_df$result == "FAIL")

alog(sprintf("  PASS   : %d", n_pass))
alog(sprintf("  REVIEW : %d", n_review))
alog(sprintf("  FAIL   : %d", n_fail))

overall <- if (n_fail > 0) {
  "REJECTED"
} else if (n_review > 0) {
  "ACCEPTED WITH REVIEW ITEMS"
} else {
  "ACCEPTED"
}

review_note <- if (n_review > 0 && n_fail == 0) {
  " (31 dm sawlog length gap is a known issue carried forward)"
} else {
  ""
}

alog("")
alog(sprintf("  Script 05 outputs: %s%s", overall, review_note))
alog("=============================================================")
alog(sprintf("  Audit log: %s", audit_log_file))

if (n_fail > 0) {
  cat("\nFAIL items:\n")
  for (i in which(checks_df$result == "FAIL"))
    cat(sprintf("  [FAIL] %s | %s\n", checks_df$label[i], checks_df$msg[i]))
  stop(sprintf("Audit FAILED: %d FAIL check(s). See %s.",
               n_fail, basename(audit_csv)))
}

message(sprintf("[%s] Script 05 audit complete.%s", overall, review_note))
