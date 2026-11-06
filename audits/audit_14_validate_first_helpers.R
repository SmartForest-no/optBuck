# ==============================================================================
# audits/audit_14_validate_first_helpers.R
#
# Purpose : Audit of R/14_validate_first_helpers.R outputs and the four helper
#           files it exercises.
#
# Checks  :
#  1.  Helper files exist (helpers_paths, helpers_products, helpers_valuation,
#      helpers_audit)
#  2.  Script 14 output files exist
#  3.  ProductData loads successfully (non-empty)
#  4.  LengthClasses loads successfully (non-empty)
#  5.  PriceMatrix_long loads successfully (non-empty)
#  6.  All six ACTIVE_PRODUCT_KEYS are present in ProductData
#  7.  DiameterUnderBark is TRUE for all active products (under-bark product
#      diameters since 2026-10-12; profiles stay over bark, see check 8)
#  8.  DiameterSource is taperNOR_over_bark for all active products
#  9.  Supported sawlog lengths for spruce_sawlog match expected 34-55 dm set
# 10.  Supported sawlog lengths for pine_sawlog match expected 34-55 dm set
# 11.  31 dm sawlog length is supported (interpolated) for spruce_sawlog
# 12.  31 dm sawlog length is supported (interpolated) for pine_sawlog
# 13.  cap_price_diameter_class() maps values > 50 to 50
# 14.  cap_price_diameter_class() preserves values <= 50
# 15.  prepare_price_join_keys() adds _uncapped and _used columns
# 16.  prepare_price_join_keys() stores original in _uncapped, cap in _used
# 17.  validate_price_matrix_against_products() returns no FAIL items
# 18.  validate_price_matrix_against_products() REVIEW items are all accepted:
#      PASS when every REVIEW is a zero-price item and every zero-price row
#      belongs to a waste product (accepted as intentional 2026-10-12);
#      REVIEW for the 31 dm gap or zero prices on non-waste products
# 19.  Accepted scripts 00-12 exist (no deletions detected)
# 20.  Accepted scripts 00-12 are unmodified (modified times are pre-today)
#
# Verdict :
#   PASS                   -- all checks pass
#   ACCEPTED WITH REVIEW   -- only REVIEW items are a 31 dm gap or a zero price
#                             on a non-waste product (check 18)
#   FAIL                   -- any helper function produces wrong results,
#                             required files missing, or accepted scripts changed
#
# Note (2026-08-03): the 31 dm sawlog gap was resolved by interpolation
# (a one-off script, since removed). Checks 11-12 now assert that
# 31 dm is supported and flagged PriceSource == "Interpolated", rather than
# asserting it is unsupported.
#
# Note (2026-10-12): product diameters are under bark (DiameterUnderBark =
# TRUE); check 7 asserts TRUE instead of FALSE. Profiles remain taperNOR over
# bark (check 8) and are converted with the bark model in R/helpers_bark.R.
#
# Note (2026-10-12): zero-priced waste (0 NOK/m3) is an accepted scope
# decision, recorded in CLAUDE.md "Accepted assumptions". Check 18 now
# records PASS when the only REVIEW items are zero prices on waste products.
#
# Outputs :
#   outputs/helper_validation/audit_14_validate_first_helpers.csv
#   outputs/helper_validation/audit_14_validate_first_helpers.txt
# ==============================================================================

suppressPackageStartupMessages(library(readr))

root_dir <- local({
  # OPTBUCK_ROOT -> walk up from getwd() -> stop()
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  env <- Sys.getenv("OPTBUCK_ROOT", unset = "")
  if (nzchar(env)) {
    d <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (!is_root(d))
      stop("Project root '", d, "' (from OPTBUCK_ROOT) does not contain ",
           "R/helpers_paths.R.", call. = FALSE)
  } else {
    d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    while (!is_root(d) && dirname(d) != d) d <- dirname(d)
    if (!is_root(d))
      stop("Cannot locate the project root: set OPTBUCK_ROOT, or run from ",
           "inside the project directory.", call. = FALSE)
  }
  d
})

suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)
source(file.path(root_dir, "audits", "helpers_audit.R"))

out_dir  <- path_output("helper_validation", root_dir = root_dir)
ensure_dir(out_dir)

audit_txt <- file.path(out_dir, "audit_14_validate_first_helpers.txt")
audit_csv <- file.path(out_dir, "audit_14_validate_first_helpers.csv")
init_audit_log(audit_txt)

# Use a simple environment to accumulate results
E <- new.env(parent = emptyenv())

rec <- function(check, result, detail = "") {
  record_check(E, check, result, detail, log_path = audit_txt)
}

PASS   <- "PASS"
REVIEW <- "REVIEW"
FAIL   <- "FAIL"

# Write header to log
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)
cat("  Audit: R/14_validate_first_helpers.R\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)


# ---- 1. Helper files exist ----

cat("\n--- 1. Helper files ---\n", file = audit_txt, append = TRUE)
cat("\n--- 1. Helper files ---\n")

helper_files <- c(
  "R/helpers_paths.R",
  "R/helpers_products.R",
  "R/helpers_valuation.R",
  "audits/helpers_audit.R"
)

for (fn in helper_files) {
  fp <- file.path(root_dir, fn)
  rec(sprintf("Helper exists: %s", fn),
      if (file.exists(fp)) PASS else FAIL,
      if (file.exists(fp)) "found" else "NOT FOUND")
}


# ---- 2. Script 14 output files ----

cat("\n--- 2. Script 14 output files ---\n", file = audit_txt, append = TRUE)
cat("\n--- 2. Script 14 output files ---\n")

s14_outputs <- c(
  "helper_product_check.csv",
  "helper_price_support_by_product.csv",
  "helper_unsupported_sawlog_lengths.csv",
  "helper_price_matrix_validation_summary.txt"
)

for (fn in s14_outputs) {
  fp <- file.path(out_dir, fn)
  rec(sprintf("Output exists: %s", fn),
      if (file.exists(fp)) PASS else FAIL,
      if (file.exists(fp)) "found" else "NOT FOUND")
}


# ---- 3-5. Data loads ----

cat("\n--- 3-5. Data loading ---\n", file = audit_txt, append = TRUE)
cat("\n--- 3-5. Data loading ---\n")

pd <- tryCatch(load_products(root_dir = root_dir),
               error = function(e) { message(e); NULL })
rec("ProductData loads (non-empty)",
    if (!is.null(pd) && nrow(pd) > 0L) PASS else FAIL,
    if (!is.null(pd)) sprintf("%d rows", nrow(pd)) else "error")

lc <- tryCatch(load_length_classes(root_dir = root_dir),
               error = function(e) { message(e); NULL })
rec("LengthClasses loads (non-empty)",
    if (!is.null(lc) && nrow(lc) > 0L) PASS else FAIL,
    if (!is.null(lc)) sprintf("%d rows", nrow(lc)) else "error")

pm <- tryCatch(load_price_matrix_long(root_dir = root_dir),
               error = function(e) { message(e); NULL })
rec("PriceMatrix_long loads (non-empty)",
    if (!is.null(pm) && nrow(pm) > 0L) PASS else FAIL,
    if (!is.null(pm)) sprintf("%d rows", nrow(pm)) else "error")


# ---- 6. Active ProductKeys ----

cat("\n--- 6. Active ProductKeys ---\n", file = audit_txt, append = TRUE)
cat("\n--- 6. Active ProductKeys ---\n")

if (!is.null(pd)) {
  present <- ACTIVE_PRODUCT_KEYS %in% pd$ProductKey
  n_miss  <- sum(!present)
  rec("All 6 ACTIVE_PRODUCT_KEYS in ProductData",
      if (n_miss == 0L) PASS else FAIL,
      if (n_miss == 0L)
        paste("present:", paste(ACTIVE_PRODUCT_KEYS, collapse = ", "))
      else
        paste("missing:", paste(ACTIVE_PRODUCT_KEYS[!present], collapse = ", ")))
} else {
  rec("All 6 ACTIVE_PRODUCT_KEYS in ProductData", FAIL, "ProductData not loaded")
}


# ---- 7. DiameterUnderBark ----

cat("\n--- 7. DiameterUnderBark ---\n", file = audit_txt, append = TRUE)
cat("\n--- 7. DiameterUnderBark ---\n")

if (!is.null(pd) && "DiameterUnderBark" %in% names(pd)) {
  active_pd <- pd[pd$ProductKey %in% ACTIVE_PRODUCT_KEYS, ]
  all_true  <- all(as.logical(active_pd$DiameterUnderBark) %in% TRUE)
  rec("DiameterUnderBark == TRUE for all active products",
      if (all_true) PASS else FAIL,
      sprintf("Values: %s",
              paste(unique(active_pd$DiameterUnderBark), collapse = ", ")))
} else {
  rec("DiameterUnderBark == TRUE", FAIL,
      if (is.null(pd)) "ProductData not loaded"
      else "DiameterUnderBark column missing")
}


# ---- 8. DiameterSource ----

cat("\n--- 8. DiameterSource ---\n", file = audit_txt, append = TRUE)
cat("\n--- 8. DiameterSource ---\n")

if (!is.null(pd) && "DiameterSource" %in% names(pd)) {
  active_pd <- pd[pd$ProductKey %in% ACTIVE_PRODUCT_KEYS, ]
  all_ok    <- all(active_pd$DiameterSource == EXPECTED_DIAMETER_SOURCE,
                   na.rm = TRUE)
  rec(sprintf("DiameterSource == '%s' for all active products",
              EXPECTED_DIAMETER_SOURCE),
      if (all_ok) PASS else FAIL,
      sprintf("Values: %s",
              paste(unique(active_pd$DiameterSource), collapse = ", ")))
} else {
  rec("DiameterSource check", FAIL,
      if (is.null(pd)) "ProductData not loaded"
      else "DiameterSource column missing")
}


# ---- 9-10. Supported sawlog lengths ----

cat("\n--- 9-10. Supported sawlog lengths ---\n", file = audit_txt, append = TRUE)
cat("\n--- 9-10. Supported sawlog lengths ---\n")

if (!is.null(pm)) {
  supported <- get_supported_sawlog_lengths(pm)

  for (pk in c("spruce_sawlog", "pine_sawlog")) {
    actual_dm <- sort(supported$LengthClass_dm[supported$ProductKey == pk])
    expected  <- EXPECTED_SUPPORTED_SAWLOG_DM
    matches   <- identical(actual_dm, expected)
    rec(
      sprintf("Supported lengths for %s match expected {%s} dm",
              pk, paste(expected, collapse = ", ")),
      if (matches) PASS else FAIL,
      sprintf("actual: {%s} dm", paste(actual_dm, collapse = ", "))
    )
  }
} else {
  for (pk in c("spruce_sawlog", "pine_sawlog")) {
    rec(sprintf("Supported lengths for %s", pk), FAIL, "PriceMatrix not loaded")
  }
}


# ---- 11-12. 31 dm sawlog length is supported (interpolated) ----

cat("\n--- 11-12. 31 dm sawlog length support ---\n", file = audit_txt, append = TRUE)
cat("\n--- 11-12. 31 dm sawlog length support ---\n")

if (!is.null(pm)) {
  for (pk in c("spruce_sawlog", "pine_sawlog")) {
    rows_31 <- pm[pm$ProductKey == pk & pm$LengthClass_dm == 31L &
                    !is.na(pm$PriceNOKperM3), ]
    is_supported <- nrow(rows_31) > 0L
    is_interpolated <- is_supported &&
      all(rows_31$PriceSource == "Interpolated", na.rm = TRUE)
    rec(
      sprintf("31 dm sawlog length is priced for %s", pk),
      if (is_supported && is_interpolated) PASS
      else if (is_supported) REVIEW
      else FAIL,
      if (is_supported && is_interpolated)
        sprintf("%d priced 31dm row(s); all PriceSource == 'Interpolated'", nrow(rows_31))
      else if (is_supported)
        sprintf("%d priced 31dm row(s); NOT all flagged PriceSource == 'Interpolated' — check provenance", nrow(rows_31))
      else "31 dm has no priced rows — gap has returned"
    )
  }
} else {
  for (pk in c("spruce_sawlog", "pine_sawlog")) {
    rec(sprintf("31 dm sawlog length is priced for %s", pk), FAIL,
        "PriceMatrix not loaded")
  }
}


# ---- 13-14. cap_price_diameter_class ----

cat("\n--- 13-14. cap_price_diameter_class ---\n", file = audit_txt, append = TRUE)
cat("\n--- 13-14. cap_price_diameter_class ---\n")

cap_tests <- as.integer(c(10, 24, 50, 51, 60, 70, 100))
cap_result <- cap_price_diameter_class(cap_tests, max_class_cm = 50L)
expected_cap <- pmin(cap_tests, 50L)

# Values > 50 → 50
above_50  <- cap_tests > 50L
n_capped  <- sum(above_50)
cap_ok    <- all(cap_result[above_50] == 50L, na.rm = TRUE)
rec("cap_price_diameter_class(): values > 50 mapped to 50",
    if (cap_ok && n_capped > 0L) PASS else FAIL,
    sprintf("%d input(s) > 50; all mapped to 50: %s",
            n_capped, cap_ok))

# Values <= 50 preserved
at_or_below <- cap_tests <= 50L
preserve_ok <- all(cap_result[at_or_below] == cap_tests[at_or_below], na.rm = TRUE)
rec("cap_price_diameter_class(): values <= 50 preserved unchanged",
    if (preserve_ok) PASS else FAIL,
    sprintf("%d input(s) <= 50; all preserved: %s",
            sum(at_or_below), preserve_ok))


# ---- 15-16. prepare_price_join_keys ----

cat("\n--- 15-16. prepare_price_join_keys ---\n", file = audit_txt, append = TRUE)
cat("\n--- 15-16. prepare_price_join_keys ---\n")

test_cands <- data.frame(
  PriceDiameterClass_cm = as.integer(c(12, 24, 50, 52, 60)),
  stringsAsFactors = FALSE)
capped_cands <- prepare_price_join_keys(test_cands, max_price_diameter_class_cm = 50L)

has_uncapped <- "PriceDiameterClass_cm_uncapped" %in% names(capped_cands)
has_used     <- "PriceDiameterClass_cm_used"     %in% names(capped_cands)
rec("prepare_price_join_keys() adds _uncapped column",
    if (has_uncapped) PASS else FAIL,
    if (has_uncapped) "column present" else "column missing")
rec("prepare_price_join_keys() adds _used column",
    if (has_used) PASS else FAIL,
    if (has_used) "column present" else "column missing")

if (has_uncapped && has_used) {
  uncapped_matches <- all(capped_cands$PriceDiameterClass_cm_uncapped ==
                            test_cands$PriceDiameterClass_cm)
  used_correct     <- all(capped_cands$PriceDiameterClass_cm_used ==
                            pmin(test_cands$PriceDiameterClass_cm, 50L))
  rec("_uncapped stores original PriceDiameterClass_cm unchanged",
      if (uncapped_matches) PASS else FAIL,
      sprintf("original match: %s", uncapped_matches))
  rec("_used == min(original, 50) for all rows",
      if (used_correct) PASS else FAIL,
      sprintf("cap correct: %s", used_correct))
}


# ---- 17-18. validate_price_matrix_against_products ----

cat("\n--- 17-18. Full price matrix validation ---\n", file = audit_txt, append = TRUE)
cat("\n--- 17-18. Full price matrix validation ---\n")

if (!is.null(pd) && !is.null(lc) && !is.null(pm)) {
  val <- validate_price_matrix_against_products(pd, lc, pm)

  n_fail_val   <- sum(val$checks$result == "FAIL",   na.rm = TRUE)
  n_review_val <- sum(val$checks$result == "REVIEW", na.rm = TRUE)

  rec("validate_price_matrix_against_products(): no FAIL items",
      if (n_fail_val == 0L) PASS else FAIL,
      sprintf("%d FAIL item(s) in validation", n_fail_val))

  review_items <- val$checks[val$checks$result == "REVIEW", ]
  only_31dm_review <- nrow(review_items) == 0L ||
    all(grepl("31 dm|zero.price", review_items$detail, ignore.case = TRUE))

  # Zero-priced waste is accepted as intentional (scope decision 2026-10-12).
  waste_keys <- pd$ProductKey[pd$ProductType == "waste"]
  zero_keys  <- unique(pm$ProductKey[!is.na(pm$PriceNOKperM3) &
                                     pm$PriceNOKperM3 == 0])
  only_zero_review   <- nrow(review_items) > 0L &&
    all(grepl("zero.price", review_items$detail, ignore.case = TRUE))
  zero_only_on_waste <- length(zero_keys) > 0L && all(zero_keys %in% waste_keys)
  waste_zero_accepted <- only_zero_review && zero_only_on_waste

  rec("validate_price_matrix_against_products(): REVIEW only for 31 dm gap or zero prices",
      if (n_review_val == 0L || waste_zero_accepted) PASS
      else if (only_31dm_review) REVIEW
      else FAIL,
      if (waste_zero_accepted)
        sprintf("%d REVIEW item(s), zero prices only on waste (%s): zero-priced waste accepted as intentional (scope decision 2026-10-12)",
                n_review_val, paste(zero_keys, collapse = ", "))
      else
        sprintf("%d REVIEW item(s); all expected: %s; zero-price products: %s",
                n_review_val, only_31dm_review,
                if (length(zero_keys) > 0L) paste(zero_keys, collapse = ", ")
                else "none"))
} else {
  rec("Full price matrix validation", FAIL,
      "One or more input files not loaded")
}


# ---- 19-20. Accepted scripts integrity ----

cat("\n--- 19-20. Accepted scripts integrity ---\n", file = audit_txt, append = TRUE)
cat("\n--- 19-20. Accepted scripts integrity ---\n")

accepted_script_names <- paste0(sprintf("%02d", 0:12),
  c("_prepare_nfi_2024_data",
    "_test_stratum_selection",
    "_generate_stem_profiles",
    "_define_products_and_volume",
    "_import_or_define_price_matrices",
    "_test_candidate_log_valuation",
    "_test_one_tree_bucking_optimizer",
    "_test_batch_bucking_optimizer",
    "_run_bucking_on_example_sample",
    "_test_pine_bucking_smoke",
    "_run_mixed_species_example_bucking",
    "_run_bucking_on_selected_stratum",
    "_summarise_selected_stratum_bucking"), ".R")

# Search in both original R/ location and scripts/prototype_reference/ (post-cleanup)
find_accepted_script <- function(name) {
  candidates <- c(
    file.path(root_dir, "R", name),
    file.path(root_dir, "scripts", "prototype_reference", name)
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) > 0L) found[1L] else NA_character_
}

accepted_scripts <- vapply(accepted_script_names, find_accepted_script, character(1))

n_exist   <- sum(!is.na(accepted_scripts))
n_missing <- sum( is.na(accepted_scripts))

rec("All accepted scripts 00-12 exist",
    if (n_missing == 0L) PASS else FAIL,
    sprintf("%d present, %d missing", n_exist, n_missing))

# Modification time check: accepted scripts 00-12 should not have been
# modified AFTER R/14_validate_first_helpers.R was created (i.e. after this
# cleanup phase began). We compare each script's mtime against the mtime of
# script 14 as the freeze boundary.  Scripts legitimately created earlier
# today (e.g. script 12, completed before helper work began) will pass.
# Script 14 may now live in scripts/prototype_reference/ after cleanup
s14_fp    <- if (file.exists(file.path(root_dir, "R", "14_validate_first_helpers.R"))) {
  file.path(root_dir, "R", "14_validate_first_helpers.R")
} else {
  file.path(root_dir, "scripts", "prototype_reference",
            "14_validate_first_helpers.R")
}
s14_mtime <- if (file.exists(s14_fp)) file.info(s14_fp)$mtime else Sys.time()

n_modified_after <- 0L
modified_names   <- character(0)

# Roadmap 8 (2026-11-05): script 00 was edited once on purpose: the Oracle alias and user name were replaced by the
# placeholders ORACLE_SERVICE / ORACLE_USER (publication check); it is exempt from the freeze check, its content is checked below.
for (fp in accepted_scripts[!is.na(accepted_scripts) & basename(accepted_scripts) != "00_prepare_nfi_2024_data.R"]) {
  mtime <- file.info(fp)$mtime
  if (!is.na(mtime) && mtime > s14_mtime) {
    n_modified_after <- n_modified_after + 1L
    modified_names   <- c(modified_names, basename(fp))
  }
}

rec("Accepted scripts 00-12 not modified after script 14 was created",
    if (n_modified_after == 0L) PASS else FAIL,
    if (n_modified_after == 0L)
      sprintf("none of %d scripts modified after script 14 (boundary: %s)",
              length(accepted_scripts),
              format(s14_mtime, "%Y-%m-%d %H:%M:%S"))
    else
      sprintf("%d modified after script 14: %s",
              n_modified_after, paste(modified_names, collapse = ", ")))


s00 <- accepted_scripts[basename(accepted_scripts) == "00_prepare_nfi_2024_data.R"]
t00 <- if (length(s00) && !is.na(s00)) readLines(s00, warn = FALSE) else character(0)
rec("Frozen script 00 holds the Oracle placeholders and no real alias or user name",
    if (any(grepl("ORACLE_SERVICE", t00, fixed = TRUE)) && any(grepl("ORACLE_USER", t00, fixed = TRUE)) &&
        !any(grepl("REDACTED41|REDACTED42", t00))) PASS else FAIL,
    sprintf("%d lines read", length(t00)))

# ---- verdict and output ----

write_audit_outputs(E$results, audit_csv, audit_txt)
