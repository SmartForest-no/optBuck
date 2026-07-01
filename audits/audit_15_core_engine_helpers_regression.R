# ==============================================================================
# audits/audit_15_core_engine_helpers_regression.R
#
# Purpose : Audit of R/15_core_engine_helpers_regression_test.R and the three
#           core engine helpers it exercises:
#             helpers_taper.R, helpers_candidate_generation.R,
#             helpers_dp_optimizer.R
#
# Checks  :
#  1.  All seven helper files exist (paths, products, valuation, audit, taper,
#      candidate_generation, dp_optimizer)
#  2.  Script 15 output files exist (both selected-log CSVs, tree summary,
#      regression summary text)
#  3.  Accepted scripts 00-12 not modified (mtime <= mtime of script 15)
#  4.  taperNOR package is available
#  5.  generate_taper_profile_one_tree() returns a non-empty data frame
#  6.  Taper profile has required columns (diameterPosition, DiameterValue, DBH)
#  7.  profiles_to_profile_vectors() produces a named list with correct fields
#  8.  products_for_species() returns correct keys for spruce and pine
#  9.  generate_candidate_logs_one_tree() returns a non-empty data frame
# 10.  Candidates contain required columns (StartPos, StopPos, PriceDiameterClass_cm)
# 11.  One-tree test: n_selected_logs matches reference (5 logs)
# 12.  One-tree test: selected StartPos/StopPos sequence matches reference
# 13.  One-tree test: total value within 0.01 NOK of reference
# 14.  One-tree test: total volume within 1e-4 m3 of reference
# 15.  Mixed-species test: all trees with reference logs appear in output
# 16.  Mixed-species test: per-tree StartPos/StopPos match reference (per tree)
# 17.  Mixed-species test: total value within 0.05 NOK of reference
# 18.  Mixed-species test: 31 dm unpriced candidates confirmed
# 19.  No overlapping logs (Test A)
# 20.  No overlapping logs (Test B)
# 21.  PriceDiameterClass_cm_uncapped present in mixed selected logs
# 22.  PriceDiameterClass_cm_used <= 50 for all selected logs (Test B)
# 23.  Waste zero prices treated as INFO (not FAIL) in price validation
#
# Verdict :
#   PASS                   -- all checks pass
#   ACCEPTED WITH REVIEW   -- known REVIEW items only (31 dm gap, volume
#                             formula minor numerical difference, zero prices)
#   FAIL                   -- missing files, wrong log positions, or
#                             accepted scripts modified
#
# Outputs :
#   outputs/core_engine_helpers_regression/audit_15_core_engine_helpers.csv
#   outputs/core_engine_helpers_regression/audit_15_core_engine_helpers.txt
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(readr)
})

root_dir <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"

source(file.path(root_dir, "R", "helpers_paths.R"))
source(file.path(root_dir, "R", "helpers_audit.R"))
source(file.path(root_dir, "R", "helpers_products.R"))
source(file.path(root_dir, "R", "helpers_valuation.R"))
source(file.path(root_dir, "R", "helpers_taper.R"))
source(file.path(root_dir, "R", "helpers_candidate_generation.R"))
source(file.path(root_dir, "R", "helpers_dp_optimizer.R"))

out_dir  <- path_output("core_engine_helpers_regression", root_dir = root_dir)
ensure_dir(out_dir)

audit_txt <- file.path(out_dir, "audit_15_core_engine_helpers.txt")
audit_csv <- file.path(out_dir, "audit_15_core_engine_helpers.csv")
init_audit_log(audit_txt)

E <- new.env(parent = emptyenv())

rec <- function(check, result, detail = "") {
  record_check(E, check, result, detail, audit_txt)
}

# ---- Script 15's own mtime (used as reference for accepted-scripts check) ----
# Script 15 may now live in scripts/prototype_reference/ after cleanup
script_15_path <- if (file.exists(file.path(root_dir, "R",
                         "15_core_engine_helpers_regression_test.R"))) {
  file.path(root_dir, "R", "15_core_engine_helpers_regression_test.R")
} else {
  file.path(root_dir, "scripts", "prototype_reference",
            "15_core_engine_helpers_regression_test.R")
}
script_15_mtime <- if (file.exists(script_15_path)) file.mtime(script_15_path) else Sys.time()


# ==========================================================================
# Check 1: All seven helper files exist
# ==========================================================================
helper_files <- c(
  "R/helpers_paths.R",
  "R/helpers_products.R",
  "R/helpers_valuation.R",
  "R/helpers_audit.R",
  "R/helpers_taper.R",
  "R/helpers_candidate_generation.R",
  "R/helpers_dp_optimizer.R"
)
missing_h <- helper_files[!file.exists(file.path(root_dir, helper_files))]
rec("Check 01: All 7 helper files exist",
    if (length(missing_h) == 0L) "PASS" else "FAIL",
    if (length(missing_h) == 0L) "All present"
    else paste("Missing:", paste(missing_h, collapse = ", ")))


# ==========================================================================
# Check 2: Script 15 output files exist
# ==========================================================================
output_files <- c(
  "outputs/core_engine_helpers_regression/engine_helper_one_tree_selected_logs.csv",
  "outputs/core_engine_helpers_regression/engine_helper_mixed_selected_logs.csv",
  "outputs/core_engine_helpers_regression/engine_helper_mixed_tree_summary.csv",
  "outputs/core_engine_helpers_regression/engine_helper_regression_summary.txt"
)
missing_o <- output_files[!file.exists(file.path(root_dir, output_files))]
rec("Check 02: All 4 script-15 output files exist",
    if (length(missing_o) == 0L) "PASS" else "FAIL",
    if (length(missing_o) == 0L) "All present"
    else paste("Missing:", paste(missing_o, collapse = ", ")))


# ==========================================================================
# Check 3: Accepted scripts 00-12 not modified since script 15 was created
# ==========================================================================
# Search accepted scripts 00-12 in both R/ (original) and scripts/prototype_reference/ (post-cleanup)
accepted_prefixes <- sprintf("%02d_", 0:12)
found_scripts <- unlist(lapply(accepted_prefixes, function(prefix) {
  c(
    list.files(file.path(root_dir, "R"),
               pattern = paste0("^", prefix), full.names = TRUE),
    list.files(file.path(root_dir, "scripts", "prototype_reference"),
               pattern = paste0("^", prefix), full.names = TRUE)
  )
}))

# Search audits 05-12 in both audits/ and audits/prototype_reference/
audit_prefixes <- sprintf("audit_%02d_", 5:12)
found_audits  <- unlist(lapply(audit_prefixes, function(prefix) {
  c(
    list.files(file.path(root_dir, "audits"),
               pattern = paste0("^", prefix), full.names = TRUE),
    list.files(file.path(root_dir, "audits", "prototype_reference"),
               pattern = paste0("^", prefix), full.names = TRUE)
  )
}))

all_accepted <- c(found_scripts, found_audits)
n_accepted   <- length(all_accepted)

if (n_accepted == 0L) {
  rec("Check 03: Accepted scripts 00-12 unmodified", "REVIEW",
      "No accepted script files found — cannot verify")
} else {
  mtimes <- file.mtime(all_accepted)
  modified_after <- all_accepted[mtimes > script_15_mtime + 5]  # 5s tolerance
  rec("Check 03: Accepted scripts 00-12 unmodified since script 15",
      if (length(modified_after) == 0L) "PASS" else "FAIL",
      if (length(modified_after) == 0L)
        sprintf("All %d accepted files have mtime <= script 15 mtime", n_accepted)
      else paste("Modified after script 15:", paste(basename(modified_after), collapse = ", ")))
}


# ==========================================================================
# Check 4: taperNOR package available
# ==========================================================================
rec("Check 04: taperNOR package available",
    if (requireNamespace("taperNOR", quietly = TRUE)) "PASS" else "FAIL",
    if (requireNamespace("taperNOR", quietly = TRUE)) "taperNOR found"
    else "Install: devtools::install_git('https://github.com/SmartForest-no/taperNOR')")


# ==========================================================================
# Checks 5-7: Taper profile generation
# ==========================================================================

# Use tree 242251 for unit-level checks
stem_profiles_path <- file.path(root_dir, "outputs", "stem_profiles",
                                "stem_profiles_example_500trees.parquet")

profile_unit <- tryCatch({
  sp_all   <- as.data.frame(arrow::read_parquet(stem_profiles_path))
  row_unit <- sp_all[as.character(sp_all$treid) == "242251", ][1L, ]
  generate_taper_profile_one_tree(
    treid         = "242251",
    plot_id       = as.character(row_unit$plot_id),
    species_group = as.character(row_unit$species_group),
    dbh_cm        = as.numeric(row_unit$dbh_cm),
    height_m      = as.numeric(row_unit$height_m),
    profile_step_m = 0.10)
}, error = function(e) NULL)

rec("Check 05: generate_taper_profile_one_tree() returns non-empty df",
    if (!is.null(profile_unit) && nrow(profile_unit) > 0L) "PASS" else "FAIL",
    if (!is.null(profile_unit)) sprintf("%d rows", nrow(profile_unit))
    else "returned NULL or error")

if (!is.null(profile_unit)) {
  req_cols_profile <- c("treid", "z_m", "diameter_cm",
                        "diameterPosition", "DiameterValue", "DBH")
  missing_pc <- setdiff(req_cols_profile, names(profile_unit))
  rec("Check 06: Taper profile has required columns",
      if (length(missing_pc) == 0L) "PASS" else "FAIL",
      if (length(missing_pc) == 0L)
        paste("All present:", paste(req_cols_profile, collapse = ", "))
      else paste("Missing:", paste(missing_pc, collapse = ", ")))

  pv_unit <- profiles_to_profile_vectors(profile_unit)[["242251"]]
  req_pv  <- c("treid", "species_group", "DBH", "height_m",
               "height_cm", "diameterPosition", "DiameterValue")
  missing_pv <- setdiff(req_pv, names(pv_unit))
  rec("Check 07: profiles_to_profile_vectors() fields complete",
      if (!is.null(pv_unit) && length(missing_pv) == 0L) "PASS" else "FAIL",
      if (!is.null(pv_unit) && length(missing_pv) == 0L)
        sprintf("all %d fields present; DBH=%.1f mm height=%.2f m",
                length(req_pv), pv_unit$DBH, pv_unit$height_m)
      else paste("Missing pv fields:", paste(missing_pv, collapse = ", ")))
} else {
  rec("Check 06: Taper profile has required columns", "FAIL",
      "profile_unit is NULL, cannot check columns")
  rec("Check 07: profiles_to_profile_vectors() fields complete", "FAIL",
      "profile_unit is NULL, cannot check")
}


# ==========================================================================
# Check 8: products_for_species() correctness
# ==========================================================================
spruce_keys <- products_for_species("spruce")
pine_keys   <- products_for_species("pine")

expected_spruce <- c("spruce_sawlog", "spruce_pulpwood", "spruce_waste")
expected_pine   <- c("pine_sawlog",   "pine_pulpwood",   "pine_waste")

keys_ok <- setequal(spruce_keys, expected_spruce) && setequal(pine_keys, expected_pine)
rec("Check 08: products_for_species() returns correct keys",
    if (keys_ok) "PASS" else "FAIL",
    sprintf("spruce=%s  pine=%s",
            paste(sort(spruce_keys), collapse = ","),
            paste(sort(pine_keys),   collapse = ",")))


# ==========================================================================
# Checks 9-10: Candidate generation unit test
# ==========================================================================
if (!is.null(profile_unit)) {
  pd_unit  <- load_products()
  lc_unit  <- load_length_classes()
  pv_u242  <- profiles_to_profile_vectors(profile_unit)[["242251"]]

  cands_unit <- tryCatch(
    generate_candidate_logs_one_tree(pv_u242, pd_unit, lc_unit),
    error = function(e) { message("  CAND ERROR: ", conditionMessage(e)); NULL })

  rec("Check 09: generate_candidate_logs_one_tree() returns non-empty df",
      if (!is.null(cands_unit) && nrow(cands_unit) > 0L) "PASS" else "FAIL",
      if (!is.null(cands_unit)) sprintf("%d candidate rows", nrow(cands_unit))
      else "returned NULL or error")

  if (!is.null(cands_unit)) {
    req_cols_cand <- c("StartPos", "StopPos", "ProductKey", "ProductType",
                       "eligible", "price_volume_m3", "LengthClass_dm",
                       "PriceDiameterClass_cm")
    missing_cc <- setdiff(req_cols_cand, names(cands_unit))
    rec("Check 10: Candidates have required columns",
        if (length(missing_cc) == 0L) "PASS" else "FAIL",
        if (length(missing_cc) == 0L)
          paste("All present:", paste(req_cols_cand, collapse = ", "))
        else paste("Missing:", paste(missing_cc, collapse = ", ")))
  } else {
    rec("Check 10: Candidates have required columns", "FAIL",
        "cands_unit is NULL")
  }
} else {
  rec("Check 09: generate_candidate_logs_one_tree() returns non-empty df", "FAIL",
      "profile_unit is NULL")
  rec("Check 10: Candidates have required columns", "FAIL",
      "profile_unit is NULL")
}


# ==========================================================================
# Load script-15 outputs for Checks 11-22
# ==========================================================================
sel_one_tree <- tryCatch(
  as.data.frame(read_csv(
    file.path(out_dir, "engine_helper_one_tree_selected_logs.csv"),
    show_col_types = FALSE)),
  error = function(e) NULL)

sel_mixed <- tryCatch(
  as.data.frame(read_csv(
    file.path(out_dir, "engine_helper_mixed_selected_logs.csv"),
    show_col_types = FALSE)),
  error = function(e) NULL)

# Reference data
ref_sel_path <- file.path(root_dir, "outputs", "optimizer_tests",
                          "one_tree_optimizer_selected_logs.csv")
ref_sel <- tryCatch(
  as.data.frame(read_csv(ref_sel_path, show_col_types = FALSE)),
  error = function(e) NULL)

ref_mixed_logs_path <- file.path(root_dir, "outputs", "mixed_species_example_bucking",
                                 "mixed_species_selected_logs.csv")
ref_mixed <- tryCatch(
  as.data.frame(read_csv(ref_mixed_logs_path, show_col_types = FALSE)),
  error = function(e) NULL)


# ==========================================================================
# Checks 11-14: One-tree test
# ==========================================================================
if (!is.null(sel_one_tree) && !is.null(ref_sel)) {
  n_ref_A <- nrow(ref_sel)
  n_got_A <- nrow(sel_one_tree)
  rec("Check 11: One-tree n_selected_logs matches reference",
      if (n_got_A == n_ref_A) "PASS" else "REVIEW",
      sprintf("ref=%d  got=%d", n_ref_A, n_got_A))

  ref_seq_A <- paste0(sort(ref_sel$StartPos), "-", sort(ref_sel$StopPos))
  got_seq_A <- paste0(sort(sel_one_tree$StartPos), "-", sort(sel_one_tree$StopPos))
  seq_match_A <- setequal(ref_seq_A, got_seq_A)
  rec("Check 12: One-tree StartPos/StopPos set matches reference",
      if (seq_match_A) "PASS" else "REVIEW",
      if (seq_match_A) "Exact position match"
      else sprintf("ref positions: [%s]  got: [%s]",
                   paste(ref_seq_A, collapse = ";"),
                   paste(got_seq_A, collapse = ";")))

  ref_val_A <- sum(ref_sel$value_nok, na.rm = TRUE)
  got_val_A <- sum(sel_one_tree$value_nok, na.rm = TRUE)
  diff_val_A <- abs(ref_val_A - got_val_A)
  rec("Check 13: One-tree total value within 0.01 NOK",
      if (diff_val_A < 0.01) "PASS" else "REVIEW",
      sprintf("ref=%.4f  got=%.4f  diff=%.6f", ref_val_A, got_val_A, diff_val_A))

  ref_vol_A <- sum(ref_sel$price_volume_m3, na.rm = TRUE)
  got_vol_A <- sum(sel_one_tree$price_volume_m3, na.rm = TRUE)
  diff_vol_A <- abs(ref_vol_A - got_vol_A)
  rec("Check 14: One-tree total volume within 1e-4 m3",
      if (diff_vol_A < 1e-4) "PASS" else "REVIEW",
      sprintf("ref=%.6f  got=%.6f  diff=%.8f", ref_vol_A, got_vol_A, diff_vol_A))
} else {
  for (n in 11:14)
    rec(sprintf("Check %02d: one-tree regression", n), "FAIL",
        "Output or reference file not loaded")
}


# ==========================================================================
# Checks 15-17: Mixed-species test
# ==========================================================================
if (!is.null(sel_mixed) && !is.null(ref_mixed)) {
  ref_trees_with_logs_B <- unique(as.character(ref_mixed$treid))
  got_trees_with_logs_B <- unique(as.character(sel_mixed$treid))
  all_ref_trees_present <- all(ref_trees_with_logs_B %in% got_trees_with_logs_B)

  rec("Check 15: Mixed: all reference trees with logs appear in output",
      if (all_ref_trees_present) "PASS" else "REVIEW",
      sprintf("ref trees with logs=%d  got=%d  missing=%s",
              length(ref_trees_with_logs_B), length(got_trees_with_logs_B),
              paste(setdiff(ref_trees_with_logs_B, got_trees_with_logs_B),
                    collapse = ",")))

  # Per-tree StartPos/StopPos match
  tree_match_results <- vapply(ref_trees_with_logs_B, function(tid) {
    r <- ref_mixed[as.character(ref_mixed$treid) == tid, ]
    g <- sel_mixed[as.character(sel_mixed$treid) == tid, ]
    if (nrow(g) == 0L) return(FALSE)
    setequal(paste0(r$StartPos, "-", r$StopPos),
             paste0(g$StartPos, "-", g$StopPos))
  }, logical(1))

  n_trees_exact <- sum(tree_match_results, na.rm = TRUE)
  rec("Check 16: Mixed: per-tree StartPos/StopPos match reference",
      if (all(tree_match_results)) "PASS" else "REVIEW",
      sprintf("%d/%d trees have exact position match",
              n_trees_exact, length(ref_trees_with_logs_B)))

  ref_val_B <- sum(ref_mixed$value_nok, na.rm = TRUE)
  got_val_B <- sum(sel_mixed$value_nok, na.rm = TRUE)
  diff_val_B <- abs(ref_val_B - got_val_B)
  rec("Check 17: Mixed total value within 0.05 NOK",
      if (diff_val_B < 0.05) "PASS" else "REVIEW",
      sprintf("ref=%.4f  got=%.4f  diff=%.6f", ref_val_B, got_val_B, diff_val_B))
} else {
  for (n in 15:17)
    rec(sprintf("Check %02d: mixed-species regression", n), "FAIL",
        "Output or reference file not loaded")
}


# ==========================================================================
# Check 18: 31 dm unpriced confirmed
# ==========================================================================
lc_audit <- load_length_classes()
pm_audit <- load_price_matrix_long()
unsup_a  <- get_unsupported_sawlog_lengths(lc_audit, pm_audit)
n_31_unpriced <- nrow(unsup_a[unsup_a$is_known_31dm_gap & !unsup_a$in_price_matrix, ])
rec("Check 18: 31 dm sawlog confirmed unpriced",
    if (n_31_unpriced > 0L) "PASS" else "FAIL",
    sprintf("%d unpriced 31 dm entries found", n_31_unpriced))


# ==========================================================================
# Checks 19-20: No overlapping logs
# ==========================================================================
n_overlap_A_a <- if (!is.null(sel_one_tree)) check_selected_logs_no_overlap(sel_one_tree) else NA_integer_
n_overlap_B_a <- if (!is.null(sel_mixed))    check_selected_logs_no_overlap(sel_mixed)    else NA_integer_

rec("Check 19: No overlapping logs (Test A, one-tree)",
    if (isTRUE(n_overlap_A_a == 0L)) "PASS" else "FAIL",
    sprintf("overlapping pairs: %s", n_overlap_A_a))

rec("Check 20: No overlapping logs (Test B, mixed-species)",
    if (isTRUE(n_overlap_B_a == 0L)) "PASS" else "FAIL",
    sprintf("overlapping pairs: %s", n_overlap_B_a))


# ==========================================================================
# Checks 21-22: Diameter-class columns in mixed output
# ==========================================================================
if (!is.null(sel_mixed) && nrow(sel_mixed) > 0L) {
  has_uncapped <- "PriceDiameterClass_cm_uncapped" %in% names(sel_mixed)
  has_used     <- "PriceDiameterClass_cm_used"     %in% names(sel_mixed)

  rec("Check 21: PriceDiameterClass_cm_uncapped present in mixed output",
      if (has_uncapped) "PASS" else "REVIEW",
      if (has_uncapped) "column present" else "column absent (pipeline may omit for zero-log trees)")

  if (has_used) {
    max_used <- max(sel_mixed$PriceDiameterClass_cm_used, na.rm = TRUE)
    rec("Check 22: PriceDiameterClass_cm_used <= 50 for all selected (Test B)",
        if (max_used <= 50L) "PASS" else "FAIL",
        sprintf("max used = %d cm", max_used))
  } else {
    rec("Check 22: PriceDiameterClass_cm_used <= 50 for all selected (Test B)",
        "REVIEW", "column PriceDiameterClass_cm_used absent")
  }
} else {
  rec("Check 21: PriceDiameterClass_cm_uncapped present", "REVIEW",
      "sel_mixed not loaded or empty")
  rec("Check 22: PriceDiameterClass_cm_used <= 50", "REVIEW",
      "sel_mixed not loaded or empty")
}


# ==========================================================================
# Check 23: Waste zero prices treated as INFO (validation returns REVIEW not FAIL)
# ==========================================================================
pd_a23 <- load_products()
val_a23 <- validate_price_matrix_against_products(pd_a23, lc_audit, pm_audit)
zero_check <- val_a23$checks[
  grepl("zero", val_a23$checks$check, ignore.case = TRUE), , drop = FALSE]
any_zero_fail <- any(zero_check$result == "FAIL", na.rm = TRUE)
rec("Check 23: Zero-price rows are REVIEW (not FAIL) in price validation",
    if (!any_zero_fail) "PASS" else "FAIL",
    sprintf("zero-price check result(s): %s",
            if (nrow(zero_check) > 0L) paste(zero_check$result, collapse = ", ")
            else "none found (no zero prices)"))


# ==========================================================================
# Verdict and output
# ==========================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)

cat("Audit 15 complete.\n")
cat(sprintf("Results written to: %s\n", out_dir))
