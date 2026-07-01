# ==============================================================================
# R/15_core_engine_helpers_regression_test.R
#
# Purpose : Regression test for the three core engine helpers:
#             helpers_taper.R, helpers_candidate_generation.R,
#             helpers_dp_optimizer.R
#
# Tests:
#   A) One-tree test (treid 242251, spruce, from script 06 accepted output):
#      Regenerate profile from DBH/height stored in stem_profiles parquet,
#      generate candidates, price, run DP.  Compare to accepted selected logs
#      in outputs/optimizer_tests/one_tree_optimizer_selected_logs.csv.
#
#   B) Mixed-species test (25 spruce/pine trees from script 10):
#      Source trees: mixed_species_selected_trees.csv
#      Compare to:  mixed_species_selected_logs.csv
#                   mixed_species_tree_summary.csv
#
#   C) Price-support checks:
#      31 dm sawlog unsupported, _uncapped and _used columns correct.
#
# Outputs (all under outputs/core_engine_helpers_regression/):
#   engine_helper_one_tree_selected_logs.csv
#   engine_helper_mixed_selected_logs.csv
#   engine_helper_mixed_tree_summary.csv
#   engine_helper_regression_summary.txt
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
})

# Source helpers ---------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a

ROOT       <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
script_dir <- file.path(ROOT, "R")

source(file.path(script_dir, "helpers_paths.R"))
source(file.path(script_dir, "helpers_products.R"))
source(file.path(script_dir, "helpers_valuation.R"))
source(file.path(script_dir, "helpers_audit.R"))
source(file.path(script_dir, "helpers_taper.R"))
source(file.path(script_dir, "helpers_candidate_generation.R"))
source(file.path(script_dir, "helpers_dp_optimizer.R"))

out_dir <- file.path(ROOT, "outputs", "core_engine_helpers_regression")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cat("=== Script 15: Core Engine Helpers Regression Test ===\n\n")

# ---- Shared data ----------------------------------------------------------
pd <- load_products()
lc <- load_length_classes()
pm <- load_price_matrix_long()

# Identify sawlog and non-sawlog product keys for price joining
sawlog_pks    <- pd$ProductKey[pd$ProductType == "sawlog"]
nonsawlog_pks <- pd$ProductKey[pd$ProductType != "sawlog"]

# Price matrix: sawlog long format (by ProductKey + LengthClass_dm + PriceDiameterClass_cm_used)
pm_mat <- pm[!is.na(pm$LengthClass_dm), c("ProductKey", "LengthClass_dm",
                                            "PriceDiameterClass_cm", "PriceNOKperM3")]
names(pm_mat)[names(pm_mat) == "PriceDiameterClass_cm"] <- "PriceDiameterClass_cm_used"

# Price matrix: flat (by ProductKey only) for non-sawlogs
pm_fl <- pm[is.na(pm$LengthClass_dm), c("ProductKey", "PriceNOKperM3")]
names(pm_fl)[names(pm_fl) == "PriceNOKperM3"] <- "PriceNOKperM3_flat"

# Helper: run the full pipeline for a single profile-vector list
run_pipeline_one_tree <- function(pv) {
  cands <- generate_candidate_logs_one_tree(pv, pd, lc)
  if (nrow(cands) == 0L) return(list(selected = data.frame(), summary = list()))

  cands <- prepare_price_join_keys(cands)

  elig_c      <- cands[cands$eligible & !is.na(cands$price_volume_m3), , drop = FALSE]
  sawlog_c    <- elig_c[elig_c$ProductKey %in% sawlog_pks, , drop = FALSE]
  nonsawlog_c <- elig_c[elig_c$ProductKey %in% nonsawlog_pks, , drop = FALSE]

  if (nrow(sawlog_c) > 0L) {
    sawlog_c <- merge(sawlog_c, pm_mat,
                      by = c("ProductKey", "LengthClass_dm", "PriceDiameterClass_cm_used"),
                      all.x = TRUE)
  } else {
    sawlog_c$PriceNOKperM3 <- numeric(0)
  }

  if (nrow(nonsawlog_c) > 0L) {
    nonsawlog_c <- merge(nonsawlog_c, pm_fl, by = "ProductKey", all.x = TRUE)
    names(nonsawlog_c)[names(nonsawlog_c) == "PriceNOKperM3_flat"] <- "PriceNOKperM3"
  } else {
    nonsawlog_c$PriceNOKperM3 <- numeric(0)
  }

  priced <- rbind(sawlog_c, nonsawlog_c)
  priced$value_nok <- priced$price_volume_m3 * priced$PriceNOKperM3

  result <- optimize_one_tree_dp(priced)
  list(
    selected = result$selected_logs,
    summary  = summarize_optimizer_result(result$selected_logs,
                                           tree_height_m = pv$height_m),
    status   = result$optimizer_status,
    n_generated = nrow(cands),
    n_priced    = sum(!is.na(priced$PriceNOKperM3)),
    n_unpriced_31dm = sum(!is.na(priced$LengthClass_dm) &
                            priced$LengthClass_dm == 31L &
                            is.na(priced$PriceNOKperM3), na.rm = TRUE)
  )
}


# ==========================================================================
# Test A: one-tree (treid 242251, spruce)
# ==========================================================================
cat("--- Test A: one-tree (treid 242251) ---\n")

# Load accepted reference
accepted_one_tree_path <- file.path(ROOT, "outputs", "optimizer_tests",
                                    "one_tree_optimizer_selected_logs.csv")
ref_sel <- as.data.frame(read_csv(accepted_one_tree_path, show_col_types = FALSE))

# Get tree inputs from stem profiles parquet (DBH and height stored there)
stem_profiles_path <- file.path(ROOT, "outputs", "stem_profiles",
                                "stem_profiles_example_500trees.parquet")
stem_profiles <- as.data.frame(read_parquet(stem_profiles_path))

tree_row_242251 <- stem_profiles[stem_profiles$treid == "242251", ][1L, ]
if (nrow(tree_row_242251) == 0L) {
  # treid might be integer
  tree_row_242251 <- stem_profiles[as.character(stem_profiles$treid) == "242251", ][1L, ]
}

dbh_cm_A  <- as.numeric(tree_row_242251$dbh_cm)
height_m_A <- as.numeric(tree_row_242251$height_m)
plot_id_A  <- as.character(tree_row_242251$plot_id)
sg_A       <- as.character(tree_row_242251$species_group)

cat(sprintf("  treid=242251  plot_id=%s  species=%s  dbh=%.1f cm  height=%.2f m\n",
            plot_id_A, sg_A, dbh_cm_A, height_m_A))

profile_A <- generate_taper_profile_one_tree(
  treid         = "242251",
  plot_id       = plot_id_A,
  species_group = sg_A,
  dbh_cm        = dbh_cm_A,
  height_m      = height_m_A,
  profile_step_m = 0.10)

pvs_A <- profiles_to_profile_vectors(profile_A)
pv_A  <- pvs_A[["242251"]]

res_A <- run_pipeline_one_tree(pv_A)
sel_A <- res_A$selected

cat(sprintf("  Generated: %d candidates | Priced: %d | Selected: %d logs\n",
            res_A$n_generated, res_A$n_priced, nrow(sel_A)))

write_csv(sel_A, file.path(out_dir, "engine_helper_one_tree_selected_logs.csv"))
cat(sprintf("  Written: engine_helper_one_tree_selected_logs.csv\n"))


# ==========================================================================
# Test B: mixed-species (25 spruce/pine trees from script 10)
# ==========================================================================
cat("\n--- Test B: mixed-species (25 trees) ---\n")

mixed_trees_path <- file.path(ROOT, "outputs", "mixed_species_example_bucking",
                              "mixed_species_selected_trees.csv")
mixed_ref_logs_path <- file.path(ROOT, "outputs", "mixed_species_example_bucking",
                                 "mixed_species_selected_logs.csv")
mixed_ref_summ_path <- file.path(ROOT, "outputs", "mixed_species_example_bucking",
                                 "mixed_species_tree_summary.csv")

trees_B      <- as.data.frame(read_csv(mixed_trees_path,   show_col_types = FALSE))
ref_logs_B   <- as.data.frame(read_csv(mixed_ref_logs_path, show_col_types = FALSE))
ref_summ_B   <- as.data.frame(read_csv(mixed_ref_summ_path, show_col_types = FALSE))

cat(sprintf("  Input trees: %d | Ref selected logs: %d | Ref tree summary: %d\n",
            nrow(trees_B), nrow(ref_logs_B), nrow(ref_summ_B)))

# Generate profiles for all trees
profiles_B <- generate_taper_profiles_for_trees(trees_B, profile_step_m = 0.10, quiet = FALSE)
pvs_B      <- profiles_to_profile_vectors(profiles_B)

# Run pipeline for each tree
sel_list_B  <- list()
summ_list_B <- list()

for (tid in names(pvs_B)) {
  pv_i <- pvs_B[[tid]]
  res_i <- tryCatch(run_pipeline_one_tree(pv_i),
                    error = function(e)
                      list(selected = data.frame(), summary = list(),
                           status = paste0("error: ", conditionMessage(e)),
                           n_generated = 0L, n_priced = 0L,
                           n_unpriced_31dm = 0L))

  # Annotate selected logs with plot_id
  if (nrow(res_i$selected) > 0L) {
    sel_list_B[[tid]] <- res_i$selected
  }

  tree_row_i <- trees_B[as.character(trees_B$treid) == tid, ]
  s <- res_i$summary
  summ_list_B[[tid]] <- data.frame(
    treid                        = tid,
    plot_id                      = as.character(tree_row_i$plot_id[1L] %||% NA),
    species_group                = pv_i$species_group,
    dbh_cm                       = pv_i$DBH / 10,
    height_m                     = pv_i$height_m,
    n_generated_candidates       = res_i$n_generated,
    n_priced_candidates          = res_i$n_priced,
    n_unpriced_31dm              = res_i$n_unpriced_31dm,
    n_selected_logs              = s$n_selected %||% 0L,
    total_selected_volume_m3     = s$total_volume_m3 %||% 0,
    total_selected_value_nok     = s$total_value_nok %||% 0,
    optimizer_status             = res_i$status %||% "unknown",
    stringsAsFactors             = FALSE
  )
}

sel_B  <- do.call(rbind, sel_list_B)
if (is.null(sel_B)) sel_B <- data.frame()
summ_B <- do.call(rbind, summ_list_B)

cat(sprintf("  Produced: %d selected logs across %d trees\n",
            nrow(sel_B), nrow(summ_B)))

write_csv(sel_B,  file.path(out_dir, "engine_helper_mixed_selected_logs.csv"))
write_csv(summ_B, file.path(out_dir, "engine_helper_mixed_tree_summary.csv"))
cat(sprintf("  Written: engine_helper_mixed_selected_logs.csv\n"))
cat(sprintf("  Written: engine_helper_mixed_tree_summary.csv\n"))


# ==========================================================================
# Test C: Price-support checks
# ==========================================================================
cat("\n--- Test C: price-support checks ---\n")

unsup <- get_unsupported_sawlog_lengths(lc, pm)
n_31dm_unpriced <- nrow(unsup[unsup$is_known_31dm_gap & !unsup$in_price_matrix, ])
cat(sprintf("  Unpriced 31 dm sawlog entries: %d (expected > 0)\n", n_31dm_unpriced))


# ==========================================================================
# Comparison summary
# ==========================================================================
cat("\n--- Comparison summary ---\n")

# Test A comparisons
ref_key_A <- paste0(ref_sel$StartPos, "-", ref_sel$StopPos)
got_key_A <- if (nrow(sel_A) > 0L)
               paste0(sel_A$StartPos, "-", sel_A$StopPos) else character(0)

n_logs_match_A   <- setequal(ref_key_A, got_key_A)
ref_value_A      <- sum(ref_sel$value_nok, na.rm = TRUE)
got_value_A      <- sum(sel_A$value_nok,   na.rm = TRUE)
ref_vol_A        <- sum(ref_sel$price_volume_m3, na.rm = TRUE)
got_vol_A        <- sum(sel_A$price_volume_m3,   na.rm = TRUE)
value_diff_A     <- abs(ref_value_A - got_value_A)
vol_diff_A       <- abs(ref_vol_A   - got_vol_A)

cat(sprintf("  A: n_logs ref=%d got=%d  match=%s\n",
            nrow(ref_sel), nrow(sel_A), n_logs_match_A))
cat(sprintf("  A: value  ref=%.4f  got=%.4f  diff=%.6f NOK\n",
            ref_value_A, got_value_A, value_diff_A))
cat(sprintf("  A: volume ref=%.6f  got=%.6f  diff=%.8f m3\n",
            ref_vol_A, got_vol_A, vol_diff_A))

# Test B comparisons: match per-tree StartPos/StopPos/ProductKey
if (nrow(sel_B) > 0L && nrow(ref_logs_B) > 0L) {
  sel_B_key   <- paste0(sel_B$treid,     "-", sel_B$StartPos,     "-", sel_B$StopPos)
  ref_B_key   <- paste0(ref_logs_B$treid, "-", ref_logs_B$StartPos, "-", ref_logs_B$StopPos)
  b_match     <- setequal(sel_B_key, ref_B_key)
  b_val_diff  <- abs(sum(sel_B$value_nok, na.rm = TRUE) -
                       sum(ref_logs_B$value_nok, na.rm = TRUE))
  b_vol_diff  <- abs(sum(sel_B$price_volume_m3, na.rm = TRUE) -
                       sum(ref_logs_B$price_volume_m3, na.rm = TRUE))
  cat(sprintf("  B: logs match=%s  value_diff=%.4f NOK  vol_diff=%.6f m3\n",
              b_match, b_val_diff, b_vol_diff))
} else {
  b_match    <- (nrow(sel_B) == nrow(ref_logs_B))
  b_val_diff <- NA_real_
  b_vol_diff <- NA_real_
  cat(sprintf("  B: ref=%d logs  got=%d logs  match=%s\n",
              nrow(ref_logs_B), nrow(sel_B), b_match))
}

cat(sprintf("  C: 31 dm unpriced entries found: %s\n",
            n_31dm_unpriced > 0L))

# Overlap check
n_overlap_A <- check_selected_logs_no_overlap(sel_A)
n_overlap_B <- check_selected_logs_no_overlap(sel_B)
cat(sprintf("  Overlap check: A=%d  B=%d  (expect both 0)\n",
            n_overlap_A, n_overlap_B))


# ==========================================================================
# Write summary
# ==========================================================================
summary_lines <- c(
  "=== Script 15: Core Engine Helpers Regression Test Summary ===",
  sprintf("Run time: %s", Sys.time()),
  "",
  "--- Test A: one-tree (treid 242251) ---",
  sprintf("  n_logs:    ref=%d  got=%d  match=%s", nrow(ref_sel), nrow(sel_A), n_logs_match_A),
  sprintf("  value_nok: ref=%.4f  got=%.4f  diff=%.6f", ref_value_A, got_value_A, value_diff_A),
  sprintf("  vol_m3:    ref=%.6f  got=%.6f  diff=%.8f", ref_vol_A, got_vol_A, vol_diff_A),
  sprintf("  n_overlap: %d", n_overlap_A),
  "",
  "--- Test B: mixed-species (25 trees) ---",
  sprintf("  n_logs: ref=%d  got=%d  match=%s", nrow(ref_logs_B), nrow(sel_B), b_match),
  sprintf("  value_diff: %.4f NOK  vol_diff: %.6f m3", b_val_diff %||% NA, b_vol_diff %||% NA),
  sprintf("  n_overlap: %d", n_overlap_B),
  sprintf("  tree_summary_rows: %d", nrow(summ_B)),
  "",
  "--- Test C: price support ---",
  sprintf("  31 dm unpriced entries: %d (expected > 0)", n_31dm_unpriced),
  "",
  "--- Overall ---",
  sprintf("  Test A: %s",
          if (n_logs_match_A && value_diff_A < 0.01 && vol_diff_A < 1e-4) "PASS" else "REVIEW"),
  sprintf("  Test B: %s",
          if (isTRUE(b_match)) "PASS"
          else if (!is.na(b_val_diff) && b_val_diff < 0.05) "REVIEW"
          else "CHECK"),
  sprintf("  Test C: %s", if (n_31dm_unpriced > 0L) "PASS" else "REVIEW"),
  sprintf("  Overlap A: %s", if (n_overlap_A == 0L) "PASS" else "FAIL"),
  sprintf("  Overlap B: %s", if (n_overlap_B == 0L) "PASS" else "FAIL")
)

writeLines(summary_lines,
           file.path(out_dir, "engine_helper_regression_summary.txt"))

cat("\n--- Summary written to outputs/core_engine_helpers_regression/ ---\n")
cat("Script 15 complete.\n")
