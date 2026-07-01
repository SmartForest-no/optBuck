# ==============================================================================
# R/14_validate_first_helpers.R
#
# Purpose : Regression check for the first four helper files:
#             R/helpers_paths.R
#             R/helpers_products.R
#             R/helpers_valuation.R
#             R/helpers_audit.R
#
#           Uses only the new helpers and existing accepted inputs.
#           Does NOT rerun taper, candidate generation, or optimisation.
#           Does NOT modify accepted scripts 00-12.
#
# Inputs  :
#   inputs/ProductData_spruce_pine_default.csv
#   inputs/LengthClasses_spruce_pine_default.csv
#   inputs/price_matrices/PriceMatrix_long_default_2021.csv
#
# Outputs (outputs/helper_validation/):
#   helper_product_check.csv
#   helper_price_support_by_product.csv
#   helper_unsupported_sawlog_lengths.csv
#   helper_price_matrix_validation_summary.txt
# ==============================================================================

suppressPackageStartupMessages(library(readr))

# ---- source helpers ----

root_dir <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"

source(file.path(root_dir, "R", "helpers_paths.R"))
source(file.path(root_dir, "R", "helpers_audit.R"))
source(file.path(root_dir, "R", "helpers_products.R"))
source(file.path(root_dir, "R", "helpers_valuation.R"))

out_dir  <- path_output("helper_validation", root_dir = root_dir)
ensure_dir(out_dir)

val_txt <- file.path(out_dir, "helper_price_matrix_validation_summary.txt")
cat("", file = val_txt)

vlog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = val_txt, append = TRUE)
}

vlog(
  "=============================================================",
  "  nfiBuck helper validation run",
  sprintf("  Run : %s", Sys.time()),
  "============================================================="
)


# ---- load inputs via helpers ----

vlog("", "--- Loading inputs via helpers ---")

pd <- load_products(root_dir = root_dir)
vlog(sprintf("  ProductData   : %d rows, %d cols", nrow(pd), ncol(pd)))

lc <- load_length_classes(root_dir = root_dir)
vlog(sprintf("  LengthClasses : %d rows, %d cols", nrow(lc), ncol(lc)))

pm <- load_price_matrix_long(root_dir = root_dir, scenario = "default_2021")
vlog(sprintf("  PriceMatrix   : %d rows, %d cols", nrow(pm), ncol(pm)))


# ---- product assumption checks ----

vlog("", "--- Product assumption checks ---")

prod_checks <- check_product_assumptions(pd)

for (i in seq_len(nrow(prod_checks))) {
  vlog(sprintf("  [%-6s] %s | %s",
               prod_checks$result[i],
               prod_checks$check[i],
               prod_checks$detail[i]))
}

readr::write_csv(prod_checks,
  file.path(out_dir, "helper_product_check.csv"))
vlog(sprintf("  Saved: helper_product_check.csv (%d checks)", nrow(prod_checks)))


# ---- supported sawlog lengths ----

vlog("", "--- Supported sawlog lengths ---")

supported <- get_supported_sawlog_lengths(pm)

for (pk in unique(supported$ProductKey)) {
  dms <- sort(supported$LengthClass_dm[supported$ProductKey == pk])
  vlog(sprintf("  %s : %s dm", pk, paste(dms, collapse = ", ")))
}

readr::write_csv(supported,
  file.path(out_dir, "helper_price_support_by_product.csv"))
vlog(sprintf("  Saved: helper_price_support_by_product.csv (%d rows)",
             nrow(supported)))


# ---- unsupported sawlog lengths ----

vlog("", "--- Unsupported sawlog lengths (in LengthClasses but not priced) ---")

unsup_all   <- get_unsupported_sawlog_lengths(lc, pm)
unsupported <- unsup_all[unsup_all$in_length_classes & !unsup_all$in_price_matrix, ]

if (nrow(unsupported) > 0L) {
  for (i in seq_len(nrow(unsupported))) {
    label <- if (unsupported$is_known_31dm_gap[i]) "[KNOWN GAP]" else "[UNEXPECTED]"
    vlog(sprintf("  %s  %s  %d dm",
                 label, unsupported$ProductKey[i], unsupported$LogLength_dm[i]))
  }
} else {
  vlog("  None found.")
}

readr::write_csv(unsup_all,
  file.path(out_dir, "helper_unsupported_sawlog_lengths.csv"))
vlog(sprintf("  Saved: helper_unsupported_sawlog_lengths.csv (%d rows)",
             nrow(unsup_all)))


# ---- full validation ----

vlog("", "--- Full price matrix validation ---")

val_result <- validate_price_matrix_against_products(pd, lc, pm)

for (i in seq_len(nrow(val_result$checks))) {
  vlog(sprintf("  [%-6s] %s | %s",
               val_result$checks$result[i],
               val_result$checks$check[i],
               val_result$checks$detail[i]))
}


# ---- diameter cap demonstration ----

vlog("", "--- Price-diameter cap demonstration ---")

# Build a small synthetic candidates frame to demonstrate cap logic
test_diams <- data.frame(
  PriceDiameterClass_cm = as.integer(c(12, 24, 36, 48, 50, 52, 60, 70)),
  stringsAsFactors = FALSE
)
test_capped <- prepare_price_join_keys(test_diams, max_price_diameter_class_cm = 50L)

vlog("  Input PriceDiameterClass_cm → uncapped → used (cap=50):")
for (i in seq_len(nrow(test_capped))) {
  vlog(sprintf("    %d → uncapped=%d  used=%d",
               test_diams$PriceDiameterClass_cm[i],
               test_capped$PriceDiameterClass_cm_uncapped[i],
               test_capped$PriceDiameterClass_cm_used[i]))
}

n_capped  <- sum(test_capped$PriceDiameterClass_cm_used <
                   test_capped$PriceDiameterClass_cm_uncapped)
n_correct <- sum(test_capped$PriceDiameterClass_cm_used ==
                   pmin(test_capped$PriceDiameterClass_cm_uncapped, 50L))
vlog(
  sprintf("  Values capped: %d / %d", n_capped, nrow(test_capped)),
  sprintf("  Cap applied correctly (used == min(uncapped, 50)): %d / %d",
          n_correct, nrow(test_capped))
)


# ---- accepted-script integrity note ----

vlog(
  "",
  "--- Accepted-script integrity ---",
  "  Scripts 00-12 were not modified by this script.",
  "  Audit 14 verifies file modification times against expected dates."
)


vlog(
  "",
  "=============================================================",
  "  Script 14 complete.",
  sprintf("  Outputs: %s", out_dir),
  "============================================================="
)

cat("Script 14 complete.\n")
