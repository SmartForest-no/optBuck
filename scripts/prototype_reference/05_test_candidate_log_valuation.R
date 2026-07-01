# ==============================================================================
# R/05_test_candidate_log_valuation.R
#
# Purpose : Test candidate-log valuation.
#           Joins candidate logs from script 03 to price matrices from script 04.
#           Calculates log value.  Reports unmatched price keys.
#
#           This script does NOT implement bucking optimisation.
#           This script does NOT modify ProductData, LengthClasses, or
#           PriceMatrix_long.
#
# Inputs  :
#   inputs/ProductData_spruce_pine_default.csv
#   inputs/LengthClasses_spruce_pine_default.csv
#   inputs/price_matrices/PriceMatrix_long_default_2021.csv
#   outputs/product_volume_tests/candidate_logs_one_tree.csv  (or .rds/.parquet)
#
# Outputs :
#   outputs/valuation_tests/candidate_logs_valued_test.csv
#   outputs/valuation_tests/candidate_logs_unmatched_price_keys.csv
#   outputs/valuation_tests/candidate_log_valuation_summary.txt
#   outputs/valuation_tests/price_join_diagnostic_by_product.csv
#
# Unit conventions (consistent with scripts 02-04):
#   lengths        : cm in candidate logs; dm for price-matrix join keys
#   diameters      : mm in candidate logs; cm for price-matrix join keys
#   volume         : m3
#   prices         : NOK per m3
#   value          : NOK  (value_nok = price_volume_m3 * PriceNOKperM3)
#
# Valuation join keys:
#   sawlog  : PriceDiameterClass_cm = floor(TopDiameter_mm / 10)
#             LengthClass_dm        = LogLength_cm / 10
#   pulpwood/waste (flat): join only on PriceScenario + ProductKey
#
# Known limitation reported (not fixed):
#   Sawlog LengthClass 31 dm exists in LengthClasses_spruce_pine_default but
#   is absent from the imported price matrix (which starts at 34 dm).
#   Affected candidates are reported as unmatched, not silently dropped.
# ==============================================================================


# ---- setup ----

library(dplyr)
library(readr)
library(tibble)
library(ggplot2)

root_dir      <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
inputs_dir    <- file.path(root_dir, "inputs")
pm_dir        <- file.path(root_dir, "inputs", "price_matrices")
pv_tests_dir  <- file.path(root_dir, "outputs", "product_volume_tests")
val_dir       <- file.path(root_dir, "outputs", "valuation_tests")

dir.create(val_dir, showWarnings = FALSE, recursive = TRUE)

log_file <- file.path(val_dir, "candidate_log_valuation_summary.txt")
cat("", file = log_file)

PRICE_SCENARIO <- "default_2021"


# ---- helpers ----

wlog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = log_file, append = TRUE)
}

save_plot <- function(p, filename, width = 18, height = 11,
                      units = "cm", dpi = 150) {
  path <- file.path(val_dir, filename)
  ggplot2::ggsave(path, plot = p, width = width, height = height,
                  units = units, dpi = dpi)
  wlog(sprintf("  Plot saved: %s", basename(path)))
}

wlog(
  "=============================================================",
  "  Candidate-log valuation test – log",
  sprintf("  Run      : %s", Sys.time()),
  sprintf("  Scenario : %s", PRICE_SCENARIO),
  "============================================================="
)


# ---- read required inputs ----

wlog("", "--- Reading inputs ---")

# ProductData.
pd_file <- file.path(inputs_dir, "ProductData_spruce_pine_default.csv")
if (!file.exists(pd_file))
  stop("ProductData file not found: ", pd_file,
       "\nRun 03_define_products_and_volume.R first.")
ProductData <- readr::read_csv(pd_file, show_col_types = FALSE)
wlog(sprintf("  ProductData   : %d rows from %s", nrow(ProductData), basename(pd_file)))

# LengthClasses.
lc_file <- file.path(inputs_dir, "LengthClasses_spruce_pine_default.csv")
if (!file.exists(lc_file))
  stop("LengthClasses file not found: ", lc_file,
       "\nRun 03_define_products_and_volume.R first.")
LengthClasses_long <- readr::read_csv(lc_file, show_col_types = FALSE)
wlog(sprintf("  LengthClasses : %d rows from %s", nrow(LengthClasses_long), basename(lc_file)))

# PriceMatrix_long.
pm_file <- file.path(pm_dir, "PriceMatrix_long_default_2021.csv")
if (!file.exists(pm_file))
  stop("PriceMatrix_long file not found: ", pm_file,
       "\nRun 04_import_or_define_price_matrices.R first.")
PriceMatrix_long <- readr::read_csv(pm_file, show_col_types = FALSE)
wlog(sprintf("  PriceMatrix   : %d rows from %s", nrow(PriceMatrix_long), basename(pm_file)))


# ---- locate candidate-log file ----

wlog("", "--- Locating candidate-log file ---")

# Preference order: .rds > .parquet > .csv.
# Look for any file starting with "candidate_logs" under outputs/product_volume_tests.
candidate_files <- list.files(pv_tests_dir,
                               pattern = "^candidate_logs.*\\.(rds|parquet|csv)$",
                               full.names = TRUE, ignore.case = TRUE)

if (length(candidate_files) == 0)
  stop("No candidate-log file found under ", pv_tests_dir,
       "\nRun 03_define_products_and_volume.R first.")

# Pick the preferred format, then most-recently modified file.
ext_priority <- function(f) {
  ext <- tolower(tools::file_ext(f))
  switch(ext, rds = 1L, parquet = 2L, csv = 3L, 99L)
}
candidate_files <- candidate_files[order(
  sapply(candidate_files, ext_priority),
  file.info(candidate_files)$mtime,
  decreasing = c(FALSE, TRUE),
  method = "radix"
)]
candidate_file <- candidate_files[1]
wlog(sprintf("  Using: %s", basename(candidate_file)))

ext <- tolower(tools::file_ext(candidate_file))
candidate_logs_raw <- switch(ext,
  rds     = readRDS(candidate_file),
  parquet = arrow::read_parquet(candidate_file),
  csv     = readr::read_csv(candidate_file, show_col_types = FALSE),
  stop("Unsupported file extension: ", ext)
)

wlog(sprintf("  Rows read     : %d", nrow(candidate_logs_raw)))


# ---- validate candidate-log columns ----

wlog("", "--- Validating candidate-log columns ---")

required_cl_cols <- c("ProductKey", "LogLength", "TopDiameter",
                       "price_volume_m3", "eligible")
missing_cl_cols  <- setdiff(required_cl_cols, names(candidate_logs_raw))
if (length(missing_cl_cols) > 0)
  stop(
    "Required columns missing from candidate-log file: ",
    paste(missing_cl_cols, collapse = ", "),
    "\n\nExpected columns: ", paste(required_cl_cols, collapse = ", "),
    "\nActual columns  : ", paste(names(candidate_logs_raw), collapse = ", "),
    "\n\nCheck that 03_define_products_and_volume.R produced the full candidate",
    " log table (generate_log_candidates_for_tree output)."
  )

wlog("  Required columns present: OK")
wlog(sprintf("  All columns: %s", paste(names(candidate_logs_raw), collapse = ", ")))


# ---- prepare candidate logs ----

wlog("", "--- Preparing candidate logs ---")

n_total    <- nrow(candidate_logs_raw)
n_eligible <- sum(candidate_logs_raw$eligible, na.rm = TRUE)

wlog(sprintf("  Total candidate rows : %d", n_total))
wlog(sprintf("  Eligible             : %d", n_eligible))
wlog(sprintf("  Not eligible         : %d", n_total - n_eligible))

# Work only with eligible logs.
cand_eligible <- candidate_logs_raw[candidate_logs_raw$eligible, ]

# Add join-key columns.
# TopDiameter is in mm (from script 03).
# LogLength is in cm (from script 03).
# PriceDiameterClass_cm = floor(TopDiameter_mm / 10)
# LengthClass_dm        = LogLength_cm / 10
cand_eligible <- cand_eligible |>
  dplyr::mutate(
    PriceDiameterClass_cm = floor(TopDiameter / 10),
    LengthClass_dm        = LogLength / 10,
    PriceScenario         = PRICE_SCENARIO
  )

wlog("", "  Join keys derived:")
wlog(sprintf("    PriceDiameterClass_cm = floor(TopDiameter_mm / 10) | range: %g – %g",
             min(cand_eligible$PriceDiameterClass_cm, na.rm = TRUE),
             max(cand_eligible$PriceDiameterClass_cm, na.rm = TRUE)))
wlog(sprintf("    LengthClass_dm = LogLength_cm / 10 | values: %s",
             paste(sort(unique(cand_eligible$LengthClass_dm)), collapse = ", ")))


# ---- split into sawlog and flat-price candidates ----

sawlog_pks   <- ProductData$ProductKey[ProductData$ProductType == "sawlog"]
flat_pks     <- ProductData$ProductKey[ProductData$ProductType != "sawlog"]

cand_sawlog  <- cand_eligible[cand_eligible$ProductKey %in% sawlog_pks, ]
cand_flat    <- cand_eligible[cand_eligible$ProductKey %in% flat_pks, ]

wlog("")
wlog("  Eligible candidates by type:")
wlog(sprintf("    Sawlog (matrix price) : %d", nrow(cand_sawlog)))
wlog(sprintf("    Pulpwood/waste (flat) : %d", nrow(cand_flat)))


# ---- join sawlog prices ----

wlog("", "--- Joining sawlog prices ---")

pm_matrix <- PriceMatrix_long[PriceMatrix_long$PriceType == "matrix",
                               c("PriceScenario", "ProductKey",
                                 "LengthClass_dm", "PriceDiameterClass_cm",
                                 "PriceNOKperM3")]

valued_sawlog <- NULL
if (nrow(cand_sawlog) > 0) {
  valued_sawlog <- dplyr::left_join(
    cand_sawlog, pm_matrix,
    by = c("PriceScenario", "ProductKey",
           "LengthClass_dm", "PriceDiameterClass_cm")
  )
  n_saw_matched   <- sum(!is.na(valued_sawlog$PriceNOKperM3))
  n_saw_unmatched <- sum( is.na(valued_sawlog$PriceNOKperM3))
  wlog(sprintf("  Sawlog rows joined  : %d", nrow(valued_sawlog)))
  wlog(sprintf("  Price matched       : %d", n_saw_matched))
  wlog(sprintf("  Price unmatched     : %d", n_saw_unmatched))
} else {
  wlog("  No sawlog candidates to join.")
}


# ---- join flat prices ----

wlog("", "--- Joining flat prices ---")

pm_flat <- PriceMatrix_long[PriceMatrix_long$PriceType == "flat",
                             c("PriceScenario", "ProductKey", "PriceNOKperM3")]

valued_flat <- NULL
if (nrow(cand_flat) > 0) {
  # For flat products, join only on PriceScenario + ProductKey.
  # Remove join keys derived for sawlog (not needed / NA for flat anyway).
  valued_flat <- dplyr::left_join(
    cand_flat, pm_flat,
    by = c("PriceScenario", "ProductKey")
  )
  n_flat_matched   <- sum(!is.na(valued_flat$PriceNOKperM3))
  n_flat_unmatched <- sum( is.na(valued_flat$PriceNOKperM3))
  wlog(sprintf("  Flat rows joined    : %d", nrow(valued_flat)))
  wlog(sprintf("  Price matched       : %d", n_flat_matched))
  wlog(sprintf("  Price unmatched     : %d", n_flat_unmatched))
} else {
  wlog("  No flat-price candidates to join.")
}


# ---- combine and calculate value ----

wlog("", "--- Calculating value ---")

# Bind sawlog and flat together.
valued_all <- dplyr::bind_rows(valued_sawlog, valued_flat)

# value_nok = price_volume_m3 * PriceNOKperM3.
# Unmatched price → NA value (not zero).
valued_all <- valued_all |>
  dplyr::mutate(
    value_nok = price_volume_m3 * PriceNOKperM3
  )

n_valued   <- sum(!is.na(valued_all$value_nok))
n_missing  <- sum( is.na(valued_all$value_nok))

wlog(sprintf("  Total valued rows    : %d", nrow(valued_all)))
wlog(sprintf("  value_nok non-NA     : %d", n_valued))
wlog(sprintf("  value_nok NA         : %d", n_missing))


# ---- diagnose unmatched price keys ----

wlog("", "--- Unmatched price keys ---")

unmatched <- valued_all[is.na(valued_all$PriceNOKperM3), ]

# How many are the known 31 dm issue?
n_unmatched_31dm <- if (nrow(unmatched) > 0 && "LengthClass_dm" %in% names(unmatched)) {
  sum(!is.na(unmatched$LengthClass_dm) & unmatched$LengthClass_dm == 31, na.rm = TRUE)
} else {
  0L
}

n_unmatched_other <- nrow(unmatched) - n_unmatched_31dm

wlog(sprintf("  Unmatched rows total             : %d", nrow(unmatched)))
wlog(sprintf("  of which LengthClass_dm == 31    : %d  (known: price matrix starts at 34 dm)",
             n_unmatched_31dm))
wlog(sprintf("  of which other unmatched         : %d", n_unmatched_other))

if (nrow(unmatched) > 0) {
  wlog("", "  Unmatched breakdown by ProductKey:")
  um_by_pk <- as.data.frame(table(ProductKey = unmatched$ProductKey),
                              stringsAsFactors = FALSE)
  names(um_by_pk)[2] <- "n_unmatched"
  wlog(capture.output(print(um_by_pk, row.names = FALSE)))

  if (!is.null(cand_sawlog) && nrow(cand_sawlog) > 0 &&
      "LengthClass_dm" %in% names(unmatched)) {
    wlog("", "  Unmatched sawlog breakdown by LengthClass_dm and PriceDiameterClass_cm:")
    um_saw <- unmatched[unmatched$ProductKey %in% sawlog_pks, ]
    if (nrow(um_saw) > 0) {
      um_saw_tab <- as.data.frame(table(
        LengthClass_dm        = um_saw$LengthClass_dm,
        PriceDiameterClass_cm = um_saw$PriceDiameterClass_cm
      ), stringsAsFactors = FALSE)
      um_saw_tab <- um_saw_tab[um_saw_tab$Freq > 0, ]
      names(um_saw_tab)[3] <- "n_unmatched"
      wlog(capture.output(print(um_saw_tab, row.names = FALSE)))
    }
  }
}


# ---- price-join diagnostic by product ----

wlog("", "--- Price-join diagnostic by ProductKey ---")

pk_diag_rows <- list()
for (pk in sort(unique(valued_all$ProductKey))) {
  rows_pk       <- valued_all[valued_all$ProductKey == pk, ]
  n_rows        <- nrow(rows_pk)
  n_with_price  <- sum(!is.na(rows_pk$PriceNOKperM3))
  n_with_value  <- sum(!is.na(rows_pk$value_nok))
  prices        <- rows_pk$PriceNOKperM3[!is.na(rows_pk$PriceNOKperM3)]
  values        <- rows_pk$value_nok[!is.na(rows_pk$value_nok)]

  pk_diag_rows[[length(pk_diag_rows) + 1L]] <- data.frame(
    ProductKey         = pk,
    n_eligible         = n_rows,
    n_price_matched    = n_with_price,
    n_price_unmatched  = n_rows - n_with_price,
    n_valued           = n_with_value,
    price_min          = if (length(prices) > 0) min(prices)  else NA_real_,
    price_mean         = if (length(prices) > 0) mean(prices) else NA_real_,
    price_max          = if (length(prices) > 0) max(prices)  else NA_real_,
    value_nok_min      = if (length(values) > 0) min(values)  else NA_real_,
    value_nok_mean     = if (length(values) > 0) mean(values) else NA_real_,
    value_nok_max      = if (length(values) > 0) max(values)  else NA_real_,
    value_nok_total    = if (length(values) > 0) sum(values)  else NA_real_,
    stringsAsFactors = FALSE
  )
}
pk_diag <- do.call(rbind, pk_diag_rows)

wlog(capture.output(print(
  pk_diag[, c("ProductKey", "n_eligible", "n_price_matched", "n_price_unmatched",
              "price_min", "price_max", "value_nok_total")],
  row.names = FALSE
)))


# ---- diagnostic plots ----

wlog("", "--- Creating diagnostic plots ---")

# 1. Price NOK/m3 by ProductKey for valued sawlog candidates.
sawlog_valued <- valued_all[valued_all$ProductKey %in% sawlog_pks &
                               !is.na(valued_all$PriceNOKperM3), ]
if (nrow(sawlog_valued) > 0) {
  p_price_dia <- sawlog_valued |>
    ggplot2::ggplot(ggplot2::aes(
      x      = PriceDiameterClass_cm,
      y      = PriceNOKperM3,
      colour = factor(LengthClass_dm),
      group  = factor(LengthClass_dm)
    )) +
    ggplot2::geom_line(alpha = 0.7) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::facet_wrap(~ProductKey) +
    ggplot2::labs(
      title  = "Sawlog price by diameter class (candidate logs, by length class)",
      x      = "PriceDiameterClass_cm",
      y      = "PriceNOKperM3",
      colour = "LengthClass_dm"
    ) +
    ggplot2::theme_minimal()
  save_plot(p_price_dia, "plot_sawlog_price_by_diameter.png")
}

# 2. value_nok by LogLength for all eligible valued candidates.
valued_not_na <- valued_all[!is.na(valued_all$value_nok), ]
if (nrow(valued_not_na) > 0) {
  p_val_len <- valued_not_na |>
    ggplot2::ggplot(ggplot2::aes(
      x      = LogLength,
      y      = value_nok,
      colour = ProductKey
    )) +
    ggplot2::geom_point(size = 2, alpha = 0.7) +
    ggplot2::labs(
      title  = "Log value (NOK) by LogLength for valued candidate logs",
      x      = "LogLength (cm)",
      y      = "value_nok (NOK)",
      colour = "ProductKey"
    ) +
    ggplot2::theme_minimal()
  save_plot(p_val_len, "plot_value_by_log_length.png")
}


# ---- save outputs ----

wlog("", "--- Saving outputs ---")

valued_csv <- file.path(val_dir, "candidate_logs_valued_test.csv")
readr::write_csv(valued_all, valued_csv)
wlog(sprintf("  Saved: %s  (%d rows)", basename(valued_csv), nrow(valued_all)))

# Always write unmatched CSV (zero-row file if all prices matched) so the
# audit script can check it consistently.
unmatched_csv <- file.path(val_dir, "candidate_logs_unmatched_price_keys.csv")
readr::write_csv(unmatched, unmatched_csv)
wlog(sprintf("  Saved: %s  (%d rows)", basename(unmatched_csv), nrow(unmatched)))

diag_csv <- file.path(val_dir, "price_join_diagnostic_by_product.csv")
readr::write_csv(pk_diag, diag_csv)
wlog(sprintf("  Saved: %s", basename(diag_csv)))


# ---- final valuation verdict ----

wlog(
  "",
  "=============================================================",
  "  VALUATION TEST SUMMARY",
  "============================================================="
)

wlog(sprintf("  Candidate logs read        : %d", n_total))
wlog(sprintf("  Eligible                   : %d", n_eligible))
wlog(sprintf("  Valued (value_nok non-NA)  : %d", n_valued))
wlog(sprintf("  Missing price (NA)         : %d", n_missing))
wlog(sprintf("    of which 31 dm sawlog    : %d", n_unmatched_31dm))
wlog(sprintf("    of which other           : %d", n_unmatched_other))

# Determine verdict.
# PASS   : every ProductKey present in the eligible candidate logs has at least
#          one priced row where a price is expected; no unexplained NA prices.
# REVIEW : the only missing prices are sawlog rows with LengthClass_dm == 31
#          (known: price matrix starts at 34 dm).
# FAIL   : required files missing, sawlog prices cannot join at all for any
#          ProductKey, or any eligible row has an unexplained missing price.
required_files_ok <- file.exists(pd_file) && file.exists(lc_file) &&
  file.exists(pm_file)

# For each ProductKey present in the candidate logs, check that at least one
# row has a matched price (ignoring the known 31 dm gap for sawlogs).
pks_in_candidates <- unique(cand_eligible$ProductKey)
pks_with_no_price <- character(0)
for (pk in pks_in_candidates) {
  rows_pk       <- valued_all[valued_all$ProductKey == pk, ]
  n_priced_pk   <- sum(!is.na(rows_pk$PriceNOKperM3))
  # Sawlog: allow zero prices only if ALL unmatched rows are 31 dm.
  if (pk %in% sawlog_pks) {
    unmatched_non31 <- rows_pk[
      is.na(rows_pk$PriceNOKperM3) &
      !("LengthClass_dm" %in% names(rows_pk) &
        !is.na(rows_pk$LengthClass_dm) & rows_pk$LengthClass_dm == 31), ]
    if (n_priced_pk == 0 || nrow(unmatched_non31) > 0)
      pks_with_no_price <- c(pks_with_no_price, pk)
  } else {
    if (n_priced_pk == 0)
      pks_with_no_price <- c(pks_with_no_price, pk)
  }
}

verdict <- "PASS"
if (!required_files_ok) {
  verdict <- "FAIL"
} else if (length(pks_with_no_price) > 0) {
  verdict <- "FAIL"
} else if (n_unmatched_other > 0) {
  verdict <- "FAIL"
} else if (n_unmatched_31dm > 0) {
  verdict <- "REVIEW"
}

verdict_msg <- if (verdict == "PASS") {
  "At least one candidate per relevant ProductKey was valued; all missing prices explained."
} else if (verdict == "REVIEW") {
  paste0("Missing prices exist only for expected unmatched 31 dm sawlog lengths (",
         n_unmatched_31dm, " rows). Price matrix starts at 34 dm. Intentional.")
} else {
  pks_msg <- if (length(pks_with_no_price) > 0)
    paste0("ProductKey(s) with no priced rows: ", paste(pks_with_no_price, collapse = ", "), ". ")
  else ""
  paste0(pks_msg, "Unexpected unmatched rows: ", n_unmatched_other, ".")
}

wlog("")
wlog(sprintf("  Verdict : %s", verdict))
wlog(sprintf("  %s", verdict_msg))
wlog("=============================================================")
wlog(sprintf("  Log: %s", log_file))

if (verdict == "FAIL")
  stop(sprintf("Valuation test FAILED: %s", verdict_msg))

message(sprintf("[%s] Valuation test: %s", verdict, verdict_msg))
