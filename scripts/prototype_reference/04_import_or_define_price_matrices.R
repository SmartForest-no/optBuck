# ==============================================================================
# 04_import_or_define_price_matrices.R
#
# Purpose : Import or define default price matrices for the nfiBuck workflow.
#           Produces a clean, validated long-format price table (PriceMatrix_long)
#           that can be joined to candidate logs in script 05.
#
#           This script does NOT do bucking optimization.
#           This script does NOT generate stem profiles.
#           This script does NOT change ProductData or LengthClasses.
#           This script only prepares price information.
#
# Inputs  :
#   inputs/price_matrix_sources/PriceMatrixGran.xlsx
#   inputs/price_matrix_sources/PriceMatrixFuru.xlsx
#   inputs/price_matrix_sources/pristabell_gran_skur.xlsx  (optional audit source)
#   inputs/ProductData_spruce_pine_default.csv
#   inputs/LengthClasses_spruce_pine_default.csv
#
# Outputs :
#   inputs/price_matrices/PriceMatrix_long_default_2021.csv / .rds
#   inputs/price_matrices/PriceMatrix_spruce_sawlog_default_2021.csv
#   inputs/price_matrices/PriceMatrix_pine_sawlog_default_2021.csv
#   inputs/price_matrices/FlatPrices_default_2021.csv
#   outputs/price_matrix_tests/raw_PriceMatrixGran.csv
#   outputs/price_matrix_tests/raw_PriceMatrixFuru.csv
#   outputs/price_matrix_tests/plot_*.png
#   outputs/price_matrix_tests/price_matrix_import_summary.txt
#
# Unit conventions (consistent with scripts 02-03):
#   lengths  : cm internally; dm for price-matrix length class keys
#   diameters: mm internally; cm for price-matrix diameter class keys
#   volume   : m3
#   prices   : NOK per m3
#
# Join key convention (for script 05):
#   PriceDiameterClass_cm = floor(TopDiameter_mm / 10)   (top-diameter class, cm)
#   LengthClass_dm        = LogLength_cm / 10             (length class, dm)
# ==============================================================================


# ---- setup ----

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(tibble)
library(ggplot2)

root_dir        <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
src_dir         <- file.path(root_dir, "inputs", "price_matrix_sources")
pm_inputs_dir   <- file.path(root_dir, "inputs",  "price_matrices")
pm_tests_dir    <- file.path(root_dir, "outputs", "price_matrix_tests")
main_inputs_dir <- file.path(root_dir, "inputs")

dir.create(src_dir,       showWarnings = FALSE, recursive = TRUE)
dir.create(pm_inputs_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(pm_tests_dir,  showWarnings = FALSE, recursive = TRUE)

log_file <- file.path(pm_tests_dir, "price_matrix_import_summary.txt")
cat("", file = log_file)

PRICE_SCENARIO <- "default_2021"

# Source Excel file paths.
gran_file      <- file.path(src_dir, "PriceMatrixGran.xlsx")
furu_file      <- file.path(src_dir, "PriceMatrixFuru.xlsx")
gran_alt_file  <- file.path(src_dir, "pristabell_gran_skur.xlsx")


# ---- helper: logging ----

wlog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = log_file, append = TRUE)
}

save_plot <- function(p, filename, width = 18, height = 12,
                      units = "cm", dpi = 150) {
  path <- file.path(pm_tests_dir, filename)
  ggplot2::ggsave(path, plot = p, width = width, height = height,
                  units = units, dpi = dpi)
  wlog(sprintf("  Plot saved: %s", basename(path)))
}

wlog(
  "=============================================================",
  "  Price matrix import / definition – log",
  sprintf("  Run        : %s", Sys.time()),
  sprintf("  Scenario   : %s", PRICE_SCENARIO),
  "=============================================================",
  "",
  "--- Source file check ---",
  sprintf("  %-40s : %s", basename(gran_file),
          if (file.exists(gran_file)) "FOUND" else "NOT FOUND"),
  sprintf("  %-40s : %s", basename(furu_file),
          if (file.exists(furu_file)) "FOUND" else "NOT FOUND"),
  sprintf("  %-40s : %s", basename(gran_alt_file),
          if (file.exists(gran_alt_file)) "FOUND" else "NOT FOUND")
)


# ---- read active ProductData ----

pd_file <- file.path(main_inputs_dir, "ProductData_spruce_pine_default.csv")
lc_file <- file.path(main_inputs_dir, "LengthClasses_spruce_pine_default.csv")

if (!file.exists(pd_file))
  stop("ProductData file not found: ", pd_file,
       "\nRun 03_define_products_and_volume.R first.")
if (!file.exists(lc_file))
  stop("LengthClasses file not found: ", lc_file,
       "\nRun 03_define_products_and_volume.R first.")

ProductData  <- readr::read_csv(pd_file, show_col_types = FALSE)
LengthClasses_long <- readr::read_csv(lc_file, show_col_types = FALSE)

active_product_keys <- ProductData$ProductKey

wlog(
  "",
  "--- Active ProductData ---",
  sprintf("  Products loaded: %d", nrow(ProductData)),
  sprintf("  ProductKeys    : %s", paste(active_product_keys, collapse = ", "))
)


# ---- helper functions ----

#' Find the rectangular numeric matrix area in an Excel sheet.
#'
#' Reads the sheet without column types and scans for the first row that
#' has a mix of numeric-like values, treating the leftmost column as a
#' row-label (diameter) and the header row above as column labels (lengths).
#'
#' @param sheet_data  Data frame as returned by readxl::read_excel with
#'                    col_names = FALSE.
#' @return List: row_labels (numeric vector), col_labels (numeric vector),
#'         matrix (numeric matrix, rows = diameter classes, cols = lengths),
#'         row_start (row index of first data row in sheet_data),
#'         col_start (col index of first data column in sheet_data).
find_matrix_area <- function(sheet_data) {

  df <- as.data.frame(sheet_data)
  n_rows <- nrow(df)
  n_cols <- ncol(df)

  is_numeric_like <- function(x) {
    v <- suppressWarnings(as.numeric(as.character(x)))
    !is.na(v)
  }

  # Find the first row where the majority of columns (excluding col 1) are numeric.
  header_row <- NA_integer_
  data_row_start <- NA_integer_

  for (r in seq_len(n_rows)) {
    row_vals <- df[r, -1]
    n_numeric <- sum(vapply(row_vals, function(x) is_numeric_like(x), logical(1)))
    if (n_numeric >= max(2, floor((n_cols - 1) * 0.5))) {
      # Check if col 1 of this row is also numeric (data row) or label-like (header row).
      col1_val <- df[r, 1]
      col1_is_num <- is_numeric_like(col1_val)
      if (!col1_is_num && is.na(header_row)) {
        header_row <- r
      } else if (col1_is_num) {
        data_row_start <- r
        if (is.na(header_row)) header_row <- r - 1L
        break
      }
    }
  }

  if (is.na(data_row_start))
    stop("Could not locate numeric data rows in sheet. Check Excel file format.")
  if (is.na(header_row) || header_row < 1L)
    stop("Could not locate header row (length classes) above data matrix.")

  # Find the data column range: all columns with a numeric header label.
  header_vals <- as.character(unlist(df[header_row, ]))
  col_numeric  <- which(vapply(header_vals, function(x) is_numeric_like(x), logical(1)))
  if (length(col_numeric) < 2)
    stop("Fewer than 2 numeric column labels found in header row.")

  # Extract column labels (length classes in dm).
  col_labels <- as.numeric(header_vals[col_numeric])

  # Extract all data rows: rows from data_row_start where col 1 is numeric.
  data_rows <- integer(0)
  for (r in seq(data_row_start, n_rows)) {
    if (is_numeric_like(df[r, 1])) {
      data_rows <- c(data_rows, r)
    }
  }
  if (length(data_rows) == 0)
    stop("No numeric row labels (diameter classes) found.")

  row_labels <- as.numeric(as.character(unlist(df[data_rows, 1])))

  # Extract the matrix values.
  mat <- matrix(NA_real_, nrow = length(data_rows), ncol = length(col_numeric))
  for (i in seq_along(data_rows)) {
    for (j in seq_along(col_numeric)) {
      v <- suppressWarnings(as.numeric(as.character(df[data_rows[i], col_numeric[j]])))
      mat[i, j] <- v
    }
  }

  list(
    row_labels  = row_labels,
    col_labels  = col_labels,
    matrix      = mat,
    row_start   = data_row_start,
    col_start   = min(col_numeric)
  )
}

#' Import a sawlog price matrix from an Excel file into long format.
#'
#' Reads the first sheet of the file, detects the numeric matrix area using
#' find_matrix_area(), and pivots to long format with columns matching
#' PriceMatrix_long.
#'
#' Row labels are treated as top-diameter classes in cm.
#' Column labels are treated as length classes in dm.
#'
#' @param filepath    Path to the Excel file.
#' @param ProductKey  ProductKey to assign (e.g. "spruce_sawlog").
#' @param pd          ProductData table (for ProductName, ProductType, etc.)
#' @param scenario    PriceScenario string.
#' @param source_tag  Short string identifying the source file.
#' @return Tibble in PriceMatrix_long format (only sawlog rows).
import_sawlog_price_matrix <- function(filepath, ProductKey, pd, scenario,
                                       source_tag = basename(filepath)) {

  wlog(sprintf("  Reading: %s", basename(filepath)))

  sheets <- readxl::excel_sheets(filepath)
  wlog(sprintf("    Sheets: %s", paste(sheets, collapse = ", ")))
  sheet  <- sheets[1]

  raw <- readxl::read_excel(filepath, sheet = sheet, col_names = FALSE,
                             .name_repair = "minimal")
  wlog(sprintf("    Raw sheet dimensions: %d rows x %d cols",
               nrow(raw), ncol(raw)))

  area <- tryCatch(
    find_matrix_area(raw),
    error = function(e) {
      stop(sprintf("find_matrix_area() failed for '%s': %s",
                   basename(filepath), conditionMessage(e)))
    }
  )

  wlog(sprintf("    Detected matrix: %d diameter classes x %d length classes",
               length(area$row_labels), length(area$col_labels)))
  wlog(sprintf("    Diameter range (cm): %g – %g",
               min(area$row_labels), max(area$row_labels)))
  wlog(sprintf("    Length range (dm)  : %g – %g",
               min(area$col_labels), max(area$col_labels)))

  # Look up product metadata.
  pd_row <- pd[pd$ProductKey == ProductKey, ]
  if (nrow(pd_row) == 0)
    stop(sprintf("ProductKey '%s' not found in ProductData.", ProductKey))

  # Build long-format tibble.
  n_dia <- length(area$row_labels)
  n_len <- length(area$col_labels)
  rows  <- vector("list", n_dia * n_len)
  idx   <- 0L
  for (i in seq_len(n_dia)) {
    for (j in seq_len(n_len)) {
      idx <- idx + 1L
      rows[[idx]] <- tibble::tibble(
        PriceScenario          = scenario,
        ProductKey             = ProductKey,
        ProductName            = pd_row$ProductName[1],
        ProductType            = pd_row$ProductType[1],
        species_group          = pd_row$species_group[1],
        VolumeDiameterCategory = pd_row$VolumeDiameterCategory[1],
        PriceType              = "matrix",
        LengthClass_dm         = area$col_labels[j],
        LengthClass_cm         = area$col_labels[j] * 10,
        PriceDiameterClass_cm  = area$row_labels[i],
        PriceDiameterClass_mm  = area$row_labels[i] * 10,
        PriceNOKperM3          = area$matrix[i, j],
        PriceSource            = source_tag,
        PriceSourceFile        = basename(filepath),
        Notes                  = NA_character_
      )
    }
  }

  dplyr::bind_rows(rows)
}


# ---- import sawlog price matrices ----

wlog("", "--- Importing sawlog price matrices ---")

pm_gran      <- NULL
pm_gran_alt  <- NULL
pm_furu      <- NULL
raw_gran_mat <- NULL
raw_furu_mat <- NULL

# --- Spruce sawlog: PriceMatrixGran.xlsx ---
if (file.exists(gran_file)) {
  pm_gran <- tryCatch(
    import_sawlog_price_matrix(gran_file, "spruce_sawlog", ProductData,
                               PRICE_SCENARIO, "PriceMatrixGran"),
    error = function(e) {
      wlog(sprintf("  ERROR importing PriceMatrixGran.xlsx: %s",
                   conditionMessage(e)))
      NULL
    }
  )
  if (!is.null(pm_gran)) {
    wlog(sprintf("  PriceMatrixGran: %d rows imported", nrow(pm_gran)))
    raw_gran_mat <- pm_gran
  }
} else {
  wlog("  PriceMatrixGran.xlsx not found – skipping.")
}

# --- Pine sawlog: PriceMatrixFuru.xlsx ---
if (file.exists(furu_file)) {
  pm_furu <- tryCatch(
    import_sawlog_price_matrix(furu_file, "pine_sawlog", ProductData,
                               PRICE_SCENARIO, "PriceMatrixFuru"),
    error = function(e) {
      wlog(sprintf("  ERROR importing PriceMatrixFuru.xlsx: %s",
                   conditionMessage(e)))
      NULL
    }
  )
  if (!is.null(pm_furu)) {
    wlog(sprintf("  PriceMatrixFuru: %d rows imported", nrow(pm_furu)))
    raw_furu_mat <- pm_furu
  }
} else {
  wlog("  PriceMatrixFuru.xlsx not found – skipping.")
}

# --- Alternate spruce sawlog: pristabell_gran_skur.xlsx ---
# Saved as audit source. Not used in default scenario if PriceMatrixGran already loaded.
if (file.exists(gran_alt_file)) {
  pm_gran_alt <- tryCatch(
    import_sawlog_price_matrix(gran_alt_file, "spruce_sawlog", ProductData,
                               PRICE_SCENARIO, "pristabell_gran_skur"),
    error = function(e) {
      wlog(sprintf("  ERROR importing pristabell_gran_skur.xlsx: %s",
                   conditionMessage(e)))
      NULL
    }
  )
  if (!is.null(pm_gran_alt)) {
    wlog(sprintf("  pristabell_gran_skur: %d rows (audit source only).",
                 nrow(pm_gran_alt)))
    # Save as audit output regardless.
    readr::write_csv(pm_gran_alt,
                     file.path(pm_tests_dir, "raw_pristabell_gran_skur_audit.csv"))
    # Use this table for spruce sawlog only if PriceMatrixGran.xlsx is absent.
    if (is.null(pm_gran)) {
      pm_gran <- pm_gran_alt
      wlog("  Using pristabell_gran_skur as primary spruce sawlog source (PriceMatrixGran not found).")
    } else {
      wlog("  pristabell_gran_skur kept as audit only. PriceMatrixGran used as primary source.")
    }
  }
} else {
  wlog("  pristabell_gran_skur.xlsx not found – skipping.")
}

# Save raw imports as CSV audit files.
if (!is.null(raw_gran_mat))
  readr::write_csv(raw_gran_mat, file.path(pm_tests_dir, "raw_PriceMatrixGran.csv"))
if (!is.null(raw_furu_mat))
  readr::write_csv(raw_furu_mat, file.path(pm_tests_dir, "raw_PriceMatrixFuru.csv"))


# ---- define flat prices ----

# NOTE: These flat prices are temporary placeholders.
# Replace with documented values from the price overview (PDF or other official source)
# or from actual mill/contractor contracts before any operational use.

wlog("", "--- Defining flat prices (temporary defaults) ---")

FlatPrices <- tibble::tribble(
  ~PriceScenario,  ~ProductKey,       ~ProductName,      ~ProductType,
  ~species_group,  ~VolumeDiameterCategory,
  ~PriceType, ~LengthClass_dm, ~LengthClass_cm,
  ~PriceDiameterClass_cm, ~PriceDiameterClass_mm,
  ~PriceNOKperM3,  ~PriceSource,                  ~PriceSourceFile,
  ~Notes,

  # --- spruce pulpwood ---
  PRICE_SCENARIO, "spruce_pulpwood", "Spruce pulpwood", "pulpwood",
  "spruce", "All diameters (solid volume)",
  "flat", NA_real_, NA_real_,
  NA_real_, NA_real_,
  260, "Manual temporary default", NA_character_,
  "Temporary flat price. Replace with documented pulpwood price later.",

  # --- pine pulpwood ---
  PRICE_SCENARIO, "pine_pulpwood",   "Pine pulpwood",   "pulpwood",
  "pine",   "All diameters (solid volume)",
  "flat", NA_real_, NA_real_,
  NA_real_, NA_real_,
  250, "Manual temporary default", NA_character_,
  "Temporary flat price. Replace with documented pulpwood price later.",

  # --- spruce waste ---
  PRICE_SCENARIO, "spruce_waste",    "Spruce waste",    "waste",
  "spruce", "All diameters (solid volume)",
  "flat", NA_real_, NA_real_,
  NA_real_, NA_real_,
  0, "Manual temporary default", NA_character_,
  "Waste assigned zero price. Replace with documented disposal cost if relevant.",

  # --- pine waste ---
  PRICE_SCENARIO, "pine_waste",      "Pine waste",      "waste",
  "pine",   "All diameters (solid volume)",
  "flat", NA_real_, NA_real_,
  NA_real_, NA_real_,
  0, "Manual temporary default", NA_character_,
  "Waste assigned zero price. Replace with documented disposal cost if relevant."
)

wlog(capture.output(print(as.data.frame(
  FlatPrices[, c("ProductKey", "PriceType", "PriceNOKperM3", "Notes")]
))))


# ---- combine price tables ----

wlog("", "--- Combining price tables ---")

# Collect all sawlog matrix parts (may be NULL if files missing).
sawlog_parts <- Filter(Negate(is.null), list(pm_gran, pm_furu))

if (length(sawlog_parts) == 0) {
  wlog("  WARNING: No sawlog price matrices loaded. PriceMatrix_long will contain flat prices only.")
  PriceMatrix_long <- FlatPrices
} else {
  sawlog_combined  <- dplyr::bind_rows(sawlog_parts)
  PriceMatrix_long <- dplyr::bind_rows(sawlog_combined, FlatPrices)
}

# Enforce consistent column types.
PriceMatrix_long <- PriceMatrix_long |>
  dplyr::mutate(
    LengthClass_dm        = as.numeric(LengthClass_dm),
    LengthClass_cm        = as.numeric(LengthClass_cm),
    PriceDiameterClass_cm = as.numeric(PriceDiameterClass_cm),
    PriceDiameterClass_mm = as.numeric(PriceDiameterClass_mm),
    PriceNOKperM3         = as.numeric(PriceNOKperM3)
  ) |>
  dplyr::arrange(ProductKey, PriceDiameterClass_cm, LengthClass_dm)

wlog(sprintf("  Total rows in PriceMatrix_long: %d", nrow(PriceMatrix_long)))
wlog(sprintf("  ProductKeys represented       : %s",
             paste(unique(PriceMatrix_long$ProductKey), collapse = ", ")))

# Warn about active product keys with no price data.
missing_pks <- setdiff(active_product_keys, unique(PriceMatrix_long$ProductKey))
if (length(missing_pks) > 0)
  wlog(sprintf("  WARNING: No price data for ProductKey(s): %s",
               paste(missing_pks, collapse = ", ")))


# ---- validate price table ----

#' Validate PriceMatrix_long for required structure and consistency.
#'
#' Stops on critical errors; warns on less severe issues.
#'
#' Checks:
#'   - All required columns present.
#'   - No negative prices.
#'   - Sawlog products ("matrix" type) have non-missing LengthClass_dm,
#'     LengthClass_cm, PriceDiameterClass_cm, PriceDiameterClass_mm.
#'   - Flat products have PriceType == "flat".
#'   - No duplicate rows for matrix prices (PriceScenario + ProductKey +
#'     LengthClass_dm + PriceDiameterClass_cm).
#'   - No duplicate rows for flat prices (PriceScenario + ProductKey).
#'
#' @param pm  PriceMatrix_long tibble.
#' @return Invisible TRUE on success.
check_PriceMatrix_long <- function(pm) {

  required_cols <- c(
    "PriceScenario", "ProductKey", "ProductName", "ProductType",
    "species_group", "VolumeDiameterCategory", "PriceType",
    "LengthClass_dm", "LengthClass_cm",
    "PriceDiameterClass_cm", "PriceDiameterClass_mm",
    "PriceNOKperM3", "PriceSource", "PriceSourceFile", "Notes"
  )
  missing_req <- setdiff(required_cols, names(pm))
  if (length(missing_req) > 0)
    stop("PriceMatrix_long is missing required columns: ",
         paste(missing_req, collapse = ", "))

  if (any(pm$PriceNOKperM3 < 0, na.rm = TRUE))
    stop("PriceMatrix_long contains negative PriceNOKperM3 values.")

  if (any(is.na(pm$PriceNOKperM3)))
    warning("PriceMatrix_long contains NA PriceNOKperM3 values.")

  # Matrix-price rows must have non-missing diameter and length classes.
  matrix_rows <- pm[pm$PriceType == "matrix", ]
  if (nrow(matrix_rows) > 0) {
    missing_len <- sum(is.na(matrix_rows$LengthClass_dm) |
                         is.na(matrix_rows$LengthClass_cm))
    if (missing_len > 0)
      stop(sprintf(
        "%d matrix-price row(s) have missing LengthClass_dm or LengthClass_cm.",
        missing_len
      ))
    missing_dia <- sum(is.na(matrix_rows$PriceDiameterClass_cm) |
                         is.na(matrix_rows$PriceDiameterClass_mm))
    if (missing_dia > 0)
      stop(sprintf(
        "%d matrix-price row(s) have missing PriceDiameterClass_cm or PriceDiameterClass_mm.",
        missing_dia
      ))
    # Duplicate check for matrix prices.
    dupes_matrix <- matrix_rows |>
      dplyr::group_by(PriceScenario, ProductKey, LengthClass_dm,
                      PriceDiameterClass_cm) |>
      dplyr::filter(dplyr::n() > 1) |>
      dplyr::ungroup()
    if (nrow(dupes_matrix) > 0)
      stop(sprintf(
        "Duplicate matrix-price rows found (PriceScenario + ProductKey + LengthClass_dm + PriceDiameterClass_cm). First duplicate: %s / %s / %s / %s",
        dupes_matrix$PriceScenario[1], dupes_matrix$ProductKey[1],
        dupes_matrix$LengthClass_dm[1], dupes_matrix$PriceDiameterClass_cm[1]
      ))
  }

  # Flat-price rows must have PriceType == "flat".
  non_sawlog_types <- c("pulpwood", "waste")
  non_sawlog_rows  <- pm[pm$ProductType %in% non_sawlog_types, ]
  if (nrow(non_sawlog_rows) > 0) {
    wrong_type <- sum(non_sawlog_rows$PriceType != "flat", na.rm = TRUE)
    if (wrong_type > 0)
      stop(sprintf(
        "%d pulpwood/waste row(s) have PriceType != 'flat'.", wrong_type
      ))
    # Duplicate check for flat prices.
    dupes_flat <- non_sawlog_rows |>
      dplyr::group_by(PriceScenario, ProductKey) |>
      dplyr::filter(dplyr::n() > 1) |>
      dplyr::ungroup()
    if (nrow(dupes_flat) > 0)
      stop(sprintf(
        "Duplicate flat-price rows found (PriceScenario + ProductKey). Affected ProductKey(s): %s",
        paste(unique(dupes_flat$ProductKey), collapse = ", ")
      ))
  }

  invisible(TRUE)
}

validation_result <- tryCatch({
  check_PriceMatrix_long(PriceMatrix_long)
  wlog("", "  Validation: PASSED")
  TRUE
}, error = function(e) {
  stop("PriceMatrix_long validation failed: ", conditionMessage(e))
}, warning = function(w) {
  wlog(sprintf("  Validation WARNING: %s", conditionMessage(w)))
  TRUE
})


# ---- save outputs ----

wlog("", "--- Saving outputs ---")

# Full long-format price table.
pm_long_csv <- file.path(pm_inputs_dir, "PriceMatrix_long_default_2021.csv")
pm_long_rds <- file.path(pm_inputs_dir, "PriceMatrix_long_default_2021.rds")
readr::write_csv(PriceMatrix_long, pm_long_csv)
saveRDS(PriceMatrix_long, pm_long_rds)
wlog(sprintf("  Saved: %s", basename(pm_long_csv)))
wlog(sprintf("  Saved: %s", basename(pm_long_rds)))

# Separate spruce sawlog matrix.
pm_gran_out <- PriceMatrix_long |>
  dplyr::filter(ProductKey == "spruce_sawlog", PriceType == "matrix")
if (nrow(pm_gran_out) > 0) {
  gran_out_csv <- file.path(pm_inputs_dir, "PriceMatrix_spruce_sawlog_default_2021.csv")
  readr::write_csv(pm_gran_out, gran_out_csv)
  wlog(sprintf("  Saved: %s (%d rows)", basename(gran_out_csv), nrow(pm_gran_out)))
}

# Separate pine sawlog matrix.
pm_furu_out <- PriceMatrix_long |>
  dplyr::filter(ProductKey == "pine_sawlog", PriceType == "matrix")
if (nrow(pm_furu_out) > 0) {
  furu_out_csv <- file.path(pm_inputs_dir, "PriceMatrix_pine_sawlog_default_2021.csv")
  readr::write_csv(pm_furu_out, furu_out_csv)
  wlog(sprintf("  Saved: %s (%d rows)", basename(furu_out_csv), nrow(pm_furu_out)))
}

# Flat prices.
flat_csv <- file.path(pm_inputs_dir, "FlatPrices_default_2021.csv")
readr::write_csv(FlatPrices, flat_csv)
wlog(sprintf("  Saved: %s", basename(flat_csv)))


# ---- diagnostic plots ----

wlog("", "--- Creating diagnostic plots ---")

matrix_data <- PriceMatrix_long |>
  dplyr::filter(PriceType == "matrix", !is.na(PriceNOKperM3))

# 1. Spruce sawlog price heatmap.
gran_heat <- matrix_data |> dplyr::filter(ProductKey == "spruce_sawlog")
if (nrow(gran_heat) > 0) {
  p_gran_heat <- gran_heat |>
    ggplot2::ggplot(ggplot2::aes(
      x    = factor(LengthClass_dm),
      y    = factor(PriceDiameterClass_cm),
      fill = PriceNOKperM3
    )) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_viridis_c(option = "D", name = "NOK/m3") +
    ggplot2::labs(
      title = sprintf("Spruce sawlog price matrix (%s)", PRICE_SCENARIO),
      x     = "Length class (dm)",
      y     = "Top-diameter class (cm)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7))

  save_plot(p_gran_heat, "plot_spruce_sawlog_price_heatmap.png",
            width = 22, height = 16)
}

# 2. Pine sawlog price heatmap.
furu_heat <- matrix_data |> dplyr::filter(ProductKey == "pine_sawlog")
if (nrow(furu_heat) > 0) {
  p_furu_heat <- furu_heat |>
    ggplot2::ggplot(ggplot2::aes(
      x    = factor(LengthClass_dm),
      y    = factor(PriceDiameterClass_cm),
      fill = PriceNOKperM3
    )) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::scale_fill_viridis_c(option = "D", name = "NOK/m3") +
    ggplot2::labs(
      title = sprintf("Pine sawlog price matrix (%s)", PRICE_SCENARIO),
      x     = "Length class (dm)",
      y     = "Top-diameter class (cm)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 7))

  save_plot(p_furu_heat, "plot_pine_sawlog_price_heatmap.png",
            width = 22, height = 16)
}

# 3. Price by diameter class for selected length classes.
target_lengths_dm <- c(34, 40, 46, 52)
len_subset <- matrix_data |>
  dplyr::filter(LengthClass_dm %in% target_lengths_dm)

if (nrow(len_subset) > 0) {
  p_by_dia <- len_subset |>
    ggplot2::ggplot(ggplot2::aes(
      x      = PriceDiameterClass_cm,
      y      = PriceNOKperM3,
      colour = factor(LengthClass_dm),
      group  = factor(LengthClass_dm)
    )) +
    ggplot2::geom_line(alpha = 0.8) +
    ggplot2::geom_point(size = 1.5, alpha = 0.7) +
    ggplot2::facet_wrap(~ProductKey) +
    ggplot2::labs(
      title  = "Price by top-diameter class (selected length classes)",
      x      = "Top-diameter class (cm)",
      y      = "Price (NOK/m3)",
      colour = "Length (dm)"
    ) +
    ggplot2::theme_minimal()

  save_plot(p_by_dia, "plot_price_by_diameter_class.png", width = 22, height = 12)
}

# 4. Price by length class for selected diameter classes.
target_dias_cm <- c(12, 16, 20, 26)
dia_subset <- matrix_data |>
  dplyr::filter(PriceDiameterClass_cm %in% target_dias_cm)

if (nrow(dia_subset) > 0) {
  p_by_len <- dia_subset |>
    ggplot2::ggplot(ggplot2::aes(
      x      = LengthClass_dm,
      y      = PriceNOKperM3,
      colour = factor(PriceDiameterClass_cm),
      group  = factor(PriceDiameterClass_cm)
    )) +
    ggplot2::geom_line(alpha = 0.8) +
    ggplot2::geom_point(size = 1.5, alpha = 0.7) +
    ggplot2::facet_wrap(~ProductKey) +
    ggplot2::labs(
      title  = "Price by length class (selected diameter classes)",
      x      = "Length class (dm)",
      y      = "Price (NOK/m3)",
      colour = "Top-diam (cm)"
    ) +
    ggplot2::theme_minimal()

  save_plot(p_by_len, "plot_price_by_length_class.png", width = 22, height = 12)
}


# ---- write log ----
# Price range summary by ProductKey.
# Use safe summaries so flat-price rows with only NA length/diameter classes
# return NA instead of Inf/-Inf.

safe_min <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  min(x)
}

safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  max(x)
}

price_summary <- PriceMatrix_long |>
  dplyr::group_by(ProductKey, PriceType) |>
  dplyr::summarise(
    n_rows      = dplyr::n(),
    price_min   = safe_min(PriceNOKperM3),
    price_max   = safe_max(PriceNOKperM3),
    len_min_dm  = safe_min(LengthClass_dm),
    len_max_dm  = safe_max(LengthClass_dm),
    dia_min_cm  = safe_min(PriceDiameterClass_cm),
    dia_max_cm  = safe_max(PriceDiameterClass_cm),
    .groups     = "drop"
  )

n_matrix_rows <- sum(PriceMatrix_long$PriceType == "matrix", na.rm = TRUE)
n_flat_rows   <- sum(PriceMatrix_long$PriceType == "flat",   na.rm = TRUE)

wlog(
  "",
  "=============================================================",
  "  SUMMARY",
  "=============================================================",
  sprintf("  PriceScenario            : %s", PRICE_SCENARIO),
  "",
  "  Source files:",
  sprintf("    PriceMatrixGran.xlsx   : %s",
          if (file.exists(gran_file)) "found and read" else "NOT FOUND"),
  sprintf("    PriceMatrixFuru.xlsx   : %s",
          if (file.exists(furu_file)) "found and read" else "NOT FOUND"),
  sprintf("    pristabell_gran_skur   : %s",
          if (file.exists(gran_alt_file)) "found (audit only)" else "NOT FOUND"),
  "",
  "  PriceMatrix_long:",
  sprintf("    Total rows             : %d", nrow(PriceMatrix_long)),
  sprintf("    Matrix-price rows      : %d", n_matrix_rows),
  sprintf("    Flat-price rows        : %d", n_flat_rows),
  sprintf("    ProductKeys            : %s",
          paste(unique(PriceMatrix_long$ProductKey), collapse = ", ")),
  "",
  "  Price range by ProductKey:"
)
wlog(capture.output(print(as.data.frame(price_summary))))

wlog(
  "",
  "  Output files:",
  sprintf("    %s", basename(pm_long_csv)),
  sprintf("    %s", basename(pm_long_rds)),
  sprintf("    %s", basename(flat_csv)),
  "",
  sprintf("  Log: %s", log_file),
  "",
  "Done."
)
