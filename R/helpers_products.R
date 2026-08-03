# ==============================================================================
# R/helpers_products.R
#
# Purpose : Load and validate ProductData and LengthClasses for the nfiBuck
#           bucking simulation.
#
# Accepted assumptions (see CLAUDE.md):
#   DiameterUnderBark = FALSE for all active products
#   DiameterSource    = taperNOR_over_bark for all active products
#   Six active ProductKey values (ACTIVE_PRODUCT_KEYS constant below)
#
# Usage   : source("R/helpers_paths.R")
#           source("R/helpers_products.R")
#           pd <- load_products()
#           lc <- load_length_classes()
#           chk <- check_product_assumptions(pd)
# ==============================================================================

if (!exists("get_root_dir", mode = "function"))
  source(file.path(dirname(sys.frame(1L)$ofile %||% "."), "helpers_paths.R"))

# Define %||% locally in case helpers_audit.R is not yet sourced.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}


# ------------------------------------------------------------------------------
# Constants
# ------------------------------------------------------------------------------

#' The six active ProductKey values recognised by nfiBuck.
ACTIVE_PRODUCT_KEYS <- c(
  "spruce_sawlog", "spruce_pulpwood", "spruce_waste",
  "pine_sawlog",   "pine_pulpwood",   "pine_waste"
)

#' Expected DiameterUnderBark value for all active products.
EXPECTED_DIAMETER_UNDER_BARK <- FALSE

#' Expected DiameterSource value for all active products.
EXPECTED_DIAMETER_SOURCE <- "taperNOR_over_bark"


# ------------------------------------------------------------------------------
# Loaders
# ------------------------------------------------------------------------------

#' Load ProductData from the default CSV.
#'
#' @param root_dir   Project root (default: get_root_dir()).
#' @param file_name  CSV file name under inputs/.
#' @return data.frame with one row per product.
load_products <- function(
    root_dir  = get_root_dir(),
    file_name = "ProductData_spruce_pine_default.csv") {

  path <- file.path(root_dir, "inputs", file_name)
  if (!file.exists(path))
    stop("ProductData file not found: ", path)

  pd <- as.data.frame(
    readr::read_csv(path, show_col_types = FALSE))
  pd
}


#' Load LengthClasses from the default CSV.
#'
#' LogLength values are stored in **centimetres** in the CSV.
#' A convenience column LogLength_dm (= LogLength / 10) is added.
#'
#' @param root_dir   Project root (default: get_root_dir()).
#' @param file_name  CSV file name under inputs/.
#' @return data.frame with columns ProductKey, LogLength (cm), LogLength_dm.
load_length_classes <- function(
    root_dir  = get_root_dir(),
    file_name = "LengthClasses_spruce_pine_default.csv") {

  path <- file.path(root_dir, "inputs", file_name)
  if (!file.exists(path))
    stop("LengthClasses file not found: ", path)

  lc <- as.data.frame(
    readr::read_csv(path, show_col_types = FALSE))

  # Add dm column for convenience
  if ("LogLength" %in% names(lc) && !"LogLength_dm" %in% names(lc)) {
    lc$LogLength_dm <- lc$LogLength / 10
  }
  lc
}


# ------------------------------------------------------------------------------
# Validation
# ------------------------------------------------------------------------------

#' Check that ProductData satisfies the accepted assumptions.
#'
#' Returns a data.frame with columns: check, result, detail.
#' Results are "PASS", "REVIEW", or "FAIL".
#'
#' Checks performed:
#'   1. All ACTIVE_PRODUCT_KEYS are present.
#'   2. No extra ProductKey values beyond ACTIVE_PRODUCT_KEYS.
#'   3. DiameterUnderBark is FALSE for all active products.
#'   4. DiameterSource is "taperNOR_over_bark" for all active products (if column exists).
#'
#' @param pd data.frame from load_products().
#' @return data.frame(check, result, detail).
check_product_assumptions <- function(pd) {
  checks <- list()

  add_check <- function(check, result, detail = "") {
    checks[[length(checks) + 1L]] <<- data.frame(
      check  = check,
      result = result,
      detail = detail,
      stringsAsFactors = FALSE
    )
  }

  # 1. All active keys present
  present  <- ACTIVE_PRODUCT_KEYS %in% pd$ProductKey
  n_miss   <- sum(!present)
  add_check(
    "All ACTIVE_PRODUCT_KEYS present in ProductData",
    if (n_miss == 0L) "PASS" else "FAIL",
    if (n_miss == 0L) "6 of 6 present"
    else sprintf("Missing: %s",
                 paste(ACTIVE_PRODUCT_KEYS[!present], collapse = ", "))
  )

  # 2. No unknown keys
  extra <- setdiff(pd$ProductKey, ACTIVE_PRODUCT_KEYS)
  add_check(
    "No unexpected ProductKey values in ProductData",
    if (length(extra) == 0L) "PASS" else "REVIEW",
    if (length(extra) == 0L) "none"
    else paste("Extra:", paste(extra, collapse = ", "))
  )

  # 3. DiameterUnderBark == FALSE
  if ("DiameterUnderBark" %in% names(pd)) {
    active_pd  <- pd[pd$ProductKey %in% ACTIVE_PRODUCT_KEYS, ]
    dub_vals   <- unique(active_pd$DiameterUnderBark)
    all_false  <- all(as.logical(active_pd$DiameterUnderBark) == FALSE)
    add_check(
      "DiameterUnderBark == FALSE for all active products",
      if (all_false) "PASS" else "FAIL",
      if (all_false) "all FALSE"
      else sprintf("Unexpected values: %s",
                   paste(unique(active_pd$DiameterUnderBark[
                     as.logical(active_pd$DiameterUnderBark) != FALSE]),
                     collapse = ", "))
    )
  } else {
    add_check("DiameterUnderBark column present", "REVIEW",
              "Column not found in ProductData")
  }

  # 4. DiameterSource == taperNOR_over_bark
  if ("DiameterSource" %in% names(pd)) {
    active_pd  <- pd[pd$ProductKey %in% ACTIVE_PRODUCT_KEYS, ]
    all_ok     <- all(active_pd$DiameterSource == EXPECTED_DIAMETER_SOURCE,
                      na.rm = TRUE)
    bad        <- unique(active_pd$DiameterSource[
      active_pd$DiameterSource != EXPECTED_DIAMETER_SOURCE])
    add_check(
      sprintf("DiameterSource == '%s' for all active products",
              EXPECTED_DIAMETER_SOURCE),
      if (all_ok) "PASS" else "FAIL",
      if (all_ok) sprintf("all '%s'", EXPECTED_DIAMETER_SOURCE)
      else sprintf("Unexpected: %s", paste(bad, collapse = ", "))
    )
  } else {
    add_check("DiameterSource column present", "REVIEW",
              "Column not found in ProductData")
  }

  do.call(rbind, checks)
}
