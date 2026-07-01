# ==============================================================================
# R/helpers_valuation.R
#
# Purpose : Price matrix loading, validation, and price-join key preparation
#           for the nfiBuck bucking simulation.
#
# This helper covers:
#   - Loading PriceMatrix_long
#   - Querying which sawlog lengths are supported / unsupported
#   - Validating price matrix completeness against ProductData and LengthClasses
#   - Capping price-diameter classes at the matrix maximum for price lookup
#   - Preparing price-join key columns on a candidates data frame
#
# Scope limitations (intentional):
#   - Does NOT generate candidate logs.
#   - Does NOT run the DP optimizer.
#   - Does NOT apply bark correction.
#   - Does NOT modify PriceMatrix_long or LengthClasses.
#   - Missing prices are NEVER silently treated as zero.
#
# Accepted assumptions (from docs/PROJECT_STATUS.md):
#   MAX_PRICE_DIAMETER_CLASS_CM = 50
#   PriceDiameterClass_cm_uncapped stores the original (unmodified) value.
#   PriceDiameterClass_cm_used   stores the capped value used for price lookup.
#
# Usage   : source("R/helpers_paths.R")
#           source("R/helpers_valuation.R")
#           pm  <- load_price_matrix_long()
#           sup <- get_supported_sawlog_lengths(pm)
#           uns <- get_unsupported_sawlog_lengths(lc, pm)
#           val <- validate_price_matrix_against_products(pd, lc, pm)
# ==============================================================================

if (!exists("get_root_dir", mode = "function"))
  source(file.path(dirname(sys.frame(1L)$ofile %||% "."), "helpers_paths.R"))

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}

#' Default maximum price-diameter class (cm) used for price lookup.
#' Candidates with PriceDiameterClass_cm above this value are capped for
#' price lookup only. The physical top diameter is NOT changed.
DEFAULT_MAX_PRICE_DIAMETER_CLASS_CM <- 50L

#' Default price scenario name.
DEFAULT_PRICE_SCENARIO <- "default_2021"

#' Sawlog length classes supported in the default_2021 price matrix (dm).
#' Used as a reference for validation checks.
EXPECTED_SUPPORTED_SAWLOG_DM <- c(34L, 37L, 40L, 43L, 46L, 49L, 52L, 55L)


# ------------------------------------------------------------------------------
# Loader
# ------------------------------------------------------------------------------

#' Load PriceMatrix_long from the canonical CSV under inputs/price_matrices/.
#'
#' @param root_dir   Project root (default: get_root_dir()).
#' @param scenario   Price scenario name (used to filter PriceScenario column).
#'                   Pass NULL to return all scenarios.
#' @param file_name  CSV file name (default: PriceMatrix_long_default_2021.csv).
#' @return data.frame — all rows matching the scenario (or all rows if NULL).
load_price_matrix_long <- function(
    root_dir   = get_root_dir(),
    scenario   = DEFAULT_PRICE_SCENARIO,
    file_name  = "PriceMatrix_long_default_2021.csv") {

  path <- file.path(root_dir, "inputs", "price_matrices", file_name)
  if (!file.exists(path))
    stop("PriceMatrix_long file not found: ", path)

  pm <- as.data.frame(readr::read_csv(path, show_col_types = FALSE))

  if (!is.null(scenario) && "PriceScenario" %in% names(pm)) {
    pm <- pm[!is.na(pm$PriceScenario) & pm$PriceScenario == scenario, ]
    if (nrow(pm) == 0L)
      warning(sprintf("No rows matching PriceScenario == '%s'", scenario))
  }
  pm
}


# ------------------------------------------------------------------------------
# Length-support queries
# ------------------------------------------------------------------------------

#' Return a data.frame of ProductKey + LengthClass_dm that have a price entry.
#'
#' Only rows with a non-NA PriceNOKperM3 and a non-NA LengthClass_dm are
#' considered supported (sawlog / length-dependent products).
#' Flat-priced products (pulpwood, waste) have NA in LengthClass_dm and are
#' returned with LengthClass_dm = NA_integer_ and a supported = TRUE flag
#' because they do not depend on length matching.
#'
#' @param pm data.frame from load_price_matrix_long().
#' @return data.frame(ProductKey, LengthClass_dm, price_type, supported).
get_supported_sawlog_lengths <- function(pm) {
  required_cols <- c("ProductKey", "LengthClass_dm", "PriceNOKperM3")
  missing <- setdiff(required_cols, names(pm))
  if (length(missing) > 0L)
    stop("PriceMatrix_long missing columns: ", paste(missing, collapse = ", "))

  # Sawlog / matrix-priced rows: LengthClass_dm is not NA
  sawlog_rows <- pm[!is.na(pm$LengthClass_dm), ]

  if (nrow(sawlog_rows) == 0L) {
    return(data.frame(ProductKey = character(0), LengthClass_dm = integer(0),
                      price_type = character(0), n_diameter_classes = integer(0),
                      stringsAsFactors = FALSE))
  }

  # Summarise: for each ProductKey x LengthClass_dm, count non-NA price rows
  result <- do.call(rbind, lapply(
    split(sawlog_rows, list(sawlog_rows$ProductKey, sawlog_rows$LengthClass_dm),
          drop = TRUE),
    function(d) {
      n_priced <- sum(!is.na(d$PriceNOKperM3))
      data.frame(
        ProductKey         = d$ProductKey[1L],
        LengthClass_dm     = as.integer(d$LengthClass_dm[1L]),
        price_type         = if ("PriceType" %in% names(d)) d$PriceType[1L] else "matrix",
        n_diameter_classes = n_priced,
        stringsAsFactors   = FALSE
      )
    }))

  result[order(result$ProductKey, result$LengthClass_dm), ]
}


#' Find sawlog length classes in LengthClasses that have no entry in PriceMatrix.
#'
#' LengthClasses stores lengths in **centimetres** (LogLength column).
#' PriceMatrix_long stores lengths in **decimetres** (LengthClass_dm column).
#' This function converts LengthClasses to dm before comparing.
#'
#' The 31 dm gap is expected: sawlog price matrices start at 34 dm under
#' default_2021. This function identifies and labels it so callers can
#' report it as a known issue rather than an error.
#'
#' @param lc data.frame from load_length_classes().
#' @param pm data.frame from load_price_matrix_long().
#' @return data.frame(ProductKey, LogLength_dm, in_length_classes,
#'                    in_price_matrix, is_known_31dm_gap).
get_unsupported_sawlog_lengths <- function(lc, pm) {
  if (!"LogLength" %in% names(lc))
    stop("LengthClasses must have a LogLength column (values in cm).")
  if (!"LengthClass_dm" %in% names(pm))
    stop("PriceMatrix_long must have a LengthClass_dm column.")

  # Work only on sawlog products that have length-dependent pricing
  sawlog_keys <- intersect(
    unique(lc$ProductKey[grepl("sawlog", lc$ProductKey)]),
    unique(pm$ProductKey[!is.na(pm$LengthClass_dm)]))

  if (length(sawlog_keys) == 0L) {
    return(data.frame(ProductKey = character(0), LogLength_dm = integer(0),
                      in_length_classes = logical(0), in_price_matrix = logical(0),
                      is_known_31dm_gap = logical(0), stringsAsFactors = FALSE))
  }

  rows <- lapply(sawlog_keys, function(pk) {
    # All lengths defined for this product (in dm)
    lc_dm  <- unique(as.integer(round(
      lc$LogLength[lc$ProductKey == pk] / 10)))
    # All lengths priced for this product (in dm)
    pm_dm  <- unique(as.integer(pm$LengthClass_dm[
      pm$ProductKey == pk & !is.na(pm$LengthClass_dm)]))

    # Union of both sets
    all_dm <- sort(unique(c(lc_dm, pm_dm)))

    data.frame(
      ProductKey        = pk,
      LogLength_dm      = all_dm,
      in_length_classes = all_dm %in% lc_dm,
      in_price_matrix   = all_dm %in% pm_dm,
      is_known_31dm_gap = (all_dm == 31L),
      stringsAsFactors  = FALSE
    )
  })

  do.call(rbind, rows)
}


# ------------------------------------------------------------------------------
# Full validation
# ------------------------------------------------------------------------------

#' Validate PriceMatrix_long completeness against ProductData and LengthClasses.
#'
#' Returns a list with:
#'   $checks       data.frame(check, result, detail)
#'   $supported    output of get_supported_sawlog_lengths()
#'   $unsupported  output of get_unsupported_sawlog_lengths()  (in lc but not pm)
#'   $missing_products  ProductKey values in ProductData but absent from pm
#'
#' Validation rules (from docs/APP_DESIGN_NOTES.md):
#'   - Technically eligible and priced  → OK
#'   - Technically eligible but unpriced → reported, not an error unless whole product missing
#'   - 31 dm sawlog gap                  → REVIEW (known issue)
#'   - Whole product missing from pm     → FAIL
#'   - Negative PriceNOKperM3           → FAIL
#'   - PriceNOKperM3 == 0               → REVIEW (warn; might be intentional)
#'
#' @param pd  data.frame from load_products().
#' @param lc  data.frame from load_length_classes().
#' @param pm  data.frame from load_price_matrix_long().
#' @return List with $checks, $supported, $unsupported, $missing_products.
validate_price_matrix_against_products <- function(pd, lc, pm) {
  checks <- list()

  add_check <- function(check, result, detail = "") {
    checks[[length(checks) + 1L]] <<- data.frame(
      check = check, result = result, detail = detail,
      stringsAsFactors = FALSE)
  }

  supported   <- get_supported_sawlog_lengths(pm)
  unsup_all   <- get_unsupported_sawlog_lengths(lc, pm)
  unsupported <- unsup_all[unsup_all$in_length_classes & !unsup_all$in_price_matrix, ]

  # 1. All active sawlog products have at least one priced length
  active_saw <- c("spruce_sawlog", "pine_sawlog")
  for (pk in active_saw) {
    priced_lengths <- supported$LengthClass_dm[supported$ProductKey == pk]
    add_check(
      sprintf("%s has priced length classes", pk),
      if (length(priced_lengths) > 0L) "PASS" else "FAIL",
      if (length(priced_lengths) > 0L)
        sprintf("%d priced lengths: %s dm",
                length(priced_lengths),
                paste(sort(priced_lengths), collapse = ", "))
      else "No priced lengths found"
    )
  }

  # 2. Flat-priced products (pulpwood, waste) have a price row
  flat_keys <- c("spruce_pulpwood", "pine_pulpwood", "spruce_waste", "pine_waste")
  pm_keys   <- unique(pm$ProductKey)
  for (pk in flat_keys) {
    in_pm <- pk %in% pm_keys
    has_price <- in_pm && any(!is.na(pm$PriceNOKperM3[pm$ProductKey == pk]))
    add_check(
      sprintf("%s has a price entry", pk),
      if (has_price) "PASS" else "REVIEW",
      if (has_price) "price present"
      else if (!in_pm) "product absent from PriceMatrix_long"
      else "all PriceNOKperM3 are NA"
    )
  }

  # 3. No negative prices
  neg_rows <- pm[!is.na(pm$PriceNOKperM3) & pm$PriceNOKperM3 < 0, ]
  add_check(
    "No negative PriceNOKperM3 values",
    if (nrow(neg_rows) == 0L) "PASS" else "FAIL",
    sprintf("%d negative price row(s)", nrow(neg_rows))
  )

  # 4. Zero prices — warn but do not fail (may be intentional waste price)
  zero_rows <- pm[!is.na(pm$PriceNOKperM3) & pm$PriceNOKperM3 == 0, ]
  add_check(
    "No zero PriceNOKperM3 values",
    if (nrow(zero_rows) == 0L) "PASS" else "REVIEW",
    sprintf("%d zero-price row(s) — check if intentional", nrow(zero_rows))
  )

  # 5. 31 dm sawlog gap
  gap_31 <- unsupported[unsupported$is_known_31dm_gap, ]
  add_check(
    "31 dm sawlog gap in price matrix",
    if (nrow(gap_31) > 0L) "REVIEW" else "PASS",
    if (nrow(gap_31) > 0L)
      sprintf("KNOWN: %s: 31 dm in LengthClasses but absent from PriceMatrix_long (matrix starts at 34 dm)",
              paste(gap_31$ProductKey, collapse = ", "))
    else "31 dm not in LengthClasses or is priced"
  )

  # 6. Unexpected unpriced sawlog lengths (not the 31 dm gap)
  unexpected_unpriced <- unsupported[!unsupported$is_known_31dm_gap, ]
  add_check(
    "No unexpected unpriced sawlog lengths",
    if (nrow(unexpected_unpriced) == 0L) "PASS" else "FAIL",
    sprintf("%d unexpected unpriced sawlog length(s)%s",
            nrow(unexpected_unpriced),
            if (nrow(unexpected_unpriced) > 0L)
              paste0(": ", paste(
                paste0(unexpected_unpriced$ProductKey, "@",
                       unexpected_unpriced$LogLength_dm, "dm"),
                collapse = ", "))
            else "")
  )

  # Collect missing products (active but absent from pm entirely)
  missing_products <- setdiff(
    if (!is.null(pd)) pd$ProductKey[pd$ProductKey %in%
      c("spruce_sawlog","spruce_pulpwood","spruce_waste",
        "pine_sawlog","pine_pulpwood","pine_waste")] else character(0),
    pm_keys)

  list(
    checks           = do.call(rbind, checks),
    supported        = supported,
    unsupported      = unsupported,
    missing_products = missing_products
  )
}


# ------------------------------------------------------------------------------
# Diameter capping helpers
# ------------------------------------------------------------------------------

#' Cap price-diameter class values at a maximum for price lookup.
#'
#' This function implements the accepted protocol: logs with a top-diameter
#' class above the matrix maximum are priced as if they were at the maximum.
#' The physical diameter is never changed; the cap applies only to the price
#' join key.
#'
#' @param x           Integer or numeric vector of PriceDiameterClass_cm values.
#' @param max_class_cm Maximum allowed value (default: 50).
#' @return Integer vector with values capped at max_class_cm.
cap_price_diameter_class <- function(x, max_class_cm = DEFAULT_MAX_PRICE_DIAMETER_CLASS_CM) {
  max_class_cm <- as.integer(max_class_cm)
  pmin(as.integer(x), max_class_cm)
}


#' Add PriceDiameterClass_cm_uncapped and PriceDiameterClass_cm_used to a
#' candidates data frame, ready for price join.
#'
#' Precondition : candidates must contain a PriceDiameterClass_cm column.
#'
#' Postcondition:
#'   - PriceDiameterClass_cm_uncapped = original PriceDiameterClass_cm (unchanged)
#'   - PriceDiameterClass_cm_used     = capped at max_price_diameter_class_cm
#'
#' @param candidates              data.frame with a PriceDiameterClass_cm column.
#' @param max_price_diameter_class_cm  Cap value in cm (default: 50).
#' @return candidates with two new columns appended.
prepare_price_join_keys <- function(candidates,
                                    max_price_diameter_class_cm =
                                      DEFAULT_MAX_PRICE_DIAMETER_CLASS_CM) {
  if (!"PriceDiameterClass_cm" %in% names(candidates))
    stop("candidates must have a PriceDiameterClass_cm column")

  candidates$PriceDiameterClass_cm_uncapped <-
    as.integer(candidates$PriceDiameterClass_cm)

  candidates$PriceDiameterClass_cm_used <-
    cap_price_diameter_class(candidates$PriceDiameterClass_cm,
                             max_class_cm = max_price_diameter_class_cm)

  candidates
}
