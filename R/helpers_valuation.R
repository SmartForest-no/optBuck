# ==============================================================================
# R/helpers_valuation.R
#
# Purpose : Price matrix loading, validation, and price-join key preparation
#           for the Apter bucking simulation.
#
# This helper covers:
#   - Loading PriceMatrix_long
#   - Querying which sawlog lengths are supported / unsupported
#   - Validating price matrix completeness against ProductData and LengthClasses
#   - Capping price-diameter classes at each product's matrix maximum for
#     price lookup (per product since Phase 4b)
#   - Preparing price-join key columns on a candidates data frame
#
# Scope limitations (intentional):
#   - Does NOT generate candidate logs.
#   - Does NOT run the DP optimizer.
#   - Does NOT apply bark correction.
#   - Does NOT modify PriceMatrix_long or LengthClasses.
#   - Missing prices are NEVER silently treated as zero.
#
# Accepted assumptions (see CLAUDE.md):
#   Price-diameter cap = largest class of the product's matrix (50 for the
#   default 2021 matrices)
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

#' Price-diameter cap (cm) of the default 2021 matrices, whose largest class
#' is 50. Since Phase 4b (2026-10-13) the cap is per product: the largest
#' class of the product's own matrix (build_price_lookup()); this constant
#' is the fallback for flat products and for cap_price_diameter_class().
#' Capping applies to price lookup only; the physical top diameter is NOT
#' changed.
DEFAULT_MAX_PRICE_DIAMETER_CLASS_CM <- 50L

#' Default price scenario name.
DEFAULT_PRICE_SCENARIO <- "default_2021"

#' Sawlog length classes supported in the default_2021 price matrix (dm).
#' Used as a reference for validation checks.
#' 31 dm was filled by interpolation (scripts/97_fill_31dm_sawlog_price_gap.R,
#' 2026-08-03) — rows are marked PriceSource == "Interpolated".
EXPECTED_SUPPORTED_SAWLOG_DM <- c(31L, 34L, 37L, 40L, 43L, 46L, 49L, 52L, 55L)


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
#' The 31 dm gap was resolved by interpolation on 2026-08-03 (see
#' scripts/97_fill_31dm_sawlog_price_gap.R) — it is kept here as a
#' defensive check in case a future price scenario reintroduces the gap,
#' in which case it will again be labelled and reported rather than erroring.
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

  # Work only on products with length-dependent (matrix) pricing: base
  # sawlogs and matrix-priced custom assortments (Phase 4b).
  sawlog_keys <- intersect(
    unique(lc$ProductKey),
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
      # The known gap is the base sawlogs' 31 dm class only.
      is_known_31dm_gap = (all_dm == 31L) & pk %in% c("spruce_sawlog", "pine_sawlog"),
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
#' Validation rules:
#'   - Technically eligible and priced  → OK
#'   - Technically eligible but unpriced → reported, not an error unless whole product missing
#'   - 31 dm sawlog gap                  → PASS as of 2026-08-03 (filled by interpolation);
#'                                          would report REVIEW again if ever reintroduced
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

  # Products by price type (ProductData$PriceType, Phase 4b; files without
  # the column: sawlogs are matrix-priced, the rest flat).
  pd_type <- if (is.null(pd)) character(0)
    else if ("PriceType" %in% names(pd)) as.character(pd$PriceType)
    else ifelse(pd$ProductType == "sawlog", "matrix", "flat")
  pd_keys <- if (is.null(pd)) character(0) else as.character(pd$ProductKey)
  pm_keys <- unique(pm$ProductKey)
  pm_matrix_keys <- unique(pm$ProductKey[!is.na(pm$LengthClass_dm)])
  pm_flat_keys   <- unique(pm$ProductKey[is.na(pm$LengthClass_dm)])

  # 1. All matrix-priced products have at least one priced length
  active_saw <- pd_keys[pd_type == "matrix"]
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

  # 2. Flat-priced products (pulpwood, waste, flat custom assortments) have
  #    a price row
  flat_keys <- pd_keys[pd_type == "flat"]
  for (pk in flat_keys) {
    in_pm <- pk %in% pm_keys
    has_price <- in_pm && any(!is.na(pm$PriceNOKperM3[pm$ProductKey == pk]))
    # Base products: REVIEW as before; a custom assortment without a price
    # could never be selected, so it FAILs (Phase 4b).
    is_base <- pk %in% c("spruce_pulpwood", "pine_pulpwood", "spruce_waste", "pine_waste")
    add_check(
      sprintf("%s has a price entry", pk),
      if (has_price) "PASS" else if (is_base) "REVIEW" else "FAIL",
      if (has_price) "price present"
      else if (!in_pm) "product absent from PriceMatrix_long"
      else "all PriceNOKperM3 are NA"
    )
  }

  # 2b. Each product is priced one way only, the way ProductData says.
  wrong_way <- c(intersect(pd_keys[pd_type == "matrix"], pm_flat_keys),
                 intersect(pd_keys[pd_type == "flat"], pm_matrix_keys))
  if (length(wrong_way) > 0L || any(!pd_type %in% c("matrix", "flat")))
    add_check(
      "Products priced as their PriceType says",
      "FAIL",
      sprintf("matrix product with a flat price, or flat product with matrix rows: %s",
              paste(unique(wrong_way), collapse = ", ")))

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

  # Collect missing products (in ProductData but absent from pm entirely)
  missing_products <- setdiff(pd_keys, pm_keys)

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
#'   - PriceDiameterClass_cm_used     = capped: per product at the largest
#'     class of its matrix when a price lookup is given (Phase 4b), else at
#'     one fixed cap
#'
#' @param candidates data.frame with ProductKey and PriceDiameterClass_cm.
#' @param max_price_diameter_class_cm A build_price_lookup() result (per-
#'   product caps; the backend) or a fixed cap in cm (default 50).
#' @return candidates with two new columns appended.
prepare_price_join_keys <- function(candidates,
                                    max_price_diameter_class_cm =
                                      DEFAULT_MAX_PRICE_DIAMETER_CLASS_CM) {
  if (!"PriceDiameterClass_cm" %in% names(candidates))
    stop("candidates must have a PriceDiameterClass_cm column")

  candidates$PriceDiameterClass_cm_uncapped <-
    as.integer(candidates$PriceDiameterClass_cm)

  candidates$PriceDiameterClass_cm_used <-
    if (is.list(max_price_diameter_class_cm))
      price_class_used(candidates, max_price_diameter_class_cm)
    else cap_price_diameter_class(candidates$PriceDiameterClass_cm,
                                  max_class_cm = max_price_diameter_class_cm)

  candidates
}


#' Price lookup tables for one PriceMatrix_long (built once per matrix).
#'
#' Matrix rows have a LengthClass_dm and are keyed by
#' "ProductKey|LengthClass_dm|PriceDiameterClass_cm"; flat rows (pulpwood,
#' waste, flat custom assortments) have no LengthClass_dm and are keyed by
#' ProductKey (first row wins if a key is duplicated).
#'
#' Per-product price-diameter cap (Phase 4b, 2026-10-13): a matrix product's
#' cap is the largest PriceDiameterClass_cm in its own matrix (50 for the
#' default 2021 matrices). Flat products keep DEFAULT_MAX_PRICE_DIAMETER_
#' CLASS_CM for the informational PriceDiameterClass_cm_used column; it does
#' not affect their price.
#'
#' @param pm PriceMatrix_long data.frame.
#' @return list(saw = named numeric, flat = named numeric, matrix_keys,
#'   max_class = named integer, ProductKey -> cap in cm).
build_price_lookup <- function(pm) {
  saw  <- pm[!is.na(pm$LengthClass_dm), , drop = FALSE]
  flat <- pm[is.na(pm$LengthClass_dm), , drop = FALSE]
  flat <- flat[!duplicated(flat$ProductKey), , drop = FALSE]
  saw_cls <- suppressWarnings(as.integer(saw$PriceDiameterClass_cm))
  max_class <- if (nrow(saw) > 0L)
    tapply(saw_cls, as.character(saw$ProductKey), max, na.rm = TRUE) else integer(0)
  list(
    saw  = setNames(as.numeric(saw$PriceNOKperM3),
                    paste(saw$ProductKey, saw$LengthClass_dm,
                          saw$PriceDiameterClass_cm, sep = "|")),
    flat = setNames(as.numeric(flat$PriceNOKperM3),
                    as.character(flat$ProductKey)),
    matrix_keys = unique(as.character(saw$ProductKey)),
    max_class   = setNames(as.integer(max_class), names(max_class)))
}

#' The price-diameter class used for lookup: the uncapped class, capped at
#' the product's largest matrix class (see build_price_lookup()). The
#' physical diameter is never changed.
#'
#' @param logs   data.frame with ProductKey and PriceDiameterClass_cm_uncapped
#'               (or PriceDiameterClass_cm).
#' @param lookup build_price_lookup() result.
#' @return Integer vector.
price_class_used <- function(logs, lookup) {
  col <- intersect(c("PriceDiameterClass_cm_uncapped", "PriceDiameterClass_cm",
                     "PriceDiameterClass_cm_used"), names(logs))
  if (length(col) == 0L) stop("logs need a PriceDiameterClass_cm column.")
  raw <- logs[[col[1L]]]
  cap <- unname(lookup$max_class[as.character(logs$ProductKey)])
  cap[is.na(cap)] <- DEFAULT_MAX_PRICE_DIAMETER_CLASS_CM
  pmin(as.integer(raw), as.integer(cap))
}

#' Price (NOK/m3) for each log or candidate. NA where the matrix has no price.
#'
#' The single price lookup used by the backend run and by scenario re-pricing
#' (helpers_comparison.R). A product is matrix-priced if the matrix has
#' length-class rows for it; its class is capped at that matrix's largest
#' class (price_class_used()), so re-pricing under another matrix uses that
#' matrix's cap.
#'
#' @param logs   data.frame with ProductKey, LengthClass_dm and
#'               PriceDiameterClass_cm_uncapped (or PriceDiameterClass_cm).
#' @param lookup build_price_lookup() result, or a PriceMatrix_long.
lookup_prices <- function(logs, lookup) {
  if (is.data.frame(lookup)) lookup <- build_price_lookup(lookup)
  price  <- rep(NA_real_, nrow(logs))
  is_saw <- as.character(logs$ProductKey) %in% lookup$matrix_keys
  if (any(is_saw)) {
    cls <- price_class_used(logs[is_saw, , drop = FALSE], lookup)
    price[is_saw] <- lookup$saw[paste(logs$ProductKey[is_saw],
                                      logs$LengthClass_dm[is_saw],
                                      cls, sep = "|")]
  }
  if (any(!is_saw))
    price[!is_saw] <- lookup$flat[as.character(logs$ProductKey[!is_saw])]
  unname(price)
}

#' Re-price logs under a price matrix: PriceNOKperM3 and value_nok replaced.
price_logs <- function(logs, pm) {
  logs$PriceNOKperM3 <- lookup_prices(logs, pm)
  logs$value_nok     <- logs$price_volume_m3 * logs$PriceNOKperM3
  logs
}
