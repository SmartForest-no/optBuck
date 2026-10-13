# ==============================================================================
# R/helpers_products.R
#
# Purpose : Load and validate ProductData and LengthClasses for the Apter
#           bucking simulation.
#
# Accepted assumptions (see CLAUDE.md):
#   DiameterUnderBark = TRUE for all active products (2026-10-12: limits,
#     price class and price volume use under-bark diameters; profiles stay
#     over bark and are converted with the bark model in helpers_bark.R)
#   DiameterSource    = taperNOR_over_bark for all active products
#   Six active ProductKey values (ACTIVE_PRODUCT_KEYS constant below): the
#     base products. Custom assortments (Phase 4b, 2026-10-13) are extra
#     ProductData rows built with custom_assortment_rows() and checked with
#     validate_products(); which products a species gets comes from
#     ProductData itself (species_products()).
#   PriceType: "matrix" (length x diameter-class grid) or "flat" (one price).
#   VolumeDiameterCategory: one of VOLUME_METHODS (helpers_volume_methods.R).
#
# Usage   : source("R/helpers_paths.R")
#           source("R/helpers_products.R")
#           pd <- load_products()
#           lc <- load_length_classes()
#           chk <- check_product_assumptions(pd)
# ==============================================================================

# Define %||% locally in case helpers_audit.R is not yet sourced.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}

if (!exists("get_root_dir", mode = "function"))
  source(file.path(dirname(sys.frame(1L)$ofile %||% "."), "helpers_paths.R"))
if (!exists("VOLUME_METHODS"))
  source(file.path(dirname(sys.frame(1L)$ofile %||% "."), "helpers_volume_methods.R"))


# ------------------------------------------------------------------------------
# Constants
# ------------------------------------------------------------------------------

#' The six active ProductKey values recognised by Apter.
ACTIVE_PRODUCT_KEYS <- c(
  "spruce_sawlog", "spruce_pulpwood", "spruce_waste",
  "pine_sawlog",   "pine_pulpwood",   "pine_waste"
)

#' Expected DiameterUnderBark value for all active products.
EXPECTED_DIAMETER_UNDER_BARK <- TRUE

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
#'   3. DiameterUnderBark equals EXPECTED_DIAMETER_UNDER_BARK (TRUE) for all
#'      active products.
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

  # 2. Extra keys are custom assortments; they must pass validate_products().
  extra <- setdiff(pd$ProductKey, ACTIVE_PRODUCT_KEYS)
  vp    <- if (length(extra) > 0L) validate_products(pd) else NULL
  add_check(
    "No unexpected ProductKey values in ProductData",
    if (length(extra) == 0L || vp$ok) "PASS" else "FAIL",
    if (length(extra) == 0L) "none"
    else if (vp$ok) paste("Custom assortments:", paste(extra, collapse = ", "))
    else paste("Invalid product definitions:", paste(vp$errors, collapse = "; "))
  )

  # 3. DiameterUnderBark == EXPECTED_DIAMETER_UNDER_BARK (TRUE)
  if ("DiameterUnderBark" %in% names(pd)) {
    active_pd  <- pd[pd$ProductKey %in% ACTIVE_PRODUCT_KEYS, ]
    dub        <- as.logical(active_pd$DiameterUnderBark)
    all_ok     <- all(dub %in% EXPECTED_DIAMETER_UNDER_BARK)
    add_check(
      sprintf("DiameterUnderBark == %s for all active products",
              EXPECTED_DIAMETER_UNDER_BARK),
      if (all_ok) "PASS" else "FAIL",
      if (all_ok) sprintf("all %s", EXPECTED_DIAMETER_UNDER_BARK)
      else sprintf("Unexpected values: %s",
                   paste(unique(active_pd$DiameterUnderBark[
                     !dub %in% EXPECTED_DIAMETER_UNDER_BARK]),
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


# ------------------------------------------------------------------------------
# Species products and custom assortments (Phase 4b, 2026-10-13)
# ------------------------------------------------------------------------------

#' Supported species groups.
PRODUCT_SPECIES <- c("spruce", "pine")

#' ProductData$PriceType values.
PRODUCT_PRICE_TYPES <- c("matrix", "flat")

#' Where a custom assortment is used: the default run, the scenario run, or both.
ASSORTMENT_USE_IN <- c("both", "default", "scenario")

#' ProductData columns a product definition must have.
PRODUCT_REQUIRED_COLS <- c(
  "ProductKey", "ProductName", "ProductType", "SpeciesGroupKey", "species_group",
  "VolumeDiameterCategory", "VolumeLengthCategory", "VolumeDiameterAdjustment",
  "DiameterTopPosition", "DiameterUnderBark", "MinTopDiameter", "MaxTopDiameter",
  "MinButtDiameter", "MaxButtDiameter", "MinLength", "MaxLength", "Priority",
  "PriceType")


#' ProductKeys of one species group, in ProductData order.
#'
#' @param pd ProductData (base products plus any custom assortments).
#' @param species_group "spruce" or "pine".
species_products <- function(pd, species_group) {
  as.character(pd$ProductKey[pd$species_group %in% species_group])
}


#' Validate product definitions (base and custom) before a run.
#'
#' Errors block a run; warnings do not.
#'
#' @param pd ProductData.
#' @param lc LengthClasses (ProductKey, LogLength in cm); NULL skips the
#'   length checks.
#' @return list(ok, errors, warnings).
validate_products <- function(pd, lc = NULL) {
  err <- character(0); warn <- character(0)
  miss <- setdiff(PRODUCT_REQUIRED_COLS, names(pd))
  if (length(miss) > 0L)
    return(list(ok = FALSE, warnings = warn,
                errors = paste("ProductData missing column(s):", paste(miss, collapse = ", "))))
  key <- as.character(pd$ProductKey)
  bad <- function(cond, msg) {
    cond[is.na(cond)] <- TRUE
    if (any(cond)) err <<- c(err, sprintf("%s: %s", msg, paste(key[cond], collapse = ", ")))
  }
  num <- function(col) suppressWarnings(as.numeric(pd[[col]]))

  if (any(is.na(key) | !nzchar(key))) err <- c(err, "Empty ProductKey.")
  if (any(duplicated(key)))
    err <- c(err, paste("Duplicated ProductKey:", paste(unique(key[duplicated(key)]), collapse = ", ")))
  bad(!pd$species_group %in% PRODUCT_SPECIES,
      sprintf("species_group must be one of %s", paste(PRODUCT_SPECIES, collapse = "/")))
  bad(as.character(pd$SpeciesGroupKey) != as.character(pd$species_group),
      "SpeciesGroupKey differs from species_group")
  bad(is.na(pd$ProductType) | !nzchar(trimws(as.character(pd$ProductType))),
      "ProductType (assortment class) is empty")
  bad(!key %in% ACTIVE_PRODUCT_KEYS & as.character(pd$ProductType) == "waste",
      "ProductType 'waste' is reserved for the base waste products")
  bad(!pd$VolumeDiameterCategory %in% VOLUME_METHODS,
      sprintf("VolumeDiameterCategory must be one of: %s", paste(VOLUME_METHODS, collapse = "; ")))
  adj <- ifelse(is.na(pd$VolumeDiameterAdjustment), "None", as.character(pd$VolumeDiameterAdjustment))
  bad(!adj %in% VOLUME_DIAMETER_ADJUSTMENTS,
      sprintf("VolumeDiameterAdjustment must be one of: %s",
              paste(VOLUME_DIAMETER_ADJUSTMENTS, collapse = "; ")))
  bad(pd$VolumeDiameterCategory == "All diameters (solid volume)" & adj == VOLUME_ROUND_DOWN_CM,
      "solid volume is never rounded; use VolumeDiameterAdjustment = None")
  bad(!pd$PriceType %in% PRODUCT_PRICE_TYPES, "PriceType must be 'matrix' or 'flat'")
  bad(is.na(as.logical(pd$DiameterUnderBark)), "DiameterUnderBark must be TRUE or FALSE")
  bad(is.na(num("Priority")), "Priority must be numeric")

  for (p in c("Top", "Butt")) {
    lo <- num(paste0("Min", p, "Diameter")); hi <- num(paste0("Max", p, "Diameter"))
    bad(is.na(lo) | lo < 0, sprintf("Min%sDiameter must be >= 0", p))
    bad(is.na(hi) | hi < lo, sprintf("Max%sDiameter must be >= Min%sDiameter", p, p))
  }
  dtp <- num("DiameterTopPosition")
  bad(is.na(dtp) | dtp < 0 | dtp %% 10 != 0,
      "DiameterTopPosition must be a non-negative multiple of 10 cm")

  if (!is.null(lc)) {
    for (i in seq_along(key)) {
      len <- suppressWarnings(as.numeric(lc$LogLength[lc$ProductKey == key[i]]))
      if (length(len) == 0L) {
        err <- c(err, sprintf("%s: no length classes in LengthClasses", key[i])); next
      }
      if (any(is.na(len) | len <= 0 | len %% 10 != 0))
        err <- c(err, sprintf("%s: lengths must be positive multiples of 10 cm", key[i]))
      else if (!is.na(dtp[i]) && dtp[i] >= min(len))
        err <- c(err, sprintf("%s: DiameterTopPosition (%g cm) must be shorter than the shortest log (%g cm)",
                              key[i], dtp[i], min(len)))
      if (any(duplicated(len)))
        err <- c(err, sprintf("%s: duplicated length classes", key[i]))
    }
    orphan <- setdiff(unique(lc$ProductKey), key)
    if (length(orphan) > 0L)
      warn <- c(warn, paste("LengthClasses for unknown products (ignored):",
                            paste(orphan, collapse = ", ")))
  }
  list(ok = length(err) == 0L, errors = err, warnings = warn)
}


#' A lower-case key part from an assortment name ("Large sawlogs" ->
#' "large_sawlogs").
assortment_slug <- function(name) {
  s <- gsub("[^a-z0-9]+", "_", tolower(trimws(as.character(name))))
  gsub("^_+|_+$", "", s)
}

#' Parse allowed lengths: "310;340, 370" or a numeric vector -> integer cm.
parse_lengths_cm <- function(x) {
  if (is.numeric(x)) return(as.integer(round(x)))
  v <- trimws(unlist(strsplit(as.character(x), "[;, ]+")))
  suppressWarnings(as.integer(v[nzchar(v)]))
}


#' ProductData and LengthClasses rows for one custom assortment.
#'
#' The definition is entered in the app's units: diameters in cm, lengths
#' and DiameterTopPosition in cm. It gives one product per species
#' ("both" -> spruce and pine), ProductKey = "<species>_<slug of Name>".
#'
#' @param def One-row data.frame or list with Name, Class, Species
#'   ("spruce", "pine" or "both"), MinTopDiameter_cm, MaxTopDiameter_cm,
#'   MinButtDiameter_cm, MaxButtDiameter_cm (NA/Inf = no limit),
#'   AllowedLengths_cm ("310;340" or numeric), Priority, DiameterUnderBark,
#'   DiameterTopPosition_cm, VolumeDiameterCategory,
#'   VolumeDiameterAdjustment, PriceType ("matrix"/"flat"), UseIn.
#' @return list(pd = ProductData rows, lc = LengthClasses rows (with
#'   LogLength_dm)).
custom_assortment_rows <- function(def) {
  def <- as.list(def)
  g <- function(n, default = NA) {
    v <- def[[n]]
    if (is.null(v) || length(v) == 0L || (length(v) == 1L && is.na(v))) default else v[[1L]]
  }
  mm <- function(cm, default) {
    v <- suppressWarnings(as.numeric(g(cm, NA)))
    if (is.na(v)) default else v * 10
  }
  sp <- as.character(g("Species", "both"))
  species <- if (identical(sp, "both")) PRODUCT_SPECIES else sp
  slug <- assortment_slug(g("Name", ""))
  lens <- sort(unique(parse_lengths_cm(g("AllowedLengths_cm", ""))))
  pd <- do.call(rbind, lapply(species, function(s) data.frame(
    ProductKey               = paste0(s, "_", slug),
    ProductName              = paste0(toupper(substring(s, 1, 1)), substring(s, 2), " ",
                                      tolower(trimws(as.character(g("Name", ""))))),
    ProductType              = trimws(as.character(g("Class", slug))),
    SpeciesGroupKey          = s,
    species_group            = s,
    VolumeDiameterCategory   = as.character(g("VolumeDiameterCategory", "Top cylinder")),
    VolumeLengthCategory     = "Physical length cm",
    VolumeDiameterAdjustment = as.character(g("VolumeDiameterAdjustment", "None")),
    DiameterTopPosition      = suppressWarnings(as.numeric(g("DiameterTopPosition_cm", 0))),
    DiameterUnderBark        = as.logical(g("DiameterUnderBark", TRUE)),
    DiameterSource           = EXPECTED_DIAMETER_SOURCE,
    MinTopDiameter           = mm("MinTopDiameter_cm", 0),
    MaxTopDiameter           = mm("MaxTopDiameter_cm", Inf),
    MinButtDiameter          = mm("MinButtDiameter_cm", 0),
    MaxButtDiameter          = mm("MaxButtDiameter_cm", Inf),
    MinLength                = if (length(lens)) min(lens) else NA_real_,
    MaxLength                = if (length(lens)) max(lens) else NA_real_,
    LengthStep               = if (length(lens) > 1L) min(diff(lens)) else 10,
    AllowAsFirstLog          = TRUE,
    AllowAsTopLog            = TRUE,
    Priority                 = suppressWarnings(as.numeric(g("Priority", 1))),
    PriceType                = as.character(g("PriceType", "matrix")),
    stringsAsFactors         = FALSE)))
  lc <- if (length(lens) == 0L)
    data.frame(ProductKey = character(0), LogLength = numeric(0), LogLength_dm = numeric(0))
  else do.call(rbind, lapply(pd$ProductKey, function(k)
    data.frame(ProductKey = k, LogLength = as.numeric(lens), LogLength_dm = lens / 10,
               stringsAsFactors = FALSE)))
  list(pd = pd, lc = lc)
}


#' Product set and length classes for one run: base products plus the custom
#' assortments whose UseIn includes the run ("default" or "scenario").
#'
#' @param base_pd,base_lc Base ProductData / LengthClasses.
#' @param defs Custom assortment definitions (data.frame, one row each; NULL
#'   or 0 rows for none).
#' @param run "default" or "scenario".
#' @return list(pd, lc).
products_for_run <- function(base_pd, base_lc, defs, run = c("default", "scenario")) {
  run <- match.arg(run)
  pd <- base_pd; lc <- base_lc
  if (!is.null(defs) && nrow(defs) > 0L) {
    use <- defs[defs$UseIn %in% c("both", run), , drop = FALSE]
    for (i in seq_len(nrow(use))) {
      r  <- custom_assortment_rows(use[i, , drop = FALSE])
      pd <- .bind_rows_fill(pd, r$pd)
      lc <- .bind_rows_fill(lc, r$lc)
    }
  }
  list(pd = pd, lc = lc)
}

# rbind that fills columns missing on either side with NA.
.bind_rows_fill <- function(a, b) {
  for (n in setdiff(names(b), names(a))) a[[n]] <- rep(NA, nrow(a))
  for (n in setdiff(names(a), names(b))) b[[n]] <- rep(NA, nrow(b))
  rbind(a, b[, names(a), drop = FALSE])
}
