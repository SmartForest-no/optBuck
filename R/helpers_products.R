# ==============================================================================
# R/helpers_products.R
#
# Purpose : Load and validate ProductData and LengthClasses for the optBuck
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




# ------------------------------------------------------------------------------
# Constants
# ------------------------------------------------------------------------------

# The six active ProductKey values recognised by optBuck.
ACTIVE_PRODUCT_KEYS <- c(
  "spruce_sawlog", "spruce_pulpwood", "spruce_waste",
  "pine_sawlog",   "pine_pulpwood",   "pine_waste"
)

# Expected DiameterUnderBark value for all active products.
EXPECTED_DIAMETER_UNDER_BARK <- TRUE

# Expected DiameterSource value for all active products.
EXPECTED_DIAMETER_SOURCE <- "taperNOR_over_bark"


# ------------------------------------------------------------------------------
# Loaders
# ------------------------------------------------------------------------------

# Load ProductData from the default CSV.
#
# @param root_dir   Project root (default: get_root_dir()).
# @param file_name  CSV file name under inputs/.
# @return data.frame with one row per product.
load_products <- function(
    root_dir  = get_root_dir(),
    file_name = "ProductData_spruce_pine_default.csv") {

  path <- path_input(file_name, root_dir = root_dir)
  if (!file.exists(path))
    stop("ProductData file not found: ", path)

  pd <- as.data.frame(
    readr::read_csv(path, show_col_types = FALSE))
  pd
}


# Load LengthClasses from the default CSV.
#
# LogLength values are stored in **centimetres** in the CSV.
# A convenience column LogLength_dm (= LogLength / 10) is added.
#
# @param root_dir   Project root (default: get_root_dir()).
# @param file_name  CSV file name under inputs/.
# @return data.frame with columns ProductKey, LogLength (cm), LogLength_dm.
load_length_classes <- function(
    root_dir  = get_root_dir(),
    file_name = "LengthClasses_spruce_pine_default.csv") {

  path <- path_input(file_name, root_dir = root_dir)
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

# Check that ProductData satisfies the accepted assumptions.
#
# Returns a data.frame with columns: check, result, detail.
# Results are "PASS", "REVIEW", or "FAIL".
#
# Checks performed:
#   1. All ACTIVE_PRODUCT_KEYS are present.
#   2. No extra ProductKey values beyond ACTIVE_PRODUCT_KEYS.
#   3. DiameterUnderBark equals EXPECTED_DIAMETER_UNDER_BARK (TRUE) for all
#      active products.
#   4. DiameterSource is "taperNOR_over_bark" for all active products (if column exists).
#
# @param pd data.frame from load_products().
# @return data.frame(check, result, detail).
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

# Supported species groups.
PRODUCT_SPECIES <- c("spruce", "pine")

# ProductData$PriceType values.
PRODUCT_PRICE_TYPES <- c("matrix", "flat")

# Where a custom assortment is used: the default run, the scenario run, or both.
ASSORTMENT_USE_IN <- c("both", "default", "scenario")

# ProductData columns a product definition must have.
PRODUCT_REQUIRED_COLS <- c(
  "ProductKey", "ProductName", "ProductType", "SpeciesGroupKey", "species_group",
  "VolumeDiameterCategory", "VolumeLengthCategory", "VolumeDiameterAdjustment",
  "DiameterTopPosition", "DiameterUnderBark", "MinTopDiameter", "MaxTopDiameter",
  "MinButtDiameter", "MaxButtDiameter", "MinLength", "MaxLength", "Priority",
  "PriceType")


# ProductKeys of one species group, in ProductData order.
#
# @param pd ProductData (base products plus any custom assortments).
# @param species_group "spruce" or "pine".
species_products <- function(pd, species_group) {
  as.character(pd$ProductKey[pd$species_group %in% species_group])
}


# Validate product definitions (base and custom) before a run.
#
# Errors block a run; warnings do not.
#
# @param pd ProductData.
# @param lc LengthClasses (ProductKey, LogLength in cm); NULL skips the
#   length checks.
# @return list(ok, errors, warnings).
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
  # (Phase 4d) A renamed base waste row keeps type "waste" under a new key;
  # custom assortments never get it (custom_type(); validate_assortment_defs()).
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


# A lower-case key part from an assortment name ("Large sawlogs" ->
# "large_sawlogs").
assortment_slug <- function(name) {
  s <- gsub("[^a-z0-9]+", "_", tolower(trimws(as.character(name))))
  gsub("^_+|_+$", "", s)
}

# Parse allowed lengths: "310;340, 370" or a numeric vector -> integer cm.
parse_lengths_cm <- function(x) {
  if (is.numeric(x)) return(as.integer(round(x)))
  v <- trimws(unlist(strsplit(as.character(x), "[;, ]+")))
  suppressWarnings(as.integer(v[nzchar(v)]))
}


# ProductData and LengthClasses rows for one custom assortment.
#
# The definition is entered in the app's units: diameters in cm, lengths
# and DiameterTopPosition in cm. It gives one product per species
# ("both" -> spruce and pine), ProductKey = "<species>_<slug of Name>".
#
# @param def One-row data.frame or list with Name, Class, Species
#   ("spruce", "pine" or "both"), MinTopDiameter_cm, MaxTopDiameter_cm,
#   MinButtDiameter_cm, MaxButtDiameter_cm (NA/Inf = no limit),
#   AllowedLengths_cm ("310;340" or numeric), Priority, DiameterUnderBark,
#   DiameterTopPosition_cm, VolumeDiameterCategory,
#   VolumeDiameterAdjustment, PriceType ("matrix"/"flat"), UseIn.
# @return list(pd = ProductData rows, lc = LengthClasses rows (with
#   LogLength_dm)).
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
    Priority                = suppressWarnings(as.numeric(g("Priority", 1))),
    PriceType                = as.character(g("PriceType", "matrix")),
    stringsAsFactors         = FALSE)))
  lc <- if (length(lens) == 0L)
    data.frame(ProductKey = character(0), LogLength = numeric(0), LogLength_dm = numeric(0))
  else do.call(rbind, lapply(pd$ProductKey, function(k)
    data.frame(ProductKey = k, LogLength = as.numeric(lens), LogLength_dm = lens / 10,
               stringsAsFactors = FALSE)))
  list(pd = pd, lc = lc)
}


# Product set and length classes for one run: base products plus the custom
# assortments whose UseIn includes the run ("default" or "scenario").
#
# @param base_pd,base_lc Base ProductData / LengthClasses.
# @param defs Custom assortment definitions (data.frame, one row each; NULL
#   or 0 rows for none).
# @param run "default" or "scenario".
# @return list(pd, lc).
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

# The base products with their limits before Phase 4c (no maximum top or
# butt diameter). Phase 4c set the default maxima from optBuck's
# example.hpr; the audits that compare against older baselines (20 check 8,
# 22 check 5) run with this, so that the limits are the only change.
pre_phase4c_limits <- function(pd) {
  base <- pd$ProductKey %in% ACTIVE_PRODUCT_KEYS
  pd$MaxTopDiameter[base]  <- Inf
  pd$MaxButtDiameter[base] <- Inf
  pd
}


# ------------------------------------------------------------------------------
# Assortment table (Phase 4c, 2026-10-13)
# ------------------------------------------------------------------------------
# The app edits all assortments, base and custom, in one table with one row
# per product (ProductKey = "<Species>_<slug of Name>", so "Sawlog" / spruce
# is spruce_sawlog). Units are the app's: cm. Hidden ProductData columns are
# generated: ProductName, VolumeLengthCategory (no effect in optBuck),
# DiameterUnderBark (always TRUE), DiameterSource and Priority (derived:
# matrix-priced 1, flat-priced commercial 2, waste 99). A matrix-priced
# product's lengths are the columns of the run's price matrix; a flat-priced
# product's are MinLength_cm..MaxLength_cm by 10 cm (Phase 4d: no step
# setting). Diameter rounding follows the volume method (volume_rounding()).
# Type (ProductType) is hidden: base rows keep theirs (a renamed base waste
# row stays waste); user-added rows are always commercial (custom_type()).
# custom_assortment_rows() / products_for_run() above are the Phase 4b
# definition format, kept for the performance cases and audit 20.
# PriceLengths_cm is hidden too (hpr import, 2026-10-20): the length-class
# lower limits (cm, comma-separated) of a matrix product imported from an
# .hpr file whose price volume uses "Length as defined in LengthClasses".
# Each limit belongs to the matrix column it rounds up to (365 -> 37 dm),
# and that column's price volume uses the limit, not the cut length
# (price_lengths()). Empty (NA) for every other assortment: price volume on
# the cut length, as before.
# PermittedGrades is hidden as well (Phase 6, 2026-10-28): the stem grades an
# .hpr product permits, ";"-separated ("1;2;3"). Empty (NA) = all grades (not
# from an .hpr file, or the file gives the product no permitted grades). Used
# only for .hpr stems with "Use stem grades" on.
# ReferenceGrades (2026-10-28, hidden too): the reference grade(s) of the
# product's species group in the .hpr file, in the same format. A stem without
# grade data (NFI, tree list) or with "Use stem grades" off has that grade in
# every cell, so such a stem can use the product only if its PermittedGrades
# meet it. Empty (NA) = no such filter.
# HprProductKey is hidden as well (.hpr export, 2026-10-28): the ProductKey of
# the .hpr product an imported assortment came from (NA otherwise). The export
# gives that product's metadata (buyer, organisation, ...) back to the
# assortment; it has no effect on runs. Kept on edits and in scenario files.

# The four distribution fields are hidden as well (Phase 7a, 2026-10-30): the
# product's LengthDistributionDefinition from an .hpr/.pin file
# (DistributionAllowed, DistributionCategory, DistributionWithinProduct,
# MAXDeviation), with the per-cell Distribution column of the price matrix
# (PM_DISTRIBUTION_COL). NA = the file has none (and every other assortment).
# Kept on edits and copies and written back by the .hpr/.pin export; not used
# by runs (products_from_defs() does not pass them on), not compared between
# A and B, and not in the scenario xlsx.

# The hidden distribution fields of the assortment table.
DISTRIBUTION_DEF_COLS <- c("DistributionAllowed", "DistributionCategory",
                           "DistributionWithinProduct", "MAXDeviation")

# Two more hidden fields keep what the StanForD export needs of an imported
# product (R4 D6): SourceXml, the source product's own elements that optBuck does
# not edit (ProductUserID, SpeciesGroupUserID, ModificationDate, ManualFreeBuck,
# TopLogFreeBuck, ProductCondition, CuttingWindowDefinition, LimitationDefinition, ...;
# R/helpers_hpr_export.R, source_xml_parts()), and SourceFingerprint, a hash of
# the product's imported price and Distribution cells (product_fingerprint()):
# a product whose current prices or targets differ gets a new ModificationDate
# on export. NA for every other assortment. Kept on edits, copies and in scenario
# files; never in run data, not compared between A and B.

# The hidden source fields of the assortment table.
SOURCE_DEF_COLS <- c("SourceXml", "SourceFingerprint", "SourceCells")   # SourceCells: cells of source diameter classes without a whole-cm row

# Where on the stem a product may be cut (StanForD ProductCondition; roadmap 8, 2026-11-04):
# "Buck butt-log only" = only as the log that starts at the first cut, "Do not buck butt log" =
# never as that log; NA ("Buck always") = anywhere. Shown read-only in the assortment table.
BUTT_CONDITIONS <- c("Buck butt-log only", "Do not buck butt log")
# Their display values in the assortment table (translated by tr_value()).
BUTT_POSITION_VALUES <- c("Butt log only", "Not butt log")

# Columns of the assortment table.
ASSORTMENT_TABLE_COLS <- c(
  "Name", "Type", "TypeUser", "Species", "MinTopDiameter_cm", "MaxTopDiameter_cm",
  "MinButtDiameter_cm", "MaxButtDiameter_cm", "MinLength_cm", "MaxLength_cm",
  "DiameterTopPosition_cm", "VolumeDiameterCategory", "PriceType", "PriceLengths_cm",
  "PermittedGrades", "ReferenceGrades", "ButtCondition", "HprProductKey", DISTRIBUTION_DEF_COLS, SOURCE_DEF_COLS)

# "TRUE"/"true"/TRUE -> TRUE, "FALSE"/... -> FALSE, anything else NA.
.dist_logical <- function(x) {
  v <- tolower(trimws(as.character(x)))
  ifelse(v %in% c("true", "1"), TRUE, ifelse(v %in% c("false", "0"), FALSE, NA))
}

# A permitted-grades string in its canonical form: sorted unique whole
# numbers joined by ";"; NA for empty, missing or unreadable.
norm_permitted_grades <- function(x) {
  vapply(as.character(x), function(s) {
    if (is.na(s) || !nzchar(trimws(s)) || s == "NA") return(NA_character_)
    g <- suppressWarnings(as.numeric(strsplit(trimws(s), "[,; ]+")[[1L]]))
    g <- sort(unique(g[!is.na(g)]))
    if (length(g) == 0L) NA_character_ else paste(format(g, scientific = FALSE, trim = TRUE), collapse = ";")
  }, "", USE.NAMES = FALSE)
}

# The grade numbers of a PermittedGrades string (numeric(0) = all grades).
permitted_grades_set <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[1L]) || !nzchar(trimws(x[1L]))) return(numeric(0))
  g <- suppressWarnings(as.numeric(strsplit(as.character(x[1L]), "[,; ]+")[[1L]]))
  sort(unique(g[!is.na(g)]))
}

# Price-volume lengths (cm) of the given log lengths (cm): the class lower
# limit in `limits` (a PriceLengths_cm string) whose whole-dm round-up is
# the log length, else the log length itself.
price_lengths <- function(limits, log_len) {
  lim <- if (is.null(limits) || length(limits) == 0L || is.na(limits) || !nzchar(trimws(limits)))
    numeric(0) else suppressWarnings(as.numeric(strsplit(as.character(limits), "[,; ]+")[[1L]]))
  lim <- lim[!is.na(lim)]
  i <- match(as.numeric(log_len), ceiling(lim / 10) * 10)
  ifelse(is.na(i), as.numeric(log_len), lim[i])
}

# Length step (cm) of flat-priced assortments (Phase 4d: fixed).
FLAT_LENGTH_STEP_CM <- 10

# The hidden Type of a user-added assortment: the slug of its name, never
# the reserved "waste" (user-added assortments are always commercial).
custom_type <- function(name) {
  s <- vapply(name, assortment_slug, character(1), USE.NAMES = FALSE)
  ifelse(s == "waste", "waste_commercial", s)
}

# The type of an assortment as the user sees it (R5): "sawlog", "pulpwood",
# "waste" (the reserved base waste rows) or "other" (any custom type).
type_choice <- function(type) {
  t <- as.character(type)
  ifelse(t %in% c("sawlog", "pulpwood", "waste"), t, "other")
}

# The Type value stored for the user's choice "sawlog" / "pulpwood" / "other"
# (R5). "other" is the slug of the name, kept apart from the reserved types.
type_from_choice <- function(choice, name) {
  if (choice %in% c("sawlog", "pulpwood")) return(choice)
  s <- custom_type(name)
  if (s %in% c("sawlog", "pulpwood", "waste")) paste0(s, "_other") else s
}

# The assortment table with row `key`'s type set by the user (R5): Type
# becomes the choice and TypeUser TRUE. Waste rows (reserved) and unknown
# keys are left alone.
set_assortment_type <- function(defs, key, choice) {
  i <- match(key, assortment_table_keys(defs))
  if (is.na(i) || identical(as.character(defs$Type[i]), "waste") ||
      !choice %in% c("sawlog", "pulpwood", "other")) return(defs)
  defs$Type[i] <- type_from_choice(choice, defs$Name[i]); defs$TypeUser[i] <- TRUE
  defs
}

# Priority in ties, derived from pricing and type.
derived_priority <- function(price_type, product_type) {
  ifelse(as.character(product_type) == "waste", 99,
         ifelse(as.character(price_type) == "matrix", 1, 2))
}

# ProductKeys of the assortment table's rows.
assortment_table_keys <- function(defs) {
  if (is.null(defs) || nrow(defs) == 0L) return(character(0))
  paste0(as.character(defs$Species), "_", vapply(defs$Name, assortment_slug, character(1),
                                                  USE.NAMES = FALSE))
}

# Column types of an assortment table.
.type_assortment_table <- function(d) {
  num <- setdiff(ASSORTMENT_TABLE_COLS, c("Name", "Type", "TypeUser", "Species", "VolumeDiameterCategory",
                                          "PriceType", "PriceLengths_cm", "PermittedGrades",
                                          "ReferenceGrades", "ButtCondition", "HprProductKey",
                                          "DistributionAllowed", "DistributionCategory",
                                          "DistributionWithinProduct", SOURCE_DEF_COLS))
  # Files without the hidden Type: rows are user assortments (commercial).
  if (!"Type" %in% names(d)) d$Type <- rep(NA_character_, nrow(d))
  # TypeUser (R5): TRUE when the user chose the Type; files without it: automatic.
  if (!"TypeUser" %in% names(d)) d$TypeUser <- rep(FALSE, nrow(d))
  d$TypeUser <- .dist_logical(d$TypeUser); d$TypeUser[is.na(d$TypeUser)] <- FALSE
  # Tables without the hidden price lengths (all but hpr imports): none.
  if (!"PriceLengths_cm" %in% names(d)) d$PriceLengths_cm <- rep(NA_character_, nrow(d))
  pl <- trimws(as.character(d$PriceLengths_cm))
  d$PriceLengths_cm <- ifelse(is.na(pl) | !nzchar(pl) | pl == "NA", NA_character_, pl)
  # Tables without the hidden permitted grades (all but hpr imports): all grades.
  if (!"PermittedGrades" %in% names(d)) d$PermittedGrades <- rep(NA_character_, nrow(d))
  d$PermittedGrades <- norm_permitted_grades(d$PermittedGrades)
  if (!"ReferenceGrades" %in% names(d)) d$ReferenceGrades <- rep(NA_character_, nrow(d))
  d$ReferenceGrades <- norm_permitted_grades(d$ReferenceGrades)
  # Tables without the butt condition (all but hpr imports): anywhere on the stem.
  if (!"ButtCondition" %in% names(d)) d$ButtCondition <- rep(NA_character_, nrow(d))
  bc <- trimws(as.character(d$ButtCondition)); d$ButtCondition <- ifelse(bc %in% BUTT_CONDITIONS, bc, NA_character_)
  # The same for the hidden .hpr ProductKey (hpr imports; used by the .hpr export).
  if (!"HprProductKey" %in% names(d)) d$HprProductKey <- rep(NA_character_, nrow(d))
  hk <- trimws(as.character(d$HprProductKey))
  d$HprProductKey <- ifelse(is.na(hk) | !nzchar(hk) | hk == "NA", NA_character_, hk)
  # The hidden distribution fields (hpr/pin imports): NA otherwise.
  for (n in DISTRIBUTION_DEF_COLS) if (!n %in% names(d)) d[[n]] <- rep(NA, nrow(d))
  d$DistributionAllowed <- .dist_logical(d$DistributionAllowed)
  d$DistributionWithinProduct <- .dist_logical(d$DistributionWithinProduct)
  dc <- trimws(as.character(d$DistributionCategory))
  d$DistributionCategory <- ifelse(is.na(dc) | !nzchar(dc) | dc == "NA", NA_character_, dc)
  # The hidden source fields (hpr/pin imports): NA otherwise.
  for (n in SOURCE_DEF_COLS) {
    if (!n %in% names(d)) d[[n]] <- rep(NA_character_, nrow(d))
    v <- as.character(d[[n]]); d[[n]] <- ifelse(is.na(v) | !nzchar(trimws(v)) | v == "NA", NA_character_, v)
  }
  no_type <- is.na(d$Type) | !nzchar(trimws(as.character(d$Type)))
  d$Type[no_type] <- custom_type(d$Name[no_type])
  for (n in num) d[[n]] <- suppressWarnings(as.numeric(d[[n]]))
  for (n in setdiff(ASSORTMENT_TABLE_COLS, c(num, "DistributionAllowed", "DistributionWithinProduct", "TypeUser")))
    d[[n]] <- as.character(d[[n]])
  d <- d[, ASSORTMENT_TABLE_COLS, drop = FALSE]
  rownames(d) <- NULL
  d
}

# The assortment table of a ProductData (e.g. the base products).
defs_from_products <- function(pd) {
  sp <- as.character(pd$species_group)
  nm <- sub(sprintf("^(%s) ", paste(PRODUCT_SPECIES, collapse = "|")), "",
            as.character(pd$ProductName), ignore.case = TRUE)
  nm <- paste0(toupper(substring(nm, 1, 1)), substring(nm, 2))
  .type_assortment_table(data.frame(
    Name = nm, Type = pd$ProductType, Species = sp,
    MinTopDiameter_cm = pd$MinTopDiameter / 10, MaxTopDiameter_cm = pd$MaxTopDiameter / 10,
    MinButtDiameter_cm = pd$MinButtDiameter / 10, MaxButtDiameter_cm = pd$MaxButtDiameter / 10,
    MinLength_cm = pd$MinLength, MaxLength_cm = pd$MaxLength,
    DiameterTopPosition_cm = pd$DiameterTopPosition,
    VolumeDiameterCategory = pd$VolumeDiameterCategory,
    PriceType = pd$PriceType,
    PermittedGrades = if ("PermittedGrades" %in% names(pd)) pd$PermittedGrades else NA_character_,
    ReferenceGrades = if ("ReferenceGrades" %in% names(pd)) pd$ReferenceGrades else NA_character_,
    ButtCondition = if ("ButtCondition" %in% names(pd)) pd$ButtCondition else NA_character_,
    stringsAsFactors = FALSE))
}

# Allowed lengths (cm) of each table row: the matrix columns in `pm` for
# matrix-priced rows, MinLength..MaxLength by 10 cm for flat ones.
#
# @return Named list (ProductKey -> integer cm, ascending).
assortment_lengths <- function(defs, pm) {
  keys <- assortment_table_keys(defs)
  setNames(lapply(seq_len(nrow(defs)), function(i) {
    if (identical(defs$PriceType[i], "matrix")) {
      dm <- pm$LengthClass_dm[pm$ProductKey == keys[i] & !is.na(pm$LengthClass_dm)]
      return(sort(unique(as.numeric(dm) * 10)))
    }
    lo <- defs$MinLength_cm[i]; hi <- defs$MaxLength_cm[i]
    if (anyNA(c(lo, hi)) || hi < lo) return(numeric(0))
    seq(lo, hi, by = FLAT_LENGTH_STEP_CM)
  }), keys)
}

# ProductData and LengthClasses of an assortment table.
#
# @param defs Assortment table (ASSORTMENT_TABLE_COLS), in product order.
# @param pm   The run's price matrix (minimal long); gives the lengths of
#   matrix-priced products.
# @return list(pd, lc (with LogLength_dm)).
products_from_defs <- function(defs, pm) {
  keys <- assortment_table_keys(defs)
  lens <- assortment_lengths(defs, pm)
  sp   <- as.character(defs$Species)
  rng  <- function(f, empty) vapply(lens, function(l) if (length(l)) f(l) else empty, numeric(1),
                                     USE.NAMES = FALSE)
  pd <- data.frame(
    ProductKey               = keys,
    ProductName              = paste0(toupper(substring(sp, 1, 1)), substring(sp, 2), " ",
                                      tolower(trimws(defs$Name))),
    ProductType              = trimws(defs$Type),
    SpeciesGroupKey          = sp,
    species_group            = sp,
    VolumeDiameterCategory   = defs$VolumeDiameterCategory,
    VolumeLengthCategory     = ifelse(defs$PriceType == "matrix",
                                      "Length as defined in LengthClasses", "Physical length cm"),
    VolumeDiameterAdjustment = volume_rounding(defs$VolumeDiameterCategory),
    DiameterTopPosition      = as.numeric(defs$DiameterTopPosition_cm),
    DiameterUnderBark        = rep(EXPECTED_DIAMETER_UNDER_BARK, nrow(defs)),
    DiameterSource           = rep(EXPECTED_DIAMETER_SOURCE, nrow(defs)),
    MinTopDiameter           = as.numeric(defs$MinTopDiameter_cm) * 10,
    MaxTopDiameter           = as.numeric(defs$MaxTopDiameter_cm) * 10,
    MinButtDiameter          = as.numeric(defs$MinButtDiameter_cm) * 10,
    MaxButtDiameter          = as.numeric(defs$MaxButtDiameter_cm) * 10,
    MinLength                = rng(min, NA_real_),
    MaxLength                = rng(max, NA_real_),
    LengthStep               = vapply(lens, function(l) if (length(l) > 1L) min(diff(l)) else 10,
                                      numeric(1), USE.NAMES = FALSE),
    Priority                 = derived_priority(defs$PriceType, defs$Type),
    PriceType                = defs$PriceType,
    stringsAsFactors = FALSE)
  lc <- data.frame(ProductKey = rep(keys, lengths(lens)),
                   LogLength = as.numeric(unlist(lens, use.names = FALSE)),
                   stringsAsFactors = FALSE)
  lc$LogLength_dm <- lc$LogLength / 10
  # Price-volume lengths (hpr imports only): the column is added only when
  # some row has them, so other tables give the same LengthClasses as before.
  pl <- if ("PriceLengths_cm" %in% names(defs)) defs$PriceLengths_cm else rep(NA, nrow(defs))
  if (any(!is.na(pl) & defs$PriceType == "matrix"))
    lc$PriceLength_cm <- unlist(lapply(seq_along(lens), function(i)
      if (identical(defs$PriceType[i], "matrix")) price_lengths(pl[i], lens[[i]])
      else as.numeric(lens[[i]])), use.names = FALSE)
  # Permitted grades (hpr imports only): the column is added only when some row
  # has them, so other tables give the same ProductData as before.
  pg <- if ("PermittedGrades" %in% names(defs)) defs$PermittedGrades else rep(NA, nrow(defs))
  if (any(!is.na(pg))) pd$PermittedGrades <- as.character(pg)
  rg <- if ("ReferenceGrades" %in% names(defs)) defs$ReferenceGrades else rep(NA, nrow(defs))
  if (any(!is.na(rg))) pd$ReferenceGrades <- as.character(rg)
  # Butt condition (hpr imports only): the column is added only when some row has one.
  bcn <- if ("ButtCondition" %in% names(defs)) defs$ButtCondition else rep(NA, nrow(defs))
  if (any(!is.na(bcn))) pd$ButtCondition <- as.character(bcn)
  list(pd = pd, lc = lc)
}

# Check an assortment table (names, species, lengths) and the products it
# gives (validate_products()). `pm` gives matrix-priced lengths.
#
# @return list(ok, errors, warnings).
validate_assortment_table <- function(defs, pm) {
  err <- character(0)
  miss <- setdiff(ASSORTMENT_TABLE_COLS, names(defs))
  if (length(miss) > 0L)
    return(list(ok = FALSE, warnings = character(0),
                errors = paste("Assortment table missing column(s):", paste(miss, collapse = ", "))))
  if (nrow(defs) == 0L)
    return(list(ok = FALSE, warnings = character(0), errors = "There are no assortments."))
  slug <- vapply(defs$Name, assortment_slug, character(1), USE.NAMES = FALSE)
  if (any(!nzchar(slug))) err <- c(err, "Every assortment needs a name with letters or digits.")
  bad_sp <- !defs$Species %in% PRODUCT_SPECIES
  if (any(bad_sp)) err <- c(err, sprintf("Species must be spruce or pine: %s",
                                         paste(defs$Name[bad_sp], collapse = ", ")))
  keys <- assortment_table_keys(defs)
  if (any(duplicated(keys)))
    err <- c(err, paste("Two assortments with the same name and species:",
                        paste(unique(keys[duplicated(keys)]), collapse = ", ")))
  lens <- assortment_lengths(defs, pm)
  none <- lengths(lens) == 0L
  if (any(none))
    err <- c(err, sprintf("No lengths (a flat price needs min length <= max length; a grid needs length columns): %s",
                          paste(keys[none], collapse = ", ")))
  if (length(err) > 0L) return(list(ok = FALSE, errors = err, warnings = character(0)))
  p <- products_from_defs(defs, pm)
  v <- validate_products(p$pd, p$lc)
  list(ok = v$ok, errors = v$errors, warnings = v$warnings)
}

# rbind that fills columns missing on either side with NA.
.bind_rows_fill <- function(a, b) {
  for (n in setdiff(names(b), names(a))) a[[n]] <- rep(NA, nrow(a))
  for (n in setdiff(names(a), names(b))) b[[n]] <- rep(NA, nrow(b))
  rbind(a, b[, names(a), drop = FALSE])
}
