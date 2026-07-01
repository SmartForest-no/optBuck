# ==============================================================================
# 03_define_products_and_volume.R
#
# Purpose : Define product/assortment rules, allowed length classes, and
#           volume-calculation functions for nfiBuck, following optBuck and
#           StanForD naming conventions where possible.  Test volume
#           calculations on a small set of example logs from the taper-based
#           stem profiles produced by 02_generate_stem_profiles.R.
#
#           No price matrices.  No bucking optimization.  No NOK values.
#
# Inputs  :
#   outputs/stem_profiles/stem_profiles_example_500trees.parquet
#   outputs/stem_profiles/trees_taper_input_example_500trees.parquet  (optional)
#
# Outputs :
#   inputs/ProductData_spruce_pine_default.csv / .rds
#   inputs/LengthClasses_spruce_pine_default.csv / .rds
#   outputs/product_volume_tests/test_log_volume_results.csv / .rds
#   outputs/product_volume_tests/candidate_logs_one_tree.csv / .rds
#   outputs/product_volume_tests/plot_*.png
#   outputs/product_volume_tests/product_volume_test_summary.txt
#
# Unit conventions (optBuck-style):
#   positions / lengths  : cm
#   diameters            : mm
#   volume               : m3
#   stem-profile interval: 10 cm
#
# Current diameter convention:
# The stem profile diameters used in this script are produced by taperNOR and are over bark.
# For this first version, all ProductData rows use DiameterUnderBark = FALSE.
# Bark correction is deliberately not implemented in this script.
# If under-bark diameters are needed later, generate them consistently in
# 02_generate_stem_profiles.R, for example by storing a separate under-bark
# diameter profile from taperNOR.
# ==============================================================================


# ---- setup ----

library(dplyr)
library(arrow)
library(tibble)
library(tidyr)
library(purrr)
library(readr)
library(ggplot2)

root_dir      <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
inputs_dir    <- file.path(root_dir, "inputs")
pv_tests_dir  <- file.path(root_dir, "outputs", "product_volume_tests")

dir.create(inputs_dir,   showWarnings = FALSE, recursive = TRUE)
dir.create(pv_tests_dir, showWarnings = FALSE, recursive = TRUE)

log_file <- file.path(pv_tests_dir, "product_volume_test_summary.txt")
cat("", file = log_file)

profile_input_file <- file.path(
  root_dir, "outputs", "stem_profiles",
  "stem_profiles_example_500trees.parquet"
)
trees_input_file <- file.path(
  root_dir, "outputs", "stem_profiles",
  "trees_taper_input_example_500trees.parquet"
)

PROFILE_STEP_CM <- 10L   # stem-profile interval (cm)


# ---- helper functions ----

wlog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = log_file, append = TRUE)
}

save_plot <- function(p, filename, width = 16, height = 10,
                      units = "cm", dpi = 150) {
  path <- file.path(pv_tests_dir, filename)
  ggplot2::ggsave(path, plot = p, width = width, height = height,
                  units = units, dpi = dpi)
  wlog(sprintf("  Plot saved: %s", basename(path)))
}


# ---- read stem profiles ----

wlog(
  "=============================================================",
  "  Product and volume definitions – test log",
  sprintf("  Run   : %s", Sys.time()),
  sprintf("  Input : %s", profile_input_file),
  "============================================================="
)

if (!file.exists(profile_input_file)) {
  stop("Stem profile file not found:\n  ", profile_input_file,
       "\nRun 02_generate_stem_profiles.R first.")
}

stem_profiles_raw <- arrow::read_parquet(profile_input_file)

# Check required columns.
required_profile_cols <- c("treid", "plot_id", "species_group",
                           "dbh_cm", "height_m", "z_m", "diameter_cm")
missing_cols <- setdiff(required_profile_cols, names(stem_profiles_raw))
if (length(missing_cols) > 0) {
  stop("Required columns missing from stem profile file: ",
       paste(missing_cols, collapse = ", "))
}

# Add helper columns if not already present.
stem_profiles <- stem_profiles_raw |>
  dplyr::mutate(
    diameter_mm      = if ("diameter_mm" %in% names(stem_profiles_raw))
                         diameter_mm else diameter_cm * 10,
    rel_height       = if ("rel_height" %in% names(stem_profiles_raw))
                         rel_height else z_m / height_m,
    # optBuck-style columns: positions in cm, diameters in mm.
    diameterPosition = as.integer(round(z_m * 100 / PROFILE_STEP_CM) * PROFILE_STEP_CM),
    DiameterValue    = diameter_mm,
    DBH              = dbh_cm * 10
  )

# Deduplicate: one row per treid + diameterPosition (keep first).
n_before_dedup <- nrow(stem_profiles)
stem_profiles  <- stem_profiles |>
  dplyr::distinct(treid, diameterPosition, .keep_all = TRUE)
n_after_dedup  <- nrow(stem_profiles)
n_dupes        <- n_before_dedup - n_after_dedup

if (n_dupes > 0) {
  wlog(sprintf(
    "  WARNING: %d duplicate (treid, diameterPosition) rows removed; kept first.",
    n_dupes
  ))
}

n_profile_rows  <- nrow(stem_profiles)
n_profile_trees <- dplyr::n_distinct(stem_profiles$treid)

wlog(
  "",
  "--- Stem profile input ---",
  sprintf("  Profile rows   : %d", n_profile_rows),
  sprintf("  Unique trees   : %d", n_profile_trees),
  sprintf("  Species groups : %s",
          paste(unique(stem_profiles$species_group), collapse = ", "))
)

if (n_profile_rows == 0) stop("No profile rows available.")


# ---- define ProductData ----

# Temporary default product rules for testing product and volume logic.
# These are not final operational assortment definitions.
# DiameterSource = "taperNOR_over_bark" for all products: profiles are over bark
# from taperNOR.  DiameterUnderBark = FALSE throughout.
ProductData <- tibble::tribble(
  ~ProductKey,       ~ProductName,         ~ProductType, ~SpeciesGroupKey, ~species_group,
  ~VolumeDiameterCategory,                    ~VolumeLengthCategory,
  ~VolumeDiameterAdjustment,
  ~DiameterTopPosition, ~DiameterUnderBark, ~DiameterSource,
  ~MinTopDiameter, ~MaxTopDiameter, ~MinButtDiameter, ~MaxButtDiameter,
  ~MinLength, ~MaxLength, ~LengthStep,
  ~AllowAsFirstLog, ~AllowAsTopLog, ~Priority,

  # --- spruce sawlog ---
  "spruce_sawlog",  "Spruce sawlog",  "sawlog",   "spruce", "spruce",
  "Top",                              "Length as defined in LengthClasses",
  "Measured diameter rounded downwards to cm",
  0L, FALSE, "taperNOR_over_bark",
  120, Inf, 0, Inf,
  310L, 550L, 30L,
  TRUE, TRUE, 1L,

  # --- spruce pulpwood ---
  "spruce_pulpwood", "Spruce pulpwood", "pulpwood", "spruce", "spruce",
  "All diameters (solid volume)",     "Physical length cm",
  "None",
  0L, FALSE, "taperNOR_over_bark",
  50, Inf, 0, Inf,
  300L, 550L, 10L,
  TRUE, TRUE, 2L,

  # --- pine sawlog ---
  "pine_sawlog",    "Pine sawlog",    "sawlog",   "pine",   "pine",
  "Top",                              "Length as defined in LengthClasses",
  "Measured diameter rounded downwards to cm",
  0L, FALSE, "taperNOR_over_bark",
  120, Inf, 0, Inf,
  310L, 550L, 30L,
  TRUE, TRUE, 1L,

  # --- pine pulpwood ---
  "pine_pulpwood",  "Pine pulpwood",  "pulpwood", "pine",   "pine",
  "All diameters (solid volume)",     "Physical length cm",
  "None",
  0L, FALSE, "taperNOR_over_bark",
  50, Inf, 0, Inf,
  300L, 550L, 10L,
  TRUE, TRUE, 2L,

  # --- spruce waste ---
  "spruce_waste",   "Spruce waste",   "waste",    "spruce", "spruce",
  "All diameters (solid volume)",     "Physical length cm",
  "None",
  0L, FALSE, "taperNOR_over_bark",
  0, Inf, 0, Inf,
  10L, 600L, 10L,
  TRUE, TRUE, 99L,

  # --- pine waste ---
  "pine_waste",     "Pine waste",     "waste",    "pine",   "pine",
  "All diameters (solid volume)",     "Physical length cm",
  "None",
  0L, FALSE, "taperNOR_over_bark",
  0, Inf, 0, Inf,
  10L, 600L, 10L,
  TRUE, TRUE, 99L
)

wlog("", "--- ProductData ---")
wlog(capture.output(print(as.data.frame(
  ProductData[, c("ProductKey", "ProductType", "species_group",
                  "DiameterSource", "DiameterUnderBark",
                  "MinTopDiameter", "MinLength", "MaxLength", "LengthStep",
                  "Priority")]
))))


# ---- define LengthClasses ----

LengthClasses <- list(
  spruce_sawlog   = seq(310L, 550L, by = 30L),
  spruce_pulpwood = seq(300L, 550L, by = 10L),
  pine_sawlog     = seq(310L, 550L, by = 30L),
  pine_pulpwood   = seq(300L, 550L, by = 10L),
  spruce_waste    = seq(10L,  600L, by = 10L),
  pine_waste      = seq(10L,  600L, by = 10L)
)

wlog("", "--- LengthClasses summary ---")
for (pk in names(LengthClasses)) {
  lc <- LengthClasses[[pk]]
  wlog(sprintf("  %-18s: %d classes, range %d – %d cm",
               pk, length(lc), min(lc), max(lc)))
}


# ---- validation functions ----

#' Validate ProductData table and LengthClasses list.
#' Stops on critical errors; writes warnings for less critical issues.
check_ProductData <- function(pd, lc) {

  required_cols <- c(
    "ProductKey", "ProductName", "ProductType", "SpeciesGroupKey",
    "species_group", "VolumeDiameterCategory", "VolumeLengthCategory",
    "VolumeDiameterAdjustment", "DiameterTopPosition", "DiameterUnderBark",
    "DiameterSource",
    "MinTopDiameter", "MaxTopDiameter", "MinButtDiameter", "MaxButtDiameter",
    "MinLength", "MaxLength", "LengthStep",
    "AllowAsFirstLog", "AllowAsTopLog", "Priority"
  )
  missing_req <- setdiff(required_cols, names(pd))
  if (length(missing_req) > 0)
    stop("ProductData is missing required columns: ",
         paste(missing_req, collapse = ", "))

  if (anyDuplicated(pd$ProductKey))
    stop("ProductData has duplicate ProductKey values: ",
         paste(pd$ProductKey[duplicated(pd$ProductKey)], collapse = ", "))

  unmatched <- setdiff(pd$ProductKey, names(lc))
  if (length(unmatched) > 0)
    stop("ProductKey values with no matching LengthClasses entry: ",
         paste(unmatched, collapse = ", "))

  if (any(is.na(pd$SpeciesGroupKey)))
    stop("Some products have missing SpeciesGroupKey.")
  if (any(is.na(pd$VolumeDiameterCategory)))
    stop("Some products have missing VolumeDiameterCategory.")
  if (any(is.na(pd$VolumeLengthCategory)))
    stop("Some products have missing VolumeLengthCategory.")

  if (any(pd$DiameterUnderBark))
    stop(paste(
      "This script currently supports only DiameterUnderBark = FALSE.",
      "Generate under-bark profiles in 02_generate_stem_profiles.R",
      "before using under-bark product definitions."
    ))

  supported_vol_dia <- c("All diameters (solid volume)", "Top")
  bad_vol_dia <- unique(pd$VolumeDiameterCategory[
    !pd$VolumeDiameterCategory %in% supported_vol_dia
  ])
  if (length(bad_vol_dia) > 0)
    stop("Unsupported VolumeDiameterCategory value(s): ",
         paste(bad_vol_dia, collapse = ", "),
         ". Supported: ", paste(supported_vol_dia, collapse = ", "))

  supported_vol_len <- c("Physical length cm",
                         "Length as defined in LengthClasses",
                         "Rounded downwards to nearest dm-module")
  bad_vol_len <- unique(pd$VolumeLengthCategory[
    !pd$VolumeLengthCategory %in% supported_vol_len
  ])
  if (length(bad_vol_len) > 0)
    stop("Unsupported VolumeLengthCategory value(s): ",
         paste(bad_vol_len, collapse = ", "),
         ". Supported: ", paste(supported_vol_len, collapse = ", "))

  for (i in seq_len(nrow(pd))) {
    row <- pd[i, ]
    if (row$MinLength >= row$MaxLength)
      stop(sprintf("Product '%s': MinLength (%d) >= MaxLength (%d).",
                   row$ProductKey, row$MinLength, row$MaxLength))
    if (row$MinTopDiameter >= row$MaxTopDiameter)
      stop(sprintf("Product '%s': MinTopDiameter >= MaxTopDiameter.",
                   row$ProductKey))
    # Length classes within MinLength / MaxLength
    lc_vec <- lc[[row$ProductKey]]
    out_of_range <- lc_vec[lc_vec < row$MinLength | lc_vec > row$MaxLength]
    if (length(out_of_range) > 0)
      warning(sprintf(
        "Product '%s': %d length class(es) outside [MinLength, MaxLength]: %s",
        row$ProductKey, length(out_of_range),
        paste(out_of_range, collapse = ", ")
      ))
    # All length classes must be multiples of PROFILE_STEP_CM (10 cm)
    non_multiples <- lc_vec[lc_vec %% PROFILE_STEP_CM != 0]
    if (length(non_multiples) > 0)
      warning(sprintf(
        "Product '%s': %d length class(es) not multiples of %d cm: %s",
        row$ProductKey, length(non_multiples), PROFILE_STEP_CM,
        paste(non_multiples, collapse = ", ")
      ))
  }

  invisible(TRUE)
}

validation_ok <- tryCatch({
  check_ProductData(ProductData, LengthClasses)
  wlog("", "  ProductData validation: PASSED")
  TRUE
}, error = function(e) {
  stop("ProductData validation failed: ", conditionMessage(e))
}, warning = function(w) {
  wlog(sprintf("  ProductData validation WARNING: %s", conditionMessage(w)))
  TRUE
})


# ---- profile helper functions ----

#' Extract taper profile vectors for one tree from the stem profile table.
#'
#' @param profiles  Stem profile data frame (already contains diameterPosition
#'                  and DiameterValue columns).
#' @param tree_id   Value of `treid` to select.
#' @return Named list: diameterPosition (cm), DiameterValue (mm), DBH (mm),
#'         height_m, height_cm, species_group, SpeciesGroupKey.
get_tree_profile_vectors <- function(profiles, tree_id) {
  tree_rows <- profiles |>
    dplyr::filter(treid == tree_id) |>
    dplyr::arrange(diameterPosition)

  if (nrow(tree_rows) == 0)
    stop(sprintf("No profile rows found for treid = '%s'.", tree_id))

  list(
    diameterPosition = tree_rows$diameterPosition,
    DiameterValue    = tree_rows$DiameterValue,
    DBH              = tree_rows$DBH[1],
    height_m         = tree_rows$height_m[1],
    height_cm        = round(tree_rows$height_m[1] * 100),
    species_group    = tree_rows$species_group[1],
    SpeciesGroupKey  = tree_rows$species_group[1],
    plot_id          = tree_rows$plot_id[1]
  )
}

#' Interpolate diameter (mm) at a given position (cm) on the stem.
#'
#' Uses exact match first; falls back to linear interpolation via approx().
#'
#' @param diameterPosition  Numeric vector of positions (cm).
#' @param DiameterValue     Numeric vector of diameters (mm).
#' @param pos               Position at which to predict diameter (cm).
#' @return Diameter in mm (numeric scalar).
get_diameter_at_position <- function(diameterPosition, DiameterValue, pos) {
  # Exact match
  idx <- which(diameterPosition == pos)
  if (length(idx) > 0) return(DiameterValue[idx[1]])

  # Linear interpolation
  result <- stats::approx(x = diameterPosition, y = DiameterValue,
                          xout = pos, rule = 2)$y
  result
}


# ---- VolumeCalc_nfi ----

#' Solid-volume calculation for a log section (optBuck-style, simplified).
#'
#' Transparent numerical stem-profile volume approximation for testing.
#' Not yet a final implementation of any formal measurement rule.
#'
#' Builds a position grid from StartPos to StopPos (including both endpoints
#' and all 10 cm profile positions in between), interpolates diameters at each
#' position, and sums section volumes.  For each section the average of the two
#' endpoint diameters is used as the section diameter:
#'   radius_mm = mean(d_start_mm, d_end_mm) / 2
#'   v_section = pi * radius_mm^2 * length_cm / 1e8   (m3)
#'
#' @param diameterPosition  Numeric vector of profile positions (cm).
#' @param DiameterValue     Numeric vector of diameters (mm).
#' @param StartPos          Start position of the log (cm).
#' @param StopPos           End position of the log (cm).
#' @param DiameterUnderBark Logical.  Must be FALSE in this script.
#' @return Solid volume in m3.
VolumeCalc_nfi <- function(diameterPosition, DiameterValue,
                           StartPos, StopPos,
                           DiameterUnderBark = FALSE) {
  if (DiameterUnderBark)
    stop(paste(
      "DiameterUnderBark = TRUE is not supported in this simplified script.",
      "Generate under-bark stem profiles in 02_generate_stem_profiles.R if needed."
    ))

  # Build position grid: all 10 cm profile positions between StartPos and StopPos,
  # plus StartPos and StopPos themselves.
  pos_seq <- seq(
    ceiling(StartPos  / PROFILE_STEP_CM) * PROFILE_STEP_CM,
    floor(StopPos     / PROFILE_STEP_CM) * PROFILE_STEP_CM,
    by = PROFILE_STEP_CM
  )
  all_pos <- sort(unique(c(StartPos, pos_seq, StopPos)))
  all_pos <- all_pos[all_pos >= StartPos & all_pos <= StopPos]

  if (length(all_pos) < 2) return(NA_real_)

  # Interpolate diameters (mm) at all positions.
  d_mm <- vapply(all_pos, function(p)
    get_diameter_at_position(diameterPosition, DiameterValue, p),
    FUN.VALUE = numeric(1)
  )

  # Sum section volumes.
  # For each section: use average of the two endpoint diameters as section diameter.
  #   radius_mm = mean(d_mm[k], d_mm[k+1]) / 2
  #   v_section = pi * radius_mm^2 * length_cm / 1e8   (m3)
  # This is a transparent numerical approximation for testing.
  v_total <- 0
  for (k in seq_len(length(all_pos) - 1L)) {
    radius_mm <- mean(d_mm[c(k, k + 1L)]) / 2
    len_cm    <- all_pos[k + 1L] - all_pos[k]
    v_total   <- v_total + pi * radius_mm^2 * len_cm / 1e8
  }
  v_total
}


# ---- PriceVolumeCalc_nfi ----

#' Calculate price volume for a log (optBuck PriceVolumeCalc-style, simplified).
#'
#' Supports VolumeDiameterCategory: "All diameters (solid volume)" and "Top".
#' Supports VolumeLengthCategory:   "Physical length cm" and
#'                                  "Length as defined in LengthClasses".
#'
#' NOTE: TopDiameter volume uses a cylinder formula based on the Norwegian
#' top-measured timber rule.  Verify against the exact rule before production use.
#'
#' @param diameterPosition      Profile positions (cm).
#' @param DiameterValue         Profile diameters (mm).
#' @param StartPos              Log start position (cm).
#' @param StopPos               Log stop position (cm, before length adjustment).
#' @param ProductRow            One-row tibble from ProductData.
#' @param LengthClassesVec      Allowed lengths vector (cm) for this product.
#' @return List: price_volume_m3, EffectiveLogLength, EffectiveStopPos.
PriceVolumeCalc_nfi <- function(diameterPosition, DiameterValue,
                                StartPos, StopPos,
                                ProductRow, LengthClassesVec) {

  if (isTRUE(ProductRow$DiameterUnderBark))
    stop(paste(
      "DiameterUnderBark = TRUE is not supported in this simplified script.",
      "Generate under-bark stem profiles in 02_generate_stem_profiles.R if needed."
    ))

  raw_length <- StopPos - StartPos

  # --- Resolve effective log length ---
  vol_len_cat <- ProductRow$VolumeLengthCategory

  if (vol_len_cat == "Physical length cm") {
    eff_length  <- raw_length
    eff_stoppos <- StopPos

  } else if (vol_len_cat == "Length as defined in LengthClasses") {
    # Round down to largest allowed length class <= raw_length.
    valid_lc <- LengthClassesVec[LengthClassesVec <= raw_length]
    if (length(valid_lc) == 0) {
      return(list(price_volume_m3   = NA_real_,
                  EffectiveLogLength = NA_real_,
                  EffectiveStopPos   = NA_real_))
    }
    eff_length  <- max(valid_lc)
    eff_stoppos <- StartPos + eff_length

  } else if (vol_len_cat == "Rounded downwards to nearest dm-module") {
    eff_length  <- floor(raw_length / 10) * 10
    eff_stoppos <- StartPos + eff_length

  } else {
    stop(sprintf("Unsupported VolumeLengthCategory: '%s'", vol_len_cat))
  }

  # --- Calculate volume ---
  vol_dia_cat <- ProductRow$VolumeDiameterCategory

  if (vol_dia_cat == "All diameters (solid volume)") {
    v <- VolumeCalc_nfi(diameterPosition, DiameterValue,
                        StartPos, eff_stoppos,
                        DiameterUnderBark = FALSE)

  } else if (vol_dia_cat == "Top") {
    # Top-measured volume: cylinder using diameter at StopPos - DiameterTopPosition.
    top_pos <- eff_stoppos - ProductRow$DiameterTopPosition
    top_d   <- get_diameter_at_position(diameterPosition, DiameterValue, top_pos)

    # Apply rounding adjustment if required.
    if (ProductRow$VolumeDiameterAdjustment ==
        "Measured diameter rounded downwards to cm") {
      top_d <- floor(top_d / 10) * 10  # mm -> round down to nearest 10 mm (= full cm)
    }

    if (is.na(top_d) || top_d <= 0) {
      return(list(price_volume_m3   = NA_real_,
                  EffectiveLogLength = eff_length,
                  EffectiveStopPos   = eff_stoppos))
    }

    radius_mm <- top_d / 2
    # TODO: This is a simplified top-measured cylinder volume. Before production use,
    # verify against the exact Norwegian timber measurement rule and the relevant
    # StanForD product settings.
    v <- pi * radius_mm^2 * eff_length / 1e8

  } else {
    stop(sprintf("Unsupported VolumeDiameterCategory: '%s'", vol_dia_cat))
  }

  list(
    price_volume_m3    = v,
    EffectiveLogLength = eff_length,
    EffectiveStopPos   = eff_stoppos
  )
}


# ---- product eligibility ----

#' Check whether a log segment is eligible for a given product.
#'
#' @param diameterPosition  Profile positions (cm).
#' @param DiameterValue     Profile diameters (mm).
#' @param StartPos          Log start position (cm).
#' @param StopPos           Log stop position (cm).
#' @param ProductRow        One-row tibble from ProductData.
#' @param LengthClassesVec  Allowed lengths (cm) for this product.
#' @return Tibble with eligibility result and diameter/length values.
check_log_against_product <- function(diameterPosition, DiameterValue,
                                      StartPos, StopPos,
                                      ProductRow, LengthClassesVec) {

  LogLength    <- StopPos - StartPos
  TopDiameter  <- get_diameter_at_position(
    diameterPosition, DiameterValue,
    StopPos - ProductRow$DiameterTopPosition
  )
  ButtDiameter <- get_diameter_at_position(
    diameterPosition, DiameterValue, StartPos
  )

  reasons <- character(0)

  if (is.na(LogLength) || LogLength < ProductRow$MinLength)
    reasons <- c(reasons, sprintf("LogLength %s < MinLength %d cm",
                                  format(LogLength), ProductRow$MinLength))
  if (!is.na(LogLength) && LogLength > ProductRow$MaxLength)
    reasons <- c(reasons, sprintf("LogLength %s > MaxLength %d cm",
                                  format(LogLength), ProductRow$MaxLength))
  if (is.na(TopDiameter) || TopDiameter < ProductRow$MinTopDiameter)
    reasons <- c(reasons, sprintf("TopDiameter %.1f mm < MinTopDiameter %.0f mm",
                                  TopDiameter %||% NaN, ProductRow$MinTopDiameter))
  if (!is.na(TopDiameter) && TopDiameter > ProductRow$MaxTopDiameter)
    reasons <- c(reasons, "TopDiameter > MaxTopDiameter")
  if (is.na(ButtDiameter) || ButtDiameter < ProductRow$MinButtDiameter)
    reasons <- c(reasons, sprintf("ButtDiameter %.1f mm < MinButtDiameter %.0f mm",
                                  ButtDiameter %||% NaN, ProductRow$MinButtDiameter))
  if (!is.na(ButtDiameter) && ButtDiameter > ProductRow$MaxButtDiameter)
    reasons <- c(reasons, "ButtDiameter > MaxButtDiameter")

  if (ProductRow$VolumeLengthCategory == "Length as defined in LengthClasses") {
    valid_lc <- LengthClassesVec[LengthClassesVec <= LogLength]
    if (length(valid_lc) == 0)
      reasons <- c(reasons, "No valid length class <= LogLength")
  }

  eligible <- length(reasons) == 0
  tibble::tibble(
    eligible     = eligible,
    LogLength    = LogLength,
    TopDiameter  = round(TopDiameter,  1),
    ButtDiameter = round(ButtDiameter, 1),
    reason       = if (eligible) "OK" else paste(reasons, collapse = "; ")
  )
}

# Null-coalescing helper used above.
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a


# ---- test volume calculations ----

wlog("", "--- Selecting test trees ---")

# Select test trees: largest, median, smallest spruce by DBH.
# Also first pine tree if present.
spruce_trees <- stem_profiles |>
  dplyr::distinct(treid, species_group, dbh_cm) |>
  dplyr::filter(species_group == "spruce") |>
  dplyr::arrange(dbh_cm)

if (nrow(spruce_trees) == 0)
  stop("No spruce trees found in the stem profile input.")

test_tree_ids <- unique(c(
  spruce_trees$treid[1],                                      # smallest
  spruce_trees$treid[round(nrow(spruce_trees) / 2)],          # median
  spruce_trees$treid[nrow(spruce_trees)]                       # largest
))

pine_trees <- stem_profiles |>
  dplyr::distinct(treid, species_group, dbh_cm) |>
  dplyr::filter(species_group == "pine") |>
  dplyr::arrange(dplyr::desc(dbh_cm))

if (nrow(pine_trees) > 0) {
  test_tree_ids <- unique(c(test_tree_ids, pine_trees$treid[1]))
  wlog(sprintf("  Pine trees present; including largest pine: treid = %s",
               pine_trees$treid[1]))
} else {
  wlog("  No pine trees in input; skipping pine test examples.")
}

wlog(sprintf("  Test tree IDs: %s", paste(test_tree_ids, collapse = ", ")))

# Test log positions and lengths.
STUMP_HEIGHT_CM <- 30L
test_lengths_cm <- c(310L, 370L, 430L, 490L, 550L)

test_results_list <- list()

for (tid in test_tree_ids) {
  pv <- get_tree_profile_vectors(stem_profiles, tid)
  sg <- pv$species_group
  height_cm <- pv$height_cm

  # Products relevant for this species.
  species_products <- ProductData |>
    dplyr::filter(species_group == sg)

  for (tlen in test_lengths_cm) {
    start_pos <- STUMP_HEIGHT_CM
    stop_pos  <- start_pos + tlen

    if (stop_pos > height_cm) next  # log exceeds tree height

    for (j in seq_len(nrow(species_products))) {
      prow    <- species_products[j, ]
      lc_vec  <- LengthClasses[[prow$ProductKey]]

      elig    <- check_log_against_product(
        pv$diameterPosition, pv$DiameterValue,
        start_pos, stop_pos, prow, lc_vec
      )

      pvc     <- PriceVolumeCalc_nfi(
        pv$diameterPosition, pv$DiameterValue,
        start_pos, stop_pos, prow, lc_vec
      )

      test_results_list[[length(test_results_list) + 1L]] <- tibble::tibble(
        treid                 = as.character(tid),
        species_group         = sg,
        ProductKey            = prow$ProductKey,
        ProductType           = prow$ProductType,
        StartPos              = start_pos,
        StopPos               = stop_pos,
        LogLength             = elig$LogLength,
        TopDiameter           = elig$TopDiameter,
        ButtDiameter          = elig$ButtDiameter,
        eligible              = elig$eligible,
        reason                = elig$reason,
        VolumeDiameterCategory = prow$VolumeDiameterCategory,
        VolumeLengthCategory   = prow$VolumeLengthCategory,
        price_volume_m3       = round(pvc$price_volume_m3, 5)
      )
    }
  }
}

test_results <- dplyr::bind_rows(test_results_list)

wlog(
  sprintf("  Test records created: %d", nrow(test_results)),
  ""
)
wlog("  Volume summary by ProductKey and eligibility:")
wlog(capture.output(print(
  test_results |>
    dplyr::group_by(ProductKey, eligible) |>
    dplyr::summarise(
      n          = dplyr::n(),
      mean_vol   = round(mean(price_volume_m3, na.rm = TRUE), 5),
      .groups    = "drop"
    )
)))


# ---- candidate log generation ----

#' Generate all technically possible candidate logs from StartPos for one tree.
#'
#' Iterates over all relevant products and all allowed length classes.
#' Returns a tibble ready for later dynamic-programming bucking.
#'
#' @param profiles     Stem profile data frame.
#' @param tree_id      treid value.
#' @param pd           ProductData table.
#' @param lc           LengthClasses named list.
#' @param start_pos    Start position (cm), e.g. stump height.
#' @return Tibble of candidate logs with eligibility and volume.
generate_log_candidates_for_tree <- function(profiles, tree_id, pd, lc,
                                             start_pos = 30L) {
  pv        <- get_tree_profile_vectors(profiles, tree_id)
  sg        <- pv$species_group
  height_cm <- pv$height_cm

  sp_products <- pd |> dplyr::filter(species_group == sg)
  candidates  <- list()

  for (j in seq_len(nrow(sp_products))) {
    prow   <- sp_products[j, ]
    lc_vec <- lc[[prow$ProductKey]]

    for (log_len in lc_vec) {
      stop_pos <- start_pos + log_len
      if (stop_pos > height_cm) next

      elig <- check_log_against_product(
        pv$diameterPosition, pv$DiameterValue,
        start_pos, stop_pos, prow, lc_vec
      )

      pvc <- PriceVolumeCalc_nfi(
        pv$diameterPosition, pv$DiameterValue,
        start_pos, stop_pos, prow, lc_vec
      )

      candidates[[length(candidates) + 1L]] <- tibble::tibble(
        treid                  = as.character(tree_id),
        plot_id                = pv$plot_id,
        species_group          = sg,
        ProductKey             = prow$ProductKey,
        ProductType            = prow$ProductType,
        StartPos               = start_pos,
        StopPos                = stop_pos,
        LogLength              = elig$LogLength,
        TopDiameter            = elig$TopDiameter,
        ButtDiameter           = elig$ButtDiameter,
        eligible               = elig$eligible,
        reason                 = elig$reason,
        VolumeDiameterCategory = prow$VolumeDiameterCategory,
        VolumeLengthCategory   = prow$VolumeLengthCategory,
        price_volume_m3        = round(pvc$price_volume_m3, 5)
      )
    }
  }

  dplyr::bind_rows(candidates)
}

# Run for the largest spruce tree.
largest_spruce_id <- spruce_trees$treid[nrow(spruce_trees)]
wlog(sprintf("\n--- Candidate logs for one tree (treid = %s) ---",
             largest_spruce_id))

candidate_logs <- generate_log_candidates_for_tree(
  stem_profiles, largest_spruce_id, ProductData, LengthClasses,
  start_pos = STUMP_HEIGHT_CM
)

wlog(
  sprintf("  Total candidate log rows  : %d", nrow(candidate_logs)),
  sprintf("  Eligible candidates       : %d",
          sum(candidate_logs$eligible, na.rm = TRUE))
)
wlog("  Candidates by ProductKey:")
wlog(capture.output(print(
  candidate_logs |>
    dplyr::count(ProductKey, eligible, name = "n")
)))


# ---- save outputs ----

wlog("", "--- Saving outputs ---")

# ProductData
pd_csv <- file.path(inputs_dir, "ProductData_spruce_pine_default.csv")
pd_rds <- file.path(inputs_dir, "ProductData_spruce_pine_default.rds")
readr::write_csv(ProductData, pd_csv)
saveRDS(ProductData, pd_rds)
wlog(sprintf("  Saved: %s", basename(pd_csv)))
wlog(sprintf("  Saved: %s", basename(pd_rds)))

# LengthClasses
lc_rds <- file.path(inputs_dir, "LengthClasses_spruce_pine_default.rds")
saveRDS(LengthClasses, lc_rds)
wlog(sprintf("  Saved: %s", basename(lc_rds)))

lc_long <- purrr::imap_dfr(LengthClasses, function(lc_vec, pk) {
  tibble::tibble(ProductKey = pk, LogLength = lc_vec)
})
lc_csv <- file.path(inputs_dir, "LengthClasses_spruce_pine_default.csv")
readr::write_csv(lc_long, lc_csv)
wlog(sprintf("  Saved: %s", basename(lc_csv)))

# Test log volume results
test_csv <- file.path(pv_tests_dir, "test_log_volume_results.csv")
test_rds <- file.path(pv_tests_dir, "test_log_volume_results.rds")
readr::write_csv(test_results, test_csv)
saveRDS(test_results,          test_rds)
wlog(sprintf("  Saved: %s", basename(test_csv)))
wlog(sprintf("  Saved: %s", basename(test_rds)))

# Candidate logs
cand_csv <- file.path(pv_tests_dir, "candidate_logs_one_tree.csv")
cand_rds <- file.path(pv_tests_dir, "candidate_logs_one_tree.rds")
readr::write_csv(candidate_logs, cand_csv)
saveRDS(candidate_logs,         cand_rds)
wlog(sprintf("  Saved: %s", basename(cand_csv)))
wlog(sprintf("  Saved: %s", basename(cand_rds)))


# ---- diagnostic plots ----

wlog("", "--- Creating diagnostic plots ---")

# 1. Test log price volume by ProductKey and LogLength
p_vol <- test_results |>
  dplyr::filter(eligible, !is.na(price_volume_m3)) |>
  ggplot2::ggplot(ggplot2::aes(
    x      = as.factor(LogLength),
    y      = price_volume_m3,
    fill   = ProductKey
  )) +
  ggplot2::geom_boxplot(alpha = 0.8) +
  ggplot2::facet_wrap(~ProductKey, scales = "free_y") +
  ggplot2::labs(
    title = "Price volume by ProductKey and LogLength (eligible logs, test trees)",
    x     = "Log length (cm)",
    y     = "Price volume (m3)"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none",
                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

save_plot(p_vol, "plot_test_volume_by_product_length.png", width = 20, height = 14)

# 2. Top diameter by LogLength for one example tree
test_one_tree <- test_results |>
  dplyr::filter(treid == as.character(largest_spruce_id), eligible)

if (nrow(test_one_tree) > 0) {
  p_topd <- test_one_tree |>
    ggplot2::ggplot(ggplot2::aes(
      x      = LogLength,
      y      = TopDiameter,
      colour = ProductKey,
      group  = ProductKey
    )) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::geom_line(alpha = 0.6) +
    ggplot2::labs(
      title  = sprintf("Top diameter by log length – largest spruce (treid %s)",
                       largest_spruce_id),
      x      = "Log length (cm)",
      y      = "Top diameter (mm)",
      colour = "ProductKey"
    ) +
    ggplot2::theme_minimal()

  save_plot(p_topd, "plot_top_diameter_one_tree.png")
}

# 3. Candidate log volume by ProductKey for one example tree
if (nrow(candidate_logs) > 0) {
  p_cand <- candidate_logs |>
    dplyr::filter(eligible, !is.na(price_volume_m3)) |>
    ggplot2::ggplot(ggplot2::aes(
      x    = LogLength,
      y    = price_volume_m3,
      colour = ProductKey
    )) +
    ggplot2::geom_point(alpha = 0.7, size = 1.5) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::labs(
      title  = sprintf("Candidate log volume by ProductKey – treid %s",
                       largest_spruce_id),
      x      = "Log length (cm)",
      y      = "Price volume (m3)",
      colour = "ProductKey"
    ) +
    ggplot2::theme_minimal()

  save_plot(p_cand, "plot_candidate_log_volume_one_tree.png")
}


# ---- write log ----

wlog(
  "",
  "=============================================================",
  "  SUMMARY",
  "=============================================================",
  sprintf("  Profile rows read          : %d", n_profile_rows),
  sprintf("  Unique trees read          : %d", n_profile_trees),
  "",
  "  ProductData:",
  sprintf("    Products defined         : %d", nrow(ProductData)),
  sprintf("    Diameter convention      : %s",
          paste(unique(ProductData$DiameterSource), collapse = ", ")),
  sprintf("    Products DiamUnderBark=T : %d",
          sum(ProductData$DiameterUnderBark, na.rm = TRUE)),
  sprintf("    VolumeDiameterCategories : %s",
          paste(unique(ProductData$VolumeDiameterCategory), collapse = ", ")),
  sprintf("    VolumeLengthCategories   : %s",
          paste(unique(ProductData$VolumeLengthCategory), collapse = ", ")),
  "",
  "  LengthClasses:",
  sprintf("    Defined for              : %s",
          paste(names(LengthClasses), collapse = ", ")),
  "",
  "  Test results:",
  sprintf("    Test records created     : %d", nrow(test_results)),
  sprintf("    NA price_volume_m3       : %d",
          sum(is.na(test_results$price_volume_m3))),
  "",
  "  Candidate logs (one tree):",
  sprintf("    Total rows               : %d", nrow(candidate_logs)),
  sprintf("    Eligible                 : %d",
          sum(candidate_logs$eligible, na.rm = TRUE)),
  sprintf("    NA price_volume_m3       : %d",
          sum(is.na(candidate_logs$price_volume_m3))),
  "",
  sprintf("  Log: %s", log_file),
  "",
  "Done."
)

if (all(is.na(test_results$price_volume_m3))) {
  stop("All price_volume_m3 values are NA. Check taper profile input and VolumeCalc_nfi().")
}
