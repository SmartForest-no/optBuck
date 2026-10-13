# ==============================================================================
# R/helpers_candidate_generation.R
#
# Purpose : Vectorised generation of candidate log combinations for one tree
#           (or a batch of trees), using the taper profile vectors produced by
#           helpers_taper.R.
#
# Design notes:
#   - Fully vectorised approach via expand.grid() (same as 11_run_bucking…R).
#   - Price-diameter capping is NOT done here; it belongs in helpers_valuation.R
#     (via prepare_price_join_keys()).  This helper outputs
#     PriceDiameterClass_cm = floor(TopDiameter/10) — the raw, uncapped value.
#   - Price volume per product (Phase 4b, 2026-10-13): price_volume_m3() in
#     helpers_volume_methods.R, by ProductData$VolumeDiameterCategory:
#       "Top cylinder" (default sawlogs): cylinder at the top diameter,
#                       rounded down to cm; volume rounded to 5 decimals.
#       "Top"         : frustum, 1 cm/m taper.
#       "Calculated Norwegian mid".
#       "All diameters (solid volume)" (default pulpwood, waste): Apter's
#                       solid volume (section_volume_fun(), shared with the
#                       NOK/m3 denominator); not rounded (2026-10-12).
#   - Measuring top diameter: read DiameterTopPosition cm below the cut (0
#     for the default products, i.e. at the cut). It drives the top limits,
#     the price-diameter class and the price volume.
#   - Products per species come from ProductData (species_products()), so
#     custom assortments are generated like the base products.
#   - Bark (2026-10-12): products with DiameterUnderBark = TRUE use the
#     under-bark profile (pv$DiameterValueUB, helpers_bark.R) for butt/top
#     diameter, eligibility limits, price-diameter class and price volume,
#     as in optBuck::buckStem (one diameter drives all three). If the
#     profile has no DiameterValueUB yet it is derived with
#     add_under_bark_profile(). Products with DiameterUnderBark = FALSE use
#     the over-bark profile, as before.
#   - No species filtering by default; products_for_species() provides
#     a convenience filter.
#
# Usage:
#   source("R/helpers_taper.R")
#   source("R/helpers_products.R")
#   source("R/helpers_candidate_generation.R")
#   pvs <- profiles_to_profile_vectors(profiles)
#   pv  <- pvs[["242251"]]
#   pd  <- load_products()
#   lc  <- load_length_classes()
#   cands <- generate_candidate_logs_one_tree(pv, pd, lc)
# ==============================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}

if (!exists("add_under_bark_profile", mode = "function"))
  source(file.path(dirname(sys.frame(1L)$ofile %||% "."), "helpers_bark.R"))
if (!exists("section_volume_fun", mode = "function"))
  source(file.path(dirname(sys.frame(1L)$ofile %||% "."),
                   "helpers_commercial_volume.R"))
if (!exists("price_volume_m3", mode = "function"))
  source(file.path(dirname(sys.frame(1L)$ofile %||% "."),
                   "helpers_volume_methods.R"))
if (!exists("species_products", mode = "function"))
  source(file.path(dirname(sys.frame(1L)$ofile %||% "."), "helpers_products.R"))

#' Default stump height in cm (stump = first possible start position).
CAND_STUMP_HEIGHT_CM <- 10L

#' Default start-position step in cm (equal to PROFILE_STEP_CM from taper).
CAND_START_POS_STEP_CM <- 10L

#' Maximum price-diameter class used as join key (cm).  Cap applied in
#' helpers_valuation.R, but referenced here for context.
CAND_MAX_PRICE_DIAMETER_CLASS_CM <- 50L


# ------------------------------------------------------------------------------
# Species × product filtering
# ------------------------------------------------------------------------------

#' Return the product keys applicable to a given species group.
#'
#' @param species_group "spruce" or "pine".
#' @param ProductData   ProductData; NULL gives the base products of
#'   ACTIVE_PRODUCT_KEYS (helpers_products.R).
#' @return Character vector of ProductKey values, or NULL if none.
products_for_species <- function(species_group, ProductData = NULL) {
  pks <- if (is.null(ProductData))
    ACTIVE_PRODUCT_KEYS[startsWith(ACTIVE_PRODUCT_KEYS, paste0(species_group, "_"))]
  else species_products(ProductData, species_group)
  if (length(pks) == 0L) NULL else pks
}


# ------------------------------------------------------------------------------
# One-tree candidate generation (vectorised)
# ------------------------------------------------------------------------------

#' Generate all candidate log combinations for one tree.
#'
#' Loops over the applicable products (filtered by species_group inside pv).
#' For each product, creates expand.grid(start_positions, valid_lengths) and
#' evaluates eligibility (length, top-diameter, butt-diameter) and volume
#' vectorially.
#'
#' @param pv              Profile vector list from profiles_to_profile_vectors().
#'                        Must contain: treid, plot_id, species_group, DBH (mm),
#'                        height_cm (int), diameterPosition (int vector, cm),
#'                        DiameterValue (numeric vector, mm).
#' @param ProductData     data.frame from load_products().
#' @param LengthClasses   data.frame from load_length_classes() (with
#'                        LogLength_dm column).
#' @param stump_height_cm First start position (cm).
#' @param start_pos_step_cm Step between start positions (cm).
#' @return data.frame with one row per candidate (all products, all positions),
#'   including ineligible rows (eligible == FALSE).  Columns:
#'   treid, plot_id, species_group, ProductKey, ProductType,
#'   StartPos, StopPos, LogLength, TopDiameter (mm), ButtDiameter (mm),
#'   eligible, price_volume_m3, LengthClass_dm,
#'   PriceDiameterClass_cm  (raw, uncapped; cap applied in valuation step),
#'   TopDiameter_ob, TopDiameter_ub (mm), DiameterUnderBark,
#'   ProductPriority (ProductData$Priority; tie-break key for the optimizers).
#'   TopDiameter / ButtDiameter are the product's measuring diameters: under
#'   bark when the product's DiameterUnderBark is TRUE, else over bark.
generate_candidate_logs_one_tree <- function(pv,
                                              ProductData,
                                              LengthClasses,
                                              stump_height_cm    = CAND_STUMP_HEIGHT_CM,
                                              start_pos_step_cm  = CAND_START_POS_STEP_CM) {

  tree_id  <- pv$treid
  sg       <- pv$species_group
  plot_id  <- pv$plot_id

  pks <- products_for_species(sg, ProductData)
  if (is.null(pks) || length(pks) == 0L)
    stop(sprintf("Tree %s: no products configured for species_group '%s'.",
                 tree_id, sg))

  # Filter ProductData to applicable products
  pd_sp <- ProductData[ProductData$ProductKey %in% pks, , drop = FALSE]
  if (nrow(pd_sp) == 0L)
    stop(sprintf("Tree %s: no matching rows in ProductData for species '%s'.",
                 tree_id, sg))

  # Profile vectors
  diam_pos <- as.integer(pv$diameterPosition)   # cm (sorted ascending)
  # Logs end within the profile: a height that is not a whole decimetre
  # (e.g. an imputed 11.296 m) has a profile ending at 11.2 m, below
  # round(height * 100). NFI heights are whole decimetres: no change there.
  h_cm     <- min(as.integer(pv$height_cm), max(diam_pos))
  step_int <- as.integer(start_pos_step_cm)

  # One diameter series (over or under bark): interpolation function
  # (rule = 2: extrapolate with boundary values) and Apter's solid volume
  # (section_volume_fun(), shared with the NOK/m3 denominator).
  make_series <- function(diam_val) {
    list(fun       = stats::approxfun(diam_pos, diam_val, method = "linear", rule = 2),
         solid_vol = section_volume_fun(diam_pos, diam_val, step_int))
  }

  ser_ob <- make_series(as.numeric(pv$DiameterValue))   # mm, over bark
  needs_ub <- "DiameterUnderBark" %in% names(pd_sp) &&
    any(as.logical(pd_sp$DiameterUnderBark) %in% TRUE)
  ser_ub <- if (needs_ub) {
    if (is.null(pv$DiameterValueUB)) pv <- add_under_bark_profile(pv)
    make_series(as.numeric(pv$DiameterValueUB))         # mm, under bark
  } else NULL

  # Valid log lengths for this tree (all products), pre-filtered by tree height.
  lc_valid <- LengthClasses[
    LengthClasses$ProductKey %in% pks &
    LengthClasses$LogLength  <= (h_cm - stump_height_cm), , drop = FALSE]
  if (nrow(lc_valid) == 0L)
    return(.empty_candidates(tree_id, plot_id, sg))

  log_lengths <- as.integer(lc_valid$LogLength)       # cm

  # Start positions (multiples of step_int, starting at stump)
  start_positions <- seq(as.integer(stump_height_cm),
                         h_cm - min(log_lengths),
                         by = step_int)
  if (length(start_positions) == 0L)
    return(.empty_candidates(tree_id, plot_id, sg))

  prod_frames <- vector("list", nrow(pd_sp))

  for (j in seq_len(nrow(pd_sp))) {
    prow <- pd_sp[j, , drop = FALSE]
    pk   <- prow$ProductKey

    # Valid lengths for this product (LogLength in cm)
    ok_len <- lc_valid[
      lc_valid$ProductKey == pk &
      lc_valid$LogLength >= (prow$MinLength %||% 0L) &
      lc_valid$LogLength <= (prow$MaxLength %||% Inf), , drop = FALSE]
    if (nrow(ok_len) == 0L) { prod_frames[[j]] <- NULL; next }

    ok_len_vec <- as.integer(ok_len$LogLength)

    # Filter start positions where at least one log can end within the tree.
    valid_sp <- start_positions[
      start_positions + min(ok_len_vec) <= h_cm]
    if (length(valid_sp) == 0L) { prod_frames[[j]] <- NULL; next }

    # Expand all combinations of start_pos × log_len
    combos <- expand.grid(start_pos = valid_sp,
                          log_len   = ok_len_vec,
                          KEEP.OUT.ATTRS = FALSE,
                          stringsAsFactors = FALSE)
    combos$stop_pos <- combos$start_pos + combos$log_len

    # Remove combinations that exceed tree height
    combos <- combos[combos$stop_pos <= h_cm, , drop = FALSE]
    if (nrow(combos) == 0L) { prod_frames[[j]] <- NULL; next }

    # Measuring diameters: under bark if the product says so, else over bark.
    ub  <- isTRUE(as.logical(prow$DiameterUnderBark))
    ser <- if (ub) ser_ub else ser_ob

    # Butt diameter at start position (mm)
    combos$ButtDiameter <- ser$fun(combos$start_pos)

    # Measuring top diameter: DiameterTopPosition cm below the cut (0 for the
    # default products, as in script 11, i.e. at stop_pos).
    top_pos <- combos$stop_pos - as.numeric(prow$DiameterTopPosition %||% 0)
    combos$TopDiameter  <- ser$fun(top_pos)
    top_ob <- ser_ob$fun(top_pos)
    top_ub <- if (!is.null(ser_ub)) ser_ub$fun(top_pos) else NA_real_

    # Eligibility flags
    e_len  <- combos$log_len  >= (prow$MinLength  %||% 0L) &
              combos$log_len  <= (prow$MaxLength  %||% Inf)
    e_top  <- combos$TopDiameter  >= (prow$MinTopDiameter  %||% 0) &
              combos$TopDiameter  <= (prow$MaxTopDiameter  %||% Inf)
    e_butt <- combos$ButtDiameter >= (prow$MinButtDiameter %||% 0) &
              combos$ButtDiameter <= (prow$MaxButtDiameter %||% Inf)
    eligible <- e_len & e_top & e_butt

    # Price volume by the product's volume method (helpers_volume_methods.R);
    # NA for ineligible candidates.
    price_vol <- ifelse(
      eligible,
      price_volume_m3(method     = prow$VolumeDiameterCategory,
                      top_d_mm   = combos$TopDiameter,
                      length_cm  = combos$log_len,
                      start_pos  = combos$start_pos,
                      stop_pos   = combos$stop_pos,
                      solid_vol  = ser$solid_vol,
                      adjustment = prow$VolumeDiameterAdjustment),
      NA_real_)

    # LengthClass in dm
    len_dm <- as.integer(combos$log_len / 10L)

    # Price diameter class: raw floor(TopDiameter_mm / 10) = floor(TopDiameter_cm)
    # NOTE: this is the UNCAPPED value.  Cap is applied in helpers_valuation.R.
    diam_class_raw <- as.integer(floor(combos$TopDiameter / 10))

    prod_frames[[j]] <- data.frame(
      treid                = as.character(tree_id),
      plot_id              = as.character(plot_id),
      species_group        = sg,
      ProductKey           = pk,
      ProductType          = prow$ProductType,
      StartPos             = as.integer(combos$start_pos),
      StopPos              = as.integer(combos$stop_pos),
      LogLength            = as.integer(combos$log_len),
      TopDiameter          = round(combos$TopDiameter, 1),
      ButtDiameter         = round(combos$ButtDiameter, 1),
      eligible             = eligible,
      price_volume_m3      = price_vol,
      LengthClass_dm       = len_dm,
      PriceDiameterClass_cm = diam_class_raw,   # uncapped; rename in valuation
      TopDiameter_ob       = round(top_ob, 1),
      TopDiameter_ub       = round(top_ub, 1),
      DiameterUnderBark    = ub,
      # Tie-break key for the optimizers (R/helpers_dp_optimizer.R).
      ProductPriority      = as.numeric(prow$Priority %||% 99),
      stringsAsFactors     = FALSE
    )
  }

  non_null <- Filter(Negate(is.null), prod_frames)
  if (length(non_null) == 0L)
    return(.empty_candidates(tree_id, plot_id, sg))

  do.call(rbind, non_null)
}


# Internal helper: empty candidate frame with correct schema.
.empty_candidates <- function(treid, plot_id, species_group) {
  data.frame(
    treid                = character(0),
    plot_id              = character(0),
    species_group        = character(0),
    ProductKey           = character(0),
    ProductType          = character(0),
    StartPos             = integer(0),
    StopPos              = integer(0),
    LogLength            = integer(0),
    TopDiameter          = numeric(0),
    ButtDiameter         = numeric(0),
    eligible             = logical(0),
    price_volume_m3      = numeric(0),
    LengthClass_dm       = integer(0),
    PriceDiameterClass_cm = integer(0),
    TopDiameter_ob       = numeric(0),
    TopDiameter_ub       = numeric(0),
    DiameterUnderBark    = logical(0),
    ProductPriority      = numeric(0),
    stringsAsFactors     = FALSE
  )
}


# ------------------------------------------------------------------------------
# Batch candidate generation
# ------------------------------------------------------------------------------

#' Generate candidate logs for all trees in a profiles data.frame.
#'
#' Internally calls profiles_to_profile_vectors() and then
#' generate_candidate_logs_one_tree() for each tree.
#'
#' @param profiles         data.frame from generate_taper_profiles_for_trees().
#' @param ProductData      data.frame from load_products().
#' @param LengthClasses    data.frame from load_length_classes().
#' @param stump_height_cm  First start position (cm); default 10.
#' @param start_pos_step_cm Step (cm); default 10.
#' @param quiet            Suppress per-tree warnings on failure.
#' @return data.frame: all candidates row-bound, with attribute
#'   `generation_errors` (named character vector of error messages).
generate_candidate_logs_for_profiles <- function(profiles,
                                                  ProductData,
                                                  LengthClasses,
                                                  stump_height_cm   = CAND_STUMP_HEIGHT_CM,
                                                  start_pos_step_cm = CAND_START_POS_STEP_CM,
                                                  quiet = FALSE) {
  pvs    <- profiles_to_profile_vectors(profiles)
  n      <- length(pvs)
  frames <- vector("list", n)
  errors <- character(n)
  names(errors) <- names(pvs)

  for (i in seq_along(pvs)) {
    pv <- pvs[[i]]
    result <- tryCatch({
      cands <- generate_candidate_logs_one_tree(
        pv                = pv,
        ProductData       = ProductData,
        LengthClasses     = LengthClasses,
        stump_height_cm   = stump_height_cm,
        start_pos_step_cm = start_pos_step_cm)
      list(data = cands, err = "")
    }, error = function(e) {
      list(data = NULL, err = conditionMessage(e))
    })

    frames[[i]] <- result$data
    errors[i]   <- result$err
    if (nchar(result$err) > 0L && !quiet)
      warning(sprintf("Tree %s: candidate generation failed: %s",
                      pv$treid, result$err))
  }

  out <- do.call(rbind, frames[!vapply(frames, is.null, logical(1))])
  if (is.null(out)) out <- .empty_candidates(NA, NA, NA)
  attr(out, "generation_errors") <- errors
  out
}
