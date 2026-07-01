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
#   - Volume formula:
#       "Top" (sawlog): cylinder at top diameter — pi * r_mm^2 * L_cm / 1e8
#       Other types   : cumulative Huber sum over the profile grid.
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

#' Active product keys for each supported species group.
#' Must match ACTIVE_PRODUCT_KEYS in helpers_products.R.
.SPECIES_PRODUCTS <- list(
  spruce = c("spruce_sawlog", "spruce_pulpwood", "spruce_waste"),
  pine   = c("pine_sawlog",   "pine_pulpwood",   "pine_waste")
)

#' Return the product keys applicable to a given species group.
#'
#' @param species_group "spruce" or "pine".
#' @return Character vector of ProductKey values, or NULL if unknown species.
products_for_species <- function(species_group) {
  .SPECIES_PRODUCTS[[species_group]]
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
#'   PriceDiameterClass_cm  (raw, uncapped; cap applied in valuation step).
generate_candidate_logs_one_tree <- function(pv,
                                              ProductData,
                                              LengthClasses,
                                              stump_height_cm    = CAND_STUMP_HEIGHT_CM,
                                              start_pos_step_cm  = CAND_START_POS_STEP_CM) {

  tree_id  <- pv$treid
  sg       <- pv$species_group
  plot_id  <- pv$plot_id

  pks <- products_for_species(sg)
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
  diam_val <- as.numeric(pv$DiameterValue)       # mm
  h_cm     <- as.integer(pv$height_cm)

  # Diameter interpolation function (rule=2: extrapolate with boundary values)
  diam_fun <- stats::approxfun(diam_pos, diam_val, method = "linear", rule = 2)

  # Cumulative solid-volume array on the profile grid (step = PROFILE_STEP_CM)
  step_int  <- as.integer(start_pos_step_cm)
  prof_pos  <- seq(0L, h_cm, by = step_int)
  prof_d    <- diam_fun(prof_pos)
  n_pp      <- length(prof_pos)
  if (n_pp >= 2L) {
    sec_r   <- (prof_d[-n_pp] + prof_d[-1L]) / 4     # mean radius in mm
    sec_vol <- pi * sec_r^2 * step_int / 1e8          # m3 per section
    cumvol  <- c(0, cumsum(sec_vol))                  # cumvol[i] = vol at prof_pos[i-1]
  } else {
    cumvol <- c(0, 0)
  }

  # Helper: solid volume between two positions (cm), vectorised over pos_a / pos_b.
  solid_vol_vec <- function(pos_a, pos_b) {
    ia <- as.integer(pos_a) / step_int + 1L           # 1-based index
    ib <- as.integer(pos_b) / step_int + 1L
    ia <- pmax(1L, pmin(ia, length(cumvol)))
    ib <- pmax(1L, pmin(ib, length(cumvol)))
    cumvol[ib] - cumvol[ia]
  }

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

    # Butt diameter at start position (mm)
    combos$ButtDiameter <- diam_fun(combos$start_pos)

    # Top diameter at stop position minus a small offset (avoid 0 at apex)
    # Using stop_pos - DiameterTopPosition (=0 in script 11, i.e. at stop_pos)
    combos$TopDiameter  <- diam_fun(combos$stop_pos)

    # Eligibility flags
    e_len  <- combos$log_len  >= (prow$MinLength  %||% 0L) &
              combos$log_len  <= (prow$MaxLength  %||% Inf)
    e_top  <- combos$TopDiameter  >= (prow$MinTopDiameter  %||% 0) &
              combos$TopDiameter  <= (prow$MaxTopDiameter  %||% Inf)
    e_butt <- combos$ButtDiameter >= (prow$MinButtDiameter %||% 0) &
              combos$ButtDiameter <= (prow$MaxButtDiameter %||% Inf)
    eligible <- e_len & e_top & e_butt

    # Price volume
    # For "Top" products, apply VolumeDiameterAdjustment if specified.
    top_d_vol <- if (!is.null(prow$VolumeDiameterAdjustment) &&
                     !is.na(prow$VolumeDiameterAdjustment) &&
                     prow$VolumeDiameterAdjustment ==
                       "Measured diameter rounded downwards to cm") {
      floor(combos$TopDiameter / 10) * 10
    } else {
      combos$TopDiameter
    }

    if (!is.null(prow$VolumeDiameterCategory) &&
        !is.na(prow$VolumeDiameterCategory) &&
        prow$VolumeDiameterCategory == "Top") {
      r_mm      <- top_d_vol / 2
      price_vol <- ifelse(
        eligible & !is.na(r_mm) & r_mm > 0,
        pi * r_mm^2 * combos$log_len / 1e8,
        NA_real_)
    } else {
      price_vol <- ifelse(
        eligible,
        solid_vol_vec(combos$start_pos, combos$stop_pos),
        NA_real_)
    }

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
      price_volume_m3      = round(price_vol, 5),
      LengthClass_dm       = len_dm,
      PriceDiameterClass_cm = diam_class_raw,   # uncapped; rename in valuation
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
