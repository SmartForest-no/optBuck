# ==============================================================================
# R/helpers_candidate_generation.R
#
# Purpose : Vectorised generation of candidate log combinations for one tree
#           (or a batch of trees), using the taper profile vectors produced by
#           helpers_taper.R.
#
# Design notes:
#   - Vectorised: start positions x lengths per product with rep() (the
#     expand.grid() order of 11_run_bucking…R), product parameters per
#     species built once (.candidate_plan()), diameters read from the 10 cm
#     grid. .candidate_columns() is the engine; the backend asks for the
#     usable candidates only (Performance phase, 2026-10-13).
#   - Price-diameter capping is NOT done here; it belongs in helpers_valuation.R
#     (via prepare_price_join_keys()).  This helper outputs
#     PriceDiameterClass_cm = floor(TopDiameter/10) — the raw, uncapped value.
#   - Price volume per product (Phase 4b, 2026-10-13): price_volume_m3() in
#     helpers_volume_methods.R, by ProductData$VolumeDiameterCategory:
#       "Top cylinder" (default sawlogs): cylinder at the top diameter,
#                       rounded down to cm; volume rounded to 5 decimals.
#       "Top"         : frustum, 1 cm/m taper.
#       "Calculated Norwegian mid".
#       "Mid"         : cylinder at the diameter at the log's middle,
#                       (StartPos + StopPos)/2, interpolated from the profile.
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

if (!exists("or_default", mode = "function"))   # helpers_utils.R
  source(file.path(dirname(if (is.null(sys.frame(1L)$ofile)) "." else sys.frame(1L)$ofile),
                   "helpers_utils.R"))

if (!exists("add_under_bark_profile", mode = "function"))
  source(file.path(dirname(or_default(sys.frame(1L)$ofile, ".")), "helpers_bark.R"))
if (!exists("section_volume_fun", mode = "function"))
  source(file.path(dirname(or_default(sys.frame(1L)$ofile, ".")),
                   "helpers_commercial_volume.R"))
if (!exists("price_volume_m3", mode = "function"))
  source(file.path(dirname(or_default(sys.frame(1L)$ofile, ".")),
                   "helpers_volume_methods.R"))
if (!exists("species_products", mode = "function"))
  source(file.path(dirname(or_default(sys.frame(1L)$ofile, ".")), "helpers_products.R"))

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
# Product parameters per species (computed once per run, not per tree)
# ------------------------------------------------------------------------------

#' Stem-grade options of a run (Phase 6, .hpr stems only): `use_grades` = a log
#' may take a product only if the product permits every grade the log covers;
#' `cut_at_changes` = no log contains a grade change in its interior. Both off
#' (the default) changes nothing.
GRADE_OPTS_OFF <- list(use_grades = FALSE, cut_at_changes = FALSE)

#' The options as the two flags, whatever the input (NULL = both off), plus `grade_files`
#' (R5, only when given): a named list species -> the stem files whose grades the run uses
#' (NULL = every file). A stem of another file is bucked like a stem without grade data.
normalize_grade_opts <- function(o) {
  out <- list(use_grades = isTRUE(o$use_grades), cut_at_changes = isTRUE(o$cut_at_changes))
  if (!is.null(o$grade_files)) out$grade_files <- lapply(o$grade_files, as.integer)
  # Roadmap 8 (bucking check): the recorded waste logs of the unclassified product are defect
  # zones where only waste may be cut (pv$reject; the grade -1 rule of the old optBuck).
  if (isTRUE(o$reject_zones)) out$reject_zones <- TRUE
  out
}

#' The two flags of grade options (for tests that ask whether any option is on).
grade_flags <- function(o) unlist(normalize_grade_opts(o)[c("use_grades", "cut_at_changes")])

#' Candidate-generation parameters for one species: its products (in
#' ProductData order) with their limits, volume method and allowed lengths
#' (LengthClasses order), and all length classes of the species' products.
#' generate_candidate_logs_one_tree() builds this per call; the backend
#' builds it once per run (.candidate_plan()).
#'
#' @param grade_opts normalize_grade_opts() input (Phase 6). With `use_grades`
#'   each product carries `grades`, its permitted grade numbers (NULL = all).
#' @return list(species_group, pks, products, all_lengths, needs_ub,
#'   use_grades, cut_at_changes), or a list with an `error` message when the
#'   species has no products.
.candidate_species_plan <- function(species_group, ProductData, LengthClasses,
                                    grade_opts = GRADE_OPTS_OFF) {
  grade_opts <- normalize_grade_opts(grade_opts)
  pks <- products_for_species(species_group, ProductData)
  if (is.null(pks) || length(pks) == 0L)
    return(list(error = sprintf("no products configured for species_group '%s'.",
                                species_group)))
  pd_sp <- ProductData[ProductData$ProductKey %in% pks, , drop = FALSE]
  if (nrow(pd_sp) == 0L)
    return(list(error = sprintf("no matching rows in ProductData for species '%s'.",
                                species_group)))

  lc_sp <- LengthClasses[LengthClasses$ProductKey %in% pks, , drop = FALSE]
  products <- lapply(seq_len(nrow(pd_sp)), function(j) {
    prow <- pd_sp[j, , drop = FALSE]
    pk   <- prow$ProductKey
    min_len <- or_default(prow$MinLength, 0L)
    max_len <- or_default(prow$MaxLength, Inf)
    ok <- lc_sp$ProductKey == pk & lc_sp$LogLength >= min_len & lc_sp$LogLength <= max_len
    # Price-volume length per allowed length (hpr imports: the class lower
    # limit, LengthClasses$PriceLength_cm); NULL = the cut length.
    price_len <- if ("PriceLength_cm" %in% names(lc_sp)) {
      pl <- as.numeric(lc_sp$PriceLength_cm[ok]); cut <- as.numeric(lc_sp$LogLength[ok])
      pl[is.na(pl)] <- cut[is.na(pl)]
      if (isTRUE(all(pl == cut))) NULL else pl
    }
    list(pk = pk, product_type = prow$ProductType,
         lengths = lc_sp$LogLength[ok], price_len = price_len,
         min_len = min_len, max_len = max_len,
         min_top = or_default(prow$MinTopDiameter, 0), max_top = or_default(prow$MaxTopDiameter, Inf),
         min_butt = or_default(prow$MinButtDiameter, 0), max_butt = or_default(prow$MaxButtDiameter, Inf),
         ub = isTRUE(as.logical(prow$DiameterUnderBark)),
         top_offset = as.numeric(or_default(prow$DiameterTopPosition, 0)),
         method = prow$VolumeDiameterCategory,
         adjustment = prow$VolumeDiameterAdjustment,
         priority = as.numeric(or_default(prow$Priority, 99)),
         # Permitted grades (hpr imports; NULL = all) and whether the product is usable for a
         # stem without grade data: no grades, no reference grade, or the two sets meet.
         grades = if ("PermittedGrades" %in% names(prow)) {
           g <- permitted_grades_set(prow$PermittedGrades); if (length(g)) g },
         # Roadmap 8: "only" = only the log that starts at the first cut, "not" = never that log
         butt = {
           b <- if ("ButtCondition" %in% names(prow)) as.character(prow$ButtCondition) else NA_character_
           if (isTRUE(b == "Buck butt-log only")) "only" else if (isTRUE(b == "Do not buck butt log")) "not" else NA_character_
         },
         ref_ok = {
           g <- if ("PermittedGrades" %in% names(prow)) permitted_grades_set(prow$PermittedGrades) else numeric(0)
           r <- if ("ReferenceGrades" %in% names(prow)) permitted_grades_set(prow$ReferenceGrades) else numeric(0)
           length(g) == 0L || length(r) == 0L || any(g %in% r)
         })
  })
  list(species_group = species_group, pks = pks, products = products,
       use_grades = grade_opts$use_grades, cut_at_changes = grade_opts$cut_at_changes,
       reject_zones = isTRUE(grade_opts$reject_zones),
       # files whose grades this species uses (R5); NULL = all
       grade_files = if (!is.null(grade_opts$grade_files))
         as.integer(grade_opts$grade_files[[species_group]]) else NULL,
       all_lengths = lc_sp$LogLength,
       needs_ub = "DiameterUnderBark" %in% names(pd_sp) &&
         any(as.logical(pd_sp$DiameterUnderBark) %in% TRUE))
}

#' Price-volume length (cm) of a product's candidates: the cut length, or
#' for hpr-imported products the length class's lower limit
#' (plan product $price_len, aligned with $lengths).
price_len_of <- function(p, log_len)
  if (is.null(p$price_len)) log_len else p$price_len[match(log_len, p$lengths)]

#' Candidate-generation parameters for every species of a run, built once.
#'
#' @return Named list (one .candidate_species_plan() per species group).
.candidate_plan <- function(ProductData, LengthClasses,
                            species_groups = c("spruce", "pine"),
                            grade_opts = GRADE_OPTS_OFF) {
  setNames(lapply(species_groups, .candidate_species_plan,
                  ProductData = ProductData, LengthClasses = LengthClasses,
                  grade_opts = grade_opts),
           species_groups)
}


# ------------------------------------------------------------------------------
# One-tree candidate generation (vectorised)
# ------------------------------------------------------------------------------

#' Candidate logs of one tree as column vectors (no data.frame).
#'
#' The engine of generate_candidate_logs_one_tree() and of the backend. For
#' each product: all start positions x allowed lengths (start position
#' varying fastest, as expand.grid()), logs ending above the tree dropped,
#' then measuring diameters, eligibility, price volume and price-diameter
#' class, all vectorised.
#'
#' Diameters on the grid: start positions are multiples of the step, and so
#' are the measuring positions when lengths and DiameterTopPosition are, so
#' the profile is interpolated once per grid point and indexed. These are
#' the values approxfun() gives at each candidate; otherwise it is called
#' per candidate.
#'
#' @param pv   Profile vector list (see generate_candidate_logs_one_tree()).
#' @param plan .candidate_species_plan() for pv's species group.
#' @param stump_height_cm,start_pos_step_cm As generate_candidate_logs_one_tree().
#' @param usable_only FALSE: every candidate with all columns of
#'   generate_candidate_logs_one_tree() (DiameterUnderBark per row). TRUE (the
#'   backend): only candidates the optimizer can use (eligible, with a price
#'   volume), with the price-dependent columns only: prod (index into
#'   plan$products), StartPos, StopPos, price_volume_m3,
#'   PriceDiameterClass_cm. .candidate_details() adds the other columns for
#'   chosen rows.
#' @return list(cols, n_all, ser_ob, ser_ub, products): cols = list of
#'   equal-length column vectors, NULL without candidates; n_all = number of
#'   candidates before the eligibility filter, per product (named by
#'   ProductKey); the diameter series and products for .candidate_details().
.candidate_columns <- function(pv, plan,
                               stump_height_cm   = CAND_STUMP_HEIGHT_CM,
                               start_pos_step_cm = CAND_START_POS_STEP_CM,
                               usable_only = FALSE) {
  if (!is.null(plan$error))
    stop(sprintf("Tree %s: %s", pv$treid, plan$error))

  # Profile vectors
  diam_pos <- as.integer(pv$diameterPosition)   # cm (sorted ascending)
  # Logs end within the profile: a height that is not a whole decimetre
  # (e.g. an imputed 11.296 m) has a profile ending at 11.2 m, below
  # round(height * 100). NFI heights are whole decimetres: no change there.
  h_cm     <- min(as.integer(pv$height_cm), max(diam_pos))
  step_int <- as.integer(start_pos_step_cm)
  grid     <- seq(0L, h_cm, by = step_int)

  # One diameter series (over or under bark): interpolation function
  # (rule = 2: extrapolate with boundary values), its values on the grid,
  # and Apter's solid volume (section_volume_fun(), shared with the NOK/m3
  # denominator).
  make_series <- function(diam_val) {
    fun <- stats::approxfun(diam_pos, diam_val, method = "linear", rule = 2)
    list(fun       = fun,
         grid      = fun(grid),
         solid_vol = section_volume_fun(diam_pos, diam_val, step_int))
  }
  ser_ob <- make_series(as.numeric(pv$DiameterValue))   # mm, over bark
  ser_ub <- if (plan$needs_ub) {
    if (is.null(pv$DiameterValueUB)) pv <- add_under_bark_profile(pv)
    make_series(as.numeric(pv$DiameterValueUB))         # mm, under bark
  } else NULL

  n_all <- setNames(integer(length(plan$products)),
                    vapply(plan$products, `[[`, "", "pk"))
  out <- list()

  # Stem grades (Phase 6; .hpr stems with an option on): the grade of each
  # 10 cm cell [g, g + step) of the grid, as running counts, so a candidate
  # [start, stop) (snapped to the grid: overlaps under one step are ignored)
  # covers cells lo..hi. bad_cum counts cells whose grade a product does not
  # permit, chg_cum the cells that start a new grade.
  gg <- NULL
  # R5: a stem of a file whose instruction differs from the scenario's has its grades ignored.
  grades_here <- is.null(plan$grade_files) || (if (is.null(pv$file)) 1L else pv$file) %in% plan$grade_files
  if ((isTRUE(plan$use_grades) || isTRUE(plan$cut_at_changes)) && !is.null(pv$grade) && grades_here) {
    gg <- as.numeric(pv$grade)[pmax(findInterval(grid, diam_pos), 1L)]
    n_cells <- length(gg)
    chg_cum <- c(0L, cumsum(c(FALSE, !is.na(gg[-1L]) & !is.na(gg[-n_cells]) & gg[-1L] != gg[-n_cells])))
  }
  use_cells <- isTRUE(plan$use_grades) && !is.null(gg)   # the recorded grades decide the products
  # Recorded waste zones (bucking check): cells where only waste may be cut.
  rj_cum <- NULL; n_rj <- 0L
  if (isTRUE(plan$reject_zones) && !is.null(pv$reject)) {
    rj <- as.logical(pv$reject)[pmax(findInterval(grid, diam_pos), 1L)]
    rj[is.na(rj)] <- FALSE
    rj_cum <- c(0L, cumsum(rj)); n_rj <- length(rj)
  }

  # Valid log lengths for this tree (all products), pre-filtered by tree height.
  max_len_cm  <- h_cm - stump_height_cm
  log_lengths <- as.integer(plan$all_lengths[plan$all_lengths <= max_len_cm])   # cm
  if (length(log_lengths) > 0L) {
    # Start positions (multiples of step_int, starting at stump)
    start_positions <- seq(as.integer(stump_height_cm), h_cm - min(log_lengths),
                           by = step_int)
    for (j in seq_along(plan$products)) {
      p <- plan$products[[j]]
      # Stems without grade data (or "Use stem grades" off) have the reference grade in every
      # cell: a product that does not permit it is not usable.
      if (!use_cells && !isTRUE(p$ref_ok)) next
      ok_len_vec <- as.integer(p$lengths[p$lengths <= max_len_cm])
      if (length(ok_len_vec) == 0L) next
      # Start positions where at least one log can end within the tree.
      valid_sp <- start_positions[start_positions + min(ok_len_vec) <= h_cm]
      if (length(valid_sp) == 0L) next

      # All start_pos x log_len combinations, start_pos varying fastest.
      start_pos <- rep(valid_sp, times = length(ok_len_vec))
      log_len   <- rep(ok_len_vec, each = length(valid_sp))
      stop_pos  <- start_pos + log_len
      keep      <- stop_pos <= h_cm   # logs ending above the tree
      if (!any(keep)) next
      if (!all(keep)) {
        start_pos <- start_pos[keep]; log_len <- log_len[keep]; stop_pos <- stop_pos[keep]
      }
      n_all[j] <- length(start_pos)

      # Measuring top diameter: DiameterTopPosition cm below the cut (0 for
      # the default products, i.e. at stop_pos).
      top_pos  <- stop_pos - p$top_offset
      use_grid <- stump_height_cm %% step_int == 0 && p$top_offset %% step_int == 0 &&
        all(ok_len_vec %% step_int == 0) && min(top_pos) >= 0
      at <- function(ser, pos)
        if (use_grid) ser$grid[pos %/% step_int + 1L] else ser$fun(pos)

      # Measuring diameters: under bark if the product says so, else over bark.
      ser  <- if (p$ub) ser_ub else ser_ob
      butt <- at(ser, start_pos)
      top  <- at(ser, top_pos)
      # Mid: the diameter at the middle of the cut log (often off the grid).
      mid  <- if (identical(p$method, "Mid")) ser$fun((start_pos + stop_pos) / 2)

      eligible <- log_len >= p$min_len & log_len <= p$max_len &
                  top  >= p$min_top  & top  <= p$max_top &
                  butt >= p$min_butt & butt <= p$max_butt
      if (!is.null(gg)) {
        lo <- pmin(as.integer(round(start_pos / step_int)) + 1L, n_cells)
        hi <- pmin(as.integer(round(stop_pos / step_int)), n_cells)
        if (use_cells && !is.null(p$grades)) {
          bad_cum <- c(0L, cumsum(!is.na(gg) & !gg %in% p$grades))
          eligible <- eligible & (bad_cum[hi + 1L] - bad_cum[lo]) == 0L
        }
        if (isTRUE(plan$cut_at_changes))
          eligible <- eligible & (chg_cum[hi + 1L] - chg_cum[pmin(lo + 1L, n_cells + 1L)]) <= 0L
      }

      # Recorded waste zones: no product but waste covers a zone cell.
      if (!is.null(rj_cum) && !identical(as.character(p$product_type), "waste")) {
        lo2 <- pmin(as.integer(round(start_pos / step_int)) + 1L, n_rj)
        hi2 <- pmin(as.integer(round(stop_pos / step_int)), n_rj)
        eligible <- eligible & (rj_cum[hi2 + 1L] - rj_cum[lo2]) == 0L
      }
      # ProductCondition: the butt log is the log that starts at the first cut.
      if (!is.na(p$butt))
        eligible <- eligible & (if (p$butt == "only") start_pos == stump_height_cm else start_pos != stump_height_cm)

      if (usable_only) {
        ii <- which(eligible)
        if (length(ii) == 0L) next
        start_pos <- start_pos[ii]; log_len <- log_len[ii]; stop_pos <- stop_pos[ii]
        top <- top[ii]; mid <- mid[ii]
        price_vol <- price_volume_m3(method = p$method, top_d_mm = top,
                                     length_cm = price_len_of(p, log_len), start_pos = start_pos,
                                     stop_pos = stop_pos, solid_vol = ser$solid_vol,
                                     adjustment = p$adjustment, mid_d_mm = mid)
        if (anyNA(price_vol)) {
          ii <- which(!is.na(price_vol))
          if (length(ii) == 0L) next
          start_pos <- start_pos[ii]; stop_pos <- stop_pos[ii]
          top <- top[ii]; price_vol <- price_vol[ii]
        }
        out[[length(out) + 1L]] <- list(
          prod                  = rep(j, length(start_pos)),
          StartPos              = as.integer(start_pos),
          StopPos               = as.integer(stop_pos),
          price_volume_m3       = price_vol,
          # Raw floor(TopDiameter_mm / 10): the UNCAPPED class; the cap is
          # applied in helpers_valuation.R.
          PriceDiameterClass_cm = as.integer(floor(top / 10)))
        next
      }

      # Price volume by the product's volume method; NA if ineligible.
      price_vol <- ifelse(
        eligible,
        price_volume_m3(method = p$method, top_d_mm = top, length_cm = price_len_of(p, log_len),
                        start_pos = start_pos, stop_pos = stop_pos,
                        solid_vol = ser$solid_vol, adjustment = p$adjustment,
                        mid_d_mm = mid),
        NA_real_)
      n <- length(start_pos)
      top_ob <- at(ser_ob, top_pos)
      top_ub <- if (!is.null(ser_ub)) at(ser_ub, top_pos) else rep(NA_real_, n)
      out[[length(out) + 1L]] <- list(
        ProductKey            = rep(p$pk, n),
        ProductType           = rep(p$product_type, n),
        StartPos              = as.integer(start_pos),
        StopPos               = as.integer(stop_pos),
        LogLength             = as.integer(log_len),
        TopDiameter           = round(top, 1),
        ButtDiameter          = round(butt, 1),
        eligible              = eligible,
        price_volume_m3       = price_vol,
        LengthClass_dm        = as.integer(log_len / 10L),
        PriceDiameterClass_cm = as.integer(floor(top / 10)),   # uncapped
        TopDiameter_ob        = round(top_ob, 1),
        TopDiameter_ub        = round(top_ub, 1),
        DiameterUnderBark     = rep(p$ub, n),
        # Tie-break key for the optimizers (R/helpers_dp_optimizer.R).
        ProductPriority       = rep(p$priority, n))
    }
  }

  cols <- if (length(out) == 0L) NULL
          else if (length(out) == 1L) out[[1L]]
          else setNames(lapply(names(out[[1L]]), function(nm)
            do.call(c, lapply(out, `[[`, nm))), names(out[[1L]]))
  list(cols = cols, n_all = n_all, ser_ob = ser_ob, ser_ub = ser_ub,
       products = plan$products)
}

#' The remaining candidate columns for chosen rows of a usable_only
#' .candidate_columns() result (e.g. the selected logs): the columns of
#' generate_candidate_logs_one_tree() except TopDiameter, ButtDiameter and
#' eligible, with the same values.
#'
#' @param geo .candidate_columns(..., usable_only = TRUE) result.
#' @param i   Row indices into geo$cols.
#' @return list of column vectors.
.candidate_details <- function(geo, i) {
  cl <- geo$cols
  prods <- geo$products[cl$prod[i]]
  start_pos <- cl$StartPos[i]; stop_pos <- cl$StopPos[i]
  log_len   <- stop_pos - start_pos
  top_pos   <- stop_pos - vapply(prods, `[[`, 0, "top_offset")
  list(
    ProductKey            = vapply(prods, `[[`, "", "pk"),
    ProductType           = unlist(lapply(prods, `[[`, "product_type")),
    StartPos              = start_pos,
    StopPos               = stop_pos,
    LogLength             = log_len,
    price_volume_m3       = cl$price_volume_m3[i],
    LengthClass_dm        = as.integer(log_len / 10L),
    PriceDiameterClass_cm = cl$PriceDiameterClass_cm[i],
    TopDiameter_ob        = round(geo$ser_ob$fun(top_pos), 1),
    TopDiameter_ub        = if (!is.null(geo$ser_ub)) round(geo$ser_ub$fun(top_pos), 1)
                            else rep(NA_real_, length(i)),
    DiameterUnderBark     = vapply(prods, `[[`, TRUE, "ub"),
    ProductPriority       = vapply(prods, `[[`, 0, "priority"))
}

#' Generate all candidate log combinations for one tree.
#'
#' For each product applicable to the tree's species: all start positions x
#' allowed lengths, with eligibility (length, top diameter, butt diameter)
#' and volume evaluated vectorially (.candidate_columns()).
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
  plan <- .candidate_species_plan(pv$species_group, ProductData, LengthClasses)
  cols <- .candidate_columns(pv, plan, stump_height_cm, start_pos_step_cm)$cols
  if (is.null(cols))
    return(.empty_candidates(pv$treid, pv$plot_id, pv$species_group))
  n <- length(cols$StartPos)
  data.frame(
    treid                 = rep(as.character(pv$treid), n),
    plot_id               = rep(as.character(pv$plot_id), n),
    species_group         = rep(pv$species_group, n),
    ProductKey            = cols$ProductKey,
    ProductType           = cols$ProductType,
    StartPos              = cols$StartPos,
    StopPos               = cols$StopPos,
    LogLength             = cols$LogLength,
    TopDiameter           = cols$TopDiameter,
    ButtDiameter          = cols$ButtDiameter,
    eligible              = cols$eligible,
    price_volume_m3       = cols$price_volume_m3,
    LengthClass_dm        = cols$LengthClass_dm,
    PriceDiameterClass_cm = cols$PriceDiameterClass_cm,   # uncapped; rename in valuation
    TopDiameter_ob        = cols$TopDiameter_ob,
    TopDiameter_ub        = cols$TopDiameter_ub,
    DiameterUnderBark     = cols$DiameterUnderBark,
    ProductPriority       = cols$ProductPriority,
    stringsAsFactors      = FALSE
  )
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
