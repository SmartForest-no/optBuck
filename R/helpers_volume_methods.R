# ==============================================================================
# R/helpers_volume_methods.R
#
# Purpose : Price volume of a log, per product (Phase 4b, 2026-10-13). The
#           method is ProductData$VolumeDiameterCategory, named after the
#           StanForD categories as implemented in optBuck's PriceVolumeCalc.R
#           (https://github.com/SmartForest-no/optBuck):
#
#   "Top cylinder"   Apter's sawlog rule (not a StanForD value; until Phase 4b
#                    labelled "Top"): cylinder at the top diameter,
#                    pi * (Dt/2)^2 * L.
#   "Top"            StanForD/optBuck "Top": frustum from the top diameter Dt
#                    to Db = Dt + 1 cm per metre of length (0.1 mm per cm),
#                    V = pi * L * (Dt^2 + Dt*Db + Db^2) / 12. optBuck had
#                    LogLength * 0.01 (a bug); 1 cm/m decided 2026-10-12.
#   "All diameters (solid volume)"
#                    Apter's solid volume: 10 cm cylinders at each section's
#                    top diameter, not rounded (section_volume_fun(),
#                    helpers_commercial_volume.R; decided 2026-10-12). Differs
#                    from optBuck, which sums one section too many and rounds.
#   "Calculated Norwegian mid"
#                    Dmid = Dt floored to whole cm + 0.5 cm (class midpoint)
#                    + 1 cm/m over half the length; V = pi/4 * Dmid^2 * L.
#                    In mm: Dmid = floor(Dt/10)*10 + 5 + L_cm/20. Not rounded
#                    to whole dm3. optBuck rounds Dt to whole mm and adds
#                    0.5 mm (units bug); Apter follows optBuck with the units
#                    fixed (Lennart, 2026-10-13), after FOR-1990-05-28-469
#                    5.1.1 ("nedslag til klassebunn") and 5.2.1 (mid diameter
#                    from the class midpoint and 1 cm/m taper). That
#                    regulation was repealed in 2001; check the current
#                    Maalereglement.
#
#   Dt is the product's measuring top diameter (mm): under or over bark per
#   DiameterUnderBark, read DiameterTopPosition cm below the cut (the
#   Norwegian rule measures 10 cm inside the top end).
#
#   VolumeDiameterAdjustment "Measured diameter rounded downwards to cm"
#   applies to Dt for "Top cylinder" and "Top". "Calculated Norwegian mid"
#   always floors (part of the rule). Solid volume never rounds; the
#   combination is rejected by validate_products().
#
#   VolumeLengthCategory has no effect: Apter cuts logs at exactly their
#   class length (no trim allowance), so physical, rounded-down and
#   length-class lengths are the same.
#
#   The three formula methods are rounded to 5 decimals (m3), as the sawlog
#   top cylinder always was; solid volume is not rounded.
# ==============================================================================

#' Price-volume methods (ProductData$VolumeDiameterCategory).
VOLUME_METHODS <- c("Top cylinder", "Top", "All diameters (solid volume)",
                    "Calculated Norwegian mid")

#' ProductData$VolumeDiameterAdjustment values.
VOLUME_ROUND_DOWN_CM <- "Measured diameter rounded downwards to cm"
VOLUME_DIAMETER_ADJUSTMENTS <- c("None", VOLUME_ROUND_DOWN_CM)

#' Frustum / Norwegian mid taper: 1 cm per metre = 0.1 mm per cm of length.
VOLUME_TAPER_MM_PER_CM <- 0.1


#' Price volume (m3) of logs, vectorised over logs of one product.
#'
#' @param method      One of VOLUME_METHODS.
#' @param top_d_mm    Measuring top diameter (mm).
#' @param length_cm   Log length (cm).
#' @param start_pos,stop_pos Log positions (cm); used by solid volume only.
#' @param solid_vol   function(start, stop) from section_volume_fun() on the
#'                    product's diameter series; used by solid volume only.
#' @param adjustment  VolumeDiameterAdjustment (NA/NULL = "None").
#' @return Numeric vector (m3). NA where the top diameter is NA or <= 0
#'   (formula methods).
price_volume_m3 <- function(method, top_d_mm, length_cm,
                            start_pos = NULL, stop_pos = NULL,
                            solid_vol = NULL, adjustment = "None") {
  if (length(method) != 1L || !method %in% VOLUME_METHODS)
    stop("Unknown volume method '", paste(method, collapse = ", "),
         "' (expected one of: ", paste(VOLUME_METHODS, collapse = ", "), ").")
  if (method == "All diameters (solid volume)") {
    if (is.null(solid_vol)) stop("Solid volume needs solid_vol().")
    return(solid_vol(start_pos, stop_pos))
  }
  round_down <- !is.null(adjustment) && length(adjustment) == 1L &&
    !is.na(adjustment) && adjustment == VOLUME_ROUND_DOWN_CM
  dt <- if (round_down || method == "Calculated Norwegian mid")
    floor(top_d_mm / 10) * 10 else top_d_mm
  L  <- length_cm
  v <- switch(method,
    "Top cylinder" = pi * (dt / 2)^2 * L / 1e8,
    "Top" = {
      db <- dt + VOLUME_TAPER_MM_PER_CM * L
      pi * L * (dt^2 + dt * db + db^2) / 12 / 1e8
    },
    "Calculated Norwegian mid" = {
      dmid <- dt + 5 + VOLUME_TAPER_MM_PER_CM * L / 2
      pi / 4 * dmid^2 * L / 1e8
    })
  ifelse(!is.na(top_d_mm) & dt > 0, round(v, 5), NA_real_)
}
