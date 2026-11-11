# ==============================================================================
# R/helpers_volume_methods.R
#
# Purpose : Price volume of a log, per product (Phase 4b, 2026-10-13). The
#           method is ProductData$VolumeDiameterCategory, named after the
#           StanForD categories as implemented in optBuck's PriceVolumeCalc.R
#           (https://github.com/SmartForest-no/optBuck):
#
#   "Top cylinder"   optBuck's sawlog rule (not a StanForD value; until Phase 4b
#                    labelled "Top"): cylinder at the top diameter,
#                    pi * (Dt/2)^2 * L.
#   "Top"            StanForD/optBuck "Top": frustum from the top diameter Dt
#                    to Db = Dt + 1 cm per metre of length (0.1 mm per cm),
#                    V = pi * L * (Dt^2 + Dt*Db + Db^2) / 12. optBuck had
#                    LogLength * 0.01 (a bug); 1 cm/m decided 2026-10-12.
#   "All diameters (solid volume)"
#                    optBuck's solid volume: 10 cm cylinders at each section's
#                    top diameter, not rounded (section_volume_fun(),
#                    helpers_commercial_volume.R; decided 2026-10-12). Differs
#                    from optBuck, which sums one section too many and rounds.
#   "Calculated Norwegian mid"
#                    Maalereglement for skogsvirke, Generelle bestemmelser
#                    (Norsk Virkesmaaling, approved 15 April 2026), sections
#                    4.1.3-4.1.4: Dt measured 10 cm from the top end, rounded
#                    down to whole cm, class lower boundary + 0.5 cm, taper
#                    1.0 cm/m; V = 3.14/4 * (Dt + L/20)^2 * L/100 (Dt cm, L dm,
#                    V dm3). In mm: Dmid = floor(Dt/10)*10 + 5 + L_cm/20,
#                    V = 3.14/4 * Dmid^2 * L_cm / 1e8 m3. Uses 3.14, not pi,
#                    to reproduce the official volume (Lennart, 2026-10-13);
#                    not rounded to whole dm3. optBuck rounds Dt to whole mm
#                    and adds 0.5 mm (units bug); optBuck follows optBuck with
#                    the units fixed. Set DiameterTopPosition = 10 for the
#                    rule's measuring point.
#   "Mid"            StanForD 2010 VolumeDiameterCategory 3 (2026-10-21):
#                    cylinder at the diameter measured at the middle of the
#                    log, pi/4 * Dmid^2 * L. Dmid is read from the stem
#                    profile (under bark) at the physical middle of the cut
#                    log, (StartPos + StopPos)/2, not calculated from the top;
#                    rounded down to whole cm, no class midpoint added. L is
#                    the price length (the cut, or the class lower limit for
#                    hpr products priced on length classes). optBuck has no
#                    Mid branch. The top diameter still drives the limits and
#                    the price class.
#
#   Dt is the product's measuring top diameter (mm): under or over bark per
#   DiameterUnderBark, read DiameterTopPosition cm below the cut (the
#   Norwegian rule measures 10 cm inside the top end).
#
#   VolumeDiameterAdjustment "Measured diameter rounded downwards to cm"
#   applies to Dt for "Top cylinder" and "Top", and to Dmid for "Mid".
#   "Calculated Norwegian mid" always floors (part of the rule). Solid volume
#   never rounds; the combination is rejected by validate_products().
#
#   VolumeLengthCategory has no effect: optBuck cuts logs at exactly their
#   class length (no trim allowance), so physical, rounded-down and
#   length-class lengths are the same.
#
#   The four formula methods are rounded to 5 decimals (m3), as the sawlog
#   top cylinder always was; solid volume is not rounded.
# ==============================================================================

# Price-volume methods (ProductData$VolumeDiameterCategory).
VOLUME_METHODS <- c("Top cylinder", "Top", "All diameters (solid volume)",
                    "Calculated Norwegian mid", "Mid")

# ProductData$VolumeDiameterAdjustment values.
VOLUME_ROUND_DOWN_CM <- "Measured diameter rounded downwards to cm"
VOLUME_DIAMETER_ADJUSTMENTS <- c("None", VOLUME_ROUND_DOWN_CM)

# The diameter rounding of each volume method (Phase 4d: fixed per method,
# no longer a setting): Top cylinder, Top, Norwegian mid and Mid round down
# to whole cm; solid volume never rounds.
volume_rounding <- function(method) {
  ifelse(as.character(method) == "All diameters (solid volume)", "None", VOLUME_ROUND_DOWN_CM)
}

# The diameter rounding of each product (2026-11-11): the product's own VolumeDiameterAdjustment (from the
# .hpr/.pin file) where it has one, else the method's default; solid volume never rounds.
volume_adjustment_of <- function(method, stored = NA) {
  d <- volume_rounding(method)
  st <- as.character(stored); ok <- !is.na(st) & st %in% VOLUME_DIAMETER_ADJUSTMENTS
  out <- ifelse(ok, st, d)
  ifelse(as.character(method) == "All diameters (solid volume)", "None", out)
}

# Short labels of the volume methods for compact tables.
VOLUME_METHOD_SHORT <- c("Top cylinder" = "Top cylinder", "Top" = "Top (frustum)",
                         "All diameters (solid volume)" = "Solid volume",
                         "Calculated Norwegian mid" = "Norwegian mid", "Mid" = "Mid")

# Frustum / Norwegian mid taper: 1 cm per metre = 0.1 mm per cm of length.
VOLUME_TAPER_MM_PER_CM <- 0.1

# The Maalereglement's pi for Norwegian mid (reproduces the official
# volume); the other methods use pi.
VOLUME_NORWEGIAN_PI <- 3.14


# Price volume (m3) of logs, vectorised over logs of one product.
#
# @param method      One of VOLUME_METHODS.
# @param top_d_mm    Measuring top diameter (mm).
# @param length_cm   Log length (cm).
# @param start_pos,stop_pos Log positions (cm); used by solid volume only.
# @param solid_vol   function(start, stop) from section_volume_fun() on the
#                    product's diameter series; used by solid volume only.
# @param adjustment  VolumeDiameterAdjustment (NA/NULL = "None").
# @param mid_d_mm    Diameter (mm) at the middle of the log; Mid only.
# @return Numeric vector (m3). NA where the measuring diameter (top, or mid
#   for Mid) is NA or <= 0 (formula methods).
price_volume_m3 <- function(method, top_d_mm, length_cm,
                            start_pos = NULL, stop_pos = NULL,
                            solid_vol = NULL, adjustment = "None",
                            mid_d_mm = NULL) {
  if (length(method) != 1L || !method %in% VOLUME_METHODS)
    stop("Unknown volume method '", paste(method, collapse = ", "),
         "' (expected one of: ", paste(VOLUME_METHODS, collapse = ", "), ").")
  if (method == "All diameters (solid volume)") {
    if (is.null(solid_vol)) stop("Solid volume needs solid_vol().")
    return(solid_vol(start_pos, stop_pos))
  }
  round_down <- !is.null(adjustment) && length(adjustment) == 1L &&
    !is.na(adjustment) && adjustment == VOLUME_ROUND_DOWN_CM
  if (method == "Mid") {
    if (is.null(mid_d_mm)) stop("Mid needs mid_d_mm (the diameter at the middle of the log).")
    dm <- if (round_down) floor(mid_d_mm / 10) * 10 else mid_d_mm
    v  <- pi * (dm / 2)^2 * length_cm / 1e8
    return(ifelse(!is.na(mid_d_mm) & dm > 0, round(v, 5), NA_real_))
  }
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
      VOLUME_NORWEGIAN_PI / 4 * dmid^2 * L / 1e8
    })
  ifelse(!is.na(top_d_mm) & dt > 0, round(v, 5), NA_real_)
}
