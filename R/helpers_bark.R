# ==============================================================================
# R/helpers_bark.R
#
# Purpose : Swappable bark model: over-bark -> under-bark diameters along the
#           whole stem, for every tree source (NFI taperNOR profiles now;
#           uploaded tree lists and over-bark stem profiles later).
#
# Default : taperNOR::barkNOR() (Hannrup 2004 via Stangle et al. 2017, as
#           packaged in taperNOR). Double bark B (mm) = b0 + b1*dbh + b2*d,
#           with d the local over-bark diameter and dbh the tree's over-bark
#           dbh, both in cm. barkNOR's help page says the result is in cm,
#           but taperNOR(with_bark = FALSE) and the help example both use
#           d - B/10, so it is mm.
#
# Floor   : B >= 0, so the under-bark diameter never exceeds the over-bark
#           diameter. barkNOR's pine coefficients make B negative near the
#           top of large pines (e.g. d < 12.6 cm for dbh 45 cm).
#           PROVISIONAL (decided 2026-10-12), pending clarification from the
#           taperNOR authors. See docs/plans/PLAN.md, open questions.
#
# Swap    : register a function(d_cm, dbh_cm, species_group) -> double bark
#           in mm in BARK_MODELS and select it with
#           options(optbuck.bark_model = "<name>").
#
# Usage   : pv <- add_under_bark_profile(pv)        # adds pv$DiameterValueUB
#           d_ub <- under_bark_diameter(d_ob_mm, dbh_ob_mm, "pine")
# ==============================================================================


# barkNOR as an optBuck bark model.
#
# taperNOR::barkNOR() type-checks h and h_top but does not use them
# (verified 2026-10-12), so they are passed as numeric placeholders.
#
# @param d_cm          Over-bark diameter(s) along the stem (cm).
# @param dbh_cm        Tree dbh over bark (cm), scalar.
# @param species_group "spruce" or "pine".
# @return Double bark thickness (mm), unfloored.
bark_model_barkNOR <- function(d_cm, dbh_cm, species_group) {
  if (!requireNamespace("taperNOR", quietly = TRUE))
    stop("Package 'taperNOR' is required for the barkNOR bark model.")
  taperNOR::barkNOR(d = as.numeric(d_cm), h = 0, dbh = as.numeric(dbh_cm),
                    h_top = 1, sp = species_group)
}

# Registry of bark models: name -> function(d_cm, dbh_cm, species_group) -> mm. An environment, so
# that a model can be registered from outside the package (BARK_MODELS$name <- fn) also when installed.
BARK_MODELS <- new.env(parent = emptyenv())
BARK_MODELS$barkNOR <- bark_model_barkNOR

# Name of the default bark model.
DEFAULT_BARK_MODEL <- "barkNOR"

# The active bark model function (option optbuck.bark_model, else barkNOR).
get_bark_model <- function(name = getOption("optbuck.bark_model", DEFAULT_BARK_MODEL)) {
  fn <- BARK_MODELS[[name]]
  if (!is.function(fn))
    stop("Unknown bark model '", name, "'. Registered: ",
         paste(sort(names(BARK_MODELS)), collapse = ", "))
  fn
}


# Double bark thickness (mm), floored at 0 (provisional; see header).
#
# @param d_ob_mm   Over-bark diameter(s) (mm).
# @param dbh_ob_mm Tree dbh over bark (mm), scalar.
# @param species_group "spruce" or "pine".
# @param model     Bark model function (default: get_bark_model()).
double_bark_mm <- function(d_ob_mm, dbh_ob_mm, species_group,
                           model = get_bark_model()) {
  b <- model(d_ob_mm / 10, dbh_ob_mm / 10, species_group)
  pmax(b, 0)
}

# Under-bark diameter(s) (mm): d_ob - B, never below 0 or above d_ob.
under_bark_diameter <- function(d_ob_mm, dbh_ob_mm, species_group,
                                model = get_bark_model()) {
  pmax(d_ob_mm - double_bark_mm(d_ob_mm, dbh_ob_mm, species_group, model), 0)
}

# Over-bark dbh (mm) read from a profile at 1.3 m (linear interpolation).
# For profiles without a measured dbh (imported stem profiles).
dbh_from_profile <- function(positions_cm, d_ob_mm) {
  stats::approx(positions_cm, d_ob_mm, xout = 130, rule = 2)$y
}

# Add the under-bark profile (DiameterValueUB, mm) to a profile vector.
#
# Uses the measured over-bark dbh (pv$DBH, mm) when present, which is also
# the dbh taperNOR uses; otherwise reads dbh from the profile at 1.3 m.
#
# @param pv    Profile vector from profiles_to_profile_vectors().
# @param model Bark model function (default: get_bark_model()).
add_under_bark_profile <- function(pv, model = get_bark_model()) {
  dbh_mm <- pv$DBH
  if (is.null(dbh_mm) || length(dbh_mm) != 1L || is.na(dbh_mm))
    dbh_mm <- dbh_from_profile(pv$diameterPosition, pv$DiameterValue)
  pv$DiameterValueUB <- under_bark_diameter(pv$DiameterValue, dbh_mm,
                                            pv$species_group, model)
  pv
}
