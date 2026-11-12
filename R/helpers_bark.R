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

# The bark function of an .hpr species group (2026-11-11, decided by Lennart): StanForD "Swedish Zacco" gives double
# bark (mm) = ConstantA + FactorB * d_ob (mm), "None" gives 0. Both reproduce the recorded log diameters of the
# owner's files (residual 0.3 mm = rounding). Any other category (e.g. "Skogforsk 2004", which also depends on the
# position on the stem) or none is not supported: a = b = NA and barkNOR is used for the stem.
#
# @param sp species table with bark_category, bark_a, bark_b.
# @return sp with bark_a / bark_b set to 0 for "None" and NA for unsupported categories.
#
# "Skogforsk 2004, Norway spruce" (2026-11-12): the published spruce function of Hannrup (2004), see
# skogforsk2004_spruce_double_bark(). It reproduces the recorded Top/Mid/Butt ub diameters of the owner's
# files within rounding (sd 0.29 mm, max 0.56 mm; audit-style check in the PLAN). "Skogforsk 2004, Scots pine"
# was tested the same way and does NOT reproduce them (residual sd 0.8-1.3 mm, up to 10 mm on stems of
# the owner's files), so it is not implemented: pine stays on barkNOR with the tab-1 message.
#
# bark_fn: "ab" (a + b * d_ob: Zacco, None), "skf_spruce", or NA (barkNOR).
HPR_BARK_SUPPORTED <- c("Swedish Zacco", "None", "Skogforsk 2004, Norway spruce")

# Hannrup, B. (2004). Funktioner for skattning av barkens tjocklek hos tall och gran vid avverkning med
# skordare. Skogforsk Arbetsrapport 575, p. 32 (Sf_gran): reldia = dia / dbh,
# db = 0.46146 + 0.01386 * dbh + 0.03571 * dbh * reldia, db = max(db, 2); db = double bark (mm),
# dia = over-bark diameter (mm), dbh = over-bark diameter at breast height (mm).
skogforsk2004_spruce_double_bark <- function(d_ob_mm, dbh_mm) {
  pmax(0.46146 + 0.01386 * dbh_mm + 0.03571 * dbh_mm * (d_ob_mm / dbh_mm), 2)
}

hpr_bark_columns <- function(sp) {
  cat <- as.character(sp$bark_category)
  none <- cat %in% "None"
  zacco <- cat %in% "Swedish Zacco" & is.finite(sp$bark_a) & is.finite(sp$bark_b)
  skf <- cat %in% "Skogforsk 2004, Norway spruce"
  sp$bark_a <- ifelse(none, 0, ifelse(zacco, sp$bark_a, NA_real_))
  sp$bark_b <- ifelse(none, 0, ifelse(zacco, sp$bark_b, NA_real_))
  sp$bark_fn <- ifelse(none | zacco, "ab", ifelse(skf, "skf_spruce", NA_character_))
  sp
}

# Add the under-bark profile (DiameterValueUB, mm) to a profile vector.
#
# Uses the measured over-bark dbh (pv$DBH, mm) when present, which is also
# the dbh taperNOR uses; otherwise reads dbh from the profile at 1.3 m.
#
# @param pv    Profile vector from profiles_to_profile_vectors().
# @param model Bark model function (default: get_bark_model()).
add_under_bark_profile <- function(pv, model = get_bark_model()) {
  # An .hpr stem whose file has a supported bark function: the harvester's own deduction.
  if (identical(pv$bark_fn, "skf_spruce") && is.finite(pv$DBH) && pv$DBH > 0) {
    pv$DiameterValueUB <- pmax(pv$DiameterValue - skogforsk2004_spruce_double_bark(pv$DiameterValue, pv$DBH), 0)
    return(pv)
  }
  ab <- pv$bark_ab
  if (length(ab) == 2L && all(is.finite(ab))) {
    pv$DiameterValueUB <- pmax(pv$DiameterValue - pmax(ab[1L] + ab[2L] * pv$DiameterValue, 0), 0)
    return(pv)
  }
  dbh_mm <- pv$DBH
  if (is.null(dbh_mm) || length(dbh_mm) != 1L || is.na(dbh_mm))
    dbh_mm <- dbh_from_profile(pv$diameterPosition, pv$DiameterValue)
  pv$DiameterValueUB <- under_bark_diameter(pv$DiameterValue, dbh_mm,
                                            pv$species_group, model)
  pv
}
