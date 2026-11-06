# ==============================================================================
# R/helpers_commercial_volume.R
#
# Purpose : The NOK/m3 denominator: commercial solid volume under bark
#           (docs/plans/PLAN.md, "Value denominator").
#
# Definition:
#   - Commercial stem: the parts of the stem allocated to non-waste products.
#     Excludes bark, the stump, the top above the last merchantable log, waste
#     logs, and any unallocated gaps.
#   - Volume: sum of 10 cm cylinders along the commercial stem; each cylinder
#     uses the under-bark diameter at the TOP of its 10 cm section.
#   - Independent of how products are priced or measured: this function takes
#     no prices and no product volume rules (sawlog top-cylinder price volume
#     does not change the denominator).
#
# Usage   : logs$commercial_volume_m3 <- commercial_volume_ub(
#             pv$diameterPosition, pv$DiameterValueUB, logs)
# ==============================================================================


# optBuck's "solid volume" rule, shared by the NOK/m3 denominator and the
# pulpwood/waste price volume (helpers_candidate_generation.R), so the two
# cannot diverge (decided 2026-10-12, see CLAUDE.md).
#
# Solid volume between two positions = the sum of 10 cm cylinders, each
# using the diameter at the TOP of its section, not rounded. Pass under-bark
# diameters for under-bark volume.
#
# @param positions_cm Profile positions (cm), ascending.
# @param d_mm         Diameters at those positions (mm).
# @param step_cm      Section length (cm).
# @return function(pos_a, pos_b), vectorised: volume (m3) between pos_a and
#   pos_b (cm, multiples of step_cm, within the profile). Uses a cumulative
#   sum over the profile grid, so it is fast for many candidates.
section_volume_fun <- function(positions_cm, d_mm, step_cm = 10L) {
  step_cm <- as.integer(step_cm)
  grid_max <- as.integer(floor(max(positions_cm) / step_cm) * step_cm)
  tops     <- seq(step_cm, grid_max, by = step_cm)
  d_top    <- stats::approx(positions_cm, d_mm, xout = tops, rule = 2)$y
  cumvol   <- c(0, cumsum(pi * (d_top / 2)^2 * step_cm / 1e8))   # mm^2 * cm -> m3
  function(pos_a, pos_b) {
    if (any(pos_a %% step_cm != 0 | pos_b %% step_cm != 0))
      stop("Solid volume: positions must be multiples of ", step_cm, " cm.")
    if (any(pos_a < 0 | pos_b > grid_max))
      stop("Solid volume: positions outside the profile (0 - ", grid_max, " cm).")
    cumvol[as.integer(pos_b %/% step_cm) + 1L] - cumvol[as.integer(pos_a %/% step_cm) + 1L]
  }
}


# Commercial volume under bark (m3) per log.
#
# @param positions_cm Profile positions (cm), ascending.
# @param d_ub_mm      Under-bark diameters at those positions (mm).
# @param logs         data.frame with StartPos, StopPos (cm) and ProductType.
#                     Rows with ProductType "waste" contribute 0.
# @param step_cm      Section length (cm); StartPos/StopPos must lie on it.
# @return Numeric vector, one commercial volume (m3) per row of `logs`.
commercial_volume_ub <- function(positions_cm, d_ub_mm, logs, step_cm = 10L) {
  if (nrow(logs) == 0L) return(numeric(0))
  vol <- section_volume_fun(positions_cm, d_ub_mm, step_cm)
  v   <- vol(logs$StartPos, logs$StopPos)
  v[as.character(logs$ProductType) == "waste" | logs$StopPos <= logs$StartPos] <- 0
  v
}


# NOK per m3 of commercial volume: total, by species and by assortment.
#
# Total and each species use their own commercial volume as denominator.
# Within a scope, an assortment's contribution is its value divided by the
# scope's (not the assortment's) commercial volume, so contributions add up
# to the scope's NOK/m3. Assortment = ProductType for the Total scope,
# ProductKey within a species. Waste counts in value (0 NOK today) but never
# in volume.
#
# @param logs data.frame of selected logs with species_group, ProductType,
#   ProductKey, value_nok, commercial_volume_m3.
# @param trees data.frame with one row per tree (species_group), used for
#   tree counts including trees without logs.
# @return list(summary = data.frame(scope, n_trees, value_nok,
#   commercial_volume_m3, nok_per_m3), by_assortment = data.frame(scope,
#   assortment, value_nok, commercial_volume_m3, nok_per_m3_contribution,
#   volume_share)).
summarise_per_m3 <- function(logs, trees) {
  per_m3 <- function(v, m3) if (m3 > 0) v / m3 else NA_real_
  scopes <- c("Total", sort(unique(as.character(trees$species_group))))

  summary <- do.call(rbind, lapply(scopes, function(s) {
    l  <- if (s == "Total") logs else logs[logs$species_group == s, , drop = FALSE]
    nt <- if (s == "Total") nrow(trees) else sum(trees$species_group == s)
    v  <- sum(l$value_nok, na.rm = TRUE)
    m3 <- sum(l$commercial_volume_m3, na.rm = TRUE)
    data.frame(scope = s, n_trees = nt, value_nok = v,
               commercial_volume_m3 = m3, nok_per_m3 = per_m3(v, m3),
               stringsAsFactors = FALSE)
  }))

  by_assortment <- do.call(rbind, lapply(scopes, function(s) {
    l   <- if (s == "Total") logs else logs[logs$species_group == s, , drop = FALSE]
    if (nrow(l) == 0L) return(NULL)
    key <- if (s == "Total") l$ProductType else l$ProductKey
    m3_scope <- sum(l$commercial_volume_m3, na.rm = TRUE)
    do.call(rbind, lapply(sort(unique(key)), function(a) {
      la <- l[key == a, , drop = FALSE]
      v  <- sum(la$value_nok, na.rm = TRUE)
      m3 <- sum(la$commercial_volume_m3, na.rm = TRUE)
      data.frame(scope = s, assortment = a, value_nok = v,
                 commercial_volume_m3 = m3,
                 nok_per_m3_contribution = per_m3(v, m3_scope),
                 volume_share = if (m3_scope > 0) m3 / m3_scope else NA_real_,
                 stringsAsFactors = FALSE)
    }))
  }))
  if (is.null(by_assortment))
    by_assortment <- data.frame(scope = character(0), assortment = character(0),
                                value_nok = numeric(0),
                                commercial_volume_m3 = numeric(0),
                                nok_per_m3_contribution = numeric(0),
                                volume_share = numeric(0),
                                stringsAsFactors = FALSE)
  list(summary = summary, by_assortment = by_assortment)
}
