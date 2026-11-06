# app/helpers_validation.R
#
# Purpose : Pure helpers that turn sidebar inputs into backend params and
#           stratum counts. No reactivity; testable without a browser
#           (see audits/audit_17_app_smoke.R).
#           HKL_MAP comes from setup.R.


#' County selection -> fylnr filter. "__all__" (or NULL) means no filter.
selected_fylnr <- function(fylnr_sel) {
  if (!is.null(fylnr_sel) && fylnr_sel != "__all__") fylnr_sel else NULL
}


#' Altitude slider -> c(min, max) in m, or NULL for no filter. The full
#' range (HOH_RANGE, setup.R) or a missing input means no filter, so plots
#' with missing altitude stay in; a narrowed range excludes them
#' (select_nfi_trees()).
selected_hoh <- function(hoh_range, full = HOH_RANGE) {
  if (is.null(hoh_range) || length(hoh_range) != 2L) return(NULL)
  r <- sort(as.numeric(hoh_range))
  if (r[1L] <= full[1L] && r[2L] >= full[2L]) NULL else r
}


#' Map sidebar inputs to the params list for run_sampled_plot_bucking().
#'
#' NFI runs use all plots of the stratum (Phase 4d): APP_N_PLOTS is NA,
#' unless the test-only OPTBUCK_N_PLOTS sets a number of plots to sample with
#' APP_SEED (42); setup.R.
#'
#' @param input_list Shiny `input` (or a plain list) with species_group,
#'   hkl_group, abonitet, fylnr_sel; optional hoh_range (altitude slider).
#' @param hkl_map    User-facing HKL group -> raw hogstkl codes.
#' @return Named list of backend params. hoh_min/hoh_max are added only for
#'   a narrowed altitude range, so the full range gives the same params as
#'   before the altitude filter.
build_run_params <- function(input_list, hkl_map = HKL_MAP,
                             n_plots = APP_N_PLOTS, seed = APP_SEED) {
  p <- list(
    species_group = input_list$species_group,
    hogstkl       = unlist(hkl_map[input_list$hkl_group]),
    abonitet      = as.integer(input_list$abonitet),
    fylnr         = selected_fylnr(input_list$fylnr_sel),
    all_plots     = is.na(n_plots),          # Phase 4d: every plot of the stratum
    n_plots       = if (!is.na(n_plots)) as.integer(n_plots),
    seed          = as.integer(seed)
  )
  hoh <- selected_hoh(input_list$hoh_range)
  if (!is.null(hoh)) {
    p$hoh_min <- hoh[1L]
    p$hoh_max <- hoh[2L]
  }
  p
}


#' The trees a run will buck, counted for the sidebar: "655 plots · 11,890
#' spruce · 537 pine trees". For NFI data `trees` is select_nfi_trees()$trees
#' (the backend's own selection) and `n_plots` its plots; for a tree list the
#' rows prepare_tree_list_job() bucks (in_range) and their distinct plot_id.
#'
#' Each species' part is a text of the translation table, by species, unit,
#' number (one or more) and whether it ends the line: English names the
#' unit once, at the end ("11,890 spruce · 537 pine trees"); Norsk in every
#' part ("11 890 graner · 537 furuer").
#'
#' @param lang "en" (the text as before) or "no".
#' @return list(n_plots, n_trees, by_species (named counts), text).
tree_counts <- function(species, n_plots = NA_integer_, unit = c("tree", "stem"), lang = "en") {
  unit <- match.arg(unit)
  by <- vapply(c("spruce", "pine"), function(s) sum(species == s), integer(1))
  by <- by[by > 0L]
  fmt <- function(n) fmt_num(n, 0L, lang)
  # English puts the unit after the last species, plural unless the total is
  # one tree; Norsk inflects every species by its own count.
  last <- seq_along(by) == length(by)
  ids <- sprintf("tc_%s_%s%s%s", names(by), unit, ifelse(by == 1L, "", "s"),
                 ifelse(!last, "", ifelse(by == 1L & sum(by) > 1L, "_last_of_many", "_last")))
  parts <- c(if (!is.na(n_plots)) tr(if (n_plots == 1L) "tc_plot" else "tc_plots", fmt(n_plots), lang = lang),
             vapply(seq_along(by), function(i) tr(ids[i], fmt(by[[i]]), lang = lang), ""))
  list(n_plots = n_plots, n_trees = sum(by), by_species = by,
       text = if (sum(by) == 0L) tr(paste0("tc_none_", unit), lang = lang) else paste(parts, collapse = " · "))
}


#' A rough run time (s) for n trees on `workers` workers (benchmarks,
#' docs/development.md: about 30 trees/s per worker, 1.4 times longer for
#' a comparison, plus loading).
estimate_run_seconds <- function(n, workers, compare = FALSE) {
  20 + n / (30 * max(1L, workers)) * (if (compare) 1.4 else 1)
}


#' Plots matching raw hogstkl codes, and optionally abonitet and county.
#'
#' @param plots_prod Productive-forest plot metadata (setup.R).
#' @param hkl_raw    Raw hogstkl codes.
#' @param abon       Integer abonitet values, or NULL for no filter.
#' @param fyl        fylnr codes, or NULL for all counties.
#' @param hoh        c(min, max) altitude in m (selected_hoh()), or NULL for
#'   no filter; plots with missing altitude are excluded when set.
filter_stratum_plots <- function(plots_prod, hkl_raw, abon = NULL, fyl = NULL,
                                 hoh = NULL) {
  keep <- plots_prod$hogstkl %in% hkl_raw
  if (!is.null(abon)) keep <- keep & plots_prod$abonitet %in% abon
  if (!is.null(fyl))  keep <- keep & plots_prod$fylnr %in% fyl
  if (!is.null(hoh))  keep <- keep & !is.na(plots_prod$hoh) &
                        plots_prod$hoh >= hoh[1L] & plots_prod$hoh <= hoh[2L]
  plots_prod[keep, , drop = FALSE]
}


#' Number of eligible plots in the selected stratum.
count_eligible_plots <- function(plots_prod, hkl_raw, abon, fyl, hoh = NULL) {
  nrow(filter_stratum_plots(plots_prod, hkl_raw, abon, fyl, hoh))
}
