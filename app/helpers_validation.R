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


#' Map sidebar inputs to the params list for run_sampled_plot_bucking().
#'
#' NFI runs use all plots of the stratum (Phase 4d): APP_N_PLOTS is NA,
#' unless the test-only APTER_N_PLOTS sets a number of plots to sample with
#' APP_SEED (42); setup.R.
#'
#' @param input_list Shiny `input` (or a plain list) with species_group,
#'   hkl_group, abonitet, fylnr_sel.
#' @param hkl_map    User-facing HKL group -> raw hogstkl codes.
#' @return Named list of backend params.
build_run_params <- function(input_list, hkl_map = HKL_MAP,
                             n_plots = APP_N_PLOTS, seed = APP_SEED) {
  list(
    species_group = input_list$species_group,
    hogstkl       = unlist(hkl_map[input_list$hkl_group]),
    abonitet      = as.integer(input_list$abonitet),
    fylnr         = selected_fylnr(input_list$fylnr_sel),
    all_plots     = is.na(n_plots),          # Phase 4d: every plot of the stratum
    n_plots       = if (!is.na(n_plots)) as.integer(n_plots),
    seed          = as.integer(seed)
  )
}


#' The trees a run will buck, counted for the sidebar: "655 plots · 11,890
#' spruce · 537 pine trees". For NFI data `trees` is select_nfi_trees()$trees
#' (the backend's own selection) and `n_plots` its plots; for a tree list the
#' rows prepare_tree_list_job() bucks (in_range) and their distinct plot_id.
#'
#' @return list(n_plots, n_trees, by_species (named counts), text).
tree_counts <- function(species, n_plots = NA_integer_) {
  by <- vapply(c("spruce", "pine"), function(s) sum(species == s), integer(1))
  by <- by[by > 0L]
  fmt <- function(n) format(n, big.mark = ",")
  parts <- c(if (!is.na(n_plots)) sprintf("%s plot%s", fmt(n_plots), if (n_plots == 1L) "" else "s"),
             sprintf("%s %s", fmt(by), names(by)))
  if (length(by)) parts[length(parts)] <- paste(parts[length(parts)],
                                                 if (sum(by) == 1L) "tree" else "trees")
  list(n_plots = n_plots, n_trees = sum(by), by_species = by,
       text = if (sum(by) == 0L) "No trees" else paste(parts, collapse = " · "))
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
filter_stratum_plots <- function(plots_prod, hkl_raw, abon = NULL, fyl = NULL) {
  keep <- plots_prod$hogstkl %in% hkl_raw
  if (!is.null(abon)) keep <- keep & plots_prod$abonitet %in% abon
  if (!is.null(fyl))  keep <- keep & plots_prod$fylnr %in% fyl
  plots_prod[keep, , drop = FALSE]
}


#' Number of eligible plots in the selected stratum.
count_eligible_plots <- function(plots_prod, hkl_raw, abon, fyl) {
  nrow(filter_stratum_plots(plots_prod, hkl_raw, abon, fyl))
}
