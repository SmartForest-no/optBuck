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
#' The number of plots and the seed are fixed (Phase 4c): APP_N_PLOTS (200,
#' or APTER_N_PLOTS) and APP_SEED (42), from setup.R.
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
    n_plots       = as.integer(n_plots),
    seed          = as.integer(seed)
  )
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
