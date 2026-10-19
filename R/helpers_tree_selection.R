# ==============================================================================
# R/helpers_tree_selection.R
#
# Purpose : Which NFI trees a run bucks (steps 3-5 of prepare_sampled_job(),
#           moved here unchanged in the layout-and-figures branch): stratum
#           filter, plot selection (all plots, or a seeded sample of
#           n_plots), and the dbh/height filter. The backend and the app's
#           live tree counts call the same function, so the counts are the
#           trees the run will buck.
#
# Errors  : none; an empty selection returns zero rows. prepare_sampled_job()
#           raises its own messages.
# ==============================================================================

if (!exists("or_default", mode = "function"))   # helpers_utils.R
  source(file.path(dirname(if (is.null(sys.frame(1L)$ofile)) "." else sys.frame(1L)$ofile),
                   "helpers_utils.R"))

NFI_MIN_DBH_CM   <- 5.0     # taperNOR validity filter (script 11 convention)
NFI_MIN_HEIGHT_M <- 4.5


#' Select the NFI trees of a run.
#'
#' @param trees_all NFI tree table (nfi_tree_plot_stratum_ready_github.parquet)
#'   with at least plot_id_github, treid, species_group, hogstkl, abonitet,
#'   fylnr, dbh_cm and beregnet_hoyde (or trehoyde); hoh for an elevation filter.
#' @param params Backend params (build_run_params() in the app): species_group,
#'   hogstkl, abonitet; optional fylnr, hoh_min, hoh_max, all_plots, n_plots
#'   (default 10, max 50), seed (default 42).
#' @return list(trees = the selected trees with height_m, plots = the selected
#'   plot ids, eligible_plots, n_plots_req, n_trees_before_filter,
#'   t_filter_sample, t_tree_filter (s)).
select_nfi_trees <- function(trees_all, params) {
  .el <- function(t0) as.numeric((proc.time() - t0)["elapsed"])
  sg           <- params$species_group
  hogstkl_vec  <- as.integer(params$hogstkl)
  abonitet_vec <- as.integer(params$abonitet)
  all_plots    <- isTRUE(params$all_plots)
  n_plots_req  <- min(as.integer(or_default(params$n_plots, 10L)), 50L)
  seed_val     <- as.integer(or_default(params$seed, 42L))
  fylnr_vec    <- params$fylnr
  hoh_min_val  <- params$hoh_min
  hoh_max_val  <- params$hoh_max

  # ---- stratum ------------------------------------------------------------------
  t0_filter_sample <- proc.time()
  sg_filter <- if (identical(sg, "both")) c("spruce", "pine") else sg

  trees_filt <- trees_all[
    trees_all$species_group %in% sg_filter &
    trees_all$hogstkl       %in% hogstkl_vec &
    trees_all$abonitet      %in% abonitet_vec, , drop = FALSE]

  if (!is.null(fylnr_vec) && length(fylnr_vec) > 0L)
    trees_filt <- trees_filt[
      trees_filt$fylnr %in% as.character(fylnr_vec), , drop = FALSE]

  if (!is.null(hoh_min_val))
    trees_filt <- trees_filt[
      !is.na(trees_filt$hoh) & trees_filt$hoh >= hoh_min_val, , drop = FALSE]

  if (!is.null(hoh_max_val))
    trees_filt <- trees_filt[
      !is.na(trees_filt$hoh) & trees_filt$hoh <= hoh_max_val, , drop = FALSE]

  eligible_plots <- sort(unique(trees_filt$plot_id_github))

  # ---- complete plots (or all of them) ---------------------------------------------
  if (all_plots || length(eligible_plots) == 0L) {
    n_plots_req   <- length(eligible_plots)
    sampled_plots <- eligible_plots
  } else {
    set.seed(seed_val)
    sampled_plots <- sort(sample(eligible_plots, min(n_plots_req, length(eligible_plots)),
                                 replace = FALSE))
  }
  trees_sampled <- trees_filt[
    trees_filt$plot_id_github %in% sampled_plots, , drop = FALSE]
  t_filter_sample <- .el(t0_filter_sample)

  # ---- height_m and the taperNOR validity filter -----------------------------------
  # beregnet_hoyde (dm) / 10, as in script 11. No tilstand filter.
  t0_tree_filter <- proc.time()
  if ("beregnet_hoyde" %in% names(trees_sampled)) {
    trees_sampled$height_m <- as.numeric(trees_sampled$beregnet_hoyde) / 10
  } else if ("trehoyde" %in% names(trees_sampled)) {
    trees_sampled$height_m <- as.numeric(trees_sampled$trehoyde) / 10
  } else {
    stop("No height column found (expected beregnet_hoyde or trehoyde).")
  }

  trees_valid <- trees_sampled[
    !is.na(trees_sampled$dbh_cm)   & trees_sampled$dbh_cm   >= NFI_MIN_DBH_CM &
    !is.na(trees_sampled$height_m) & trees_sampled$height_m >= NFI_MIN_HEIGHT_M,
    , drop = FALSE]

  list(trees = trees_valid, plots = sampled_plots, eligible_plots = eligible_plots,
       n_plots_req = n_plots_req, n_trees_before_filter = nrow(trees_sampled),
       t_filter_sample = t_filter_sample, t_tree_filter = .el(t0_tree_filter))
}
