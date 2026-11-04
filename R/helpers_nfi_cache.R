# ==============================================================================
# R/helpers_nfi_cache.R  (roadmap 8, step 6, 2026-11-04)
#
# Purpose : The NFI stem profiles (taperNOR profiles of the 101,156 usable trees of
#           data_processed_github/) are built on first use instead of by a separate script:
#           a run on the built-in NFI source that finds no cache builds it (about 3 minutes on
#           one core, 0.3 minutes on 8 workers, about 200 MB of parquet), with a progress
#           callback, and never at load/start. Nothing is built for tree lists or .hpr stems.
#           scripts/app_backend/00_precompute_taper_profiles_github.R still builds the same cache
#           (plus a single-file copy); both use generate_taper_profiles_for_trees() at the same
#           0.10 m step, so the profiles are identical (audit 36 check 14).
# The cache : outputs/app_cache/taper_profiles_by_plot_block_github/, a parquet dataset
#           partitioned by plot_block = "B%03d" of floor((plot number - 1) / 500).
# ==============================================================================

NFI_CACHE_PROFILE_STEP_M <- 0.10
NFI_CACHE_BLOCK_PLOTS    <- 500L
NFI_CACHE_COLS <- c("plot_id_github", "treid", "sesong", "species_group", "dbh_cm", "height_m",
                    "z_m", "diameter_cm", "DiameterValue", "diameterPosition", "DBH")

#' Folder of the partitioned profile cache.
nfi_cache_dir <- function(root = get_root_dir())
  file.path(root, "outputs", "app_cache", "taper_profiles_by_plot_block_github")

#' TRUE when the cache holds profile files.
nfi_profiles_ready <- function(root = get_root_dir()) {
  d <- nfi_cache_dir(root)
  dir.exists(d) && length(list.files(d, "[.]parquet$", recursive = TRUE)) > 0L
}

#' The trees to build profiles for (the backend's filter: DBH >= 5 cm, height >= 4.5 m, no tilstand
#' filter), in the layout generate_taper_profiles_for_trees() takes (plot_id, not plot_id_github).
#' @param plots NULL (all) or plot IDs (tests).
nfi_cache_tree_data <- function(root = get_root_dir(), plots = NULL) {
  f <- file.path(root, "data_processed_github", "nfi_tree_plot_stratum_ready_github.parquet")
  if (!file.exists(f)) stop("NFI tree table not found: ", f)
  tr <- as.data.frame(arrow::read_parquet(f))
  h <- as.numeric(if ("beregnet_hoyde" %in% names(tr)) tr$beregnet_hoyde else tr$trehoyde) / 10
  keep <- !is.na(tr$dbh_cm) & tr$dbh_cm >= 5 & !is.na(h) & h >= 4.5
  if (!is.null(plots)) keep <- keep & tr$plot_id_github %in% plots
  td <- data.frame(plot_id = tr$plot_id_github[keep], treid = tr$treid[keep], sesong = tr$sesong[keep],
                   species_group = tr$species_group[keep], dbh_cm = tr$dbh_cm[keep], height_m = h[keep],
                   stringsAsFactors = FALSE)
  td
}

#' Write profiles (generate_taper_profiles_for_trees() result) as the partitioned cache.
nfi_cache_write <- function(profiles, out_dir) {
  if (is.null(profiles) || nrow(profiles) == 0L) stop("No taper profiles to save.")
  names(profiles)[names(profiles) == "plot_id"] <- "plot_id_github"
  p <- profiles[, intersect(NFI_CACHE_COLS, names(profiles)), drop = FALSE]
  n <- as.integer(substr(p$plot_id_github, 2L, nchar(p$plot_id_github)))
  p$plot_block <- sprintf("B%03d", floor((n - 1L) / NFI_CACHE_BLOCK_PLOTS))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  arrow::write_dataset(dplyr::group_by(p, plot_block), path = out_dir, format = "parquet",
                       existing_data_behavior = "overwrite")
  invisible(nrow(p))
}

#' Build the profile cache (sequentially in chunks of `chunk` trees, or on the future workers).
#'
#' @param progress NULL or function(done, total) called after each chunk (trees).
#' @param plots,out_dir NULL / the default folder; tests build a few plots into a temporary folder.
build_nfi_cache <- function(root = get_root_dir(), out_dir = nfi_cache_dir(root), plots = NULL,
                            progress = NULL, chunk = 5000L, workers = 1L) {
  td <- nfi_cache_tree_data(root, plots)
  total <- nrow(td); done <- 0L
  if (is.function(progress)) progress(0L, total)
  parts <- list()
  for (ix in .chunks(total, chunk)) {
    parts[[length(parts) + 1L]] <- if (workers > 1L) .taper_profiles_parallel(td[ix, , drop = FALSE], workers)
      else generate_taper_profiles_for_trees(td[ix, , drop = FALSE], profile_step_m = NFI_CACHE_PROFILE_STEP_M, quiet = TRUE)
    done <- done + length(ix)
    if (is.function(progress)) progress(done, total)
  }
  nfi_cache_write(.combine_taper(parts), out_dir)
}

#' Build the cache when it does not exist (called by the NFI runners before they read it).
ensure_nfi_profiles <- function(root = get_root_dir(), progress = NULL, workers = 1L) {
  if (!nfi_profiles_ready(root)) build_nfi_cache(root, progress = progress, workers = workers)
  invisible(TRUE)
}
