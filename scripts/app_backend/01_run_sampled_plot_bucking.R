# ==============================================================================
# scripts/app_backend/01_run_sampled_plot_bucking.R
#
# Purpose : Sampled-plot bucking backend for Apter (Shiny app v1 support).
#           Exposes a single function:
#             run_sampled_plot_bucking(params, price_matrix_path, root)
#
# Design principles (from interface inspection 2026-07-01):
#   - Tree filtering matches script 11 exactly: dbh_cm >= 5.0 AND
#     height_m >= 4.5. No tilstand filter (matching prototype convention).
#   - Taper profiles are loaded from the precomputed cache:
#       outputs/app_cache/taper_profiles_github/taper_profiles_github.parquet
#     Generate this file by running:
#       Rscript scripts/app_backend/00_precompute_taper_profiles_github.R
#     taperNOR is NOT called during normal backend runs.
#   - plot_id_github is renamed to plot_id before all helper calls, and
#     renamed back before any data is returned. No returned object contains
#     a column named plot_id.
#   - Prices come from lookup_prices() (R/helpers_valuation.R): matrix
#     products by 3 keys, flat products by 1 key; each matrix product's
#     price-diameter class is capped at its matrix's largest class.
#   - Products (Phase 4b): `products` / `length_classes` give the run's
#     product set (base products plus custom assortments); NULL loads the
#     default files. validate_products() errors stop the run.
#   - Data is loaded from data_processed_github/ by default.
#   - This script never writes output files.
#   - Bark (2026-10-12): profiles are over bark (taperNOR cache); an
#     under-bark profile is added per tree with the bark model in
#     R/helpers_bark.R (barkNOR, B >= 0, provisional). Products with
#     DiameterUnderBark = TRUE use it for limits, price class and price volume.
#   - Outputs (Phase 3): NOK per m3 of commercial volume under bark
#     (R/helpers_commercial_volume.R), total, by species and by assortment.
#     No per-ha or plot-level outputs: results describe the selected trees
#     only. Plots are still how NFI trees are sampled.
#
# Usage:
#   source("scripts/app_backend/01_run_sampled_plot_bucking.R")
#   result <- run_sampled_plot_bucking(
#     params = list(species_group = "spruce", hogstkl = 41L, abonitet = 17L,
#                   n_plots = 10L, seed = 42L),
#     price_matrix_path = "default_2021"
#   )
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
})

# ---- source helpers ----------------------------------------------------------
# Resolve R/ relative to project root. This runs before helpers_paths.R is
# sourced, so it carries an inline copy of get_root_dir()'s env/walk-up logic.
.BACKEND_ROOT <- local({
  # APTER_ROOT -> walk up from getwd() -> stop()
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  env <- Sys.getenv("APTER_ROOT", unset = "")
  if (nzchar(env)) {
    d <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (!is_root(d))
      stop("Project root '", d, "' (from APTER_ROOT) does not contain ",
           "R/helpers_paths.R.", call. = FALSE)
  } else {
    d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    while (!is_root(d) && dirname(d) != d) d <- dirname(d)
    if (!is_root(d))
      stop("Cannot locate the project root: set APTER_ROOT, or run from ",
           "inside the project directory.", call. = FALSE)
  }
  d
})

for (.h in c("helpers_paths", "helpers_volume_methods", "helpers_products",
             "helpers_valuation",
             "helpers_taper", "helpers_bark", "helpers_commercial_volume",
             "helpers_candidate_generation", "helpers_dp_optimizer", "helpers_bucking_core",
             "helpers_parallel",
             "helpers_price_matrix", "helpers_comparison", "helpers_heights",
             "helpers_tree_list")) {
  src <- file.path(.BACKEND_ROOT, "R", paste0(.h, ".R"))
  if (!file.exists(src)) stop("Helper not found: ", src)
  source(src, local = FALSE)
}
rm(.h, src)

# ---- constants ---------------------------------------------------------------
BACKEND_MIN_DBH_CM               <- 5.0
BACKEND_MIN_HEIGHT_M             <- 4.5
BACKEND_MAX_PRICE_DIAMETER_CLASS_CM <- 50L   # default matrices' cap; runs cap per product (Phase 4b)

# ---- internal utilities ------------------------------------------------------

# Rename plot_id -> plot_id_github in a data.frame (safe no-op if absent).
.rename_to_github <- function(df) {
  if (!is.null(df) && is.data.frame(df) && "plot_id" %in% names(df)) {
    names(df)[names(df) == "plot_id"] <- "plot_id_github"
  }
  df
}

# Stop if any returned data.frame still has a bare plot_id column.
.assert_no_plot_id <- function(df, label) {
  if (!is.null(df) && is.data.frame(df) && "plot_id" %in% names(df))
    stop("SAFETY: 'plot_id' column found in ", label,
         " -- original NFI plot ID must not be returned.")
  invisible(df)
}

# Empty data.frames with required structure.
.empty_selected_logs <- function() {
  data.frame(plot_id_github = character(0), treid = character(0),
             species_group = character(0), ProductKey = character(0),
             ProductType = character(0), StartPos = integer(0),
             StopPos = integer(0), LogLength = integer(0),
             LengthClass_dm = integer(0), price_volume_m3 = numeric(0),
             PriceNOKperM3 = numeric(0), value_nok = numeric(0),
             PriceDiameterClass_cm_uncapped = integer(0),
             PriceDiameterClass_cm_used = integer(0),
             TopDiameter_ob = numeric(0), TopDiameter_ub = numeric(0),
             commercial_volume_m3 = numeric(0),
             stringsAsFactors = FALSE)
}

.empty_tree_summary <- function() {
  data.frame(plot_id_github = character(0), treid = character(0),
             species_group = character(0), dbh_cm = numeric(0),
             height_m = numeric(0), n_logs_selected = integer(0),
             total_value_nok = numeric(0), total_volume_m3 = numeric(0),
             commercial_volume_m3 = numeric(0), nok_per_m3 = numeric(0),
             optimizer_status = character(0), optimizer_message = character(0),
             height_source = character(0),
             stringsAsFactors = FALSE)
}

.empty_unpriced_summary <- function() {
  data.frame(ProductKey = character(0), LengthClass_dm = integer(0),
             n_candidates = integer(0), stringsAsFactors = FALSE)
}

# Lightweight proc.time helper: elapsed seconds since t0.
.pt <- function(t0) as.numeric((proc.time() - t0)["elapsed"])

# Fast DP optimizer (v3): pre-split candidates by StartPos + vectorised inner loop.
# Benchmark showed 10x speedup vs optimize_one_tree_dp() with identical results
# across all tested trees. Returns the same structure as optimize_one_tree_dp().
# Tie-breaking (Phase 4a): the same documented rule as optimize_one_tree_dp()
# in R/helpers_dp_optimizer.R (DP_TIE_TOL_NOK; fewer logs, priority, longer
# log, ProductKey; unallocated last at equal log count). Audits 16 and 19
# assert the two agree exactly.
.optimize_dp_fast <- function(valued_candidates) {
  dp_in <- valued_candidates[
    !is.na(valued_candidates$value_nok) &
    !is.na(valued_candidates$price_volume_m3), , drop = FALSE]
  dp_in$StartPos <- as.integer(round(dp_in$StartPos))
  dp_in$StopPos  <- as.integer(round(dp_in$StopPos))
  n_dp <- nrow(dp_in)

  if (n_dp == 0L)
    return(list(selected_logs = data.frame(), dp_best_value = 0, n_selected = 0L,
                n_dp_candidates = 0L, optimizer_status = "no_candidates",
                optimizer_message = "No priced candidates; DP not run"))

  # Tie-break keys (see R/helpers_dp_optimizer.R).
  prio_vec <- if ("ProductPriority" %in% names(dp_in))
    as.numeric(dp_in$ProductPriority) else rep(99, n_dp)
  prio_vec[is.na(prio_vec)] <- 99
  pk_vec <- match(as.character(dp_in$ProductKey),
                  sort(unique(as.character(dp_in$ProductKey))))
  if (length(pk_vec) == 0L || anyNA(pk_vec)) pk_vec <- rep(0L, n_dp)

  # Backward induction (R/helpers_bucking_core.R).
  res     <- .dp_select(dp_in$StartPos, dp_in$StopPos, dp_in$value_nok, prio_vec, pk_vec)
  sel_idx <- res$sel_idx
  n_sel   <- length(sel_idx)
  if (n_sel == 0L)
    return(list(selected_logs = data.frame(), dp_best_value = res$best,
                n_selected = 0L, n_dp_candidates = n_dp,
                optimizer_status = "zero_logs",
                optimizer_message = "DP ran but selected 0 logs"))

  sel <- dp_in[sel_idx, , drop = FALSE]
  rownames(sel) <- NULL
  list(selected_logs = sel, dp_best_value = res$best, n_selected = n_sel,
       n_dp_candidates = n_dp, optimizer_status = "ok",
       optimizer_message = sprintf("Selected %d logs", n_sel))
}

# Products, length classes and prices for one run, validated (step 6 of the
# runners). Stops on invalid products or a FAILing price matrix.
.load_products_prices <- function(price_matrix_path, root, products, length_classes) {
  t0_products_prices <- proc.time()
  pd <- products       %||% load_products(root)
  lc <- length_classes %||% load_length_classes(root)
  if (!"LogLength_dm" %in% names(lc)) lc$LogLength_dm <- lc$LogLength / 10
  prod_val <- validate_products(pd, lc)
  if (!prod_val$ok)
    stop("Product definitions invalid:\n",
         paste0("  ", prod_val$errors, collapse = "\n"))

  if (identical(price_matrix_path, "default_2021")) {
    pm <- load_price_matrix_long(root)
  } else if (grepl("\\.rds$", price_matrix_path, ignore.case = TRUE)) {
    pm <- readRDS(price_matrix_path)
    pm <- as.data.frame(pm)
  } else {
    pm <- as.data.frame(readr::read_csv(price_matrix_path, show_col_types = FALSE))
  }

  val_result <- validate_price_matrix_against_products(pd, lc, pm)
  fail_rows  <- val_result$checks[val_result$checks$result == "FAIL", ]
  if (nrow(fail_rows) > 0L)
    stop("Price matrix validation FAILED:\n",
         paste0("  ", fail_rows$check, ": ", fail_rows$detail, collapse = "\n"))

  price_review_items <- val_result$checks$check[val_result$checks$result == "REVIEW"]
  price_val_checks   <- val_result$checks

  # Price lookup tables, built once (R/helpers_valuation.R; also used for
  # scenario re-pricing, so runs and comparisons share one lookup).
  price_lookup <- build_price_lookup(pm)
  t_products_prices <- .pt(t0_products_prices)

  list(pd = pd, lc = lc, pm = pm, lookup = price_lookup, checks = price_val_checks,
       review = price_review_items, secs = t_products_prices)
}

# Result tables of one run (steps 10-15) from the per-tree rows, selected
# logs and unpriced tables (lists in tree order, NULL where none).
.assemble_results <- function(all_tree_rows, all_selected, all_unpriced) {
  n_unpriced_total <- sum(vapply(all_unpriced, function(u)
    if (is.null(u)) 0L else as.integer(sum(u$n_candidates)), integer(1)))

  # ---- 10. Assemble tree_summary ---------------------------------------------
  t0_summary <- proc.time()
  tree_summary_raw <- .bind_cols_lists(all_tree_rows) %||% .empty_tree_summary()

  tree_summary <- .rename_to_github(tree_summary_raw)
  .assert_no_plot_id(tree_summary, "tree_summary")

  # ---- 11. Assemble selected_logs_all ----------------------------------------
  sel_list   <- Filter(Negate(is.null), all_selected)
  n_trees_with_logs <- length(sel_list)

  required_log_cols <- c("plot_id", "treid", "species_group",
                         "ProductKey", "ProductType",
                         "StartPos", "StopPos", "LogLength", "LengthClass_dm",
                         "price_volume_m3", "PriceNOKperM3", "value_nok",
                         "PriceDiameterClass_cm_uncapped",
                         "PriceDiameterClass_cm_used",
                         "TopDiameter_ob", "TopDiameter_ub",
                         "commercial_volume_m3")

  if (length(sel_list) > 0L) {
    sel_all_raw <- .bind_cols_lists(sel_list)
    # Keep only required columns (drop TopDiameter, ButtDiameter, eligible, etc.)
    avail_cols <- intersect(required_log_cols, names(sel_all_raw))
    sel_all_raw <- sel_all_raw[, avail_cols, drop = FALSE]
    # Rename plot_id -> plot_id_github
    selected_logs_all <- .rename_to_github(sel_all_raw)
  } else {
    selected_logs_all <- .empty_selected_logs()
  }
  .assert_no_plot_id(selected_logs_all, "selected_logs_all")

  # ---- 12. NOK per m3 of commercial volume under bark ----------------------
  per_m3 <- summarise_per_m3(selected_logs_all, tree_summary)
  per_m3_summary       <- per_m3$summary
  per_m3_by_assortment <- per_m3$by_assortment

  # ---- 13. product_summary ----------------------------------------------------
  if (nrow(selected_logs_all) > 0L) {
    prd_keys <- unique(selected_logs_all$ProductKey)
    prod_summary <- do.call(rbind, lapply(prd_keys, function(pk) {
      rows  <- selected_logs_all[selected_logs_all$ProductKey == pk, , drop = FALSE]
      pt    <- if ("ProductType" %in% names(rows)) rows$ProductType[1L] else NA_character_
      t_vol <- sum(rows$price_volume_m3, na.rm = TRUE)
      t_val <- sum(rows$value_nok, na.rm = TRUE)
      data.frame(
        ProductKey            = pk,
        ProductType           = pt,
        n_logs                = nrow(rows),
        total_volume_m3       = round(t_vol, 5),
        total_value_nok       = round(t_val, 2),
        mean_value_nok_per_m3 = if (t_vol > 0) round(t_val / t_vol, 2) else NA_real_,
        stringsAsFactors      = FALSE
      )
    }))
  } else {
    prod_summary <- data.frame(
      ProductKey = character(0), ProductType = character(0),
      n_logs = integer(0), total_volume_m3 = numeric(0),
      total_value_nok = numeric(0), mean_value_nok_per_m3 = numeric(0),
      stringsAsFactors = FALSE)
  }

  # ---- 14. unpriced_summary ---------------------------------------------------
  up_list <- Filter(Negate(is.null), all_unpriced)
  if (length(up_list) > 0L) {
    up_all <- do.call(rbind, up_list)
    up_agg <- aggregate(n_candidates ~ ProductKey + LengthClass_dm, data = up_all,
                        FUN = sum)
    up_agg <- up_agg[order(up_agg$ProductKey, up_agg$LengthClass_dm), ]
    unpriced_summary <- up_agg
  } else {
    unpriced_summary <- .empty_unpriced_summary()
  }

  # ---- 15. zero_log_trees -----------------------------------------------------
  if (nrow(tree_summary) > 0L) {
    zl_rows <- tree_summary[tree_summary$n_logs_selected == 0L, , drop = FALSE]
    zero_log_trees <- if (nrow(zl_rows) > 0L) {
      zl_rows[, c("plot_id_github", "treid", "species_group", "dbh_cm",
                  "height_m", "optimizer_status"), drop = FALSE]
    } else {
      data.frame(plot_id_github = character(0), treid = character(0),
                 species_group = character(0), dbh_cm = numeric(0),
                 height_m = numeric(0), optimizer_status = character(0),
                 stringsAsFactors = FALSE)
    }
    names(zero_log_trees)[names(zero_log_trees) == "optimizer_status"] <- "reason"
  } else {
    zero_log_trees <- data.frame(
      plot_id_github = character(0), treid = character(0),
      species_group = character(0), dbh_cm = numeric(0),
      height_m = numeric(0), reason = character(0),
      stringsAsFactors = FALSE)
  }
  .assert_no_plot_id(zero_log_trees, "zero_log_trees")

  t_summary <- .pt(t0_summary)
  list(tree_summary = tree_summary, selected_logs_all = selected_logs_all,
       per_m3_summary = per_m3_summary, per_m3_by_assortment = per_m3_by_assortment,
       prod_summary = prod_summary, unpriced_summary = unpriced_summary,
       zero_log_trees = zero_log_trees, n_trees_with_logs = n_trees_with_logs,
       n_unpriced_total = n_unpriced_total,
       t_summary = t_summary)
}

#' Run the sampled-plot bucking pipeline.
#'
#' @param params Named list. Required: species_group, hogstkl, abonitet.
#'   Optional: n_plots (default 10, max 50), seed (default 42),
#'   fylnr (character vector or NULL), hoh_min, hoh_max (numeric or NULL),
#'   stump_height_cm (default 10), start_pos_step_cm (default 10).
#' @param price_matrix_path "default_2021" (default) or a path to a CSV/RDS
#'   file in PriceMatrix_long format.
#' @param root Project root directory. Default: get_root_dir().
#' @param products,length_classes The run's ProductData / LengthClasses
#'   (NULL: the default files).
#' @param scenario NULL, or list(price_matrix_path, products, length_classes)
#'   for a second run on the same trees (a comparison). Both runs are priced
#'   from one candidate pass per tree; the scenario's result is returned as
#'   `$scenario` (the same structure).
#' @param workers Number of parallel workers (apter_workers(); 1 = in this
#'   process). Results do not depend on it.
#'
#' @return Named list with run outputs, summaries, diagnostics, and label.
run_sampled_plot_bucking <- function(
    params,
    price_matrix_path = "default_2021",
    root = get_root_dir(),
    products = NULL,
    length_classes = NULL,
    scenario = NULL,
    workers = apter_workers()) {
  job <- prepare_sampled_job(params, price_matrix_path, root, products,
                             length_classes, scenario)
  finish_job(job, buck_job(job, workers))
}

#' Steps 1-8 of run_sampled_plot_bucking(): sample the trees, load their
#' profiles, products and prices. Returns a job for buck_job() / finish_job().
prepare_sampled_job <- function(params, price_matrix_path = "default_2021",
                                root = get_root_dir(), products = NULL,
                                length_classes = NULL, scenario = NULL) {
  t_start <- Sys.time()

  # ---- 1. Validate and unpack params ----------------------------------------
  if (!is.list(params)) stop("params must be a named list.")

  sg <- params$species_group
  if (is.null(sg) || !sg %in% c("spruce", "pine", "both"))
    stop("params$species_group must be 'spruce', 'pine', or 'both'.")

  hogstkl_vec  <- as.integer(params$hogstkl  %||% stop("params$hogstkl required."))
  abonitet_vec <- as.integer(params$abonitet %||% stop("params$abonitet required."))
  n_plots_req  <- min(as.integer(params$n_plots %||% 10L), 50L)
  seed_val     <- as.integer(params$seed     %||% 42L)

  fylnr_vec   <- params$fylnr      # NULL or character/integer vector
  hoh_min_val <- params$hoh_min    # NULL or numeric
  hoh_max_val <- params$hoh_max    # NULL or numeric

  stump_cm <- as.integer(params$stump_height_cm   %||% 10L)
  step_cm  <- as.integer(params$start_pos_step_cm %||% 10L)

  # ---- 2. Load sanitized data ------------------------------------------------
  t0_load_data <- proc.time()
  gh_dir <- file.path(root, "data_processed_github")
  if (!dir.exists(gh_dir))
    stop("data_processed_github/ not found at: ", gh_dir)

  trees_all <- arrow::read_parquet(
    file.path(gh_dir, "nfi_tree_plot_stratum_ready_github.parquet"))
  plots_all <- arrow::read_parquet(
    file.path(gh_dir, "nfi_plots_github.parquet"))
  t_load_data <- .pt(t0_load_data)

  # ---- 3. Filter to requested stratum ----------------------------------------
  t0_filter_sample <- proc.time()
  sg_filter <- if (sg == "both") c("spruce", "pine") else sg

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

  eligible_plots  <- sort(unique(trees_filt$plot_id_github))
  n_eligible_plots <- length(eligible_plots)

  if (n_eligible_plots == 0L)
    stop("No eligible plots found for the requested stratum. ",
         "Check species_group=", sg, ", hogstkl=",
         paste(hogstkl_vec, collapse = "/"), ", abonitet=",
         paste(abonitet_vec, collapse = "/"), ".")

  # ---- 4. Sample complete plots ----------------------------------------------
  set.seed(seed_val)
  n_plots_sampled <- min(n_plots_req, n_eligible_plots)
  sampled_plots   <- sort(sample(eligible_plots, n_plots_sampled, replace = FALSE))
  trees_sampled   <- trees_filt[
    trees_filt$plot_id_github %in% sampled_plots, , drop = FALSE]
  t_filter_sample <- .pt(t0_filter_sample)

  # ---- 5. Compute height_m and apply taperNOR validity filter ----------------
  t0_tree_filter <- proc.time()
  # Use beregnet_hoyde (dm) / 10 -- identical to script 11 convention.
  # No tilstand filter -- matches script 11 exactly.
  if ("beregnet_hoyde" %in% names(trees_sampled)) {
    trees_sampled$height_m <- as.numeric(trees_sampled$beregnet_hoyde) / 10
  } else if ("trehoyde" %in% names(trees_sampled)) {
    trees_sampled$height_m <- as.numeric(trees_sampled$trehoyde) / 10
  } else {
    stop("No height column found (expected beregnet_hoyde or trehoyde).")
  }

  n_trees_before_filter <- nrow(trees_sampled)

  trees_valid <- trees_sampled[
    !is.na(trees_sampled$dbh_cm)   & trees_sampled$dbh_cm   >= BACKEND_MIN_DBH_CM &
    !is.na(trees_sampled$height_m) & trees_sampled$height_m >= BACKEND_MIN_HEIGHT_M,
    , drop = FALSE]

  n_trees_after_filter <- nrow(trees_valid)
  n_trees_dropped      <- n_trees_before_filter - n_trees_after_filter

  if (n_trees_after_filter == 0L)
    stop("All trees were filtered out (DBH < ", BACKEND_MIN_DBH_CM, " cm or ",
         "height < ", BACKEND_MIN_HEIGHT_M, " m). Check your stratum selection.")
  t_tree_filter <- .pt(t0_tree_filter)

  # ---- 6. Load products and price matrix -------------------------------------
  pps <- .load_runs(price_matrix_path, root, products, length_classes, scenario)
  t_products_prices <- sum(vapply(pps, `[[`, 0, "secs"))

  # ---- 7. Build tree_data metadata lookup -----------------------------------
  # tree_data: needed for metadata lookups inside the per-tree loop.
  # Rename plot_id_github -> plot_id for helper compatibility.
  tree_data <- trees_valid[, c("plot_id_github", "treid", "sesong",
                                "species_group", "dbh_cm", "height_m"),
                           drop = FALSE]
  names(tree_data)[names(tree_data) == "plot_id_github"] <- "plot_id"
  tree_data$height_source <- "NFI"   # tree lists: helpers_heights.R

  # ---- 8. Load precomputed taper profiles from partitioned cache -------------
  t0_taper_profiles <- proc.time()
  taper_cache_dir <- file.path(root, "outputs", "app_cache",
                                "taper_profiles_by_plot_block_github")

  if (!dir.exists(taper_cache_dir))
    stop("Partitioned taper profile cache not found:\n  ", taper_cache_dir,
         "\nRun this script first:\n  ",
         "Rscript scripts/app_backend/00_precompute_taper_profiles_github.R")

  # Safety check on schema before reading.
  cache_ds      <- arrow::open_dataset(taper_cache_dir)
  cache_schema  <- names(cache_ds)
  if ("plot_id" %in% cache_schema)
    stop("SAFETY: cached taper profiles contain 'plot_id'. ",
         "Regenerate the cache with 00_precompute_taper_profiles_github.R")
  if (!"plot_id_github" %in% cache_schema)
    stop("SAFETY: cached taper profiles missing 'plot_id_github'. ",
         "Regenerate the cache with 00_precompute_taper_profiles_github.R")
  if (!"plot_block" %in% cache_schema)
    stop("SAFETY: cached taper profiles missing 'plot_block' partition column. ",
         "Regenerate the cache with 00_precompute_taper_profiles_github.R")

  # Compute the plot_block values that cover the sampled plots.
  # Mirrors the formula used in 00_precompute: B = floor((numeric_id-1)/500).
  sampled_treids <- as.character(trees_valid$treid)
  numeric_ids    <- as.integer(substr(sampled_plots, 2L, nchar(sampled_plots)))
  needed_blocks  <- unique(sprintf("B%03d", floor((numeric_ids - 1L) / 500L)))

  # Read only the needed blocks, then filter to sampled plots and valid trees.
  profiles_raw <- as.data.frame(
    cache_ds |>
      dplyr::filter(plot_block %in% needed_blocks,
                    plot_id_github %in% sampled_plots) |>
      dplyr::select(-plot_block) |>
      dplyr::collect())

  # Keep only profiles for valid (filtered) trees to exclude other species/strata
  # that share the same plots.
  profiles_raw <- profiles_raw[
    profiles_raw$treid %in% sampled_treids, , drop = FALSE]

  n_profiles_loaded <- nrow(profiles_raw)

  # Verify: every valid tree must have a precomputed profile.
  profiles_treids      <- as.character(unique(profiles_raw$treid))
  missing_treids       <- setdiff(sampled_treids, profiles_treids)
  n_trees_with_profile <- length(intersect(sampled_treids, profiles_treids))
  n_trees_missing      <- length(missing_treids)

  if (n_trees_missing > 0L)
    stop("Precomputed taper profiles missing for ", n_trees_missing,
         " sampled valid tree(s). ",
         "Regenerate the full cache:\n  ",
         "Rscript scripts/app_backend/00_precompute_taper_profiles_github.R")
  t_taper_profiles <- .pt(t0_taper_profiles)

  # Rename plot_id_github -> plot_id for profiles_to_profile_vectors().
  profiles_for_helpers <- profiles_raw
  names(profiles_for_helpers)[
    names(profiles_for_helpers) == "plot_id_github"] <- "plot_id"

  t0_profile_convert <- proc.time()
  pvs              <- profiles_to_profile_vectors(profiles_for_helpers)
  t_profile_convert <- .pt(t0_profile_convert)

  # Under-bark profile per tree (bark model in R/helpers_bark.R).
  t0_under_bark <- proc.time()
  bark_model    <- get_bark_model()
  pvs           <- lapply(pvs, add_under_bark_profile, model = bark_model)
  t_under_bark  <- .pt(t0_under_bark)
  tree_ids_valid   <- names(pvs)
  n_trees_in_optim <- length(tree_ids_valid)

  # profile_failures is empty (we do not run taperNOR; missing = hard error above).
  profile_failures <- data.frame(
    plot_id_github = character(0), treid = character(0),
    species_group = character(0), dbh_cm = numeric(0),
    height_m = numeric(0), error_message = character(0),
    stringsAsFactors = FALSE)

  .new_job("sampled", t_start, pvs, tree_data, pps, stump_cm, step_cm,
           timings = c(load_data = t_load_data,
                       filter_stratum_and_sample_plots = t_filter_sample,
                       tree_filtering = t_tree_filter,
                       load_products_and_prices = t_products_prices,
                       load_precomputed_taper_profiles = t_taper_profiles,
                       profile_vector_conversion = t_profile_convert,
                       under_bark_profiles = t_under_bark),
           meta = list(params = params, n_eligible_plots = n_eligible_plots,
                       n_plots_req = n_plots_req, n_plots_sampled = n_plots_sampled,
                       n_trees_before_filter = n_trees_before_filter,
                       n_trees_after_filter = n_trees_after_filter,
                       n_trees_dropped = n_trees_dropped,
                       taper_cache_dir = taper_cache_dir,
                       n_profiles_loaded = n_profiles_loaded,
                       n_trees_with_profile = n_trees_with_profile,
                       n_trees_missing = n_trees_missing,
                       sampled_plots = sampled_plots,
                       profile_failures = profile_failures))
}

# Steps 16-18 of run_sampled_plot_bucking() for one run's result tables.
.result_sampled <- function(job, core, pp, elapsed_secs) {
  m <- job$meta
  n_zero_log <- if (nrow(core$tree_summary) > 0L)
    sum(core$tree_summary$n_logs_selected == 0L) else 0L

  # ---- 16. Diagnostics --------------------------------------------------------
  diagnostics <- list(
    n_eligible_plots                           = m$n_eligible_plots,
    n_requested_plots                          = m$n_plots_req,
    n_plots_sampled                            = m$n_plots_sampled,
    n_trees_before_filter                      = m$n_trees_before_filter,
    n_trees_after_filter                       = m$n_trees_after_filter,
    n_trees_dropped_low_dbh_or_height          = m$n_trees_dropped,
    taper_source                               = "precomputed",
    bark_model                                 = job$bark_model,
    taper_profile_file                         = basename(m$taper_cache_dir),
    n_profiles_loaded                          = m$n_profiles_loaded,
    n_trees_with_precomputed_profiles          = m$n_trees_with_profile,
    n_valid_trees_missing_precomputed_profiles = m$n_trees_missing,
    n_trees_entering_optimizer                 = length(job$pvs),
    n_zero_log_trees                           = n_zero_log,
    n_unpriced_candidates                      = core$n_unpriced_total,
    price_review_items                         = pp$review,
    elapsed_seconds                            = elapsed_secs,
    stage_timings                              = .stage_timings(job, core, elapsed_secs),
    workers                                    = core$workers
  )

  # ---- 17. Final safety assertions -------------------------------------------
  .assert_no_plot_id(core$tree_summary,         "tree_summary (final)")
  .assert_no_plot_id(core$selected_logs_all,    "selected_logs_all (final)")
  .assert_no_plot_id(core$per_m3_summary,       "per_m3_summary (final)")
  .assert_no_plot_id(core$per_m3_by_assortment, "per_m3_by_assortment (final)")
  .assert_no_plot_id(core$zero_log_trees,       "zero_log_trees (final)")
  .assert_no_plot_id(m$profile_failures,        "profile_failures (final)")

  # ---- 18. Return -------------------------------------------------------------
  list(
    run_id                  = paste0("sampled_", format(Sys.time(), "%Y%m%d_%H%M%S")),
    params                  = m$params,
    n_eligible_plots        = m$n_eligible_plots,
    n_plots_sampled         = m$n_plots_sampled,
    n_trees_total           = m$n_trees_after_filter,
    n_trees_with_logs       = core$n_trees_with_logs,
    selected_logs_all       = core$selected_logs_all,
    tree_summary            = core$tree_summary,
    per_m3_summary          = core$per_m3_summary,
    per_m3_by_assortment    = core$per_m3_by_assortment,
    sampled_plot_ids        = sort(as.character(m$sampled_plots)),
    product_summary         = core$prod_summary,
    unpriced_summary        = core$unpriced_summary,
    zero_log_trees          = core$zero_log_trees,
    price_validation_checks = pp$checks,
    profile_failures        = m$profile_failures,
    diagnostics             = diagnostics,
    label                   = paste("Results describe the selected trees only.",
                                    "Values are NOK per m³ of commercial volume under bark."),
    elapsed_seconds         = elapsed_secs
  )
}


#' Buck an uploaded tree list (Phase 5).
#'
#' @param prepared Result of prepare_tree_list() (R/helpers_tree_list.R):
#'   validated trees with heights (measured or imputed) and height_source.
#'   Trees outside taperNOR's range are already flagged (in_range = FALSE)
#'   and are not bucked.
#' @param price_matrix_path,root,products,length_classes,scenario,workers As
#'   for run_sampled_plot_bucking().
#' @param params Optional list stored in the result (e.g. file name).
#' @return The same structure as run_sampled_plot_bucking(), with
#'   tree_source = "tree_list", the height summary (heights) and the
#'   excluded trees. plot_id_github holds the user's plot_id.
run_tree_list_bucking <- function(prepared,
                                  price_matrix_path = "default_2021",
                                  root = get_root_dir(),
                                  products = NULL,
                                  length_classes = NULL,
                                  params = list(),
                                  scenario = NULL,
                                  workers = apter_workers()) {
  job <- prepare_tree_list_job(prepared, price_matrix_path, root, products,
                               length_classes, params, scenario, workers)
  finish_job(job, buck_job(job, workers))
}

#' The preparation part of run_tree_list_bucking(): taperNOR profiles, under
#' bark, products and prices. Returns a job for buck_job() / finish_job().
prepare_tree_list_job <- function(prepared, price_matrix_path = "default_2021",
                                  root = get_root_dir(), products = NULL,
                                  length_classes = NULL, params = list(),
                                  scenario = NULL, workers = 1L) {
  t_start <- Sys.time()
  if (!isTRUE(prepared$ok)) stop("The tree list did not pass validation.")
  stump_cm <- as.integer(params$stump_height_cm   %||% 10L)
  step_cm  <- as.integer(params$start_pos_step_cm %||% 10L)

  # Trees within taperNOR's range (others are reported as excluded).
  t0_tree_filter <- proc.time()
  trees <- prepared$trees[prepared$trees$in_range, , drop = FALSE]
  tree_data <- data.frame(plot_id = trees$plot_id, treid = trees$treid, sesong = NA,
                          species_group = trees$species_group, dbh_cm = trees$dbh_cm,
                          height_m = trees$height_m, height_source = trees$height_source,
                          stringsAsFactors = FALSE)
  t_tree_filter <- .pt(t0_tree_filter)

  pps <- .load_runs(price_matrix_path, root, products, length_classes, scenario)

  # taperNOR at run time (no cache for uploaded trees).
  t0_taper <- proc.time()
  profiles <- .taper_profiles_parallel(tree_data, workers)   # helpers_parallel.R
  ok <- attr(profiles, "profile_ok")
  profile_failures <- .rename_to_github(data.frame(
    plot_id = tree_data$plot_id[!ok], treid = tree_data$treid[!ok],
    species_group = tree_data$species_group[!ok], dbh_cm = tree_data$dbh_cm[!ok],
    height_m = tree_data$height_m[!ok],
    error_message = attr(profiles, "profile_errors")[!ok], stringsAsFactors = FALSE))
  t_taper <- .pt(t0_taper)
  t0_conv <- proc.time()
  pvs <- profiles_to_profile_vectors(profiles)
  t_conv <- .pt(t0_conv)
  t0_ub <- proc.time()
  bark_model <- get_bark_model()
  pvs <- lapply(pvs, add_under_bark_profile, model = bark_model)
  t_ub <- .pt(t0_ub)

  excluded <- prepared$excluded
  excluded <- .rename_to_github(excluded[, c("plot_id", "treid", "species_group", "dbh_cm",
                                             "height_m", "height_source", "excluded_reason"),
                                         drop = FALSE])
  .new_job("tree_list", t_start, pvs, tree_data[ok, , drop = FALSE], pps, stump_cm, step_cm,
           timings = c(load_data = 0, filter_stratum_and_sample_plots = 0,
                       tree_filtering = t_tree_filter,
                       load_products_and_prices = sum(vapply(pps, `[[`, 0, "secs")),
                       load_precomputed_taper_profiles = t_taper,
                       profile_vector_conversion = t_conv, under_bark_profiles = t_ub),
           meta = list(params = params, trees = trees, prepared = prepared,
                       excluded = excluded, profile_failures = profile_failures))
}

# The tree-list result for one run's result tables.
.result_tree_list <- function(job, core, pp, elapsed) {
  m <- job$meta
  trees <- m$trees
  plots <- sort(unique(as.character(trees$plot_id[!is.na(trees$plot_id)])))
  n_zero_log <- sum(core$tree_summary$n_logs_selected == 0L)
  for (x in list(core$tree_summary, core$selected_logs_all, core$per_m3_summary,
                 core$per_m3_by_assortment, core$zero_log_trees, m$profile_failures,
                 m$excluded))
    .assert_no_plot_id(x, "tree list result")

  list(
    run_id                  = paste0("treelist_", format(Sys.time(), "%Y%m%d_%H%M%S")),
    tree_source             = "tree_list",
    params                  = utils::modifyList(list(species_group = paste(
                                sort(unique(trees$species_group)), collapse = " + ")), m$params),
    n_eligible_plots        = length(plots),
    n_plots_sampled         = length(plots),
    n_trees_total           = nrow(trees),
    n_trees_with_logs       = core$n_trees_with_logs,
    selected_logs_all       = core$selected_logs_all,
    tree_summary            = core$tree_summary,
    per_m3_summary          = core$per_m3_summary,
    per_m3_by_assortment    = core$per_m3_by_assortment,
    sampled_plot_ids        = plots,
    product_summary         = core$prod_summary,
    unpriced_summary        = core$unpriced_summary,
    zero_log_trees          = core$zero_log_trees,
    price_validation_checks = pp$checks,
    profile_failures        = m$profile_failures,
    heights                 = m$prepared$heights,
    height_warnings         = m$prepared$warnings,
    excluded_trees          = m$excluded,
    diagnostics             = list(
      n_trees_in_list                   = nrow(m$prepared$trees),
      n_trees_excluded_out_of_range     = nrow(m$excluded),
      n_trees_entering_optimizer        = length(job$pvs),
      taper_source                      = "taperNOR at run time",
      bark_model                        = job$bark_model,
      n_zero_log_trees                  = n_zero_log,
      n_unpriced_candidates             = core$n_unpriced_total,
      price_review_items                = pp$review,
      elapsed_seconds                   = elapsed,
      stage_timings                     = .stage_timings(job, core, elapsed),
      workers                           = core$workers),
    label                   = paste("Results describe the uploaded trees only.",
                                    "Values are NOK per m³ of commercial volume under bark."),
    elapsed_seconds         = elapsed
  )
}


# ------------------------------------------------------------------------------
# Jobs: prepare -> buck (in this process or on parallel workers) -> finish
# ------------------------------------------------------------------------------

# Products, length classes and prices of the run, and of the scenario run
# when `scenario` is given (list(price_matrix_path, products, length_classes)).
.load_runs <- function(price_matrix_path, root, products, length_classes, scenario) {
  pps <- list(.load_products_prices(price_matrix_path, root, products, length_classes))
  if (!is.null(scenario))
    pps[[2L]] <- .load_products_prices(scenario$price_matrix_path %||% "default_2021",
                                       root, scenario$products, scenario$length_classes)
  pps
}

# A prepared job. Trees are bucked in a fixed order (sorted by treid), so
# results do not depend on the order the profiles were read in, nor on how
# the trees are split over workers.
.new_job <- function(kind, t_start, pvs, tree_data, pps, stump_cm, step_cm,
                     timings, meta) {
  pvs <- pvs[order(names(pvs), method = "radix")]
  list(kind = kind, t_start = t_start, pvs = pvs, tree_data = tree_data, pps = pps,
       prep = .prepare_runs(lapply(pps, function(pp)
         list(pd = pp$pd, lc = pp$lc, lookup = pp$lookup))),
       stump_cm = stump_cm, step_cm = step_cm, timings = timings, meta = meta,
       bark_model = getOption("apter.bark_model", DEFAULT_BARK_MODEL))
}

#' Step 9 of a job: the per-tree loop (.buck_trees(), R/helpers_bucking_core.R),
#' in this process (workers = 1) or in chunks on parallel workers
#' (R/helpers_parallel.R). Returns .buck_trees()'s structure for all trees.
buck_job <- function(job, workers = apter_workers()) {
  out <- if (workers <= 1L || length(job$pvs) == 0L)
    .buck_trees(job$pvs, job$tree_data, job$prep, job$stump_cm, job$step_cm)
  else .combine_chunks(lapply(.chunk_futures(job, workers), future::value))
  out$workers <- if (workers <= 1L) 1L else as.integer(workers)
  out
}

#' Steps 10-18 of a job: result tables per run and the result list (the
#' scenario's, if any, as `$scenario`).
finish_job <- function(job, b) {
  cores <- lapply(b$runs, function(x) {
    core <- .assemble_results(x$rows, x$sel, x$unpriced)
    core$acc_cand_gen   <- b$t_cand
    core$acc_price_join <- x$t_price
    core$acc_dp_optim   <- x$t_dp
    core$workers        <- b$workers
    core
  })
  elapsed <- as.numeric(difftime(Sys.time(), job$t_start, units = "secs"))
  build <- if (identical(job$kind, "sampled")) .result_sampled else .result_tree_list
  res <- build(job, cores[[1L]], job$pps[[1L]], elapsed)
  if (length(cores) > 1L) res$scenario <- build(job, cores[[2L]], job$pps[[2L]], elapsed)
  res
}

# Stage timings (seconds). Candidate generation is shared by the runs of a
# comparison; with workers it is summed over the workers.
.stage_timings <- function(job, core, elapsed) {
  t <- c(job$timings, candidate_generation = core$acc_cand_gen,
         price_joining = core$acc_price_join, dp_optimization = core$acc_dp_optim,
         summary_assembly = core$t_summary, total = elapsed)
  data.frame(stage = names(t), elapsed_seconds = round(unname(t), 3),
             stringsAsFactors = FALSE)
}
