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
#   - Price join is done manually (sawlogs by 3 keys; non-sawlogs by 1 key)
#     because no single helper function covers the full join.
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

for (.h in c("helpers_paths", "helpers_products", "helpers_valuation",
             "helpers_taper", "helpers_bark", "helpers_candidate_generation",
             "helpers_commercial_volume", "helpers_dp_optimizer")) {
  src <- file.path(.BACKEND_ROOT, "R", paste0(.h, ".R"))
  if (!file.exists(src)) stop("Helper not found: ", src)
  source(src, local = FALSE)
}
rm(.h, src)

# ---- constants ---------------------------------------------------------------
BACKEND_MIN_DBH_CM               <- 5.0
BACKEND_MIN_HEIGHT_M             <- 4.5
BACKEND_MAX_PRICE_DIAMETER_CLASS_CM <- 50L

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
# Does NOT replace helpers_dp_optimizer.R; that file is unchanged.
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

  # Pre-extract vectors: avoids repeated $-column access in hot loop.
  sp_vec  <- dp_in$StartPos
  ep_vec  <- dp_in$StopPos
  val_vec <- dp_in$value_nok

  all_pos  <- sort(unique(c(sp_vec, ep_vec)))
  sentinel <- max(all_pos) + 1L
  all_pos  <- c(all_pos, sentinel)
  m        <- length(all_pos)

  max_pos    <- max(all_pos)
  pos_to_idx <- integer(max_pos + 1L)
  for (k in seq_len(m)) pos_to_idx[all_pos[k] + 1L] <- k

  # Pre-split candidate indices by StartPos once (O(n_dp)) so each position
  # lookup is O(1) instead of O(n_dp) with which().
  cands_by_start <- split(seq_len(n_dp), sp_vec)  # named list, char keys

  dp_val <- numeric(m)
  dp_log <- integer(m)

  # Backward induction with vectorised inner operations.
  for (i in seq(m - 1L, 1L)) {
    cur_pos     <- all_pos[i]
    cands       <- cands_by_start[[as.character(cur_pos)]]
    default_val <- dp_val[i + 1L]

    if (!is.null(cands)) {
      # Vectorised: compute stop-position indices and total values at once.
      stop_idx <- pos_to_idx[ep_vec[cands] + 1L]
      valid    <- stop_idx > 0L & ep_vec[cands] <= max_pos
      if (any(valid)) {
        cv      <- cands[valid]
        sv      <- stop_idx[valid]
        total_v <- val_vec[cv] + dp_val[sv]   # vectorised C addition
        best_k  <- which.max(total_v)          # vectorised C max-search
        if (total_v[best_k] > default_val) {
          dp_val[i] <- total_v[best_k]
          dp_log[i] <- cv[best_k]
          next
        }
      }
    }
    dp_val[i] <- default_val
    # dp_log[i] stays 0 (integer default = skip)
  }

  # Traceback
  sel_idx <- integer(0)
  i <- 1L
  while (i < m) {
    ci <- dp_log[i]
    if (ci == 0L) { i <- i + 1L
    } else {
      sel_idx <- c(sel_idx, ci)
      sp <- ep_vec[ci]
      if (sp > max_pos) break
      si <- pos_to_idx[sp + 1L]
      if (si == 0L) break
      i <- si
    }
  }

  n_sel <- length(sel_idx)
  if (n_sel == 0L)
    return(list(selected_logs = data.frame(), dp_best_value = dp_val[1L],
                n_selected = 0L, n_dp_candidates = n_dp,
                optimizer_status = "zero_logs",
                optimizer_message = "DP ran but selected 0 logs"))

  sel <- dp_in[sel_idx, , drop = FALSE]
  rownames(sel) <- NULL
  list(selected_logs = sel, dp_best_value = dp_val[1L], n_selected = n_sel,
       n_dp_candidates = n_dp, optimizer_status = "ok",
       optimizer_message = sprintf("Selected %d logs", n_sel))
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
#'
#' @return Named list with run outputs, summaries, diagnostics, and label.
run_sampled_plot_bucking <- function(
    params,
    price_matrix_path = "default_2021",
    root = get_root_dir()) {

  t_start <- proc.time()

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
  t0_products_prices <- proc.time()
  pd <- load_products(root)
  lc <- load_length_classes(root)

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

  # Prepare price matrix subsets for the join (done once, outside the tree loop).
  # sawlog matrix rows: have a non-NA LengthClass_dm
  pm_mat_join <- pm[
    !is.na(pm$LengthClass_dm),
    intersect(c("ProductKey", "LengthClass_dm", "PriceDiameterClass_cm",
                "PriceNOKperM3"), names(pm)),
    drop = FALSE]

  # flat price rows: LengthClass_dm is NA (pulpwood, waste)
  pm_fl_raw  <- pm[is.na(pm$LengthClass_dm),
                   intersect(c("ProductKey", "PriceNOKperM3"), names(pm)),
                   drop = FALSE]
  # one price per ProductKey (first row wins if duplicates exist)
  pm_fl_join <- pm_fl_raw[!duplicated(pm_fl_raw$ProductKey), , drop = FALSE]
  t_products_prices <- .pt(t0_products_prices)

  # Build precomputed price lookup vectors (built once; replaces per-tree merge()).
  # Sawlog: key = "ProductKey|LengthClass_dm|PriceDiameterClass_cm"
  # The candidate's PriceDiameterClass_cm_used maps to PriceDiameterClass_cm in pm.
  pm_saw_lookup <- setNames(
    as.numeric(pm_mat_join$PriceNOKperM3),
    paste(pm_mat_join$ProductKey, pm_mat_join$LengthClass_dm,
          pm_mat_join$PriceDiameterClass_cm, sep = "|"))
  # Flat: key = ProductKey
  pm_flat_lookup <- setNames(
    as.numeric(pm_fl_join$PriceNOKperM3),
    as.character(pm_fl_join$ProductKey))

  # ---- 7. Build tree_data metadata lookup -----------------------------------
  # tree_data: needed for metadata lookups inside the per-tree loop.
  # Rename plot_id_github -> plot_id for helper compatibility.
  tree_data <- trees_valid[, c("plot_id_github", "treid", "sesong",
                                "species_group", "dbh_cm", "height_m"),
                           drop = FALSE]
  names(tree_data)[names(tree_data) == "plot_id_github"] <- "plot_id"

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

  # Fast tree-metadata lookup by treid.
  td_list <- setNames(
    lapply(seq_len(nrow(tree_data)), function(i) tree_data[i, , drop = FALSE]),
    tree_data$treid)

  # ---- 9. Per-tree candidate generation, pricing, and optimization ----------
  all_selected  <- vector("list", n_trees_in_optim)
  all_tree_rows <- vector("list", n_trees_in_optim)
  all_unpriced  <- vector("list", n_trees_in_optim)
  n_unpriced_total <- 0L
  # Per-stage accumulators (seconds, summed across all trees).
  acc_cand_gen   <- 0
  acc_price_join <- 0
  acc_dp_optim   <- 0

  for (i in seq_len(n_trees_in_optim)) {

    tid  <- tree_ids_valid[i]
    pv   <- pvs[[tid]]
    tmeta <- td_list[[tid]]

    if (is.null(tmeta)) next   # should not happen

    # Helper: build a no-logs tree row with a given status.
    make_tree_row <- function(status, msg) {
      data.frame(
        plot_id          = as.character(tmeta$plot_id),
        treid            = tid,
        species_group    = pv$species_group,
        dbh_cm           = as.numeric(tmeta$dbh_cm),
        height_m         = as.numeric(tmeta$height_m),
        n_logs_selected  = 0L,
        total_value_nok  = 0,
        total_volume_m3  = 0,
        commercial_volume_m3 = 0,
        nok_per_m3       = NA_real_,
        optimizer_status  = status,
        optimizer_message = msg,
        stringsAsFactors = FALSE
      )
    }

    # Generate all candidate log combinations.
    t0_cand <- proc.time()
    cands_raw <- tryCatch(
      generate_candidate_logs_one_tree(pv, pd, lc, stump_cm, step_cm),
      error = function(e) {
        warning(sprintf("Tree %s: candidate generation error: %s",
                        tid, conditionMessage(e)))
        NULL
      }
    )
    acc_cand_gen <- acc_cand_gen + .pt(t0_cand)

    if (is.null(cands_raw) || nrow(cands_raw) == 0L) {
      all_tree_rows[[i]] <- make_tree_row(
        "no_candidates", "Candidate generation returned 0 rows")
      next
    }

    # Keep only technically eligible candidates with valid volume.
    elig <- cands_raw[
      cands_raw$eligible & !is.na(cands_raw$price_volume_m3), , drop = FALSE]

    if (nrow(elig) == 0L) {
      all_tree_rows[[i]] <- make_tree_row(
        "no_candidates", "No eligible candidates after eligibility filter")
      next
    }

    # Add PriceDiameterClass_cm_uncapped and PriceDiameterClass_cm_used.
    t0_join <- proc.time()
    elig <- prepare_price_join_keys(elig, BACKEND_MAX_PRICE_DIAMETER_CLASS_CM)

    # Look up prices using precomputed named-vector lookups.
    # This replaces per-tree merge() calls with O(n) vectorised hash lookup.
    is_saw <- grepl("sawlog", elig$ProductKey, ignore.case = TRUE)
    elig$PriceNOKperM3 <- NA_real_
    if (any(is_saw)) {
      saw_keys <- paste(elig$ProductKey[is_saw],
                        elig$LengthClass_dm[is_saw],
                        elig$PriceDiameterClass_cm_used[is_saw], sep = "|")
      elig$PriceNOKperM3[is_saw] <- pm_saw_lookup[saw_keys]
    }
    if (any(!is_saw)) {
      elig$PriceNOKperM3[!is_saw] <- pm_flat_lookup[elig$ProductKey[!is_saw]]
    }
    priced           <- elig
    priced$value_nok <- priced$price_volume_m3 * priced$PriceNOKperM3
    acc_price_join <- acc_price_join + .pt(t0_join)

    # Track unpriced candidates compactly (ProductKey x LengthClass_dm counts).
    unpriced_rows <- priced[is.na(priced$PriceNOKperM3), , drop = FALSE]
    n_unpriced_total <- n_unpriced_total + nrow(unpriced_rows)
    if (nrow(unpriced_rows) > 0L) {
      up_tab <- as.data.frame(
        table(ProductKey     = unpriced_rows$ProductKey,
              LengthClass_dm = as.character(unpriced_rows$LengthClass_dm)),
        stringsAsFactors = FALSE)
      up_tab <- up_tab[up_tab$Freq > 0L, , drop = FALSE]
      names(up_tab)[names(up_tab) == "Freq"] <- "n_candidates"
      up_tab$LengthClass_dm <- as.integer(up_tab$LengthClass_dm)
      all_unpriced[[i]] <- up_tab
    }

    # DP optimizer: only priced candidates enter.
    dp_in <- priced[
      !is.na(priced$PriceNOKperM3) & !is.na(priced$value_nok), , drop = FALSE]

    if (nrow(dp_in) == 0L) {
      all_tree_rows[[i]] <- make_tree_row(
        "no_priced_candidates", "No priced candidates; all candidates unpriced")
      next
    }

    t0_dp <- proc.time()
    res <- tryCatch(
      .optimize_dp_fast(dp_in),
      error = function(e) {
        warning(sprintf("Tree %s: optimizer error: %s", tid, conditionMessage(e)))
        list(selected_logs = data.frame(), dp_best_value = 0, n_selected = 0L,
             n_dp_candidates = nrow(dp_in), optimizer_status = "error",
             optimizer_message = conditionMessage(e))
      }
    )
    acc_dp_optim <- acc_dp_optim + .pt(t0_dp)

    sel_vol <- if (res$n_selected > 0L)
      sum(res$selected_logs$price_volume_m3, na.rm = TRUE) else 0
    # Commercial volume under bark (the NOK/m3 denominator) per selected log.
    if (res$n_selected > 0L)
      res$selected_logs$commercial_volume_m3 <- commercial_volume_ub(
        pv$diameterPosition, pv$DiameterValueUB, res$selected_logs)
    com_vol <- if (res$n_selected > 0L)
      sum(res$selected_logs$commercial_volume_m3) else 0

    all_tree_rows[[i]] <- data.frame(
      plot_id           = as.character(tmeta$plot_id),
      treid             = tid,
      species_group     = pv$species_group,
      dbh_cm            = as.numeric(tmeta$dbh_cm),
      height_m          = as.numeric(tmeta$height_m),
      n_logs_selected   = res$n_selected,
      total_value_nok   = res$dp_best_value,
      total_volume_m3   = sel_vol,
      commercial_volume_m3 = com_vol,
      nok_per_m3        = if (com_vol > 0) res$dp_best_value / com_vol else NA_real_,
      optimizer_status  = res$optimizer_status,
      optimizer_message = res$optimizer_message,
      stringsAsFactors  = FALSE
    )

    if (res$n_selected > 0L) {
      sel <- res$selected_logs
      sel$plot_id <- as.character(tmeta$plot_id)
      all_selected[[i]] <- sel
    }
  }   # end per-tree loop

  # ---- 10. Assemble tree_summary ---------------------------------------------
  t0_summary <- proc.time()
  tree_summary_raw <- do.call(
    rbind, Filter(Negate(is.null), all_tree_rows)) %||% .empty_tree_summary()

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
    sel_all_raw <- do.call(rbind, sel_list)
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

  # ---- 16. Diagnostics --------------------------------------------------------
  n_zero_log <- if (nrow(tree_summary) > 0L)
    sum(tree_summary$n_logs_selected == 0L) else 0L

  t_summary      <- .pt(t0_summary)
  elapsed_secs   <- as.numeric((proc.time() - t_start)["elapsed"])

  # Assemble stage timings data.frame.
  stage_names <- c("load_data", "filter_stratum_and_sample_plots",
                   "tree_filtering", "load_products_and_prices",
                   "load_precomputed_taper_profiles", "profile_vector_conversion",
                   "under_bark_profiles",
                   "candidate_generation", "price_joining", "dp_optimization",
                   "summary_assembly", "total")
  stage_secs  <- round(c(t_load_data, t_filter_sample, t_tree_filter,
                          t_products_prices, t_taper_profiles, t_profile_convert,
                          t_under_bark,
                          acc_cand_gen, acc_price_join, acc_dp_optim,
                          t_summary, elapsed_secs), 3)
  stage_timings <- data.frame(stage = stage_names, elapsed_seconds = stage_secs,
                               stringsAsFactors = FALSE)

  diagnostics <- list(
    n_eligible_plots                           = n_eligible_plots,
    n_requested_plots                          = n_plots_req,
    n_plots_sampled                            = n_plots_sampled,
    n_trees_before_filter                      = n_trees_before_filter,
    n_trees_after_filter                       = n_trees_after_filter,
    n_trees_dropped_low_dbh_or_height          = n_trees_dropped,
    taper_source                               = "precomputed",
    bark_model                                 = getOption("apter.bark_model", DEFAULT_BARK_MODEL),
    taper_profile_file                         = basename(taper_cache_dir),
    n_profiles_loaded                          = n_profiles_loaded,
    n_trees_with_precomputed_profiles          = n_trees_with_profile,
    n_valid_trees_missing_precomputed_profiles = n_trees_missing,
    n_trees_entering_optimizer                 = n_trees_in_optim,
    n_zero_log_trees                           = n_zero_log,
    n_unpriced_candidates                      = n_unpriced_total,
    price_review_items                         = price_review_items,
    elapsed_seconds                            = elapsed_secs,
    stage_timings                              = stage_timings
  )

  # ---- 17. Final safety assertions -------------------------------------------
  .assert_no_plot_id(tree_summary,      "tree_summary (final)")
  .assert_no_plot_id(selected_logs_all, "selected_logs_all (final)")
  .assert_no_plot_id(per_m3_summary,       "per_m3_summary (final)")
  .assert_no_plot_id(per_m3_by_assortment, "per_m3_by_assortment (final)")
  .assert_no_plot_id(zero_log_trees,    "zero_log_trees (final)")
  .assert_no_plot_id(profile_failures,  "profile_failures (final)")

  # ---- 18. Return -------------------------------------------------------------
  list(
    run_id                  = paste0("sampled_", format(Sys.time(), "%Y%m%d_%H%M%S")),
    params                  = params,
    n_eligible_plots        = n_eligible_plots,
    n_plots_sampled         = n_plots_sampled,
    n_trees_total           = n_trees_after_filter,
    n_trees_with_logs       = n_trees_with_logs,
    selected_logs_all       = selected_logs_all,
    tree_summary            = tree_summary,
    per_m3_summary          = per_m3_summary,
    per_m3_by_assortment    = per_m3_by_assortment,
    sampled_plot_ids        = sort(as.character(sampled_plots)),
    product_summary         = prod_summary,
    unpriced_summary        = unpriced_summary,
    zero_log_trees          = zero_log_trees,
    price_validation_checks = price_val_checks,
    profile_failures        = profile_failures,
    diagnostics             = diagnostics,
    label                   = paste("Results describe the selected trees only.",
                                    "Values are NOK per m\u00b3 of commercial volume under bark."),
    elapsed_seconds         = elapsed_secs
  )
}
