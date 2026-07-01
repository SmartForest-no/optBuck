# ==============================================================================
# scripts/app_backend/01_run_sampled_plot_bucking.R
#
# Purpose : Sampled-plot bucking backend for nfiBuck (Shiny app v1 support).
#           Exposes a single function:
#             run_sampled_plot_bucking(params, price_matrix_path, root)
#
# Design principles (from interface inspection 2026-07-01):
#   - Tree filtering matches script 11 exactly: dbh_cm >= 5.0 AND
#     height_m >= 4.5. No tilstand filter (matching prototype convention).
#   - plot_id_github is renamed to plot_id before all helper calls, and
#     renamed back before any data is returned. No returned object contains
#     a column named plot_id.
#   - Price join is done manually (sawlogs by 3 keys; non-sawlogs by 1 key)
#     because no single helper function covers the full join.
#   - Data is loaded from data_processed_github/ by default.
#   - This script never writes output files.
#
# Usage:
#   source("scripts/app_backend/01_run_sampled_plot_bucking.R")
#   result <- run_sampled_plot_bucking(
#     params = list(species_group = "spruce", hogstkl = 41L, abonitet = 17L,
#                   n_plots = 10L, seed = 42L),
#     price_matrix_path = "default_2021"
#   )
#
# Smoke test (run interactively):
#   result <- run_sampled_plot_bucking(
#     params = list(species_group = "spruce", hogstkl = 41L, abonitet = 17L,
#                   n_plots = 5L, seed = 42L),
#     price_matrix_path = "default_2021"
#   )
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(readr)
})

# ---- source helpers ----------------------------------------------------------
# Resolve R/ relative to project root (supports NFIBUCK_ROOT env var and the
# default hardcoded path used throughout the project).
.BACKEND_ROOT <- local({
  env <- Sys.getenv("NFIBUCK_ROOT", unset = "")
  if (nchar(env) > 0L) normalizePath(env, winslash = "/", mustWork = FALSE)
  else normalizePath("S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck",
                     winslash = "/", mustWork = FALSE)
})

for (.h in c("helpers_paths", "helpers_products", "helpers_valuation",
             "helpers_taper", "helpers_candidate_generation",
             "helpers_dp_optimizer")) {
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
             stringsAsFactors = FALSE)
}

.empty_tree_summary <- function() {
  data.frame(plot_id_github = character(0), treid = character(0),
             species_group = character(0), dbh_cm = numeric(0),
             height_m = numeric(0), n_logs_selected = integer(0),
             total_value_nok = numeric(0), total_volume_m3 = numeric(0),
             optimizer_status = character(0), optimizer_message = character(0),
             stringsAsFactors = FALSE)
}

.empty_unpriced_summary <- function() {
  data.frame(ProductKey = character(0), LengthClass_dm = integer(0),
             n_candidates = integer(0), stringsAsFactors = FALSE)
}

# ==============================================================================
# Main exported function
# ==============================================================================

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
  gh_dir <- file.path(root, "data_processed_github")
  if (!dir.exists(gh_dir))
    stop("data_processed_github/ not found at: ", gh_dir)

  trees_all <- arrow::read_parquet(
    file.path(gh_dir, "nfi_tree_plot_stratum_ready_github.parquet"))
  plots_all <- arrow::read_parquet(
    file.path(gh_dir, "nfi_plots_github.parquet"))

  # ---- 3. Filter to requested stratum ----------------------------------------
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

  # ---- 5. Compute height_m and apply taperNOR validity filter ----------------
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

  # ---- 6. Load products and price matrix -------------------------------------
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

  # ---- 7. Build tree_data for taper helpers ----------------------------------
  # Rename plot_id_github -> plot_id so helpers receive the expected column name.
  tree_data <- trees_valid[, c("plot_id_github", "treid", "sesong",
                                "species_group", "dbh_cm", "height_m"),
                           drop = FALSE]
  names(tree_data)[names(tree_data) == "plot_id_github"] <- "plot_id"

  # Also keep plot_area_m2 for later plot_summary (keyed by original order).
  plot_area_lookup <- unique(
    trees_valid[, c("plot_id_github", "plot_area_m2"), drop = FALSE])

  # ---- 8. Generate taper profiles --------------------------------------------
  profiles    <- generate_taper_profiles_for_trees(tree_data, quiet = TRUE)
  profile_ok  <- attr(profiles, "profile_ok")  %||% rep(TRUE, nrow(tree_data))
  profile_err <- attr(profiles, "profile_errors") %||% rep("", nrow(tree_data))

  n_profile_fail <- sum(!profile_ok)

  profile_failures <- if (n_profile_fail > 0L) {
    fd <- tree_data[!profile_ok, c("plot_id", "treid", "species_group",
                                   "dbh_cm", "height_m"), drop = FALSE]
    fd$error_message <- profile_err[!profile_ok]
    .rename_to_github(fd)
  } else {
    data.frame(plot_id_github = character(0), treid = character(0),
               species_group = character(0), dbh_cm = numeric(0),
               height_m = numeric(0), error_message = character(0),
               stringsAsFactors = FALSE)
  }

  pvs              <- profiles_to_profile_vectors(profiles)
  tree_ids_valid   <- names(pvs)
  n_trees_in_optim <- length(tree_ids_valid)

  # Fast tree-metadata lookup by treid.
  td_list <- setNames(
    lapply(seq_len(nrow(tree_data)), function(i) tree_data[i, , drop = FALSE]),
    tree_data$treid)

  # ---- 9. Per-tree candidate generation, pricing, and optimization ----------
  all_selected  <- vector("list", n_trees_in_optim)
  all_tree_rows <- vector("list", n_trees_in_optim)
  all_unpriced  <- vector("list", n_trees_in_optim)
  n_unpriced_total <- 0L

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
        optimizer_status  = status,
        optimizer_message = msg,
        stringsAsFactors = FALSE
      )
    }

    # Generate all candidate log combinations.
    cands_raw <- tryCatch(
      generate_candidate_logs_one_tree(pv, pd, lc, stump_cm, step_cm),
      error = function(e) {
        warning(sprintf("Tree %s: candidate generation error: %s",
                        tid, conditionMessage(e)))
        NULL
      }
    )

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
    elig <- prepare_price_join_keys(elig, BACKEND_MAX_PRICE_DIAMETER_CLASS_CM)

    # Split by product type for the price join.
    is_saw  <- grepl("sawlog", elig$ProductKey, ignore.case = TRUE)
    saw_c   <- elig[ is_saw, , drop = FALSE]
    flat_c  <- elig[!is_saw, , drop = FALSE]

    # Sawlog price join: ProductKey + LengthClass_dm + PriceDiameterClass_cm_used
    # matches pm_mat_join's ProductKey + LengthClass_dm + PriceDiameterClass_cm.
    if (nrow(saw_c) > 0L) {
      saw_c <- merge(
        saw_c, pm_mat_join,
        by.x = c("ProductKey", "LengthClass_dm", "PriceDiameterClass_cm_used"),
        by.y = c("ProductKey", "LengthClass_dm", "PriceDiameterClass_cm"),
        all.x = TRUE)
    } else {
      saw_c$PriceNOKperM3 <- numeric(0)
    }

    # Flat price join: ProductKey only.
    if (nrow(flat_c) > 0L) {
      flat_c <- merge(flat_c, pm_fl_join, by = "ProductKey", all.x = TRUE)
    } else {
      flat_c$PriceNOKperM3 <- numeric(0)
    }

    priced           <- rbind(saw_c, flat_c)
    priced$value_nok <- priced$price_volume_m3 * priced$PriceNOKperM3

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

    res <- tryCatch(
      optimize_one_tree_dp(dp_in),
      error = function(e) {
        warning(sprintf("Tree %s: optimizer error: %s", tid, conditionMessage(e)))
        list(selected_logs = data.frame(), dp_best_value = 0, n_selected = 0L,
             n_dp_candidates = nrow(dp_in), optimizer_status = "error",
             optimizer_message = conditionMessage(e))
      }
    )

    sel_vol <- if (res$n_selected > 0L)
      sum(res$selected_logs$price_volume_m3, na.rm = TRUE) else 0

    all_tree_rows[[i]] <- data.frame(
      plot_id           = as.character(tmeta$plot_id),
      treid             = tid,
      species_group     = pv$species_group,
      dbh_cm            = as.numeric(tmeta$dbh_cm),
      height_m          = as.numeric(tmeta$height_m),
      n_logs_selected   = res$n_selected,
      total_value_nok   = res$dp_best_value,
      total_volume_m3   = sel_vol,
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
                         "PriceDiameterClass_cm_used")

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

  # ---- 12. plot_summary -------------------------------------------------------
  if (nrow(tree_summary) > 0L) {
    ps_agg <- do.call(rbind, lapply(sampled_plots, function(pid) {
      rows <- tree_summary[tree_summary$plot_id_github == pid, , drop = FALSE]
      n_tr   <- nrow(rows)
      n_logs <- sum(rows$n_logs_selected > 0L, na.rm = TRUE)
      tot_v  <- sum(rows$total_value_nok,  na.rm = TRUE)
      tot_m3 <- sum(rows$total_volume_m3,  na.rm = TRUE)
      area_row <- plot_area_lookup[
        plot_area_lookup$plot_id_github == pid, , drop = FALSE]
      area <- if (nrow(area_row) > 0L) area_row$plot_area_m2[1L] else NA_real_
      data.frame(
        plot_id_github    = pid,
        n_trees           = n_tr,
        n_trees_with_logs = n_logs,
        total_value_nok   = round(tot_v, 2),
        total_volume_m3   = round(tot_m3, 5),
        plot_area_m2      = area,
        value_per_ha_nok  = if (!is.na(area) && area > 0)
                              round(tot_v / area * 10000, 2) else NA_real_,
        volume_per_ha_m3  = if (!is.na(area) && area > 0)
                              round(tot_m3 / area * 10000, 5) else NA_real_,
        stringsAsFactors  = FALSE
      )
    }))
    plot_summary <- ps_agg
  } else {
    plot_summary <- data.frame(
      plot_id_github = sampled_plots, n_trees = 0L,
      n_trees_with_logs = 0L, total_value_nok = 0,
      total_volume_m3 = 0, plot_area_m2 = NA_real_,
      value_per_ha_nok = NA_real_, volume_per_ha_m3 = NA_real_,
      stringsAsFactors = FALSE)
  }
  .assert_no_plot_id(plot_summary, "plot_summary")

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

  elapsed_secs <- as.numeric((proc.time() - t_start)["elapsed"])

  diagnostics <- list(
    n_eligible_plots               = n_eligible_plots,
    n_requested_plots              = n_plots_req,
    n_plots_sampled                = n_plots_sampled,
    n_trees_before_filter          = n_trees_before_filter,
    n_trees_after_filter           = n_trees_after_filter,
    n_trees_dropped_low_dbh_or_height = n_trees_dropped,
    n_profile_failures             = n_profile_fail,
    n_trees_entering_optimizer     = n_trees_in_optim,
    n_zero_log_trees               = n_zero_log,
    n_unpriced_candidates          = n_unpriced_total,
    price_review_items             = price_review_items,
    elapsed_seconds                = elapsed_secs
  )

  # ---- 17. Final safety assertions -------------------------------------------
  .assert_no_plot_id(tree_summary,      "tree_summary (final)")
  .assert_no_plot_id(selected_logs_all, "selected_logs_all (final)")
  .assert_no_plot_id(plot_summary,      "plot_summary (final)")
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
    plot_summary            = plot_summary,
    product_summary         = prod_summary,
    unpriced_summary        = unpriced_summary,
    zero_log_trees          = zero_log_trees,
    price_validation_checks = price_val_checks,
    profile_failures        = profile_failures,
    diagnostics             = diagnostics,
    label                   = "Sampled-plot estimate. Not ha-expanded. No NFI factors applied.",
    elapsed_seconds         = elapsed_secs
  )
}
