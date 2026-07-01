# ==============================================================================
# R/08_run_bucking_on_example_sample.R
#
# Purpose : Run the accepted one-tree bucking workflow (scripts 06/07 logic)
#           on all trees in the current 500-tree example sample.
#           Tests robustness across the full sample and produces tree-level
#           and product-level summaries.
#
#           This is NOT the full NFI stratum run.
#           No expansion factors.  No scaling to ha.
#           No bark correction.  No HPR / StanForD.
#           ProductData, LengthClasses, and PriceMatrix_long are not modified.
#           31 dm sawlog price gap is reported but not fixed.
#
# Positions are in cm above terrain.  The first bucking position is the assumed
# stump cut height.  Default 10 cm; later sensitivity tests can use
# 5, 10, 20, and 30 cm.
#
# Inputs  :
#   outputs/stem_profiles/stem_profiles_example_500trees.parquet
#   outputs/stem_profiles/stem_profile_tree_summary_example_500trees.csv
#   inputs/ProductData_spruce_pine_default.csv
#   inputs/LengthClasses_spruce_pine_default.csv
#   inputs/price_matrices/PriceMatrix_long_default_2021.csv
#
# Outputs :
#   outputs/example_sample_bucking/sample_selected_logs.csv
#   outputs/example_sample_bucking/sample_tree_summary.csv
#   outputs/example_sample_bucking/sample_product_summary.csv
#   outputs/example_sample_bucking/sample_diagnostics.csv
#   outputs/example_sample_bucking/sample_bucking_summary.txt
#   outputs/example_sample_bucking/plot_value_by_dbh.png
#   outputs/example_sample_bucking/plot_n_logs_by_dbh.png
#   outputs/example_sample_bucking/plot_volume_by_product.png
# ==============================================================================


# ---- setup ----

library(dplyr)
library(readr)
library(arrow)
library(ggplot2)
library(tibble)

# Positions are in cm above terrain. The first bucking position is the assumed
# stump cut height. Default 10 cm; later sensitivity tests can use
# 5, 10, 20, and 30 cm.
STUMP_HEIGHT_CM <- 10L

# Profile step must match the interval used in stem profile generation.
PROFILE_STEP_CM <- 10L

root_dir   <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
inputs_dir <- file.path(root_dir, "inputs")
prof_dir   <- file.path(root_dir, "outputs", "stem_profiles")
out_dir    <- file.path(root_dir, "outputs", "example_sample_bucking")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

log_file <- file.path(out_dir, "sample_bucking_summary.txt")
cat("", file = log_file)

wlog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = log_file, append = TRUE)
}

save_plot <- function(p, filename, width = 20, height = 13,
                      units = "cm", dpi = 150) {
  path <- file.path(out_dir, filename)
  ggplot2::ggsave(path, plot = p, width = width, height = height,
                  units = units, dpi = dpi)
  wlog(sprintf("  Plot saved: %s", basename(path)))
}

wlog(
  "=============================================================",
  "  Example sample bucking run – log",
  sprintf("  Run                 : %s", Sys.time()),
  sprintf("  STUMP_HEIGHT_CM     : %d cm", STUMP_HEIGHT_CM),
  sprintf("  PROFILE_STEP_CM     : %d cm", PROFILE_STEP_CM),
  "============================================================="
)


# ---- helpers ----

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

get_tree_profile_vectors <- function(profiles, tree_id) {
  tree_rows <- profiles[profiles$treid == tree_id, ]
  tree_rows <- tree_rows[order(tree_rows$diameterPosition), ]
  if (nrow(tree_rows) == 0)
    stop(sprintf("No profile rows for treid = '%s'.", tree_id))
  list(
    diameterPosition = tree_rows$diameterPosition,
    DiameterValue    = tree_rows$DiameterValue,
    DBH              = tree_rows$DBH[1],
    height_m         = tree_rows$height_m[1],
    height_cm        = round(tree_rows$height_m[1] * 100),
    species_group    = tree_rows$species_group[1],
    plot_id          = tree_rows$plot_id[1]
  )
}

run_dp_optimizer <- function(cands) {
  all_pos  <- sort(unique(c(cands$StartPos, cands$StopPos)))
  sentinel <- max(all_pos) + 1L
  all_pos  <- c(all_pos, sentinel)
  n_pos    <- length(all_pos)
  dp_val   <- numeric(n_pos)
  dp_log   <- integer(n_pos)
  for (i in seq(n_pos - 1L, 1L)) {
    pos      <- all_pos[i]
    best_val <- dp_val[i + 1L]
    best_log <- 0L
    here_idx <- which(cands$StartPos == pos)
    for (j in here_idx) {
      stop_i     <- match(cands$StopPos[j], all_pos)
      future_val <- if (!is.na(stop_i)) dp_val[stop_i] else 0
      total_val  <- cands$value_nok[j] + future_val
      if (total_val > best_val) { best_val <- total_val; best_log <- j }
    }
    dp_val[i] <- best_val
    dp_log[i] <- best_log
  }
  sel_rows <- integer(0)
  i <- 1L
  while (i < n_pos) {
    if (dp_log[i] > 0L) {
      j        <- dp_log[i]
      sel_rows <- c(sel_rows, j)
      stop_i   <- match(cands$StopPos[j], all_pos)
      i        <- if (!is.na(stop_i)) stop_i else n_pos
    } else {
      i <- i + 1L
    }
  }
  list(dp_val        = dp_val,
       dp_log        = dp_log,
       all_positions = all_pos,
       selected_rows = sel_rows,
       best_value    = dp_val[1])
}


# ---- read inputs ----

wlog("", "--- Reading inputs ---")

prof_parquet <- file.path(prof_dir, "stem_profiles_example_500trees.parquet")
prof_rds     <- file.path(prof_dir, "stem_profiles_example_500trees.rds")

if (file.exists(prof_parquet)) {
  stem_profiles_raw <- arrow::read_parquet(prof_parquet)
  wlog(sprintf("  Stem profiles  : %s", basename(prof_parquet)))
} else if (file.exists(prof_rds)) {
  stem_profiles_raw <- readRDS(prof_rds)
  wlog(sprintf("  Stem profiles  : %s", basename(prof_rds)))
} else {
  stop("Stem profile file not found. Run 02_generate_stem_profiles.R first.\n  ",
       prof_parquet)
}

# Optional: load tree summary for context (plot/stratum metadata).
tree_summary_csv <- file.path(prof_dir,
  "stem_profile_tree_summary_example_500trees.csv")
tree_meta <- if (file.exists(tree_summary_csv)) {
  readr::read_csv(tree_summary_csv, show_col_types = FALSE)
} else {
  NULL
}
if (!is.null(tree_meta)) {
  wlog(sprintf("  Tree summary   : %d rows from %s",
               nrow(tree_meta), basename(tree_summary_csv)))
}

stem_profiles <- stem_profiles_raw |>
  dplyr::mutate(
    diameter_mm      = if ("diameter_mm" %in% names(stem_profiles_raw))
                         diameter_mm else diameter_cm * 10,
    diameterPosition = as.integer(
      round(z_m * 100 / PROFILE_STEP_CM) * PROFILE_STEP_CM),
    DiameterValue    = diameter_mm,
    DBH              = dbh_cm * 10
  ) |>
  dplyr::distinct(treid, diameterPosition, .keep_all = TRUE)

all_tree_ids <- as.character(unique(stem_profiles$treid))
n_trees_total <- length(all_tree_ids)

wlog(sprintf("  Profile rows   : %d (%d trees)",
             nrow(stem_profiles), n_trees_total))
wlog(sprintf("  Species groups : %s",
             paste(sort(unique(stem_profiles$species_group)), collapse = ", ")))

pd_csv <- file.path(inputs_dir, "ProductData_spruce_pine_default.csv")
if (!file.exists(pd_csv))
  stop("ProductData not found: ", pd_csv)
ProductData <- readr::read_csv(pd_csv, show_col_types = FALSE)
wlog(sprintf("  ProductData    : %d products", nrow(ProductData)))

lc_csv <- file.path(inputs_dir, "LengthClasses_spruce_pine_default.csv")
if (!file.exists(lc_csv))
  stop("LengthClasses not found: ", lc_csv)
lc_long <- readr::read_csv(lc_csv, show_col_types = FALSE)
LengthClasses <- split(as.integer(lc_long$LogLength), lc_long$ProductKey)
wlog(sprintf("  LengthClasses  : %d products", length(LengthClasses)))

pm_csv <- file.path(inputs_dir, "price_matrices", "PriceMatrix_long_default_2021.csv")
if (!file.exists(pm_csv))
  stop("PriceMatrix_long not found: ", pm_csv)
PriceMatrix_long <- readr::read_csv(pm_csv, show_col_types = FALSE)
PRICE_SCENARIO   <- unique(PriceMatrix_long$PriceScenario)[1]
wlog(sprintf("  PriceMatrix    : %d rows, scenario '%s'",
             nrow(PriceMatrix_long), PRICE_SCENARIO))

# Build price lookup tables once.
sawlog_pks    <- unique(ProductData$ProductKey[ProductData$ProductType == "sawlog"])
nonsawlog_pks <- unique(ProductData$ProductKey[ProductData$ProductType != "sawlog"])

pm_matrix <- PriceMatrix_long |>
  dplyr::filter(PriceScenario == PRICE_SCENARIO, PriceType == "matrix") |>
  dplyr::select(ProductKey, LengthClass_dm, PriceDiameterClass_cm,
                PriceNOKperM3) |>
  dplyr::mutate(LengthClass_dm        = as.integer(LengthClass_dm),
                PriceDiameterClass_cm = as.integer(PriceDiameterClass_cm))

pm_flat <- PriceMatrix_long |>
  dplyr::filter(PriceScenario == PRICE_SCENARIO, PriceType == "flat") |>
  dplyr::select(ProductKey, PriceNOKperM3_flat = PriceNOKperM3)


# ---- per-tree optimizer (vectorized) ----
#
# Speed strategy:
#  - approxfun() built once per tree; vectorized diameter lookups
#  - Cumulative solid volume precomputed at 10-cm grid; section lookup is O(1)
#  - Candidate set built with expand.grid() + vector math, no per-candidate loop
#  - Price join uses merge(); DP unchanged

optimize_one_tree <- function(tree_id, profiles, pd, lc,
                               pm_mat, pm_fl, stump, step) {

  pv <- get_tree_profile_vectors(profiles, tree_id)
  sg <- pv$species_group

  sp_products <- pd[pd$species_group == sg, ]
  if (nrow(sp_products) == 0) {
    return(list(tree_id = tree_id, species_group = sg,
                dbh_mm = pv$DBH, height_cm = pv$height_cm,
                n_generated = 0L, n_priced = 0L,
                n_selected = 0L, total_value_nok = 0,
                total_volume_m3 = 0, total_length_cm = 0L,
                n_overlapping = 0L, n_unmatched_price = 0L,
                n_dm31_unpriced = 0L, dp_best_value = 0,
                selected_logs = NULL,
                error = paste0("No products for species_group='", sg, "'")))
  }

  # ---- vectorized diameter function (built once per tree) ----
  diam_fun <- approxfun(pv$diameterPosition, pv$DiameterValue, rule = 2)

  # ---- precompute cumulative solid volume on 10-cm grid ----
  # All positions from 0 to height_cm in steps of `step`.
  # Since stump, log lengths, and profile positions are all multiples of `step`,
  # every relevant StartPos and StopPos is on this grid.
  prof_pos <- seq(0L, as.integer(pv$height_cm), by = as.integer(step))
  prof_d   <- diam_fun(prof_pos)          # diameter (mm) at each grid point
  sec_r    <- (prof_d[-length(prof_d)] + prof_d[-1]) / 4  # mean radius over section
  sec_len  <- as.integer(step)
  sec_vol  <- pi * sec_r^2 * sec_len / 1e8   # m3 per 10-cm section
  cumvol   <- c(0, cumsum(sec_vol))            # cumvol[k] = vol from 0 to prof_pos[k]
  # Index: position p -> cumvol index = p / step + 1  (both 0-based and 1-indexed)
  step_int <- as.integer(step)

  solid_vol_vec <- function(pos_a, pos_b) {
    # Vectorized: pos_a and pos_b are integer vectors, multiples of step_int.
    ia <- pos_a / step_int + 1L
    ib <- pos_b / step_int + 1L
    cumvol[ib] - cumvol[ia]
  }

  # ---- generate candidates per product (vectorized) ----
  start_positions <- seq(
    as.integer(stump),
    as.integer(pv$height_cm - 10L),   # at least 10 cm headroom
    by = step_int
  )

  if (length(start_positions) == 0) {
    return(list(tree_id = tree_id, species_group = sg,
                dbh_mm = pv$DBH, height_cm = pv$height_cm,
                n_generated = 0L, n_priced = 0L,
                n_selected = 0L, total_value_nok = 0,
                total_volume_m3 = 0, total_length_cm = 0L,
                n_overlapping = 0L, n_unmatched_price = 0L,
                n_dm31_unpriced = 0L, dp_best_value = 0,
                selected_logs = NULL, error = "Tree too short for any log"))
  }

  prod_frames <- vector("list", nrow(sp_products))

  for (j in seq_len(nrow(sp_products))) {
    prow   <- sp_products[j, ]
    pk     <- prow$ProductKey
    lc_vec <- as.integer(lc[[pk]])

    # All (start_pos, log_len) combos where stop_pos fits within the tree.
    combos <- expand.grid(start_pos = start_positions,
                          log_len   = lc_vec,
                          KEEP.OUT.ATTRS = FALSE,
                          stringsAsFactors = FALSE)
    combos$stop_pos <- combos$start_pos + combos$log_len
    combos <- combos[combos$stop_pos <= pv$height_cm, , drop = FALSE]
    if (nrow(combos) == 0) next

    # Vectorized diameter lookups.
    top_meas_pos        <- combos$stop_pos - as.integer(prow$DiameterTopPosition)
    combos$TopDiameter  <- diam_fun(top_meas_pos)
    combos$ButtDiameter <- diam_fun(combos$start_pos)

    # Top diameter rounding adjustment (sawlogs).
    top_d_vol <- if (prow$VolumeDiameterAdjustment ==
                     "Measured diameter rounded downwards to cm") {
      floor(combos$TopDiameter / 10) * 10
    } else {
      combos$TopDiameter
    }

    # Eligibility (vectorized).
    e_len  <- combos$log_len >= prow$MinLength &
               combos$log_len <= prow$MaxLength
    e_top  <- !is.na(combos$TopDiameter) &
               combos$TopDiameter >= prow$MinTopDiameter &
               combos$TopDiameter <= prow$MaxTopDiameter
    e_butt <- !is.na(combos$ButtDiameter) &
               combos$ButtDiameter >= prow$MinButtDiameter &
               combos$ButtDiameter <= prow$MaxButtDiameter

    # For "Length as defined in LengthClasses": since we iterate exactly over
    # lc_vec entries, log_len IS a valid length class, so eff_length = log_len.
    # For other volume length categories, eff_length also equals log_len here.
    eff_length  <- combos$log_len
    eff_stop    <- combos$stop_pos

    eligible <- e_len & e_top & e_butt

    # Volume (vectorized, only for eligible rows).
    if (prow$VolumeDiameterCategory == "Top") {
      r_mm <- top_d_vol / 2
      price_vol <- ifelse(
        eligible & !is.na(r_mm) & r_mm > 0,
        pi * r_mm^2 * eff_length / 1e8,
        NA_real_)
    } else {
      # Solid volume via precomputed cumulative volumes.
      price_vol <- ifelse(
        eligible,
        solid_vol_vec(combos$start_pos, eff_stop),
        NA_real_)
    }

    prod_frames[[j]] <- data.frame(
      treid                  = as.character(tree_id),
      plot_id                = pv$plot_id,
      species_group          = sg,
      ProductKey             = pk,
      ProductType            = prow$ProductType,
      StartPos               = as.integer(combos$start_pos),
      StopPos                = as.integer(combos$stop_pos),
      LogLength              = as.integer(combos$log_len),
      TopDiameter            = round(combos$TopDiameter, 1),
      ButtDiameter           = round(combos$ButtDiameter, 1),
      eligible               = eligible,
      price_volume_m3        = round(price_vol, 5),
      LengthClass_dm         = as.integer(eff_length / 10L),
      PriceDiameterClass_cm  = as.integer(floor(combos$TopDiameter / 10)),
      stringsAsFactors = FALSE
    )
  }

  generated <- do.call(rbind, prod_frames[!vapply(prod_frames, is.null, logical(1))])
  n_gen     <- nrow(generated)

  # ---- price join ----
  elig_c      <- generated[generated$eligible & !is.na(generated$price_volume_m3), ]
  sawlog_c    <- elig_c[elig_c$ProductKey %in% sawlog_pks, ]
  nonsawlog_c <- elig_c[elig_c$ProductKey %in% nonsawlog_pks, ]

  if (nrow(sawlog_c) > 0) {
    sawlog_c <- merge(sawlog_c, pm_mat,
                      by = c("ProductKey", "LengthClass_dm", "PriceDiameterClass_cm"),
                      all.x = TRUE)
  } else {
    sawlog_c$PriceNOKperM3 <- numeric(0)
  }
  if (nrow(nonsawlog_c) > 0) {
    nonsawlog_c <- merge(nonsawlog_c, pm_fl, by = "ProductKey", all.x = TRUE)
    names(nonsawlog_c)[names(nonsawlog_c) == "PriceNOKperM3_flat"] <- "PriceNOKperM3"
  } else {
    nonsawlog_c$PriceNOKperM3 <- numeric(0)
  }

  priced           <- rbind(sawlog_c, nonsawlog_c)
  priced$value_nok <- priced$price_volume_m3 * priced$PriceNOKperM3

  n_unmatched <- sum(is.na(priced$PriceNOKperM3))
  n_dm31      <- sum(!is.na(priced$LengthClass_dm) &
                       priced$LengthClass_dm == 31 &
                       is.na(priced$PriceNOKperM3))

  dp_in <- priced[!is.na(priced$PriceNOKperM3) &
                    !is.na(priced$value_nok) &
                    !is.na(priced$price_volume_m3), ]
  dp_in$StartPos <- as.integer(round(dp_in$StartPos))
  dp_in$StopPos  <- as.integer(round(dp_in$StopPos))
  n_priced       <- nrow(dp_in)

  if (n_priced == 0) {
    return(list(tree_id = tree_id, species_group = sg,
                dbh_mm = pv$DBH, height_cm = pv$height_cm,
                n_generated = n_gen, n_priced = 0L,
                n_selected = 0L, total_value_nok = 0,
                total_volume_m3 = 0, total_length_cm = 0L,
                n_overlapping = 0L, n_unmatched_price = n_unmatched,
                n_dm31_unpriced = n_dm31, dp_best_value = 0,
                selected_logs = NULL, error = NA_character_))
  }

  # ---- DP optimizer ----
  dp  <- run_dp_optimizer(dp_in)
  sel <- if (length(dp$selected_rows) > 0) {
    s <- dp_in[dp$selected_rows, ]
    s[order(s$StartPos), ]
  } else {
    dp_in[integer(0), ]
  }

  n_overlap <- 0L
  if (nrow(sel) > 1) {
    for (k in seq_len(nrow(sel) - 1L)) {
      if (sel$StopPos[k] > sel$StartPos[k + 1L]) n_overlap <- n_overlap + 1L
    }
  }

  list(
    tree_id           = tree_id,
    species_group     = sg,
    dbh_mm            = pv$DBH,
    height_cm         = pv$height_cm,
    n_generated       = n_gen,
    n_priced          = n_priced,
    n_selected        = nrow(sel),
    total_value_nok   = round(sum(sel$value_nok,      na.rm = TRUE), 2),
    total_volume_m3   = round(sum(sel$price_volume_m3, na.rm = TRUE), 5),
    total_length_cm   = sum(sel$LogLength,            na.rm = TRUE),
    n_overlapping     = n_overlap,
    n_unmatched_price = n_unmatched,
    n_dm31_unpriced   = n_dm31,
    dp_best_value     = round(dp$best_value, 2),
    selected_logs     = sel,
    error             = NA_character_
  )
}


# ---- run on all trees ----

wlog(sprintf("\n--- Running optimizer on all %d trees ---", n_trees_total))
t_start <- proc.time()

results_list <- vector("list", n_trees_total)
progress_step <- max(1L, as.integer(n_trees_total / 10L))

for (i in seq_along(all_tree_ids)) {
  tid <- all_tree_ids[i]
  results_list[[i]] <- tryCatch(
    optimize_one_tree(tid, stem_profiles, ProductData, LengthClasses,
                      pm_matrix, pm_flat, STUMP_HEIGHT_CM, PROFILE_STEP_CM),
    error = function(e) {
      list(tree_id = tid, species_group = NA_character_,
           dbh_mm = NA_real_, height_cm = NA_real_,
           n_generated = NA_integer_, n_priced = NA_integer_,
           n_selected = NA_integer_, total_value_nok = NA_real_,
           total_volume_m3 = NA_real_, total_length_cm = NA_integer_,
           n_overlapping = NA_integer_, n_unmatched_price = NA_integer_,
           n_dm31_unpriced = NA_integer_, dp_best_value = NA_real_,
           selected_logs = NULL, error = conditionMessage(e))
    }
  )

  # Progress report every 10%.
  if (i %% progress_step == 0L || i == n_trees_total) {
    elapsed <- (proc.time() - t_start)["elapsed"]
    wlog(sprintf("  Progress: %d / %d trees  (%.0f s elapsed)",
                 i, n_trees_total, elapsed))
  }
}

elapsed_total <- (proc.time() - t_start)["elapsed"]
wlog(sprintf("  Completed in %.1f s", elapsed_total))


# ---- assemble outputs ----

wlog("", "--- Assembling results ---")

tree_summ <- do.call(rbind, lapply(results_list, function(r) {
  data.frame(
    treid             = r$tree_id,
    species_group     = r$species_group %||% NA_character_,
    dbh_mm            = r$dbh_mm,
    height_cm         = r$height_cm,
    n_generated       = r$n_generated,
    n_priced          = r$n_priced,
    n_selected        = r$n_selected,
    total_value_nok   = r$total_value_nok,
    total_volume_m3   = r$total_volume_m3,
    total_length_cm   = r$total_length_cm,
    n_overlapping     = r$n_overlapping,
    n_unmatched_price = r$n_unmatched_price,
    n_dm31_unpriced   = r$n_dm31_unpriced,
    dp_best_value     = r$dp_best_value,
    error             = r$error %||% NA_character_,
    stringsAsFactors  = FALSE
  )
}))

# All selected logs stacked.
sel_list   <- lapply(results_list, function(r) r$selected_logs)
sel_list   <- sel_list[!vapply(sel_list, is.null, logical(1))]
sel_list   <- sel_list[vapply(sel_list, nrow, integer(1)) > 0]
all_selected <- if (length(sel_list) > 0) do.call(rbind, sel_list) else data.frame()

n_ok    <- sum(is.na(tree_summ$error))
n_err   <- sum(!is.na(tree_summ$error))

# Product-level summary.
prod_summ <- if (nrow(all_selected) > 0) {
  do.call(rbind, lapply(unique(all_selected$ProductKey), function(pk) {
    r <- all_selected[all_selected$ProductKey == pk, ]
    data.frame(
      ProductKey       = pk,
      n_logs           = nrow(r),
      total_volume_m3  = round(sum(r$price_volume_m3, na.rm = TRUE), 4),
      total_value_nok  = round(sum(r$value_nok,       na.rm = TRUE), 2),
      mean_value_nok   = round(mean(r$value_nok,      na.rm = TRUE), 2),
      mean_log_len_cm  = round(mean(r$LogLength,      na.rm = TRUE), 1),
      stringsAsFactors = FALSE
    )
  }))
} else {
  data.frame()
}

n_dm31_total   <- sum(tree_summ$n_dm31_unpriced,   na.rm = TRUE)
n_overlap_total <- sum(tree_summ$n_overlapping,    na.rm = TRUE)
n_zero_sel      <- sum(is.na(tree_summ$error) & tree_summ$n_selected == 0)

# Diagnostics.
diag_df <- data.frame(
  item  = c("stump_height_cm", "profile_step_cm", "price_scenario",
            "n_trees_input", "n_trees_completed", "n_trees_error",
            "n_trees_zero_selected",
            "n_selected_logs_total", "total_value_nok_all_trees",
            "total_volume_m3_all_trees", "n_overlapping_pairs_total",
            "n_unmatched_31dm_total", "elapsed_seconds"),
  value = c(as.character(STUMP_HEIGHT_CM), as.character(PROFILE_STEP_CM),
            PRICE_SCENARIO,
            n_trees_total, n_ok, n_err,
            n_zero_sel,
            nrow(all_selected),
            round(sum(tree_summ$total_value_nok, na.rm = TRUE), 2),
            round(sum(tree_summ$total_volume_m3,  na.rm = TRUE), 4),
            n_overlap_total,
            n_dm31_total,
            round(elapsed_total, 1)),
  stringsAsFactors = FALSE
)


# ---- save outputs ----

wlog("", "--- Saving outputs ---")

ts_csv   <- file.path(out_dir, "sample_tree_summary.csv")
sel_csv  <- file.path(out_dir, "sample_selected_logs.csv")
ps_csv   <- file.path(out_dir, "sample_product_summary.csv")
diag_csv <- file.path(out_dir, "sample_diagnostics.csv")

readr::write_csv(tree_summ,    ts_csv)
readr::write_csv(all_selected, sel_csv)
readr::write_csv(prod_summ,    ps_csv)
readr::write_csv(diag_df,      diag_csv)

wlog(sprintf("  Saved: %s  (%d rows)", basename(ts_csv),  nrow(tree_summ)))
wlog(sprintf("  Saved: %s  (%d rows)", basename(sel_csv),  nrow(all_selected)))
wlog(sprintf("  Saved: %s  (%d rows)", basename(ps_csv),  nrow(prod_summ)))
wlog(sprintf("  Saved: %s",             basename(diag_csv)))

if (n_err > 0) {
  err_csv <- file.path(out_dir, "sample_error_trees.csv")
  readr::write_csv(tree_summ[!is.na(tree_summ$error), ], err_csv)
  wlog(sprintf("  Saved: %s  (%d error trees)", basename(err_csv), n_err))
}


# ---- diagnostic plots ----

wlog("", "--- Diagnostic plots ---")

ok_tab <- tree_summ[is.na(tree_summ$error), ]

if (nrow(ok_tab) > 0) {
  ok_tab$dbh_cm <- ok_tab$dbh_mm / 10

  # 1. Total value vs DBH, coloured by species.
  p_val <- ok_tab |>
    ggplot2::ggplot(ggplot2::aes(
      x      = dbh_cm,
      y      = total_value_nok,
      colour = species_group
    )) +
    ggplot2::geom_point(alpha = 0.55, size = 1.4) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                          se = FALSE, linewidth = 0.8) +
    ggplot2::labs(
      title  = sprintf("Total DP value vs DBH – %d trees (STUMP_HEIGHT = %d cm)",
                       nrow(ok_tab), STUMP_HEIGHT_CM),
      x      = "DBH (cm)",
      y      = "Total value_nok (NOK)",
      colour = "Species group"
    ) +
    ggplot2::theme_minimal()

  save_plot(p_val, "plot_value_by_dbh.png")

  # 2. Number of selected logs vs DBH.
  p_nlogs <- ok_tab |>
    ggplot2::ggplot(ggplot2::aes(
      x      = dbh_cm,
      y      = n_selected,
      colour = species_group
    )) +
    ggplot2::geom_jitter(alpha = 0.5, size = 1.2, width = 0.3, height = 0.1) +
    ggplot2::labs(
      title  = "Number of selected logs vs DBH – example sample",
      x      = "DBH (cm)",
      y      = "Number of selected logs",
      colour = "Species group"
    ) +
    ggplot2::theme_minimal()

  save_plot(p_nlogs, "plot_n_logs_by_dbh.png")
}

# 3. Volume by ProductKey (bar chart).
if (nrow(prod_summ) > 0) {
  p_prod <- prod_summ |>
    ggplot2::ggplot(ggplot2::aes(
      x    = reorder(ProductKey, -total_volume_m3),
      y    = total_volume_m3,
      fill = ProductKey
    )) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f m3", total_volume_m3)),
                        vjust = -0.4, size = 3) +
    ggplot2::labs(
      title = "Total selected volume by ProductKey – example sample",
      x     = "ProductKey",
      y     = "Volume (m3)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

  save_plot(p_prod, "plot_volume_by_product.png", width = 18, height = 12)
}


# ---- final summary ----

wlog(
  "",
  "=============================================================",
  "  EXAMPLE SAMPLE BUCKING SUMMARY",
  "=============================================================",
  sprintf("  STUMP_HEIGHT_CM          : %d cm", STUMP_HEIGHT_CM),
  sprintf("  Trees in input           : %d", n_trees_total),
  sprintf("  Trees completed          : %d", n_ok),
  sprintf("  Trees with errors        : %d", n_err),
  sprintf("  Trees with 0 logs sel.   : %d  (too small / no priced candidates)",
          n_zero_sel),
  ""
)

if (nrow(prod_summ) > 0) {
  wlog("  Product summary:")
  wlog(capture.output(print(
    prod_summ[order(-prod_summ$total_volume_m3), ],
    row.names = FALSE)))
}

wlog(
  "",
  sprintf("  Total selected logs      : %d", nrow(all_selected)),
  sprintf("  Total value (NOK)        : %.2f",
          sum(tree_summ$total_value_nok, na.rm = TRUE)),
  sprintf("  Total volume (m3)        : %.4f",
          sum(tree_summ$total_volume_m3, na.rm = TRUE)),
  sprintf("  Total overlapping pairs  : %d", n_overlap_total)
)

if (n_dm31_total > 0) {
  wlog(
    "",
    sprintf("  REVIEW: %d total unpriced 31 dm sawlog candidate(s).", n_dm31_total),
    "  Known issue: sawlog price matrix starts at 34 dm."
  )
}

wlog(
  "",
  sprintf("  Elapsed time             : %.1f s", elapsed_total),
  sprintf("  Log: %s", log_file),
  ""
)

message(sprintf(
  "Script 08 complete. %d/%d trees, %d logs, total value = %.2f NOK, total volume = %.4f m3.",
  n_ok, n_trees_total, nrow(all_selected),
  sum(tree_summ$total_value_nok, na.rm = TRUE),
  sum(tree_summ$total_volume_m3, na.rm = TRUE)
))
