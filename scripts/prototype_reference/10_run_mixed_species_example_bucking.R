# ==============================================================================
# R/10_run_mixed_species_example_bucking.R
#
# Purpose : Mixed spruce/pine integration test.
#           Selects up to 250 spruce and 250 pine trees from the stratum-ready
#           NFI dataset via DBH quantile sampling, generates taperNOR stem
#           profiles inline, and runs the accepted vectorised DP bucking
#           optimiser on all selected trees.
#
#           This is NOT the full NFI stratum run.
#           No expansion factors.  No scaling to ha.
#           No bark correction.  No HPR / StanForD.
#           31 dm sawlog price gap is reported as REVIEW, not fixed.
#           STUMP_HEIGHT_CM = 10 cm.  START_POS_STEP_CM = 10 cm.
#
#           MAX_PRICE_DIAMETER_CLASS_CM = 50:
#             When a candidate log's top-diameter pricing class
#             (floor(TopDiameter_mm / 10)) exceeds the maximum class in the
#             price matrix (50 cm for both spruce_sawlog and pine_sawlog in
#             the default_2021 scenario), it is capped at
#             MAX_PRICE_DIAMETER_CLASS_CM for PRICING ONLY.
#             The physical TopDiameter and price_volume_m3 are NOT affected.
#             PriceDiameterClass_cm_uncapped stores the original class;
#             PriceDiameterClass_cm_used stores the capped value used for the
#             price-matrix join.
#
# Inputs  :
#   data_processed/nfi_tree_plot_2024_spruce_pine_stratum_ready.parquet
#   inputs/ProductData_spruce_pine_default.csv
#   inputs/LengthClasses_spruce_pine_default.csv
#   inputs/price_matrices/PriceMatrix_long_default_2021.csv
#
# Outputs :
#   outputs/mixed_species_example_bucking/mixed_species_selected_trees.csv
#   outputs/mixed_species_example_bucking/mixed_species_stem_profiles_summary.csv
#   outputs/mixed_species_example_bucking/mixed_species_selected_logs.csv
#   outputs/mixed_species_example_bucking/mixed_species_tree_summary.csv
#   outputs/mixed_species_example_bucking/mixed_species_candidate_diagnostics.csv
#   outputs/mixed_species_example_bucking/mixed_species_unpriced_candidates.csv
#   outputs/mixed_species_example_bucking/mixed_species_run_summary.txt
#   outputs/mixed_species_example_bucking/plot_mixed_species_value_by_dbh.png
#   outputs/mixed_species_example_bucking/plot_mixed_species_product_volume.png
#   outputs/mixed_species_example_bucking/plot_mixed_species_value_by_species.png
# ==============================================================================


# ---- setup ----

library(dplyr)
library(readr)
library(arrow)
library(ggplot2)
library(tibble)

if (!requireNamespace("taperNOR", quietly = TRUE)) {
  stop(
    "Package 'taperNOR' is not installed.\n",
    "Install with:\n",
    "  devtools::install_git(\"https://github.com/SmartForest-no/taperNOR\")"
  )
}
library(taperNOR)


# ---- parameters ----

STUMP_HEIGHT_CM   <- 10L     # cm above terrain; first possible log StartPos
START_POS_STEP_CM <- 10L     # step between candidate StartPos values
PROFILE_STEP_CM   <- 10L     # must match START_POS_STEP_CM
PROFILE_STEP_M    <- 0.10    # m; passed to taperNOR::taperNOR()

N_TREES_PER_SPECIES <- 250L  # target trees per species group

MIN_DBH_CM   <- 5.0          # minimum DBH (cm) for taperNOR validity
MIN_HEIGHT_M <- 4.5          # minimum height (m) for taperNOR validity

# Sawlog price matrices (default_2021) cover PriceDiameterClass_cm in 12-50 cm
# for both spruce_sawlog and pine_sawlog.  Logs whose top-diameter class
# exceeds 50 cm are priced at the 50 cm class.  This cap applies ONLY to
# PriceDiameterClass_cm_used (the key used for the price-matrix join).
# TopDiameter and price_volume_m3 are computed from the true physical diameter.
MAX_PRICE_DIAMETER_CLASS_CM <- 50L

root_dir   <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
inputs_dir <- file.path(root_dir, "inputs")
data_dir   <- file.path(root_dir, "data_processed")
out_dir    <- file.path(root_dir, "outputs", "mixed_species_example_bucking")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

log_file <- file.path(out_dir, "mixed_species_run_summary.txt")
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
  wlog(sprintf("  Plot saved: %s", filename))
}

wlog(
  "=============================================================",
  "  Mixed-species example bucking run \u2013 log",
  sprintf("  Run                          : %s", Sys.time()),
  sprintf("  STUMP_HEIGHT_CM              : %d cm", STUMP_HEIGHT_CM),
  sprintf("  START_POS_STEP_CM / PROFILE  : %d cm", PROFILE_STEP_CM),
  sprintf("  N_TREES_PER_SPECIES          : %d",    N_TREES_PER_SPECIES),
  sprintf("  MAX_PRICE_DIAMETER_CLASS_CM  : %d cm", MAX_PRICE_DIAMETER_CLASS_CM),
  "============================================================="
)


# ---- helpers ----

`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}

# Select N trees distributed across the DBH range using evenly-spaced
# quantile positions (same strategy as scripts 07 and 09).
select_quantile_trees <- function(trees_df, n, dbh_col = "dbh_cm") {
  dbh_vals <- trees_df[[dbh_col]]
  o        <- order(dbh_vals, na.last = TRUE)
  sorted   <- trees_df[o, ]
  sorted   <- sorted[!is.na(sorted[[dbh_col]]), ]
  n_avail  <- nrow(sorted)
  if (n_avail <= n) {
    return(sorted)
  }
  idx <- unique(round(seq(1, n_avail, length.out = n)))
  sorted[idx, ]
}

# Generate one taper profile using taperNOR (mirrors scripts 02 and 09).
generate_profile_one_tree <- function(treid, plot_id, sesong,
                                       dbh_cm, height_m, species_group,
                                       step_m = 0.10) {
  z_vec <- seq(step_m, height_m, by = step_m)
  d_cm  <- taperNOR::taperNOR(
    h         = z_vec,
    dbh       = dbh_cm,
    h_top     = height_m,
    sp        = species_group,
    with_bark = TRUE
  )
  data.frame(
    treid         = as.character(treid),
    plot_id       = as.character(plot_id),
    sesong        = sesong,
    species_group = species_group,
    dbh_cm        = dbh_cm,
    height_m      = height_m,
    z_m           = z_vec,
    diameter_cm   = d_cm,
    stringsAsFactors = FALSE
  )
}

# Build internal per-tree profile vectors from the converted profile data frame.
# diameterPosition is in cm (multiples of PROFILE_STEP_CM).
# DiameterValue is in mm.  DBH is in mm.
get_tree_profile_vectors <- function(profiles, tree_id) {
  tree_rows <- profiles[profiles$treid == tree_id, ]
  tree_rows <- tree_rows[order(tree_rows$diameterPosition), ]
  if (nrow(tree_rows) == 0L) {
    stop(sprintf("No profile rows for treid = '%s'.", tree_id))
  }
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

# Backward-induction DP over position state space.
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
      if (total_val > best_val) {
        best_val <- total_val
        best_log <- j
      }
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


# ---- per-tree optimizer (vectorised) ----
#
# Extends the script 08/09 approach with an explicit max_price_diam parameter.
#
# max_price_diam : MAX_PRICE_DIAMETER_CLASS_CM (50 cm).
#   Caps PriceDiameterClass_cm for the price-matrix join only.
#   TopDiameter and price_volume_m3 are NOT affected.
#   PriceDiameterClass_cm_uncapped = floor(TopDiameter / 10)  [original]
#   PriceDiameterClass_cm          = pmin(uncapped, max_price_diam) [join key]
#   After join, PriceDiameterClass_cm is renamed to PriceDiameterClass_cm_used.
#
# sawlog_pks / nonsawlog_pks are read from the enclosing script environment.

optimize_one_tree <- function(tree_id, profiles, pd, lc,
                               pm_mat, pm_fl, stump, step,
                               max_price_diam) {

  t0 <- proc.time()

  pv <- tryCatch(
    get_tree_profile_vectors(profiles, tree_id),
    error = function(e) NULL
  )

  if (is.null(pv)) {
    elapsed <- as.numeric((proc.time() - t0)["elapsed"])
    return(list(
      tree_id = tree_id, species_group = NA_character_,
      dbh_mm = NA_real_, height_cm = NA_real_,
      n_generated = 0L, n_priced = 0L, n_selected = 0L,
      total_value_nok = 0, total_volume_m3 = 0, total_length_cm = 0L,
      n_overlapping = 0L, n_unmatched_price = 0L, n_dm31_unpriced = 0L,
      n_price_diam_capped = 0L,
      dp_best_value = 0,
      n_unique_StartPos_generated = 0L, n_unique_StartPos_priced = 0L,
      selected_logs = NULL, unpriced_cands = NULL,
      optimizer_status = "error",
      optimizer_message = "Profile not found",
      runtime_seconds = elapsed, error = "Profile not found"
    ))
  }

  sg          <- pv$species_group
  sp_products <- pd[pd$species_group == sg, ]

  if (nrow(sp_products) == 0L) {
    elapsed <- as.numeric((proc.time() - t0)["elapsed"])
    msg     <- paste0("No products for species_group='", sg, "'")
    return(list(
      tree_id = tree_id, species_group = sg,
      dbh_mm = pv$DBH, height_cm = pv$height_cm,
      n_generated = 0L, n_priced = 0L, n_selected = 0L,
      total_value_nok = 0, total_volume_m3 = 0, total_length_cm = 0L,
      n_overlapping = 0L, n_unmatched_price = 0L, n_dm31_unpriced = 0L,
      n_price_diam_capped = 0L,
      dp_best_value = 0,
      n_unique_StartPos_generated = 0L, n_unique_StartPos_priced = 0L,
      selected_logs = NULL, unpriced_cands = NULL,
      optimizer_status = "no_products",
      optimizer_message = msg,
      runtime_seconds = elapsed, error = msg
    ))
  }

  # Vectorised diameter lookup built once per tree.
  diam_fun <- approxfun(pv$diameterPosition, pv$DiameterValue, rule = 2)

  # Precompute cumulative solid volume on PROFILE_STEP_CM grid.
  # All candidate positions are multiples of step, so index lookup is O(1).
  prof_pos <- seq(0L, as.integer(pv$height_cm), by = as.integer(step))
  prof_d   <- diam_fun(prof_pos)
  sec_r    <- (prof_d[-length(prof_d)] + prof_d[-1]) / 4
  sec_vol  <- pi * sec_r^2 * as.integer(step) / 1e8
  cumvol   <- c(0, cumsum(sec_vol))
  step_int <- as.integer(step)

  solid_vol_vec <- function(pos_a, pos_b) {
    ia <- pos_a / step_int + 1L
    ib <- pos_b / step_int + 1L
    cumvol[ib] - cumvol[ia]
  }

  start_positions <- seq(
    as.integer(stump),
    as.integer(pv$height_cm - 10L),
    by = step_int
  )

  if (length(start_positions) == 0L) {
    elapsed <- as.numeric((proc.time() - t0)["elapsed"])
    return(list(
      tree_id = tree_id, species_group = sg,
      dbh_mm = pv$DBH, height_cm = pv$height_cm,
      n_generated = 0L, n_priced = 0L, n_selected = 0L,
      total_value_nok = 0, total_volume_m3 = 0, total_length_cm = 0L,
      n_overlapping = 0L, n_unmatched_price = 0L, n_dm31_unpriced = 0L,
      n_price_diam_capped = 0L,
      dp_best_value = 0,
      n_unique_StartPos_generated = 0L, n_unique_StartPos_priced = 0L,
      selected_logs = NULL, unpriced_cands = NULL,
      optimizer_status = "too_short",
      optimizer_message = "Tree too short for any log",
      runtime_seconds = elapsed, error = NA_character_
    ))
  }

  n_unique_sp_gen <- length(start_positions)
  n_capped_total  <- 0L
  prod_frames     <- vector("list", nrow(sp_products))

  for (j in seq_len(nrow(sp_products))) {
    prow   <- sp_products[j, ]
    pk     <- prow$ProductKey
    lc_vec <- as.integer(lc[[pk]])

    combos <- expand.grid(start_pos = start_positions,
                          log_len   = lc_vec,
                          KEEP.OUT.ATTRS = FALSE,
                          stringsAsFactors = FALSE)
    combos$stop_pos <- combos$start_pos + combos$log_len
    combos <- combos[combos$stop_pos <= pv$height_cm, , drop = FALSE]
    if (nrow(combos) == 0L) next

    top_meas_pos        <- combos$stop_pos - as.integer(prow$DiameterTopPosition)
    combos$TopDiameter  <- diam_fun(top_meas_pos)
    combos$ButtDiameter <- diam_fun(combos$start_pos)

    top_d_vol <- if (prow$VolumeDiameterAdjustment ==
                     "Measured diameter rounded downwards to cm") {
      floor(combos$TopDiameter / 10) * 10
    } else {
      combos$TopDiameter
    }

    e_len  <- combos$log_len >= prow$MinLength &
               combos$log_len <= prow$MaxLength
    e_top  <- !is.na(combos$TopDiameter) &
               combos$TopDiameter >= prow$MinTopDiameter &
               combos$TopDiameter <= prow$MaxTopDiameter
    e_butt <- !is.na(combos$ButtDiameter) &
               combos$ButtDiameter >= prow$MinButtDiameter &
               combos$ButtDiameter <= prow$MaxButtDiameter
    eligible <- e_len & e_top & e_butt

    eff_length <- combos$log_len
    eff_stop   <- combos$stop_pos

    if (prow$VolumeDiameterCategory == "Top") {
      r_mm      <- top_d_vol / 2
      price_vol <- ifelse(
        eligible & !is.na(r_mm) & r_mm > 0,
        pi * r_mm^2 * eff_length / 1e8,
        NA_real_)
    } else {
      price_vol <- ifelse(
        eligible,
        solid_vol_vec(combos$start_pos, eff_stop),
        NA_real_)
    }

    # Diameter class: compute uncapped original, then cap for the price join.
    # Physical TopDiameter and price_volume_m3 are NOT affected by this cap.
    diam_uncapped <- as.integer(floor(combos$TopDiameter / 10))
    diam_used     <- as.integer(pmin(diam_uncapped, max_price_diam))

    # Count eligible candidates where top-diameter class exceeds the cap.
    n_capped_j     <- sum(eligible & !is.na(diam_uncapped) &
                           diam_uncapped > max_price_diam, na.rm = TRUE)
    n_capped_total <- n_capped_total + as.integer(n_capped_j)

    prod_frames[[j]] <- data.frame(
      treid                          = as.character(tree_id),
      species_group                  = sg,
      ProductKey                     = pk,
      ProductType                    = prow$ProductType,
      StartPos                       = as.integer(combos$start_pos),
      StopPos                        = as.integer(combos$stop_pos),
      LogLength                      = as.integer(combos$log_len),
      TopDiameter                    = round(combos$TopDiameter, 1),
      ButtDiameter                   = round(combos$ButtDiameter, 1),
      eligible                       = eligible,
      price_volume_m3                = round(price_vol, 5),
      LengthClass_dm                 = as.integer(eff_length / 10L),
      PriceDiameterClass_cm_uncapped = diam_uncapped,
      PriceDiameterClass_cm          = diam_used,  # capped; used as join key
      stringsAsFactors = FALSE
    )
  }

  generated <- do.call(rbind,
    prod_frames[!vapply(prod_frames, is.null, logical(1))])
  n_gen <- if (is.null(generated) || nrow(generated) == 0L) 0L else nrow(generated)

  if (n_gen == 0L) {
    elapsed <- as.numeric((proc.time() - t0)["elapsed"])
    return(list(
      tree_id = tree_id, species_group = sg,
      dbh_mm = pv$DBH, height_cm = pv$height_cm,
      n_generated = 0L, n_priced = 0L, n_selected = 0L,
      total_value_nok = 0, total_volume_m3 = 0, total_length_cm = 0L,
      n_overlapping = 0L, n_unmatched_price = 0L, n_dm31_unpriced = 0L,
      n_price_diam_capped = n_capped_total,
      dp_best_value = 0,
      n_unique_StartPos_generated = n_unique_sp_gen,
      n_unique_StartPos_priced = 0L,
      selected_logs = NULL, unpriced_cands = NULL,
      optimizer_status = "no_candidates",
      optimizer_message = "No candidates fit within tree height",
      runtime_seconds = elapsed, error = NA_character_
    ))
  }

  # ---- price join ----
  elig_c      <- generated[generated$eligible &
                              !is.na(generated$price_volume_m3), , drop = FALSE]
  sawlog_c    <- elig_c[elig_c$ProductKey %in% sawlog_pks, , drop = FALSE]
  nonsawlog_c <- elig_c[elig_c$ProductKey %in% nonsawlog_pks, , drop = FALSE]

  # Sawlogs: join by ProductKey + LengthClass_dm + PriceDiameterClass_cm
  # PriceDiameterClass_cm is the capped (used) value; see prod_frames above.
  if (nrow(sawlog_c) > 0L) {
    sawlog_c <- merge(sawlog_c, pm_mat,
                      by = c("ProductKey", "LengthClass_dm",
                             "PriceDiameterClass_cm"),
                      all.x = TRUE)
  } else {
    sawlog_c$PriceNOKperM3 <- numeric(0)
  }

  # Non-sawlogs: flat price join by ProductKey only.
  if (nrow(nonsawlog_c) > 0L) {
    nonsawlog_c <- merge(nonsawlog_c, pm_fl, by = "ProductKey", all.x = TRUE)
    names(nonsawlog_c)[names(nonsawlog_c) == "PriceNOKperM3_flat"] <-
      "PriceNOKperM3"
  } else {
    nonsawlog_c$PriceNOKperM3 <- numeric(0)
  }

  priced <- rbind(sawlog_c, nonsawlog_c)

  # Rename PriceDiameterClass_cm (= capped join key) to PriceDiameterClass_cm_used
  # so both the original and the used value are clearly named in all downstream
  # data frames (dp_in, sel, unpriced_cands).
  names(priced)[names(priced) == "PriceDiameterClass_cm"] <-
    "PriceDiameterClass_cm_used"

  priced$value_nok <- priced$price_volume_m3 * priced$PriceNOKperM3

  n_unmatched <- sum(is.na(priced$PriceNOKperM3))
  n_dm31      <- sum(!is.na(priced$LengthClass_dm) &
                       priced$LengthClass_dm == 31L &
                       is.na(priced$PriceNOKperM3))

  unpriced_cands <- priced[is.na(priced$PriceNOKperM3), , drop = FALSE]

  dp_in <- priced[!is.na(priced$PriceNOKperM3) &
                    !is.na(priced$value_nok) &
                    !is.na(priced$price_volume_m3), , drop = FALSE]
  dp_in$StartPos <- as.integer(round(dp_in$StartPos))
  dp_in$StopPos  <- as.integer(round(dp_in$StopPos))
  n_priced       <- nrow(dp_in)
  n_unique_sp_priced <- if (n_priced > 0L) length(unique(dp_in$StartPos)) else 0L

  if (n_priced == 0L) {
    elapsed <- as.numeric((proc.time() - t0)["elapsed"])
    return(list(
      tree_id = tree_id, species_group = sg,
      dbh_mm = pv$DBH, height_cm = pv$height_cm,
      n_generated = n_gen, n_priced = 0L, n_selected = 0L,
      total_value_nok = 0, total_volume_m3 = 0, total_length_cm = 0L,
      n_overlapping = 0L, n_unmatched_price = n_unmatched,
      n_dm31_unpriced = n_dm31, n_price_diam_capped = n_capped_total,
      dp_best_value = 0,
      n_unique_StartPos_generated = n_unique_sp_gen,
      n_unique_StartPos_priced = 0L,
      selected_logs = NULL, unpriced_cands = unpriced_cands,
      optimizer_status = "no_priced_candidates",
      optimizer_message = sprintf(
        "%d generated, %d eligible with vol, 0 priced (all unpriced)",
        n_gen, nrow(elig_c)),
      runtime_seconds = elapsed, error = NA_character_
    ))
  }

  # ---- DP optimizer ----
  dp  <- run_dp_optimizer(dp_in)
  sel <- if (length(dp$selected_rows) > 0L) {
    s <- dp_in[dp$selected_rows, ]
    s[order(s$StartPos), ]
  } else {
    dp_in[integer(0), ]
  }

  n_overlap <- 0L
  if (nrow(sel) > 1L) {
    for (k in seq_len(nrow(sel) - 1L)) {
      if (sel$StopPos[k] > sel$StartPos[k + 1L]) {
        n_overlap <- n_overlap + 1L
      }
    }
  }

  status <- if (nrow(sel) == 0L) "zero_logs" else "ok"
  msg    <- if (nrow(sel) == 0L) {
    "DP ran but selected 0 logs"
  } else {
    sprintf("%d logs, value = %.2f NOK",
            nrow(sel), sum(sel$value_nok, na.rm = TRUE))
  }

  elapsed <- as.numeric((proc.time() - t0)["elapsed"])

  list(
    tree_id                     = tree_id,
    species_group               = sg,
    dbh_mm                      = pv$DBH,
    height_cm                   = pv$height_cm,
    n_generated                 = n_gen,
    n_priced                    = n_priced,
    n_selected                  = nrow(sel),
    total_value_nok             = round(sum(sel$value_nok,       na.rm = TRUE), 2),
    total_volume_m3             = round(sum(sel$price_volume_m3,  na.rm = TRUE), 5),
    total_length_cm             = sum(sel$LogLength,              na.rm = TRUE),
    n_overlapping               = n_overlap,
    n_unmatched_price           = n_unmatched,
    n_dm31_unpriced             = n_dm31,
    n_price_diam_capped         = n_capped_total,
    dp_best_value               = round(dp$best_value, 2),
    n_unique_StartPos_generated = n_unique_sp_gen,
    n_unique_StartPos_priced    = n_unique_sp_priced,
    selected_logs               = sel,
    unpriced_cands              = unpriced_cands,
    optimizer_status            = status,
    optimizer_message           = msg,
    runtime_seconds             = elapsed,
    error                       = NA_character_
  )
}


# ---- read stratum-ready data and select trees ----

wlog("", "--- Selecting trees ---")

nfi_parquet <- file.path(data_dir,
  "nfi_tree_plot_2024_spruce_pine_stratum_ready.parquet")
if (!file.exists(nfi_parquet)) {
  stop("Stratum-ready parquet not found:\n  ", nfi_parquet,
       "\nRun 00_prepare_nfi_2024_data.R first.")
}

nfi_all <- arrow::read_parquet(nfi_parquet)

# Filter to taper-valid trees and compute height_m.
nfi_all$height_m <- as.numeric(nfi_all$beregnet_hoyde) / 10
nfi_valid <- nfi_all[!is.na(nfi_all$dbh_cm) &
                       nfi_all$dbh_cm >= MIN_DBH_CM &
                       !is.na(nfi_all$height_m) &
                       nfi_all$height_m >= MIN_HEIGHT_M, ]

nfi_spruce <- nfi_valid[nfi_valid$species_group == "spruce", ]
nfi_pine   <- nfi_valid[nfi_valid$species_group == "pine",   ]

n_spruce_avail <- nrow(nfi_spruce)
n_pine_avail   <- nrow(nfi_pine)

wlog(sprintf("  Spruce trees available (after filter) : %d", n_spruce_avail))
wlog(sprintf("  Pine trees available (after filter)   : %d", n_pine_avail))

sel_spruce <- select_quantile_trees(nfi_spruce, N_TREES_PER_SPECIES, "dbh_cm")
sel_pine   <- select_quantile_trees(nfi_pine,   N_TREES_PER_SPECIES, "dbh_cm")

n_sel_spruce <- nrow(sel_spruce)
n_sel_pine   <- nrow(sel_pine)

wlog(sprintf("  Spruce trees selected : %d  (DBH %.1f\u2013%.1f cm, ht %.1f\u2013%.1f m)",
             n_sel_spruce,
             min(sel_spruce$dbh_cm), max(sel_spruce$dbh_cm),
             min(sel_spruce$height_m), max(sel_spruce$height_m)))
wlog(sprintf("  Pine trees selected   : %d  (DBH %.1f\u2013%.1f cm, ht %.1f\u2013%.1f m)",
             n_sel_pine,
             min(sel_pine$dbh_cm), max(sel_pine$dbh_cm),
             min(sel_pine$height_m), max(sel_pine$height_m)))

# Combine selected trees.
keep_cols <- c("treid", "plot_id", "sesong", "species_group",
               "dbh_cm", "height_m")
get_cols <- function(df) {
  df[, intersect(keep_cols, names(df))]
}
selected_trees <- rbind(get_cols(sel_spruce), get_cols(sel_pine))
n_sel_total <- nrow(selected_trees)

wlog(sprintf("  Total trees selected  : %d", n_sel_total))

trees_out <- data.frame(
  treid         = as.character(selected_trees$treid),
  plot_id       = as.character(selected_trees$plot_id),
  species_group = selected_trees$species_group,
  dbh_cm        = selected_trees$dbh_cm,
  height_m      = selected_trees$height_m,
  stringsAsFactors = FALSE
)
readr::write_csv(trees_out,
  file.path(out_dir, "mixed_species_selected_trees.csv"))
wlog(sprintf("  Saved: mixed_species_selected_trees.csv (%d rows)", n_sel_total))


# ---- generate taper profiles ----

wlog("", "--- Generating taper profiles ---")

profile_list    <- vector("list", n_sel_total)
failed_profiles <- character(0)

for (i in seq_len(n_sel_total)) {
  row    <- selected_trees[i, ]
  sesong <- if ("sesong" %in% names(row)) row$sesong else NA_character_
  result <- tryCatch(
    generate_profile_one_tree(
      treid         = row$treid,
      plot_id       = row$plot_id,
      sesong        = sesong,
      dbh_cm        = row$dbh_cm,
      height_m      = row$height_m,
      species_group = row$species_group,
      step_m        = PROFILE_STEP_M
    ),
    error = function(e) {
      failed_profiles <<- c(failed_profiles, as.character(row$treid))
      NULL
    }
  )
  profile_list[[i]] <- result
}

n_failed_profile  <- length(failed_profiles)
good_profiles     <- profile_list[!vapply(profile_list, is.null, logical(1))]
profiles_raw      <- do.call(rbind, good_profiles)

wlog(sprintf("  Profiles generated  : %d", length(good_profiles)))
wlog(sprintf("  Profile failures    : %d", n_failed_profile))
if (n_failed_profile > 0L) {
  wlog(sprintf("  Failed tree ids     : %s",
               paste(failed_profiles, collapse = ", ")))
}

# Convert to optimizer format: diameterPosition (cm), DiameterValue (mm), DBH (mm).
profiles_conv <- profiles_raw
profiles_conv$diameterPosition <- as.integer(
  round(profiles_conv$z_m * 100 / PROFILE_STEP_CM) * PROFILE_STEP_CM)
profiles_conv$DiameterValue    <- profiles_conv$diameter_cm * 10
profiles_conv$DBH              <- profiles_conv$dbh_cm * 10
profiles_conv <- profiles_conv[
  !duplicated(profiles_conv[, c("treid", "diameterPosition")]), ]

# Per-tree profile summary (written to disk instead of full profile CSV).
prof_summ_list <- lapply(unique(profiles_conv$treid), function(tid) {
  rows <- profiles_conv[profiles_conv$treid == tid, ]
  data.frame(
    treid         = tid,
    species_group = rows$species_group[1],
    dbh_cm        = rows$dbh_cm[1],
    height_m      = rows$height_m[1],
    n_profile_rows = nrow(rows),
    min_diam_cm   = round(min(rows$diameter_cm, na.rm = TRUE), 2),
    max_diam_cm   = round(max(rows$diameter_cm, na.rm = TRUE), 2),
    stringsAsFactors = FALSE
  )
})
profiles_summary <- do.call(rbind, prof_summ_list)

readr::write_csv(profiles_summary,
  file.path(out_dir, "mixed_species_stem_profiles_summary.csv"))
wlog(sprintf("  Saved: mixed_species_stem_profiles_summary.csv (%d rows)",
             nrow(profiles_summary)))

# Map treid -> n_profile_rows for tree summary.
prof_n_rows <- setNames(profiles_summary$n_profile_rows,
                         as.character(profiles_summary$treid))


# ---- load products and prices ----

wlog("", "--- Loading products and prices ---")

pd_csv <- file.path(inputs_dir, "ProductData_spruce_pine_default.csv")
if (!file.exists(pd_csv)) stop("ProductData not found: ", pd_csv)
ProductData <- readr::read_csv(pd_csv, show_col_types = FALSE)
wlog(sprintf("  Products : %d (%s)",
             nrow(ProductData),
             paste(ProductData$ProductKey, collapse = ", ")))

lc_csv <- file.path(inputs_dir, "LengthClasses_spruce_pine_default.csv")
if (!file.exists(lc_csv)) stop("LengthClasses not found: ", lc_csv)
lc_long <- readr::read_csv(lc_csv, show_col_types = FALSE)
LengthClasses <- split(as.integer(lc_long$LogLength), lc_long$ProductKey)
wlog(sprintf("  LengthClasses : %d products (%s)",
             length(LengthClasses),
             paste(names(LengthClasses), collapse = ", ")))

pm_csv <- file.path(inputs_dir, "price_matrices", "PriceMatrix_long_default_2021.csv")
if (!file.exists(pm_csv)) stop("PriceMatrix_long not found: ", pm_csv)
PriceMatrix_long <- readr::read_csv(pm_csv, show_col_types = FALSE)
PRICE_SCENARIO   <- unique(PriceMatrix_long$PriceScenario)[1]
wlog(sprintf("  PriceMatrix : %d rows, scenario '%s'",
             nrow(PriceMatrix_long), PRICE_SCENARIO))

# Split into sawlog matrix lookup and flat lookup.
sawlog_pks    <- unique(ProductData$ProductKey[ProductData$ProductType == "sawlog"])
nonsawlog_pks <- unique(ProductData$ProductKey[ProductData$ProductType != "sawlog"])

pm_matrix <- PriceMatrix_long |>
  dplyr::filter(PriceScenario == PRICE_SCENARIO, PriceType == "matrix") |>
  dplyr::select(ProductKey, LengthClass_dm, PriceDiameterClass_cm, PriceNOKperM3) |>
  dplyr::mutate(LengthClass_dm        = as.integer(LengthClass_dm),
                PriceDiameterClass_cm = as.integer(PriceDiameterClass_cm))

pm_flat <- PriceMatrix_long |>
  dplyr::filter(PriceScenario == PRICE_SCENARIO, PriceType == "flat") |>
  dplyr::select(ProductKey, PriceNOKperM3_flat = PriceNOKperM3)

# Verify MAX_PRICE_DIAMETER_CLASS_CM matches the actual matrix maximum.
actual_max_diam <- max(pm_matrix$PriceDiameterClass_cm, na.rm = TRUE)
if (actual_max_diam != MAX_PRICE_DIAMETER_CLASS_CM) {
  warning(sprintf(
    "MAX_PRICE_DIAMETER_CLASS_CM (%d) does not match actual matrix max (%d). ",
    MAX_PRICE_DIAMETER_CLASS_CM, actual_max_diam),
    "Check price matrix and update parameter if needed.")
}
wlog(sprintf("  Price matrix max PriceDiameterClass_cm : %d cm (parameter: %d cm)",
             actual_max_diam, MAX_PRICE_DIAMETER_CLASS_CM))


# ---- run optimizer on all selected trees ----

all_tree_ids <- as.character(unique(profiles_conv$treid))
n_opt        <- length(all_tree_ids)

wlog(sprintf("\n--- Running optimizer on %d trees ---", n_opt))
t_start <- proc.time()

results_list  <- vector("list", n_opt)
progress_step <- max(1L, as.integer(n_opt / 10L))

for (i in seq_along(all_tree_ids)) {
  tid <- all_tree_ids[i]
  results_list[[i]] <- tryCatch(
    optimize_one_tree(tid, profiles_conv, ProductData, LengthClasses,
                      pm_matrix, pm_flat,
                      STUMP_HEIGHT_CM, PROFILE_STEP_CM,
                      MAX_PRICE_DIAMETER_CLASS_CM),
    error = function(e) {
      list(
        tree_id = tid, species_group = NA_character_,
        dbh_mm = NA_real_, height_cm = NA_real_,
        n_generated = NA_integer_, n_priced = NA_integer_,
        n_selected = NA_integer_, total_value_nok = NA_real_,
        total_volume_m3 = NA_real_, total_length_cm = NA_integer_,
        n_overlapping = NA_integer_, n_unmatched_price = NA_integer_,
        n_dm31_unpriced = NA_integer_,
        n_price_diam_capped = NA_integer_,
        dp_best_value = NA_real_,
        n_unique_StartPos_generated = NA_integer_,
        n_unique_StartPos_priced    = NA_integer_,
        selected_logs = NULL, unpriced_cands = NULL,
        optimizer_status  = "FAIL",
        optimizer_message = conditionMessage(e),
        runtime_seconds = 0,
        error = conditionMessage(e)
      )
    }
  )

  if (i %% progress_step == 0L || i == n_opt) {
    elapsed <- (proc.time() - t_start)["elapsed"]
    wlog(sprintf("  Progress: %d / %d trees  (%.0f s elapsed)", i, n_opt, elapsed))
  }
}

elapsed_total <- as.numeric((proc.time() - t_start)["elapsed"])
wlog(sprintf("  Completed in %.1f s", elapsed_total))


# ---- assemble results ----

wlog("", "--- Assembling results ---")

tree_summ <- do.call(rbind, lapply(results_list, function(r) {
  dbh_mm_val <- r$dbh_mm %||% NA_real_
  ht_cm_val  <- r$height_cm %||% NA_real_
  data.frame(
    treid                             = r$tree_id,
    plot_id                           = NA_character_,
    species_group                     = r$species_group %||% NA_character_,
    dbh_cm                            = if (!is.na(dbh_mm_val)) dbh_mm_val / 10 else NA_real_,
    height_m                          = if (!is.na(ht_cm_val)) ht_cm_val / 100 else NA_real_,
    n_profile_rows                    = NA_integer_,
    n_generated_candidates            = r$n_generated             %||% NA_integer_,
    n_priced_candidates               = r$n_priced                %||% NA_integer_,
    n_unpriced_candidates             = r$n_unmatched_price       %||% NA_integer_,
    n_unpriced_31dm_sawlog            = r$n_dm31_unpriced         %||% NA_integer_,
    n_price_diameter_capped_candidates = r$n_price_diam_capped    %||% NA_integer_,
    n_unique_StartPos_generated       = r$n_unique_StartPos_generated %||% NA_integer_,
    n_unique_StartPos_priced          = r$n_unique_StartPos_priced    %||% NA_integer_,
    n_selected_logs                   = r$n_selected              %||% NA_integer_,
    total_selected_volume_m3          = r$total_volume_m3         %||% NA_real_,
    total_selected_value_nok          = r$total_value_nok         %||% NA_real_,
    selected_sawlog_volume_m3         = 0,
    selected_pulpwood_volume_m3       = 0,
    selected_waste_volume_m3          = 0,
    selected_sawlog_value_nok         = 0,
    selected_pulpwood_value_nok       = 0,
    selected_waste_value_nok          = 0,
    stem_coverage_pct                 = NA_real_,
    runtime_seconds                   = r$runtime_seconds         %||% NA_real_,
    optimizer_status                  = r$optimizer_status        %||% NA_character_,
    optimizer_message                 = r$optimizer_message       %||% NA_character_,
    error                             = r$error                   %||% NA_character_,
    stringsAsFactors = FALSE
  )
}))

# Fill plot_id and profile row counts from profiles.
plot_id_map <- unique(profiles_conv[, c("treid", "plot_id")])
for (k in seq_len(nrow(tree_summ))) {
  tid <- as.character(tree_summ$treid[k])
  pid <- plot_id_map$plot_id[plot_id_map$treid == tid]
  if (length(pid) > 0L) tree_summ$plot_id[k] <- as.character(pid[1])
  n <- prof_n_rows[tid]
  if (!is.null(n) && !is.na(n)) tree_summ$n_profile_rows[k] <- as.integer(n)
}

# Stack selected logs.
sel_list <- lapply(results_list, function(r) r$selected_logs)
sel_list <- sel_list[!vapply(sel_list, is.null, logical(1))]
sel_list <- sel_list[vapply(sel_list, nrow, integer(1)) > 0L]

empty_sel <- data.frame(
  treid = character(0), species_group = character(0),
  ProductKey = character(0), ProductType = character(0),
  StartPos = integer(0), StopPos = integer(0),
  LogLength = integer(0), TopDiameter = numeric(0),
  price_volume_m3 = numeric(0),
  PriceNOKperM3 = numeric(0), value_nok = numeric(0),
  LengthClass_dm = integer(0),
  PriceDiameterClass_cm_uncapped = integer(0),
  PriceDiameterClass_cm_used = integer(0),
  stringsAsFactors = FALSE)

all_selected <- if (length(sel_list) > 0L) do.call(rbind, sel_list) else empty_sel

# Fill per-ProductType columns in tree summary.
if (nrow(all_selected) > 0L) {
  for (ptype in c("sawlog", "pulpwood", "waste")) {
    vol_col <- paste0("selected_", ptype, "_volume_m3")
    val_col <- paste0("selected_", ptype, "_value_nok")
    pk_sel  <- all_selected[all_selected$ProductType == ptype, ]
    if (nrow(pk_sel) > 0L) {
      vol_t <- tapply(pk_sel$price_volume_m3, pk_sel$treid, sum, na.rm = TRUE)
      val_t <- tapply(pk_sel$value_nok,       pk_sel$treid, sum, na.rm = TRUE)
      idx   <- as.character(tree_summ$treid)
      tree_summ[[vol_col]] <- as.numeric(vol_t[idx])
      tree_summ[[val_col]] <- as.numeric(val_t[idx])
    }
    tree_summ[[vol_col]][is.na(tree_summ[[vol_col]])] <- 0
    tree_summ[[val_col]][is.na(tree_summ[[val_col]])] <- 0
  }
}

# Stem coverage.
denom <- tree_summ$height_m * 100 - STUMP_HEIGHT_CM
tree_summ$stem_coverage_pct <- ifelse(
  !is.na(denom) & denom > 0 & !is.na(tree_summ$total_selected_value_nok),
  round(tree_summ$n_selected_logs * 0 +  # ensure numeric
    (tree_summ$selected_sawlog_volume_m3 * 0) +   # placeholder; use length:
    0, 1),
  NA_real_)
# Recompute from log lengths in tree_summ.
total_len_by_tree <- tapply(
  all_selected$LogLength, all_selected$treid, sum, na.rm = TRUE)
for (k in seq_len(nrow(tree_summ))) {
  tid <- as.character(tree_summ$treid[k])
  tl  <- total_len_by_tree[tid]
  ht  <- tree_summ$height_m[k]
  if (!is.null(tl) && !is.na(tl) && !is.na(ht) && ht > 0) {
    usable <- ht * 100 - STUMP_HEIGHT_CM
    if (usable > 0) {
      tree_summ$stem_coverage_pct[k] <- round(as.numeric(tl) / usable * 100, 1)
    }
  }
}

# Summary counts.
n_ok             <- sum(is.na(tree_summ$error))
n_err            <- sum(!is.na(tree_summ$error))
n_zero_sel       <- sum(is.na(tree_summ$error) &
                          !is.na(tree_summ$n_selected_logs) &
                          tree_summ$n_selected_logs == 0L)
n_dm31_total     <- sum(tree_summ$n_unpriced_31dm_sawlog, na.rm = TRUE)
n_capped_total   <- sum(tree_summ$n_price_diameter_capped_candidates, na.rm = TRUE)
n_overlap_total  <- sum(tree_summ$n_overlapping_pairs %||% 0, na.rm = TRUE)

# Stack unpriced candidates.
unpr_list <- lapply(results_list, function(r) r$unpriced_cands)
unpr_list <- unpr_list[!vapply(unpr_list, is.null, logical(1))]
unpr_list <- unpr_list[vapply(unpr_list, nrow, integer(1)) > 0L]
unpriced_all <- if (length(unpr_list) > 0L) {
  do.call(rbind, unpr_list)
} else {
  data.frame(stringsAsFactors = FALSE)
}

# Product summary by species_group and ProductKey.
prod_summ <- if (nrow(all_selected) > 0L) {
  do.call(rbind, lapply(
    unique(paste(all_selected$species_group, all_selected$ProductKey)),
    function(sg_pk) {
      parts <- strsplit(sg_pk, " ")[[1]]
      sg    <- parts[1]
      pk    <- parts[2]
      r     <- all_selected[all_selected$species_group == sg &
                               all_selected$ProductKey == pk, ]
      data.frame(
        species_group   = sg,
        ProductKey      = pk,
        n_logs          = nrow(r),
        total_volume_m3 = round(sum(r$price_volume_m3, na.rm = TRUE), 4),
        total_value_nok = round(sum(r$value_nok,        na.rm = TRUE), 2),
        mean_log_len_cm = round(mean(r$LogLength,        na.rm = TRUE), 1),
        stringsAsFactors = FALSE
      )
    }))
} else {
  data.frame(species_group = character(0), ProductKey = character(0),
             n_logs = integer(0), total_volume_m3 = numeric(0),
             total_value_nok = numeric(0), mean_log_len_cm = numeric(0),
             stringsAsFactors = FALSE)
}

# Missing-price summary.
if (nrow(unpriced_all) > 0L && "ProductKey" %in% names(unpriced_all) &&
    "LengthClass_dm" %in% names(unpriced_all)) {
  miss_summ <- as.data.frame(
    table(ProductKey    = unpriced_all$ProductKey,
          LengthClass_dm = unpriced_all$LengthClass_dm),
    stringsAsFactors = FALSE)
  miss_summ <- miss_summ[miss_summ$Freq > 0L, ]
} else {
  miss_summ <- data.frame(ProductKey = character(0),
                          LengthClass_dm = character(0),
                          Freq = integer(0),
                          stringsAsFactors = FALSE)
}

# Price-diameter capping summary by species_group.
cap_by_species <- tapply(
  tree_summ$n_price_diameter_capped_candidates,
  tree_summ$species_group, sum, na.rm = TRUE)

# Run-level diagnostics (key-value pairs).
diag_df <- data.frame(
  item  = c("stump_height_cm", "start_pos_step_cm", "profile_step_cm",
            "price_scenario", "max_price_diameter_class_cm",
            "n_spruce_available", "n_spruce_selected",
            "n_pine_available",   "n_pine_selected",
            "n_profile_failures",
            "n_trees_optimizer_ok", "n_trees_optimizer_fail",
            "n_trees_zero_selected_logs",
            "n_selected_logs_total",
            "total_value_nok_all_trees",
            "total_volume_m3_all_trees",
            "n_overlapping_pairs_total",
            "n_unpriced_31dm_sawlog_total",
            "n_price_diameter_capped_total",
            "elapsed_seconds",
            "mean_runtime_seconds_per_tree"),
  value = c(as.character(STUMP_HEIGHT_CM),
            as.character(START_POS_STEP_CM),
            as.character(PROFILE_STEP_CM),
            PRICE_SCENARIO,
            as.character(MAX_PRICE_DIAMETER_CLASS_CM),
            as.character(n_spruce_avail),
            as.character(n_sel_spruce),
            as.character(n_pine_avail),
            as.character(n_sel_pine),
            as.character(n_failed_profile),
            as.character(n_ok),
            as.character(n_err),
            as.character(n_zero_sel),
            as.character(nrow(all_selected)),
            as.character(round(sum(tree_summ$total_selected_value_nok, na.rm = TRUE), 2)),
            as.character(round(sum(tree_summ$total_selected_volume_m3,  na.rm = TRUE), 4)),
            as.character(n_overlap_total),
            as.character(n_dm31_total),
            as.character(n_capped_total),
            as.character(round(elapsed_total, 1)),
            as.character(round(elapsed_total / max(n_opt, 1L), 3))),
  stringsAsFactors = FALSE
)


# ---- save outputs ----

wlog("", "--- Saving outputs ---")

# tree summary (drop internal total_length_cm column used only for coverage calc).
readr::write_csv(tree_summ,
  file.path(out_dir, "mixed_species_tree_summary.csv"))
wlog(sprintf("  Saved: mixed_species_tree_summary.csv (%d rows)", nrow(tree_summ)))

# selected logs: required columns only.
sel_cols <- c("treid", "species_group", "ProductKey",
              "StartPos", "StopPos", "LogLength", "TopDiameter",
              "price_volume_m3", "PriceNOKperM3", "value_nok",
              "LengthClass_dm",
              "PriceDiameterClass_cm_uncapped", "PriceDiameterClass_cm_used")
sel_out <- if (nrow(all_selected) > 0L) {
  all_selected[, intersect(sel_cols, names(all_selected))]
} else {
  data.frame(matrix(ncol = length(sel_cols), nrow = 0,
                    dimnames = list(NULL, sel_cols)))
}
readr::write_csv(sel_out,
  file.path(out_dir, "mixed_species_selected_logs.csv"))
wlog(sprintf("  Saved: mixed_species_selected_logs.csv (%d rows)", nrow(sel_out)))

readr::write_csv(diag_df,
  file.path(out_dir, "mixed_species_candidate_diagnostics.csv"))
wlog(sprintf("  Saved: mixed_species_candidate_diagnostics.csv"))

readr::write_csv(unpriced_all,
  file.path(out_dir, "mixed_species_unpriced_candidates.csv"))
wlog(sprintf("  Saved: mixed_species_unpriced_candidates.csv (%d rows)",
             nrow(unpriced_all)))


# ---- diagnostic plots ----

wlog("", "--- Diagnostic plots ---")

if (nrow(tree_summ) > 0L &&
    any(!is.na(tree_summ$total_selected_value_nok))) {
  p_val <- ggplot2::ggplot(
    tree_summ[!is.na(tree_summ$dbh_cm), ],
    ggplot2::aes(x = dbh_cm,
                 y = ifelse(is.na(total_selected_value_nok),
                            0, total_selected_value_nok),
                 colour = species_group,
                 shape  = optimizer_status)) +
    ggplot2::geom_point(size = 2, alpha = 0.7) +
    ggplot2::labs(title = "Mixed-species example: selected value by DBH",
                  x = "DBH (cm)", y = "Total selected value (NOK)",
                  colour = "Species", shape = "Status") +
    ggplot2::theme_bw()
  save_plot(p_val, "plot_mixed_species_value_by_dbh.png")
}

if (nrow(prod_summ) > 0L) {
  p_vol <- ggplot2::ggplot(
    prod_summ,
    ggplot2::aes(x = ProductKey, y = total_volume_m3,
                 fill = species_group)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(title = "Mixed-species example: selected volume by product",
                  x = "Product", y = "Total volume (m\u00b3)",
                  fill = "Species") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  save_plot(p_vol, "plot_mixed_species_product_volume.png")
}

if (nrow(tree_summ) > 0L &&
    any(!is.na(tree_summ$total_selected_value_nok))) {
  sp_val <- tree_summ[!is.na(tree_summ$species_group) &
                        !is.na(tree_summ$total_selected_value_nok), ]
  if (nrow(sp_val) > 0L) {
    p_sp <- ggplot2::ggplot(
      sp_val,
      ggplot2::aes(x = species_group, y = total_selected_value_nok,
                   fill = species_group)) +
      ggplot2::geom_boxplot(alpha = 0.7) +
      ggplot2::labs(title = "Mixed-species example: per-tree value by species",
                    x = "Species", y = "Selected value per tree (NOK)") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none")
    save_plot(p_sp, "plot_mixed_species_value_by_species.png")
  }
}


# ---- summary ----

wlog(
  "",
  "=============================================================",
  "  MIXED-SPECIES EXAMPLE BUCKING SUMMARY",
  "=============================================================",
  sprintf("  STUMP_HEIGHT_CM              : %d cm",  STUMP_HEIGHT_CM),
  sprintf("  MAX_PRICE_DIAMETER_CLASS_CM  : %d cm",  MAX_PRICE_DIAMETER_CLASS_CM),
  sprintf("  Spruce trees available       : %d",     n_spruce_avail),
  sprintf("  Spruce trees selected        : %d",     n_sel_spruce),
  sprintf("  Pine trees available         : %d",     n_pine_avail),
  sprintf("  Pine trees selected          : %d",     n_sel_pine),
  sprintf("  Profile failures             : %d",     n_failed_profile),
  sprintf("  Optimizer errors             : %d",     n_err),
  sprintf("  Trees with 0 logs selected   : %d",     n_zero_sel),
  "",
  "  Product summary:"
)

for (j in seq_len(nrow(prod_summ))) {
  wlog(sprintf("    %-26s %4d logs  %8.4f m3  %10.2f NOK",
               paste(prod_summ$species_group[j], prod_summ$ProductKey[j]),
               prod_summ$n_logs[j],
               prod_summ$total_volume_m3[j],
               prod_summ$total_value_nok[j]))
}

wlog(
  "",
  sprintf("  Total selected logs          : %d",  nrow(all_selected)),
  sprintf("  Total value (NOK)            : %.2f",
          sum(tree_summ$total_selected_value_nok, na.rm = TRUE)),
  sprintf("  Total volume (m3)            : %.4f",
          sum(tree_summ$total_selected_volume_m3,  na.rm = TRUE)),
  sprintf("  Total overlapping pairs      : %d",
          n_overlap_total)
)

wlog(sprintf(
  "\n  Price-diameter capping (PriceDiameterClass_cm_uncapped > %d cm):",
  MAX_PRICE_DIAMETER_CLASS_CM))
for (sg in names(cap_by_species)) {
  wlog(sprintf("    %-8s : %d eligible candidate(s) capped", sg, cap_by_species[sg]))
}

if (n_dm31_total > 0L) {
  wlog(sprintf(
    "\n  REVIEW: %d unpriced 31 dm sawlog candidate(s).", n_dm31_total),
    "  Known issue: sawlog price matrices start at 34 dm.")
}
if (nrow(miss_summ) > 0L) {
  other_miss <- miss_summ[!(miss_summ$LengthClass_dm == "31" &
                              grepl("sawlog", miss_summ$ProductKey)), ]
  if (nrow(other_miss) > 0L) {
    wlog("  WARNING: Missing prices outside known 31 dm sawlog gap:")
    for (j in seq_len(nrow(other_miss))) {
      wlog(sprintf("    %s  LengthClass_dm=%s  n=%d",
                   other_miss$ProductKey[j],
                   other_miss$LengthClass_dm[j],
                   other_miss$Freq[j]))
    }
  }
}

wlog(
  sprintf("\n  Elapsed time                 : %.1f s", elapsed_total),
  sprintf("  Mean time per tree           : %.3f s", elapsed_total / max(n_opt, 1L)),
  sprintf("  Log: %s", log_file),
  "",
  "Script 10 complete."
)
