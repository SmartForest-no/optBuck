# ==============================================================================
# R/11_run_bucking_on_selected_stratum.R
#
# Purpose : Run the accepted vectorised DP bucking optimiser on all trees in
#           the selected NFI spruce stratum (hogstkl 41/51, bonitet 11/14/17)
#           as produced by script 01.
#
#           This is the selected example stratum (21 403 trees, 1 243 plots) --
#           NOT the full national NFI dataset.
#           No expansion factors.  No scaling to ha.
#           No bark correction.  No HPR / StanForD.
#           31 dm sawlog price gap is reported as REVIEW, not fixed.
#           STUMP_HEIGHT_CM = 10 cm.  START_POS_STEP_CM = 10 cm.
#
#           MAX_PRICE_DIAMETER_CLASS_CM = 50:
#             Caps PriceDiameterClass_cm for the price-matrix join only.
#             Physical TopDiameter and price_volume_m3 are NOT affected.
#             PriceDiameterClass_cm_uncapped = floor(TopDiameter_mm / 10) [original]
#             PriceDiameterClass_cm_used     = pmin(uncapped, 50)          [join key]
#             After join, PriceDiameterClass_cm is renamed to
#             PriceDiameterClass_cm_used so both columns appear in output.
#
#           SAVE_GENERATED_CANDIDATES = FALSE (default):
#             Full per-tree candidate tables are NOT written to disk.
#             Set TRUE only for debugging (files may be very large).
#             Unpriced candidates are always summarised per
#             (ProductKey, LengthClass_dm) and saved as
#             selected_stratum_unpriced_summary.csv.
#
#           N_CHECKPOINT_TREES = 1000:
#             Checkpoint status CSV is overwritten every N trees so progress
#             can be monitored externally.
#
# Inputs  :
#   outputs/stratum_tests/example_spruce_hkl_41_51_bonitet_11_14_17.parquet
#   inputs/ProductData_spruce_pine_default.csv
#   inputs/LengthClasses_spruce_pine_default.csv
#   inputs/price_matrices/PriceMatrix_long_default_2021.csv
#
# Outputs :
#   outputs/selected_stratum_bucking/selected_stratum_selected_logs.csv
#   outputs/selected_stratum_bucking/selected_stratum_tree_summary.csv
#   outputs/selected_stratum_bucking/selected_stratum_candidate_diagnostics.csv
#   outputs/selected_stratum_bucking/selected_stratum_unpriced_summary.csv
#   outputs/selected_stratum_bucking/selected_stratum_checkpoint_status.csv
#   outputs/selected_stratum_bucking/selected_stratum_run_summary.txt
#   outputs/selected_stratum_bucking/plot_selected_stratum_value_by_dbh.png
#   outputs/selected_stratum_bucking/plot_selected_stratum_product_volume.png
#   outputs/selected_stratum_bucking/plot_selected_stratum_zero_log_trees.png
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

STUMP_HEIGHT_CM             <- 10L     # cm above terrain; first log StartPos
START_POS_STEP_CM           <- 10L     # step between candidate StartPos values
PROFILE_STEP_CM             <- 10L     # must equal START_POS_STEP_CM
PROFILE_STEP_M              <- 0.10    # m; passed to taperNOR::taperNOR()
MIN_DBH_CM                  <- 5.0    # minimum DBH (cm) for taperNOR validity
MIN_HEIGHT_M                <- 4.5    # minimum height (m) for taperNOR validity

# Sawlog price matrices (default_2021) cover PriceDiameterClass_cm 12-50 cm
# for both spruce_sawlog and pine_sawlog.  Logs whose top-diameter class
# exceeds 50 cm are priced at the 50 cm class.  This cap applies ONLY to
# PriceDiameterClass_cm_used (the key used for the price-matrix join).
# TopDiameter and price_volume_m3 are computed from the true physical diameter.
MAX_PRICE_DIAMETER_CLASS_CM <- 50L

# Set to TRUE only for debugging -- full per-tree candidate tables are very
# large for a 21 000-tree run.
SAVE_GENERATED_CANDIDATES   <- FALSE

# Checkpoint status CSV is overwritten every N_CHECKPOINT_TREES trees.
N_CHECKPOINT_TREES          <- 1000L

root_dir   <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
inputs_dir <- file.path(root_dir, "inputs")
strat_dir  <- file.path(root_dir, "outputs", "stratum_tests")
out_dir    <- file.path(root_dir, "outputs", "selected_stratum_bucking")

STRATUM_FILE <- file.path(strat_dir,
  "example_spruce_hkl_41_51_bonitet_11_14_17.parquet")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

log_file <- file.path(out_dir, "selected_stratum_run_summary.txt")
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
  "  Selected stratum bucking run -- log",
  sprintf("  Run                          : %s", Sys.time()),
  sprintf("  STUMP_HEIGHT_CM              : %d cm", STUMP_HEIGHT_CM),
  sprintf("  START_POS_STEP_CM / PROFILE  : %d cm", PROFILE_STEP_CM),
  sprintf("  MAX_PRICE_DIAMETER_CLASS_CM  : %d cm", MAX_PRICE_DIAMETER_CLASS_CM),
  sprintf("  SAVE_GENERATED_CANDIDATES    : %s",    SAVE_GENERATED_CANDIDATES),
  sprintf("  N_CHECKPOINT_TREES           : %d",    N_CHECKPOINT_TREES),
  sprintf("  Stratum file                 : %s",    basename(STRATUM_FILE)),
  "============================================================="
)


# ---- helpers ----

`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}

# Generate one taper profile using taperNOR (mirrors scripts 02, 09, 10).
# Returns a data frame with one row per height step.
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


# ---- per-tree optimizer ----
#
# Identical logic to scripts 08/09/10 but returns unpriced_summary (a small
# per-(ProductKey, LengthClass_dm) count data frame) instead of the full
# unpriced candidates table.  This keeps memory usage manageable for a
# ~21 000-tree run.
#
# Returns a list with:
#   selected_logs     -- data frame of DP-selected logs (may be 0 rows)
#   unpriced_summary  -- data frame (ProductKey, LengthClass_dm, n)
#   ... per-tree counts and metadata

optimize_one_tree <- function(tree_id, profiles, pd, lc,
                               pm_mat, pm_fl, stump, step,
                               max_price_diam) {

  t0 <- proc.time()

  make_unpr_summ <- function() {
    data.frame(ProductKey     = character(0),
               LengthClass_dm = integer(0),
               n              = integer(0),
               stringsAsFactors = FALSE)
  }

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
      total_value_nok = 0, total_volume_m3 = 0,
      n_overlapping_pairs = 0L, n_unmatched_price = 0L,
      n_dm31_unpriced = 0L, n_price_diam_capped = 0L,
      dp_best_value = 0,
      n_unique_StartPos_generated = 0L, n_unique_StartPos_priced = 0L,
      selected_logs = NULL, unpriced_summary = make_unpr_summ(),
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
      total_value_nok = 0, total_volume_m3 = 0,
      n_overlapping_pairs = 0L, n_unmatched_price = 0L,
      n_dm31_unpriced = 0L, n_price_diam_capped = 0L,
      dp_best_value = 0,
      n_unique_StartPos_generated = 0L, n_unique_StartPos_priced = 0L,
      selected_logs = NULL, unpriced_summary = make_unpr_summ(),
      optimizer_status = "no_products",
      optimizer_message = msg,
      runtime_seconds = elapsed, error = msg
    ))
  }

  # Vectorised diameter lookup built once per tree.
  diam_fun <- approxfun(pv$diameterPosition, pv$DiameterValue, rule = 2)

  # Precompute cumulative solid volume on PROFILE_STEP_CM grid.
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
      total_value_nok = 0, total_volume_m3 = 0,
      n_overlapping_pairs = 0L, n_unmatched_price = 0L,
      n_dm31_unpriced = 0L, n_price_diam_capped = 0L,
      dp_best_value = 0,
      n_unique_StartPos_generated = 0L, n_unique_StartPos_priced = 0L,
      selected_logs = NULL, unpriced_summary = make_unpr_summ(),
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
      total_value_nok = 0, total_volume_m3 = 0,
      n_overlapping_pairs = 0L, n_unmatched_price = 0L,
      n_dm31_unpriced = 0L, n_price_diam_capped = n_capped_total,
      dp_best_value = 0,
      n_unique_StartPos_generated = n_unique_sp_gen,
      n_unique_StartPos_priced = 0L,
      selected_logs = NULL, unpriced_summary = make_unpr_summ(),
      optimizer_status = "no_candidates",
      optimizer_message = "No candidates fit within tree height",
      runtime_seconds = elapsed, error = NA_character_
    ))
  }

  # ---- price join ----
  # Eligible candidates only (non-NA price_volume_m3).
  elig_c      <- generated[generated$eligible &
                              !is.na(generated$price_volume_m3), , drop = FALSE]
  sawlog_c    <- elig_c[elig_c$ProductKey %in% sawlog_pks, , drop = FALSE]
  nonsawlog_c <- elig_c[elig_c$ProductKey %in% nonsawlog_pks, , drop = FALSE]

  # Sawlogs: join by ProductKey + LengthClass_dm + PriceDiameterClass_cm.
  # PriceDiameterClass_cm is the capped (used) value.
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

  # Rename capped join key to _used so both original and used are clearly named.
  names(priced)[names(priced) == "PriceDiameterClass_cm"] <-
    "PriceDiameterClass_cm_used"

  priced$value_nok <- priced$price_volume_m3 * priced$PriceNOKperM3

  n_unmatched <- sum(is.na(priced$PriceNOKperM3))
  n_dm31      <- sum(!is.na(priced$LengthClass_dm) &
                       priced$LengthClass_dm == 31L &
                       is.na(priced$PriceNOKperM3))

  unpriced_cands <- priced[is.na(priced$PriceNOKperM3), , drop = FALSE]

  # Build per-tree unpriced summary instead of storing full candidates.
  # This is the memory-efficient approach for large runs.
  unpr_summ <- if (nrow(unpriced_cands) > 0L) {
    t_up <- table(
      ProductKey     = unpriced_cands$ProductKey,
      LengthClass_dm = as.character(unpriced_cands$LengthClass_dm))
    updf <- as.data.frame(t_up, stringsAsFactors = FALSE)
    updf <- updf[updf$Freq > 0L, , drop = FALSE]
    names(updf)[names(updf) == "Freq"] <- "n"
    updf$LengthClass_dm <- as.integer(updf$LengthClass_dm)
    updf
  } else {
    make_unpr_summ()
  }

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
      total_value_nok = 0, total_volume_m3 = 0,
      n_overlapping_pairs = 0L, n_unmatched_price = n_unmatched,
      n_dm31_unpriced = n_dm31, n_price_diam_capped = n_capped_total,
      dp_best_value = 0,
      n_unique_StartPos_generated = n_unique_sp_gen,
      n_unique_StartPos_priced = 0L,
      selected_logs = NULL, unpriced_summary = unpr_summ,
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
    n_overlapping_pairs         = n_overlap,
    n_unmatched_price           = n_unmatched,
    n_dm31_unpriced             = n_dm31,
    n_price_diam_capped         = n_capped_total,
    dp_best_value               = round(dp$best_value, 2),
    n_unique_StartPos_generated = n_unique_sp_gen,
    n_unique_StartPos_priced    = n_unique_sp_priced,
    selected_logs               = sel,
    unpriced_summary            = unpr_summ,
    optimizer_status            = status,
    optimizer_message           = msg,
    runtime_seconds             = elapsed,
    error                       = NA_character_
  )
}


# ---- checkpoint writer ----
# Overwrites checkpoint CSV at every N_CHECKPOINT_TREES and at completion.

write_checkpoint <- function(n_done, n_total, results_so_far, t0, out_dir) {
  elapsed <- as.numeric((proc.time() - t0)["elapsed"])
  done_rs <- results_so_far[seq_len(n_done)]

  n_err  <- sum(vapply(done_rs,
                       function(r) !is.na(r$error %||% NA_character_),
                       logical(1)))
  n_logs <- sum(vapply(done_rs,
                       function(r) {
                         if (!is.null(r$selected_logs)) nrow(r$selected_logs)
                         else 0L
                       },
                       integer(1)))
  tot_val <- sum(vapply(done_rs,
                        function(r) r$total_value_nok %||% 0,
                        numeric(1)),
                 na.rm = TRUE)

  chk <- data.frame(
    checkpoint_time    = format(Sys.time()),
    n_trees_processed  = as.integer(n_done),
    n_trees_total      = as.integer(n_total),
    pct_complete       = round(n_done / max(n_total, 1L) * 100, 1),
    n_optimizer_errors = as.integer(n_err),
    n_selected_logs    = as.integer(n_logs),
    total_value_nok    = round(tot_val, 2),
    elapsed_seconds    = round(elapsed, 1),
    trees_per_second   = round(n_done / max(elapsed, 0.001), 3),
    stringsAsFactors   = FALSE
  )
  readr::write_csv(chk,
    file.path(out_dir, "selected_stratum_checkpoint_status.csv"))
}


# ---- read stratum parquet ----

wlog("", "--- Reading stratum file ---")

if (!file.exists(STRATUM_FILE)) {
  stop("Stratum parquet not found:\n  ", STRATUM_FILE,
       "\nRun 01_test_stratum_selection.R first.")
}

stratum_raw <- arrow::read_parquet(STRATUM_FILE)

wlog(sprintf("  Input file   : %s", basename(STRATUM_FILE)))
wlog(sprintf("  Rows (raw)   : %d", nrow(stratum_raw)))
wlog(sprintf("  Columns      : %s", paste(names(stratum_raw), collapse = ", ")))

# Compute height_m from beregnet_hoyde (dm) if height_m not already present.
if ("height_m" %in% names(stratum_raw)) {
  stratum_raw$height_m_use <- as.numeric(stratum_raw$height_m)
} else if ("beregnet_hoyde" %in% names(stratum_raw)) {
  stratum_raw$height_m_use <- as.numeric(stratum_raw$beregnet_hoyde) / 10
} else {
  stop("Neither 'height_m' nor 'beregnet_hoyde' found in stratum file.")
}

# Filter to trees valid for taperNOR.
stratum <- stratum_raw[
  !is.na(stratum_raw$dbh_cm)       & stratum_raw$dbh_cm       >= MIN_DBH_CM  &
  !is.na(stratum_raw$height_m_use) & stratum_raw$height_m_use >= MIN_HEIGHT_M, ]

n_raw   <- nrow(stratum_raw)
n_valid <- nrow(stratum)
n_filt  <- n_raw - n_valid
n_plots <- length(unique(stratum$plot_id))

wlog(sprintf("  Trees (raw)           : %d", n_raw))
wlog(sprintf("  Trees filtered out    : %d  (DBH < %.1f cm or ht < %.1f m)",
             n_filt, MIN_DBH_CM, MIN_HEIGHT_M))
wlog(sprintf("  Trees (valid, used)   : %d  (%.1f%% of raw)",
             n_valid, n_valid / max(n_raw, 1L) * 100))
wlog(sprintf("  Plots represented     : %d", n_plots))

if ("species_group" %in% names(stratum)) {
  sp_tab <- table(stratum$species_group)
  for (sg in names(sp_tab)) {
    wlog(sprintf("  Species %-8s      : %d trees", sg, sp_tab[sg]))
  }
} else {
  stop("Column 'species_group' not found in stratum file.")
}

has_plot_area <- "plot_area_m2" %in% names(stratum)
wlog(sprintf("  plot_area_m2 present  : %s", has_plot_area))

# Build tree table for the run.
tree_table <- data.frame(
  treid         = as.character(stratum$treid),
  plot_id       = as.character(stratum$plot_id),
  sesong        = if ("sesong" %in% names(stratum)) stratum$sesong else NA_character_,
  species_group = stratum$species_group,
  dbh_cm        = stratum$dbh_cm,
  height_m      = stratum$height_m_use,
  stringsAsFactors = FALSE
)
if (has_plot_area) {
  tree_table$plot_area_m2 <- stratum$plot_area_m2
}


# ---- generate taper profiles ----

wlog("", "--- Generating taper profiles ---")

n_trees         <- nrow(tree_table)
profile_list    <- vector("list", n_trees)
failed_profiles <- character(0)

for (i in seq_len(n_trees)) {
  row    <- tree_table[i, ]
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

n_failed_profile <- length(failed_profiles)
good_profiles    <- profile_list[!vapply(profile_list, is.null, logical(1))]
profiles_raw     <- do.call(rbind, good_profiles)

wlog(sprintf("  Profiles generated   : %d", length(good_profiles)))
wlog(sprintf("  Profile failures     : %d", n_failed_profile))
if (n_failed_profile > 0L) {
  shown <- failed_profiles[seq_len(min(10L, n_failed_profile))]
  more  <- if (n_failed_profile > 10L) " ..." else ""
  wlog(sprintf("  Failed tree ids      : %s%s",
               paste(shown, collapse = ", "), more))
}

# Convert to optimizer format.
# diameterPosition: cm (multiples of PROFILE_STEP_CM).
# DiameterValue: mm.  DBH: mm.
profiles_conv <- profiles_raw
profiles_conv$diameterPosition <- as.integer(
  round(profiles_conv$z_m * 100 / PROFILE_STEP_CM) * PROFILE_STEP_CM)
profiles_conv$DiameterValue    <- profiles_conv$diameter_cm * 10
profiles_conv$DBH              <- profiles_conv$dbh_cm * 10
profiles_conv <- profiles_conv[
  !duplicated(profiles_conv[, c("treid", "diameterPosition")]), ]

# Build lookup maps from profile data.
prof_n_rows <- tapply(profiles_conv$diameterPosition,
                       profiles_conv$treid, length)
plot_id_map <- unique(profiles_conv[, c("treid", "plot_id")])

wlog(sprintf("  Profile rows total   : %d", nrow(profiles_conv)))
wlog(sprintf("  Unique trees in prof : %d", length(unique(profiles_conv$treid))))


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


# ---- run optimizer on all trees ----

all_tree_ids  <- as.character(unique(profiles_conv$treid))
n_opt         <- length(all_tree_ids)

wlog(sprintf("\n--- Running optimizer on %d trees ---", n_opt))
t_start <- proc.time()

results_list  <- vector("list", n_opt)
progress_step <- max(1L, as.integer(n_opt / 20L))

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
        total_volume_m3 = NA_real_,
        n_overlapping_pairs = NA_integer_, n_unmatched_price = NA_integer_,
        n_dm31_unpriced = NA_integer_, n_price_diam_capped = NA_integer_,
        dp_best_value = NA_real_,
        n_unique_StartPos_generated = NA_integer_,
        n_unique_StartPos_priced    = NA_integer_,
        selected_logs = NULL,
        unpriced_summary = data.frame(
          ProductKey     = character(0),
          LengthClass_dm = integer(0),
          n              = integer(0),
          stringsAsFactors = FALSE),
        optimizer_status  = "FAIL",
        optimizer_message = conditionMessage(e),
        runtime_seconds   = 0,
        error             = conditionMessage(e)
      )
    }
  )

  # Write checkpoint and log progress at every N_CHECKPOINT_TREES.
  if (i %% N_CHECKPOINT_TREES == 0L || i == n_opt) {
    write_checkpoint(i, n_opt, results_list, t_start, out_dir)
    elapsed_so_far <- as.numeric((proc.time() - t_start)["elapsed"])
    wlog(sprintf("  Checkpoint %d / %d  (%.0f s elapsed, %.3f s/tree)",
                 i, n_opt, elapsed_so_far,
                 elapsed_so_far / max(i, 1L)))
  } else if (i %% progress_step == 0L) {
    elapsed_so_far <- as.numeric((proc.time() - t_start)["elapsed"])
    wlog(sprintf("  Progress: %d / %d trees  (%.0f s elapsed)",
                 i, n_opt, elapsed_so_far))
  }
}

elapsed_total <- as.numeric((proc.time() - t_start)["elapsed"])
wlog(sprintf("  Completed in %.1f s  (%.3f s/tree)",
             elapsed_total, elapsed_total / max(n_opt, 1L)))


# ---- assemble results ----

wlog("", "--- Assembling results ---")

tree_summ <- do.call(rbind, lapply(results_list, function(r) {
  dbh_mm_val <- r$dbh_mm %||% NA_real_
  ht_cm_val  <- r$height_cm %||% NA_real_
  data.frame(
    treid                              = r$tree_id,
    plot_id                            = NA_character_,
    species_group                      = r$species_group %||% NA_character_,
    dbh_cm                             = if (!is.na(dbh_mm_val)) dbh_mm_val / 10
                                         else NA_real_,
    height_m                           = if (!is.na(ht_cm_val)) ht_cm_val / 100
                                         else NA_real_,
    n_profile_rows                     = NA_integer_,
    n_generated_candidates             = r$n_generated             %||% NA_integer_,
    n_priced_candidates                = r$n_priced                %||% NA_integer_,
    n_unpriced_candidates              = r$n_unmatched_price       %||% NA_integer_,
    n_unpriced_31dm_sawlog             = r$n_dm31_unpriced         %||% NA_integer_,
    n_price_diameter_capped_candidates = r$n_price_diam_capped     %||% NA_integer_,
    n_unique_StartPos_generated        = r$n_unique_StartPos_generated %||% NA_integer_,
    n_unique_StartPos_priced           = r$n_unique_StartPos_priced    %||% NA_integer_,
    n_selected_logs                    = r$n_selected              %||% NA_integer_,
    total_selected_volume_m3           = r$total_volume_m3         %||% NA_real_,
    total_selected_value_nok           = r$total_value_nok         %||% NA_real_,
    selected_sawlog_volume_m3          = 0,
    selected_pulpwood_volume_m3        = 0,
    selected_waste_volume_m3           = 0,
    selected_sawlog_value_nok          = 0,
    selected_pulpwood_value_nok        = 0,
    selected_waste_value_nok           = 0,
    stem_coverage_pct                  = NA_real_,
    runtime_seconds                    = r$runtime_seconds         %||% NA_real_,
    optimizer_status                   = r$optimizer_status        %||% NA_character_,
    optimizer_message                  = r$optimizer_message       %||% NA_character_,
    error                              = r$error                   %||% NA_character_,
    stringsAsFactors = FALSE
  )
}))

# Fill plot_id and profile row counts from the profile lookup maps.
for (k in seq_len(nrow(tree_summ))) {
  tid <- as.character(tree_summ$treid[k])
  pid <- plot_id_map$plot_id[plot_id_map$treid == tid]
  if (length(pid) > 0L) {
    tree_summ$plot_id[k] <- as.character(pid[1])
  }
  n <- prof_n_rows[tid]
  if (!is.null(n) && !is.na(n)) {
    tree_summ$n_profile_rows[k] <- as.integer(n)
  }
}

# Stack selected logs from all trees.
sel_list <- lapply(results_list, function(r) r$selected_logs)
sel_list <- sel_list[!vapply(sel_list, is.null, logical(1))]
sel_list <- sel_list[vapply(sel_list, nrow, integer(1)) > 0L]

empty_sel <- data.frame(
  treid = character(0), species_group = character(0),
  ProductKey = character(0), ProductType = character(0),
  StartPos = integer(0), StopPos = integer(0),
  LogLength = integer(0), TopDiameter = numeric(0),
  price_volume_m3 = numeric(0), PriceNOKperM3 = numeric(0),
  value_nok = numeric(0), LengthClass_dm = integer(0),
  PriceDiameterClass_cm_uncapped = integer(0),
  PriceDiameterClass_cm_used     = integer(0),
  stringsAsFactors = FALSE)

all_selected <- if (length(sel_list) > 0L) do.call(rbind, sel_list) else empty_sel

# Add plot_id to selected logs via treid lookup.
if (nrow(all_selected) > 0L) {
  all_selected$plot_id <- plot_id_map$plot_id[
    match(as.character(all_selected$treid), as.character(plot_id_map$treid))]
}

# Fill per-ProductType volume and value columns in tree summary.
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

# Stem coverage from total log length vs usable stem length.
if (nrow(all_selected) > 0L) {
  total_len_by_tree <- tapply(all_selected$LogLength, all_selected$treid,
                               sum, na.rm = TRUE)
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
}

# Run-level counts.
n_ok            <- sum(is.na(tree_summ$error))
n_err           <- sum(!is.na(tree_summ$error))
n_zero_sel      <- sum(is.na(tree_summ$error) &
                         !is.na(tree_summ$n_selected_logs) &
                         tree_summ$n_selected_logs == 0L)
n_dm31_total    <- sum(tree_summ$n_unpriced_31dm_sawlog,          na.rm = TRUE)
n_capped_total  <- sum(tree_summ$n_price_diameter_capped_candidates, na.rm = TRUE)
n_overlap_total <- sum(vapply(results_list,
                               function(r) r$n_overlapping_pairs %||% 0L,
                               integer(1)), na.rm = TRUE)

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

# Aggregate unpriced summaries from all trees.
unpr_list <- lapply(results_list, function(r) r$unpriced_summary)
unpr_list <- unpr_list[!vapply(unpr_list, is.null, logical(1))]
unpr_list <- unpr_list[vapply(unpr_list, nrow, integer(1)) > 0L]

if (length(unpr_list) > 0L) {
  unpr_combined    <- do.call(rbind, unpr_list)
  unpriced_summary <- aggregate(n ~ ProductKey + LengthClass_dm,
                                 data = unpr_combined, FUN = sum)
  unpriced_summary$LengthClass_dm <- as.integer(unpriced_summary$LengthClass_dm)
  unpriced_summary <- unpriced_summary[order(unpriced_summary$ProductKey,
                                             unpriced_summary$LengthClass_dm), ]
} else {
  unpriced_summary <- data.frame(
    ProductKey     = character(0),
    LengthClass_dm = integer(0),
    n              = integer(0),
    stringsAsFactors = FALSE)
}

# Missing-price summary for logging.
miss_summ <- unpriced_summary

# Price-diameter capping summary by species_group.
cap_by_species <- tapply(
  tree_summ$n_price_diameter_capped_candidates,
  tree_summ$species_group, sum, na.rm = TRUE)

# Run-level diagnostics (key-value).
diag_df <- data.frame(
  item  = c("stratum_file", "stump_height_cm", "start_pos_step_cm",
            "profile_step_cm", "price_scenario",
            "max_price_diameter_class_cm",
            "n_trees_raw", "n_trees_valid",
            "n_plots_represented",
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
  value = c(basename(STRATUM_FILE),
            as.character(STUMP_HEIGHT_CM),
            as.character(START_POS_STEP_CM),
            as.character(PROFILE_STEP_CM),
            PRICE_SCENARIO,
            as.character(MAX_PRICE_DIAMETER_CLASS_CM),
            as.character(n_raw),
            as.character(n_valid),
            as.character(n_plots),
            as.character(n_failed_profile),
            as.character(n_ok),
            as.character(n_err),
            as.character(n_zero_sel),
            as.character(nrow(all_selected)),
            as.character(round(sum(tree_summ$total_selected_value_nok,
                                   na.rm = TRUE), 2)),
            as.character(round(sum(tree_summ$total_selected_volume_m3,
                                   na.rm = TRUE), 4)),
            as.character(n_overlap_total),
            as.character(n_dm31_total),
            as.character(n_capped_total),
            as.character(round(elapsed_total, 1)),
            as.character(round(elapsed_total / max(n_opt, 1L), 3))),
  stringsAsFactors = FALSE
)


# ---- optional per-plot summary ----
# NOTE: Simple per-plot aggregation of bucking results.
# NOT ha-expanded.  No NFI expansion factors applied.
# plot_area_m2 is included as metadata only (not used for area correction).

if (has_plot_area && nrow(all_selected) > 0L) {
  plot_area_map <- unique(stratum[, c("plot_id", "plot_area_m2")])
  plot_area_map$plot_id <- as.character(plot_area_map$plot_id)

  ts_plot <- data.frame(
    plot_id          = tree_summ$plot_id,
    n_selected_logs  = tree_summ$n_selected_logs,
    total_vol_m3     = tree_summ$total_selected_volume_m3,
    total_val_nok    = tree_summ$total_selected_value_nok,
    stringsAsFactors = FALSE
  )
  ts_plot <- ts_plot[!is.na(ts_plot$plot_id), ]

  plot_summ <- aggregate(
    cbind(n_trees       = 1,
          n_sel_logs    = n_selected_logs,
          total_vol_m3  = total_vol_m3,
          total_val_nok = total_val_nok) ~ plot_id,
    data = ts_plot, FUN = sum)

  plot_summ <- merge(plot_summ, plot_area_map, by = "plot_id", all.x = TRUE)
  plot_summ$note <- "NOT_ha_expanded_no_expansion_factors"

  readr::write_csv(plot_summ,
    file.path(out_dir, "selected_stratum_plot_summary.csv"))
  wlog(sprintf("  Saved: selected_stratum_plot_summary.csv (%d plots)",
               nrow(plot_summ)))
  wlog("  NOTE: plot_summary is NOT ha-expanded.  No NFI expansion factors applied.")
}


# ---- save outputs ----

wlog("", "--- Saving outputs ---")

# Tree summary.
readr::write_csv(tree_summ,
  file.path(out_dir, "selected_stratum_tree_summary.csv"))
wlog(sprintf("  Saved: selected_stratum_tree_summary.csv (%d rows)", nrow(tree_summ)))

# Selected logs: required columns + plot_id.
sel_cols <- c("treid", "plot_id", "species_group", "ProductKey",
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
  file.path(out_dir, "selected_stratum_selected_logs.csv"))
wlog(sprintf("  Saved: selected_stratum_selected_logs.csv (%d rows)", nrow(sel_out)))

# Candidate diagnostics (key-value).
readr::write_csv(diag_df,
  file.path(out_dir, "selected_stratum_candidate_diagnostics.csv"))
wlog("  Saved: selected_stratum_candidate_diagnostics.csv")

# Unpriced summary (aggregated -- not full candidate rows).
readr::write_csv(unpriced_summary,
  file.path(out_dir, "selected_stratum_unpriced_summary.csv"))
wlog(sprintf("  Saved: selected_stratum_unpriced_summary.csv (%d rows)",
             nrow(unpriced_summary)))

# Final checkpoint (all trees complete).
write_checkpoint(n_opt, n_opt, results_list, t_start, out_dir)
wlog("  Saved: selected_stratum_checkpoint_status.csv")


# ---- diagnostic plots ----

wlog("", "--- Diagnostic plots ---")

if (nrow(tree_summ) > 0L && any(!is.na(tree_summ$total_selected_value_nok))) {
  p_val <- ggplot2::ggplot(
    tree_summ[!is.na(tree_summ$dbh_cm), ],
    ggplot2::aes(
      x      = dbh_cm,
      y      = ifelse(is.na(total_selected_value_nok),
                      0, total_selected_value_nok),
      colour = optimizer_status)) +
    ggplot2::geom_point(size = 1.2, alpha = 0.5) +
    ggplot2::labs(
      title   = "Selected stratum: total selected value by DBH",
      x       = "DBH (cm)",
      y       = "Total selected value (NOK)",
      colour  = "Status",
      caption = sprintf("n = %d trees  |  stratum: %s",
                        n_opt, basename(STRATUM_FILE))) +
    ggplot2::theme_bw()
  save_plot(p_val, "plot_selected_stratum_value_by_dbh.png")
}

if (nrow(prod_summ) > 0L) {
  p_vol <- ggplot2::ggplot(
    prod_summ,
    ggplot2::aes(x = ProductKey, y = total_volume_m3,
                 fill = species_group)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(
      title = "Selected stratum: selected volume by product",
      x     = "Product",
      y     = "Total volume (m\u00b3)",
      fill  = "Species") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  save_plot(p_vol, "plot_selected_stratum_product_volume.png")
}

# Zero-log tree DBH distribution.
zero_log_trees <- tree_summ[
  !is.na(tree_summ$n_selected_logs) & tree_summ$n_selected_logs == 0L &
  is.na(tree_summ$error), ]

if (nrow(zero_log_trees) > 0L && any(!is.na(zero_log_trees$dbh_cm))) {
  p_zero <- ggplot2::ggplot(
    zero_log_trees[!is.na(zero_log_trees$dbh_cm), ],
    ggplot2::aes(x = dbh_cm, fill = optimizer_status)) +
    ggplot2::geom_histogram(binwidth = 1, colour = "white") +
    ggplot2::labs(
      title   = "Selected stratum: zero-log trees by DBH",
      x       = "DBH (cm)",
      y       = "Count",
      fill    = "Status",
      caption = "Trees with 0 selected logs (DP found no value-positive sequence)") +
    ggplot2::theme_bw()
  save_plot(p_zero, "plot_selected_stratum_zero_log_trees.png")
}


# ---- summary ----

wlog(
  "",
  "=============================================================",
  "  SELECTED STRATUM BUCKING SUMMARY",
  "=============================================================",
  sprintf("  Stratum file                 : %s", basename(STRATUM_FILE)),
  sprintf("  STUMP_HEIGHT_CM              : %d cm",  STUMP_HEIGHT_CM),
  sprintf("  MAX_PRICE_DIAMETER_CLASS_CM  : %d cm",  MAX_PRICE_DIAMETER_CLASS_CM),
  "",
  sprintf("  Trees in stratum (raw)       : %d",     n_raw),
  sprintf("  Trees valid (used)           : %d",     n_valid),
  sprintf("  Trees filtered out           : %d",     n_filt),
  sprintf("  Plots represented            : %d",     n_plots),
  sprintf("  Profile failures             : %d",     n_failed_profile),
  sprintf("  Optimizer errors             : %d",     n_err),
  sprintf("  Trees with 0 logs selected   : %d (%.1f%%)",
          n_zero_sel, n_zero_sel / max(n_valid, 1L) * 100),
  "",
  "  Product summary:"
)

for (j in seq_len(nrow(prod_summ))) {
  wlog(sprintf("    %-26s %6d logs  %10.4f m3  %12.2f NOK",
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
          sum(tree_summ$total_selected_volume_m3, na.rm = TRUE)),
  sprintf("  Total overlapping pairs      : %d",  n_overlap_total)
)

wlog(sprintf(
  "\n  Price-diameter capping (PriceDiameterClass_cm_uncapped > %d cm):",
  MAX_PRICE_DIAMETER_CLASS_CM))
for (sg in names(cap_by_species)) {
  wlog(sprintf("    %-8s : %d eligible candidate(s) capped", sg, cap_by_species[sg]))
}

if (n_dm31_total > 0L) {
  wlog(
    sprintf("\n  REVIEW: %d unpriced 31 dm sawlog candidate(s).", n_dm31_total),
    "  Known issue: sawlog price matrices start at 34 dm.")
}

if (nrow(miss_summ) > 0L) {
  other_miss <- miss_summ[!(miss_summ$LengthClass_dm == 31L &
                              grepl("sawlog", miss_summ$ProductKey)), ]
  if (nrow(other_miss) > 0L) {
    wlog("  WARNING: Missing prices outside known 31 dm sawlog gap:")
    for (j in seq_len(nrow(other_miss))) {
      wlog(sprintf("    %s  LengthClass_dm=%d  n=%d",
                   other_miss$ProductKey[j],
                   other_miss$LengthClass_dm[j],
                   other_miss$n[j]))
    }
  }
}

wlog(
  sprintf("\n  Elapsed time                 : %.1f s", elapsed_total),
  sprintf("  Mean time per tree           : %.3f s",
          elapsed_total / max(n_opt, 1L)),
  sprintf("  Log: %s", log_file),
  "",
  "Script 11 complete."
)
