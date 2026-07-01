# ==============================================================================
# R/09_test_pine_bucking_smoke.R
#
# Purpose : Pine end-to-end smoke test.
#           Selects up to 25 pine trees across the DBH range from the
#           stratum-ready NFI dataset, generates taperNOR stem profiles
#           inline, runs the accepted vectorised DP bucking optimiser, and
#           writes per-tree, product-level, and candidate-level summaries.
#
#           This is NOT the full NFI pine stratum run.
#           No expansion factors.  No scaling to ha.
#           No bark correction.  No HPR / StanForD.
#           31 dm sawlog price gap is reported as REVIEW, not fixed.
#           STUMP_HEIGHT_CM = 10 cm.
#
# Inputs  :
#   data_processed/nfi_tree_plot_2024_spruce_pine_stratum_ready.parquet
#   inputs/ProductData_spruce_pine_default.csv
#   inputs/LengthClasses_spruce_pine_default.csv
#   inputs/price_matrices/PriceMatrix_long_default_2021.csv
#
# Outputs :
#   outputs/pine_smoke_bucking/pine_smoke_selected_trees.csv
#   outputs/pine_smoke_bucking/pine_smoke_stem_profiles.csv
#   outputs/pine_smoke_bucking/pine_smoke_selected_logs.csv
#   outputs/pine_smoke_bucking/pine_smoke_tree_summary.csv
#   outputs/pine_smoke_bucking/pine_smoke_candidate_diagnostics.csv
#   outputs/pine_smoke_bucking/pine_smoke_unpriced_candidates.csv
#   outputs/pine_smoke_bucking/pine_smoke_run_summary.txt
#   outputs/pine_smoke_bucking/plot_pine_smoke_value_by_dbh.png
#   outputs/pine_smoke_bucking/plot_pine_smoke_product_volume.png
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

# Parameters (near top for easy inspection / sensitivity testing).
STUMP_HEIGHT_CM  <- 10L    # cm above terrain; first possible StartPos
PROFILE_STEP_CM  <- 10L    # must match taperNOR profile step
PROFILE_STEP_M   <- 0.10   # m; passed to taperNOR
N_TREES_TARGET   <- 25L    # pine trees to select
MIN_DBH_CM       <- 5.0    # minimum DBH for taperNOR
MIN_HEIGHT_M     <- 4.5    # minimum height for taperNOR

root_dir   <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
inputs_dir <- file.path(root_dir, "inputs")
data_dir   <- file.path(root_dir, "data_processed")
out_dir    <- file.path(root_dir, "outputs", "pine_smoke_bucking")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

log_file <- file.path(out_dir, "pine_smoke_run_summary.txt")
cat("", file = log_file)   # reset on each run

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
  "  Pine bucking smoke test \u2013 log",
  sprintf("  Run                 : %s", Sys.time()),
  sprintf("  STUMP_HEIGHT_CM     : %d cm", STUMP_HEIGHT_CM),
  sprintf("  PROFILE_STEP_CM     : %d cm", PROFILE_STEP_CM),
  sprintf("  N_TREES_TARGET      : %d",    N_TREES_TARGET),
  "============================================================="
)


# ---- helpers ----

`%||%` <- function(a, b) {
  if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
}

# Select N trees distributed across the DBH range using evenly-spaced
# quantile positions (same strategy as script 07).
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

# Generate one taper profile using taperNOR (mirrors script 02 conventions).
# Returns a data frame with raw z_m / diameter_cm columns.
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

# Build internal profile vectors from the converted profiles data frame.
# diameterPosition is in cm (multiples of PROFILE_STEP_CM).
# DiameterValue is in mm.  DBH is in mm.
get_tree_profile_vectors <- function(profiles, tree_id) {
  tree_rows <- profiles[profiles$treid == tree_id, ]
  tree_rows <- tree_rows[order(tree_rows$diameterPosition), ]
  if (nrow(tree_rows) == 0) {
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


# Vectorised per-tree optimizer.
# Extended relative to script 08: also returns unpriced_cands and per-StartPos
# diagnostics.  Uses approxfun() + precomputed cumulative volumes.
# sawlog_pks and nonsawlog_pks are read from the enclosing script environment.
optimize_one_tree <- function(tree_id, profiles, pd, lc,
                               pm_mat, pm_fl, stump, step) {

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
      dp_best_value = 0,
      n_unique_StartPos_generated = 0L, n_unique_StartPos_priced = 0L,
      selected_logs = NULL, unpriced_cands = NULL,
      optimizer_status = "error", optimizer_message = "Profile not found",
      runtime_seconds = elapsed, error = "Profile not found"
    ))
  }

  sg          <- pv$species_group
  sp_products <- pd[pd$species_group == sg, ]

  if (nrow(sp_products) == 0) {
    elapsed <- as.numeric((proc.time() - t0)["elapsed"])
    msg     <- paste0("No products for species_group='", sg, "'")
    return(list(
      tree_id = tree_id, species_group = sg,
      dbh_mm = pv$DBH, height_cm = pv$height_cm,
      n_generated = 0L, n_priced = 0L, n_selected = 0L,
      total_value_nok = 0, total_volume_m3 = 0, total_length_cm = 0L,
      n_overlapping = 0L, n_unmatched_price = 0L, n_dm31_unpriced = 0L,
      dp_best_value = 0,
      n_unique_StartPos_generated = 0L, n_unique_StartPos_priced = 0L,
      selected_logs = NULL, unpriced_cands = NULL,
      optimizer_status = "no_products", optimizer_message = msg,
      runtime_seconds = elapsed, error = msg
    ))
  }

  # Vectorised diameter lookup (built once per tree).
  diam_fun <- approxfun(pv$diameterPosition, pv$DiameterValue, rule = 2)

  # Precomputed cumulative solid volume on PROFILE_STEP_CM grid.
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

  # All candidate StartPos values on the grid from stump to near top.
  start_positions <- seq(
    as.integer(stump),
    as.integer(pv$height_cm - 10L),
    by = step_int
  )

  if (length(start_positions) == 0) {
    elapsed <- as.numeric((proc.time() - t0)["elapsed"])
    return(list(
      tree_id = tree_id, species_group = sg,
      dbh_mm = pv$DBH, height_cm = pv$height_cm,
      n_generated = 0L, n_priced = 0L, n_selected = 0L,
      total_value_nok = 0, total_volume_m3 = 0, total_length_cm = 0L,
      n_overlapping = 0L, n_unmatched_price = 0L, n_dm31_unpriced = 0L,
      dp_best_value = 0,
      n_unique_StartPos_generated = 0L, n_unique_StartPos_priced = 0L,
      selected_logs = NULL, unpriced_cands = NULL,
      optimizer_status = "too_short",
      optimizer_message = "Tree too short for any log",
      runtime_seconds = elapsed, error = NA_character_
    ))
  }

  n_unique_sp_gen <- length(start_positions)
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
    if (nrow(combos) == 0) next

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

    prod_frames[[j]] <- data.frame(
      treid                 = as.character(tree_id),
      species_group         = sg,
      ProductKey            = pk,
      ProductType           = prow$ProductType,
      StartPos              = as.integer(combos$start_pos),
      StopPos               = as.integer(combos$stop_pos),
      LogLength             = as.integer(combos$log_len),
      TopDiameter           = round(combos$TopDiameter, 1),
      ButtDiameter          = round(combos$ButtDiameter, 1),
      eligible              = eligible,
      price_volume_m3       = round(price_vol, 5),
      LengthClass_dm        = as.integer(eff_length / 10L),
      PriceDiameterClass_cm = as.integer(floor(combos$TopDiameter / 10)),
      stringsAsFactors = FALSE
    )
  }

  generated <- do.call(rbind,
    prod_frames[!vapply(prod_frames, is.null, logical(1))])
  n_gen <- if (is.null(generated)) 0L else nrow(generated)

  if (n_gen == 0L) {
    elapsed <- as.numeric((proc.time() - t0)["elapsed"])
    return(list(
      tree_id = tree_id, species_group = sg,
      dbh_mm = pv$DBH, height_cm = pv$height_cm,
      n_generated = 0L, n_priced = 0L, n_selected = 0L,
      total_value_nok = 0, total_volume_m3 = 0, total_length_cm = 0L,
      n_overlapping = 0L, n_unmatched_price = 0L, n_dm31_unpriced = 0L,
      dp_best_value = 0,
      n_unique_StartPos_generated = n_unique_sp_gen,
      n_unique_StartPos_priced = 0L,
      selected_logs = NULL, unpriced_cands = NULL,
      optimizer_status = "no_candidates",
      optimizer_message = "No candidates generated (all combos exceed tree height)",
      runtime_seconds = elapsed, error = NA_character_
    ))
  }

  # ---- price join ----
  elig_c      <- generated[generated$eligible & !is.na(generated$price_volume_m3), ]
  sawlog_c    <- elig_c[elig_c$ProductKey %in% sawlog_pks, ]
  nonsawlog_c <- elig_c[elig_c$ProductKey %in% nonsawlog_pks, ]

  if (nrow(sawlog_c) > 0) {
    # Cap PriceDiameterClass_cm at the matrix maximum per ProductKey.
    # In Norwegian timber pricing the top diameter class covers all larger logs.
    max_diam_by_pk <- tapply(pm_mat$PriceDiameterClass_cm,
                              pm_mat$ProductKey, max)
    for (pk in unique(sawlog_c$ProductKey)) {
      if (pk %in% names(max_diam_by_pk)) {
        mask <- sawlog_c$ProductKey == pk
        sawlog_c$PriceDiameterClass_cm[mask] <- pmin(
          sawlog_c$PriceDiameterClass_cm[mask],
          as.integer(max_diam_by_pk[pk]))
      }
    }
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
                       priced$LengthClass_dm == 31L &
                       is.na(priced$PriceNOKperM3))

  # Collect eligible rows that received no price (for diagnostic export).
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
      n_dm31_unpriced = n_dm31,
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
      if (sel$StopPos[k] > sel$StartPos[k + 1L]) n_overlap <- n_overlap + 1L
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


# ---- read stratum-ready data and select pine trees ----

wlog("", "--- Selecting pine trees ---")

nfi_parquet <- file.path(data_dir,
  "nfi_tree_plot_2024_spruce_pine_stratum_ready.parquet")
if (!file.exists(nfi_parquet)) {
  stop("Stratum-ready parquet not found: ", nfi_parquet,
       "\nRun 00_prepare_nfi_2024_data.R first.")
}

nfi_all <- arrow::read_parquet(nfi_parquet)

nfi_pine <- nfi_all[nfi_all$species_group == "pine" &
                      !is.na(nfi_all$dbh_cm) &
                      nfi_all$dbh_cm >= MIN_DBH_CM &
                      !is.na(nfi_all$beregnet_hoyde), ]
nfi_pine$height_m <- as.numeric(nfi_pine$beregnet_hoyde) / 10
nfi_pine <- nfi_pine[nfi_pine$height_m >= MIN_HEIGHT_M, ]

n_pine_avail <- nrow(nfi_pine)
wlog(sprintf("  Pine trees in dataset (after filter) : %d", n_pine_avail))

if (n_pine_avail == 0L) stop("No pine trees available after filtering.")

selected_trees <- select_quantile_trees(nfi_pine, N_TREES_TARGET, "dbh_cm")
n_selected     <- nrow(selected_trees)

wlog(sprintf("  Pine trees selected                  : %d", n_selected))
wlog(sprintf("  DBH range selected (cm)              : %.1f \u2013 %.1f",
             min(selected_trees$dbh_cm), max(selected_trees$dbh_cm)))
wlog(sprintf("  Height range selected (m)            : %.1f \u2013 %.1f",
             min(selected_trees$height_m), max(selected_trees$height_m)))

# Save selected-tree metadata.
trees_out <- data.frame(
  treid         = as.character(selected_trees$treid),
  plot_id       = as.character(selected_trees$plot_id),
  species_group = selected_trees$species_group,
  dbh_cm        = selected_trees$dbh_cm,
  height_m      = selected_trees$height_m,
  stringsAsFactors = FALSE
)
readr::write_csv(trees_out,
  file.path(out_dir, "pine_smoke_selected_trees.csv"))
wlog(sprintf("  Saved: pine_smoke_selected_trees.csv (%d rows)", n_selected))


# ---- generate taper profiles ----

wlog("", "--- Generating taper profiles ---")

profile_list    <- vector("list", n_selected)
failed_profiles <- character(0)

for (i in seq_len(n_selected)) {
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
profiles_raw_list <- profile_list[!vapply(profile_list, is.null, logical(1))]
profiles_raw      <- do.call(rbind, profiles_raw_list)

wlog(sprintf("  Profiles generated  : %d", length(profiles_raw_list)))
wlog(sprintf("  Profile failures    : %d", n_failed_profile))
if (n_failed_profile > 0L) {
  wlog(sprintf("  Failed tree ids     : %s",
               paste(failed_profiles, collapse = ", ")))
}

# Save raw profiles (matches script 02 output format: z_m, diameter_cm).
readr::write_csv(profiles_raw,
  file.path(out_dir, "pine_smoke_stem_profiles.csv"))
wlog(sprintf("  Saved: pine_smoke_stem_profiles.csv (%d rows)", nrow(profiles_raw)))

# Convert to optimizer format: diameterPosition (cm), DiameterValue (mm), DBH (mm).
profiles_conv <- profiles_raw
profiles_conv$diameterPosition <- as.integer(
  round(profiles_conv$z_m * 100 / PROFILE_STEP_CM) * PROFILE_STEP_CM)
profiles_conv$DiameterValue    <- profiles_conv$diameter_cm * 10
profiles_conv$DBH              <- profiles_conv$dbh_cm * 10
# Remove floating-point duplicates from rounding.
profiles_conv <- profiles_conv[
  !duplicated(profiles_conv[, c("treid", "diameterPosition")]), ]

# Profile row counts per tree (used in tree summary).
prof_n_rows <- tapply(
  seq_len(nrow(profiles_conv)),
  profiles_conv$treid,
  length)


# ---- load products / prices ----

wlog("", "--- Loading products and prices ---")

pd_csv <- file.path(inputs_dir, "ProductData_spruce_pine_default.csv")
if (!file.exists(pd_csv)) stop("ProductData not found: ", pd_csv)
ProductData_all <- readr::read_csv(pd_csv, show_col_types = FALSE)
ProductData     <- ProductData_all[ProductData_all$species_group == "pine", ]
wlog(sprintf("  Pine products : %d (%s)",
             nrow(ProductData),
             paste(ProductData$ProductKey, collapse = ", ")))

lc_csv <- file.path(inputs_dir, "LengthClasses_spruce_pine_default.csv")
if (!file.exists(lc_csv)) stop("LengthClasses not found: ", lc_csv)
lc_long <- readr::read_csv(lc_csv, show_col_types = FALSE)
lc_pine <- lc_long[lc_long$ProductKey %in% ProductData$ProductKey, ]
LengthClasses <- split(as.integer(lc_pine$LogLength), lc_pine$ProductKey)
wlog(sprintf("  LengthClasses : %d pine products (%s)",
             length(LengthClasses),
             paste(names(LengthClasses), collapse = ", ")))

pm_csv <- file.path(inputs_dir, "price_matrices", "PriceMatrix_long_default_2021.csv")
if (!file.exists(pm_csv)) stop("PriceMatrix_long not found: ", pm_csv)
PriceMatrix_long <- readr::read_csv(pm_csv, show_col_types = FALSE)
PRICE_SCENARIO   <- unique(PriceMatrix_long$PriceScenario)[1]
wlog(sprintf("  PriceMatrix   : %d rows, scenario '%s'",
             nrow(PriceMatrix_long), PRICE_SCENARIO))

# Price lookup tables for sawlog (matrix) and non-sawlog (flat).
sawlog_pks    <- unique(ProductData$ProductKey[ProductData$ProductType == "sawlog"])
nonsawlog_pks <- unique(ProductData$ProductKey[ProductData$ProductType != "sawlog"])

pm_matrix <- PriceMatrix_long |>
  dplyr::filter(PriceScenario == PRICE_SCENARIO,
                PriceType     == "matrix",
                ProductKey    %in% ProductData$ProductKey) |>
  dplyr::select(ProductKey, LengthClass_dm, PriceDiameterClass_cm, PriceNOKperM3) |>
  dplyr::mutate(LengthClass_dm        = as.integer(LengthClass_dm),
                PriceDiameterClass_cm = as.integer(PriceDiameterClass_cm))

pm_flat <- PriceMatrix_long |>
  dplyr::filter(PriceScenario == PRICE_SCENARIO,
                PriceType     == "flat",
                ProductKey    %in% ProductData$ProductKey) |>
  dplyr::select(ProductKey, PriceNOKperM3_flat = PriceNOKperM3)


# ---- run optimizer on all selected pine trees ----

all_tree_ids <- as.character(unique(profiles_conv$treid))
n_opt        <- length(all_tree_ids)

wlog(sprintf("\n--- Running optimizer on %d pine trees ---", n_opt))
t_start <- proc.time()

results_list <- vector("list", n_opt)

for (i in seq_along(all_tree_ids)) {
  tid <- all_tree_ids[i]
  results_list[[i]] <- tryCatch(
    optimize_one_tree(tid, profiles_conv, ProductData, LengthClasses,
                      pm_matrix, pm_flat, STUMP_HEIGHT_CM, PROFILE_STEP_CM),
    error = function(e) {
      list(
        tree_id = tid, species_group = NA_character_,
        dbh_mm = NA_real_, height_cm = NA_real_,
        n_generated = NA_integer_, n_priced = NA_integer_,
        n_selected = NA_integer_, total_value_nok = NA_real_,
        total_volume_m3 = NA_real_, total_length_cm = NA_integer_,
        n_overlapping = NA_integer_, n_unmatched_price = NA_integer_,
        n_dm31_unpriced = NA_integer_, dp_best_value = NA_real_,
        n_unique_StartPos_generated = NA_integer_,
        n_unique_StartPos_priced    = NA_integer_,
        selected_logs = NULL, unpriced_cands = NULL,
        optimizer_status = "error",
        optimizer_message = conditionMessage(e),
        runtime_seconds = 0, error = conditionMessage(e)
      )
    }
  )
}

elapsed_total <- as.numeric((proc.time() - t_start)["elapsed"])
wlog(sprintf("  Completed in %.1f s", elapsed_total))


# ---- assemble results ----

wlog("", "--- Assembling results ---")

# Base tree summary (per-product columns filled below).
tree_summ <- do.call(rbind, lapply(results_list, function(r) {
  dbh_mm_val <- r$dbh_mm %||% NA_real_
  ht_cm_val  <- r$height_cm %||% NA_real_
  data.frame(
    treid                       = r$tree_id,
    species_group               = r$species_group %||% NA_character_,
    dbh_cm                      = if (!is.na(dbh_mm_val)) dbh_mm_val / 10 else NA_real_,
    height_m                    = if (!is.na(ht_cm_val)) ht_cm_val / 100 else NA_real_,
    n_profile_rows              = NA_integer_,   # filled below
    n_generated_candidates      = r$n_generated       %||% NA_integer_,
    n_priced_candidates         = r$n_priced           %||% NA_integer_,
    n_unpriced_candidates       = r$n_unmatched_price  %||% NA_integer_,
    n_unpriced_31dm_sawlog      = r$n_dm31_unpriced    %||% NA_integer_,
    n_unique_StartPos_generated = r$n_unique_StartPos_generated %||% NA_integer_,
    n_unique_StartPos_priced    = r$n_unique_StartPos_priced    %||% NA_integer_,
    n_selected_logs             = r$n_selected         %||% NA_integer_,
    total_selected_volume_m3    = r$total_volume_m3    %||% NA_real_,
    total_selected_value_nok    = r$total_value_nok    %||% NA_real_,
    total_length_cm             = r$total_length_cm    %||% NA_integer_,
    selected_sawlog_volume_m3   = 0,
    selected_pulpwood_volume_m3 = 0,
    selected_waste_volume_m3    = 0,
    selected_sawlog_value_nok   = 0,
    selected_pulpwood_value_nok = 0,
    selected_waste_value_nok    = 0,
    stem_coverage_pct           = NA_real_,
    runtime_seconds             = r$runtime_seconds    %||% NA_real_,
    optimizer_status            = r$optimizer_status   %||% NA_character_,
    optimizer_message           = r$optimizer_message  %||% NA_character_,
    error                       = r$error              %||% NA_character_,
    stringsAsFactors = FALSE
  )
}))

# Attach plot_id from profiles.
plot_id_map <- unique(profiles_conv[, c("treid", "plot_id")])
tree_summ   <- merge(tree_summ, plot_id_map, by = "treid", all.x = TRUE)

# Fill profile row counts.
for (tid in tree_summ$treid) {
  n <- prof_n_rows[as.character(tid)]
  if (!is.null(n) && !is.na(n)) {
    tree_summ$n_profile_rows[tree_summ$treid == tid] <- as.integer(n)
  }
}

# Reorder columns: treid, plot_id first.
col_order <- c("treid", "plot_id",
               setdiff(names(tree_summ), c("treid", "plot_id")))
tree_summ <- tree_summ[, col_order]

# Stack selected logs.
sel_list <- lapply(results_list, function(r) r$selected_logs)
sel_list <- sel_list[!vapply(sel_list, is.null, logical(1))]
sel_list <- sel_list[vapply(sel_list, nrow, integer(1)) > 0L]

empty_sel <- data.frame(
  treid = character(0), species_group = character(0),
  ProductKey = character(0), ProductType = character(0),
  StartPos = integer(0), StopPos = integer(0), LogLength = integer(0),
  TopDiameter = numeric(0), ButtDiameter = numeric(0),
  price_volume_m3 = numeric(0), LengthClass_dm = integer(0),
  PriceDiameterClass_cm = integer(0), PriceNOKperM3 = numeric(0),
  value_nok = numeric(0), stringsAsFactors = FALSE)

all_selected <- if (length(sel_list) > 0L) do.call(rbind, sel_list) else empty_sel

# Fill per-product volume/value in tree summary.
if (nrow(all_selected) > 0L) {
  for (pk in c("pine_sawlog", "pine_pulpwood", "pine_waste")) {
    pk_tag  <- sub("pine_", "", pk)
    vol_col <- paste0("selected_", pk_tag, "_volume_m3")
    val_col <- paste0("selected_", pk_tag, "_value_nok")
    pk_sel  <- all_selected[all_selected$ProductKey == pk, ]
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

# Stem coverage: fraction of usable stem length occupied by selected logs.
denom <- tree_summ$height_m * 100 - STUMP_HEIGHT_CM
tree_summ$stem_coverage_pct <- ifelse(
  !is.na(denom) & denom > 0 & !is.na(tree_summ$total_length_cm),
  round(tree_summ$total_length_cm / denom * 100, 1),
  NA_real_)

# Summary counts.
n_ok           <- sum(is.na(tree_summ$error))
n_err          <- sum(!is.na(tree_summ$error))
n_zero_sel     <- sum(is.na(tree_summ$error) &
                         !is.na(tree_summ$n_selected_logs) &
                         tree_summ$n_selected_logs == 0L)
n_dm31_total   <- sum(tree_summ$n_unpriced_31dm_sawlog, na.rm = TRUE)
n_overlap_total <- sum(tree_summ$n_overlapping_pairs %||% 0, na.rm = TRUE)

# Stack unpriced candidates.
unpr_list <- lapply(results_list, function(r) r$unpriced_cands)
unpr_list <- unpr_list[!vapply(unpr_list, is.null, logical(1))]
unpr_list <- unpr_list[vapply(unpr_list, nrow, integer(1)) > 0L]
unpriced_all <- if (length(unpr_list) > 0L) {
  do.call(rbind, unpr_list)
} else {
  data.frame(stringsAsFactors = FALSE)
}

# Product summary.
prod_summ <- if (nrow(all_selected) > 0L) {
  do.call(rbind, lapply(unique(all_selected$ProductKey), function(pk) {
    r <- all_selected[all_selected$ProductKey == pk, ]
    data.frame(
      ProductKey      = pk,
      n_logs          = nrow(r),
      total_volume_m3 = round(sum(r$price_volume_m3, na.rm = TRUE), 4),
      total_value_nok = round(sum(r$value_nok,        na.rm = TRUE), 2),
      mean_log_len_cm = round(mean(r$LogLength,        na.rm = TRUE), 1),
      stringsAsFactors = FALSE
    )
  }))
} else {
  data.frame(ProductKey = character(0), n_logs = integer(0),
             total_volume_m3 = numeric(0), total_value_nok = numeric(0),
             mean_log_len_cm = numeric(0), stringsAsFactors = FALSE)
}

# Missing-price summary (by ProductKey + LengthClass_dm).
if (nrow(unpriced_all) > 0L && "ProductKey" %in% names(unpriced_all)) {
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

# Run-level candidate diagnostics.
diag_df <- data.frame(
  item  = c("stump_height_cm", "profile_step_cm", "price_scenario",
            "n_pine_trees_available", "n_pine_trees_selected",
            "n_profile_failures",
            "n_trees_optimizer_ok", "n_trees_optimizer_error",
            "n_trees_zero_selected_logs",
            "n_selected_logs_total",
            "total_value_nok_all_trees",
            "total_volume_m3_all_trees",
            "n_overlapping_pairs_total",
            "n_unpriced_31dm_sawlog_total",
            "elapsed_seconds"),
  value = c(as.character(STUMP_HEIGHT_CM),
            as.character(PROFILE_STEP_CM),
            PRICE_SCENARIO,
            as.character(n_pine_avail),
            as.character(n_selected),
            as.character(n_failed_profile),
            as.character(n_ok),
            as.character(n_err),
            as.character(n_zero_sel),
            as.character(nrow(all_selected)),
            as.character(round(sum(tree_summ$total_selected_value_nok, na.rm = TRUE), 2)),
            as.character(round(sum(tree_summ$total_selected_volume_m3,  na.rm = TRUE), 4)),
            as.character(sum(tree_summ$n_overlapping_pairs %||% 0, na.rm = TRUE)),
            as.character(n_dm31_total),
            as.character(round(elapsed_total, 1))),
  stringsAsFactors = FALSE
)


# ---- save outputs ----

wlog("", "--- Saving outputs ---")

readr::write_csv(tree_summ[, setdiff(names(tree_summ), "total_length_cm")],
  file.path(out_dir, "pine_smoke_tree_summary.csv"))
wlog(sprintf("  Saved: pine_smoke_tree_summary.csv (%d rows)", nrow(tree_summ)))

# Select only required columns for selected_logs output.
sel_cols <- c("treid", "species_group", "ProductKey",
              "StartPos", "StopPos", "LogLength",
              "price_volume_m3", "PriceNOKperM3", "value_nok",
              "LengthClass_dm", "PriceDiameterClass_cm")
sel_out <- if (nrow(all_selected) > 0L) {
  all_selected[, intersect(sel_cols, names(all_selected))]
} else {
  all_selected[, character(0)]
}
readr::write_csv(sel_out,
  file.path(out_dir, "pine_smoke_selected_logs.csv"))
wlog(sprintf("  Saved: pine_smoke_selected_logs.csv (%d rows)", nrow(sel_out)))

readr::write_csv(diag_df,
  file.path(out_dir, "pine_smoke_candidate_diagnostics.csv"))
wlog(sprintf("  Saved: pine_smoke_candidate_diagnostics.csv"))

readr::write_csv(unpriced_all,
  file.path(out_dir, "pine_smoke_unpriced_candidates.csv"))
wlog(sprintf("  Saved: pine_smoke_unpriced_candidates.csv (%d rows)",
             nrow(unpriced_all)))


# ---- optional plots ----

wlog("", "--- Diagnostic plots ---")

if (nrow(tree_summ) > 0L && any(!is.na(tree_summ$total_selected_value_nok))) {
  p_val <- ggplot2::ggplot(
    tree_summ[!is.na(tree_summ$dbh_cm), ],
    ggplot2::aes(x = dbh_cm, y = total_selected_value_nok %||% 0,
                 colour = optimizer_status)) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::labs(title = "Pine smoke test: selected value by DBH",
                  x = "DBH (cm)", y = "Total selected value (NOK)",
                  colour = "Status") +
    ggplot2::theme_bw()
  save_plot(p_val, "plot_pine_smoke_value_by_dbh.png")
}

if (nrow(prod_summ) > 0L) {
  p_vol <- ggplot2::ggplot(
    prod_summ,
    ggplot2::aes(x = ProductKey, y = total_volume_m3,
                 fill = ProductKey, label = sprintf("%.3f m3", total_volume_m3))) +
    ggplot2::geom_col() +
    ggplot2::geom_text(vjust = -0.3, size = 3) +
    ggplot2::labs(title = "Pine smoke test: selected volume by product",
                  x = "Product", y = "Total volume (m3)") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
  save_plot(p_vol, "plot_pine_smoke_product_volume.png")
}


# ---- summary ----

wlog(
  "",
  "=============================================================",
  "  PINE BUCKING SMOKE TEST SUMMARY",
  "=============================================================",
  sprintf("  STUMP_HEIGHT_CM              : %d cm",  STUMP_HEIGHT_CM),
  sprintf("  Pine trees available         : %d",     n_pine_avail),
  sprintf("  Pine trees selected          : %d",     n_selected),
  sprintf("  Profile failures             : %d",     n_failed_profile),
  sprintf("  Optimizer errors             : %d",     n_err),
  sprintf("  Trees with 0 logs selected   : %d",     n_zero_sel),
  "",
  "  Product summary:"
)

for (j in seq_len(nrow(prod_summ))) {
  wlog(sprintf("    %-24s %4d logs  %8.4f m3  %10.2f NOK",
               prod_summ$ProductKey[j],
               prod_summ$n_logs[j],
               prod_summ$total_volume_m3[j],
               prod_summ$total_value_nok[j]))
}

wlog(
  "",
  sprintf("  Total selected logs          : %d",
          nrow(all_selected)),
  sprintf("  Total value (NOK)            : %.2f",
          sum(tree_summ$total_selected_value_nok, na.rm = TRUE)),
  sprintf("  Total volume (m3)            : %.4f",
          sum(tree_summ$total_selected_volume_m3,  na.rm = TRUE)),
  sprintf("  Total overlapping pairs      : %d",
          sum(tree_summ$n_overlapping_pairs %||% 0, na.rm = TRUE))
)

if (n_dm31_total > 0L) {
  wlog(sprintf(
    "\n  REVIEW: %d unpriced 31 dm pine_sawlog candidate(s).",
    n_dm31_total),
    "  Known issue: pine_sawlog price matrix starts at 34 dm.")
}
if (nrow(miss_summ) > 0L) {
  other_miss <- miss_summ[miss_summ$ProductKey != "pine_sawlog" |
                            miss_summ$LengthClass_dm != "31", ]
  if (nrow(other_miss) > 0L) {
    wlog("  WARNING: Missing prices outside the known 31 dm issue:")
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
  sprintf("  Log: %s", log_file),
  "",
  "Script 09 complete."
)
