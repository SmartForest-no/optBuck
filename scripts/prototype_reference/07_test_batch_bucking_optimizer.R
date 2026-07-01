# ==============================================================================
# R/07_test_batch_bucking_optimizer.R
#
# Purpose : Batch test of the one-tree bucking optimizer (script 06 logic)
#           applied to a small set of trees spanning the DBH range.
#           This is not the full stratum run.
#
#           Candidate generation, valuation, and DP are run per-tree using
#           the same functions as script 06.  Results are collected into
#           per-tree and cross-tree summary tables.
#
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
#   inputs/ProductData_spruce_pine_default.csv
#   inputs/LengthClasses_spruce_pine_default.csv
#   inputs/price_matrices/PriceMatrix_long_default_2021.csv
#
# Outputs :
#   outputs/batch_optimizer_tests/batch_selected_logs.csv
#   outputs/batch_optimizer_tests/batch_tree_summary.csv
#   outputs/batch_optimizer_tests/batch_optimizer_summary.txt
#   outputs/batch_optimizer_tests/batch_diagnostics.csv
#   outputs/batch_optimizer_tests/plot_batch_value_by_dbh.png    (optional)
#   outputs/batch_optimizer_tests/plot_batch_n_logs_by_dbh.png   (optional)
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

# Profile step must match the interval used in stem profile generation (script 02/03).
PROFILE_STEP_CM <- 10L

# Number of test trees to select from each species group.
# Trees are sampled across the DBH range (min, 25th, 50th, 75th, max percentile).
N_TREES_PER_SPECIES <- 5L

root_dir   <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
inputs_dir <- file.path(root_dir, "inputs")
prof_dir   <- file.path(root_dir, "outputs", "stem_profiles")
out_dir    <- file.path(root_dir, "outputs", "batch_optimizer_tests")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

log_file <- file.path(out_dir, "batch_optimizer_summary.txt")
cat("", file = log_file)

wlog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = log_file, append = TRUE)
}

save_plot <- function(p, filename, width = 20, height = 12,
                      units = "cm", dpi = 150) {
  path <- file.path(out_dir, filename)
  ggplot2::ggsave(path, plot = p, width = width, height = height,
                  units = units, dpi = dpi)
  wlog(sprintf("  Plot saved: %s", basename(path)))
}

wlog(
  "=============================================================",
  "  Batch bucking optimizer test – log",
  sprintf("  Run                 : %s", Sys.time()),
  sprintf("  STUMP_HEIGHT_CM     : %d cm", STUMP_HEIGHT_CM),
  sprintf("  PROFILE_STEP_CM     : %d cm", PROFILE_STEP_CM),
  sprintf("  N_TREES_PER_SPECIES : %d", N_TREES_PER_SPECIES),
  "============================================================="
)


# ---- volume / eligibility / DP helpers (same logic as script 06) ----
# These functions mirror 06_test_one_tree_bucking_optimizer.R so that script
# 07 is self-contained.  If core logic changes in script 03 or 06, update here.

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

get_tree_profile_vectors <- function(profiles, tree_id) {
  tree_rows <- profiles |>
    dplyr::filter(treid == tree_id) |>
    dplyr::arrange(diameterPosition)
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

get_diameter_at_position <- function(diameterPosition, DiameterValue, pos) {
  idx <- which(diameterPosition == pos)
  if (length(idx) > 0) return(DiameterValue[idx[1]])
  stats::approx(x = diameterPosition, y = DiameterValue,
                xout = pos, rule = 2)$y
}

VolumeCalc_nfi <- function(diameterPosition, DiameterValue,
                            StartPos, StopPos) {
  pos_seq <- seq(
    ceiling(StartPos  / PROFILE_STEP_CM) * PROFILE_STEP_CM,
    floor(StopPos     / PROFILE_STEP_CM) * PROFILE_STEP_CM,
    by = PROFILE_STEP_CM
  )
  all_pos <- sort(unique(c(StartPos, pos_seq, StopPos)))
  all_pos <- all_pos[all_pos >= StartPos & all_pos <= StopPos]
  if (length(all_pos) < 2) return(NA_real_)
  d_mm <- vapply(all_pos, function(p)
    get_diameter_at_position(diameterPosition, DiameterValue, p),
    FUN.VALUE = numeric(1))
  v_total <- 0
  for (k in seq_len(length(all_pos) - 1L)) {
    radius_mm <- mean(d_mm[c(k, k + 1L)]) / 2
    len_cm    <- all_pos[k + 1L] - all_pos[k]
    v_total   <- v_total + pi * radius_mm^2 * len_cm / 1e8
  }
  v_total
}

PriceVolumeCalc_nfi <- function(diameterPosition, DiameterValue,
                                 StartPos, StopPos,
                                 ProductRow, LengthClassesVec) {
  raw_length  <- StopPos - StartPos
  vol_len_cat <- ProductRow$VolumeLengthCategory
  if (vol_len_cat == "Physical length cm") {
    eff_length  <- raw_length
    eff_stoppos <- StopPos
  } else if (vol_len_cat == "Length as defined in LengthClasses") {
    valid_lc <- LengthClassesVec[LengthClassesVec <= raw_length]
    if (length(valid_lc) == 0) {
      return(list(price_volume_m3    = NA_real_,
                  EffectiveLogLength = NA_real_,
                  EffectiveStopPos   = NA_real_))
    }
    eff_length  <- max(valid_lc)
    eff_stoppos <- StartPos + eff_length
  } else if (vol_len_cat == "Rounded downwards to nearest dm-module") {
    eff_length  <- floor(raw_length / 10) * 10
    eff_stoppos <- StartPos + eff_length
  } else {
    stop(sprintf("Unsupported VolumeLengthCategory: '%s'", vol_len_cat))
  }
  vol_dia_cat <- ProductRow$VolumeDiameterCategory
  if (vol_dia_cat == "All diameters (solid volume)") {
    v <- VolumeCalc_nfi(diameterPosition, DiameterValue, StartPos, eff_stoppos)
  } else if (vol_dia_cat == "Top") {
    top_pos <- eff_stoppos - ProductRow$DiameterTopPosition
    top_d   <- get_diameter_at_position(diameterPosition, DiameterValue, top_pos)
    if (ProductRow$VolumeDiameterAdjustment ==
        "Measured diameter rounded downwards to cm") {
      top_d <- floor(top_d / 10) * 10
    }
    if (is.na(top_d) || top_d <= 0) {
      return(list(price_volume_m3    = NA_real_,
                  EffectiveLogLength = eff_length,
                  EffectiveStopPos   = eff_stoppos))
    }
    v <- pi * (top_d / 2)^2 * eff_length / 1e8
  } else {
    stop(sprintf("Unsupported VolumeDiameterCategory: '%s'", vol_dia_cat))
  }
  list(price_volume_m3    = v,
       EffectiveLogLength = eff_length,
       EffectiveStopPos   = eff_stoppos)
}

check_log_against_product <- function(diameterPosition, DiameterValue,
                                       StartPos, StopPos,
                                       ProductRow, LengthClassesVec) {
  LogLength    <- StopPos - StartPos
  TopDiameter  <- get_diameter_at_position(
    diameterPosition, DiameterValue,
    StopPos - ProductRow$DiameterTopPosition)
  ButtDiameter <- get_diameter_at_position(
    diameterPosition, DiameterValue, StartPos)
  reasons <- character(0)
  if (is.na(LogLength) || LogLength < ProductRow$MinLength)
    reasons <- c(reasons, sprintf("LogLength %s < MinLength %d cm",
                                  format(LogLength), ProductRow$MinLength))
  if (!is.na(LogLength) && LogLength > ProductRow$MaxLength)
    reasons <- c(reasons, sprintf("LogLength %s > MaxLength %d cm",
                                  format(LogLength), ProductRow$MaxLength))
  if (is.na(TopDiameter) || TopDiameter < ProductRow$MinTopDiameter)
    reasons <- c(reasons, sprintf("TopDiameter %.1f mm < MinTopDiameter %.0f mm",
                                  TopDiameter %||% NaN, ProductRow$MinTopDiameter))
  if (!is.na(TopDiameter) && TopDiameter > ProductRow$MaxTopDiameter)
    reasons <- c(reasons, "TopDiameter > MaxTopDiameter")
  if (is.na(ButtDiameter) || ButtDiameter < ProductRow$MinButtDiameter)
    reasons <- c(reasons, sprintf("ButtDiameter %.1f mm < MinButtDiameter %.0f mm",
                                  ButtDiameter %||% NaN, ProductRow$MinButtDiameter))
  if (!is.na(ButtDiameter) && ButtDiameter > ProductRow$MaxButtDiameter)
    reasons <- c(reasons, "ButtDiameter > MaxButtDiameter")
  if (ProductRow$VolumeLengthCategory == "Length as defined in LengthClasses") {
    valid_lc <- LengthClassesVec[LengthClassesVec <= LogLength]
    if (length(valid_lc) == 0)
      reasons <- c(reasons, "No valid length class <= LogLength")
  }
  eligible <- length(reasons) == 0
  list(
    eligible     = eligible,
    LogLength    = LogLength,
    TopDiameter  = round(TopDiameter,  1),
    ButtDiameter = round(ButtDiameter, 1),
    reason       = if (eligible) "OK" else paste(reasons, collapse = "; ")
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

wlog(sprintf("  Profile rows   : %d (%d trees)",
             nrow(stem_profiles), dplyr::n_distinct(stem_profiles$treid)))

pd_csv <- file.path(inputs_dir, "ProductData_spruce_pine_default.csv")
if (!file.exists(pd_csv))
  stop("ProductData not found: ", pd_csv,
       "\nRun 03_define_products_and_volume.R first.")
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
  stop("PriceMatrix_long not found: ", pm_csv,
       "\nRun 04_import_or_define_price_matrices.R first.")
PriceMatrix_long <- readr::read_csv(pm_csv, show_col_types = FALSE)
PRICE_SCENARIO   <- unique(PriceMatrix_long$PriceScenario)[1]
wlog(sprintf("  PriceMatrix    : %d rows, scenario '%s'",
             nrow(PriceMatrix_long), PRICE_SCENARIO))

# Prepare price lookup tables once (reused for every tree).
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


# ---- select test trees ----

wlog("", "--- Selecting test trees ---")

tree_tab <- stem_profiles |>
  dplyr::distinct(treid, species_group, dbh_cm) |>
  dplyr::arrange(species_group, dbh_cm)

# For each species group, pick N_TREES_PER_SPECIES trees at evenly-spaced
# quantile positions across the DBH range.
select_quantile_trees <- function(tab, n) {
  if (nrow(tab) <= n) return(tab$treid)
  probs   <- seq(0, 1, length.out = n)
  q_dbh   <- quantile(tab$dbh_cm, probs = probs)
  sel_idx <- vapply(q_dbh, function(q) which.min(abs(tab$dbh_cm - q)),
                    FUN.VALUE = integer(1))
  tab$treid[unique(sel_idx)]
}

test_tree_ids <- character(0)
for (sg in unique(tree_tab$species_group)) {
  sg_tab <- tree_tab[tree_tab$species_group == sg, ]
  ids    <- select_quantile_trees(sg_tab, N_TREES_PER_SPECIES)
  test_tree_ids <- c(test_tree_ids, as.character(ids))
  wlog(sprintf("  %s: selected %d trees (DBH: %s cm)",
               sg, length(ids),
               paste(round(sg_tab$dbh_cm[match(ids, sg_tab$treid)], 1),
                     collapse = ", ")))
}
test_tree_ids <- unique(test_tree_ids)
wlog(sprintf("  Total test trees: %d", length(test_tree_ids)))


# ---- per-tree optimizer function ----

#' Run candidate generation, valuation, and DP for one tree.
#'
#' @param tree_id  Character tree identifier.
#' @param profiles Stem profile data frame (with diameterPosition, DiameterValue, DBH).
#' @param pd       ProductData.
#' @param lc       LengthClasses named list.
#' @param pm_mat   Sawlog price matrix (long format).
#' @param pm_fl    Flat prices (per ProductKey).
#' @param stump    Stump height in cm.
#' @param step     Profile step in cm.
#' @return Named list with per-tree results.
optimize_one_tree <- function(tree_id, profiles, pd, lc,
                               pm_mat, pm_fl,
                               stump, step) {

  pv <- get_tree_profile_vectors(profiles, tree_id)
  sg <- pv$species_group

  sp_products <- pd[pd$species_group == sg, ]

  min_log_len <- min(vapply(sp_products$ProductKey, function(pk) {
    if (pk %in% names(lc)) min(lc[[pk]]) else Inf
  }, FUN.VALUE = numeric(1)))

  start_positions <- seq(
    as.integer(stump),
    as.integer(pv$height_cm - min_log_len),
    by = as.integer(step)
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

  # Generate candidates.
  cands_list <- vector("list",
    length(start_positions) * nrow(sp_products) * 65L)
  cand_idx <- 0L

  for (start_pos in start_positions) {
    for (j in seq_len(nrow(sp_products))) {
      prow   <- sp_products[j, ]
      pk     <- prow$ProductKey
      lc_vec <- lc[[pk]]
      for (log_len in lc_vec) {
        stop_pos <- as.integer(start_pos) + as.integer(log_len)
        if (stop_pos > pv$height_cm) next
        elig <- check_log_against_product(
          pv$diameterPosition, pv$DiameterValue,
          start_pos, stop_pos, prow, lc_vec)
        pvc <- PriceVolumeCalc_nfi(
          pv$diameterPosition, pv$DiameterValue,
          start_pos, stop_pos, prow, lc_vec)
        cand_idx <- cand_idx + 1L
        cands_list[[cand_idx]] <- data.frame(
          treid                  = as.character(tree_id),
          plot_id                = pv$plot_id,
          species_group          = sg,
          ProductKey             = pk,
          ProductType            = prow$ProductType,
          StartPos               = as.integer(start_pos),
          StopPos                = as.integer(stop_pos),
          LogLength              = as.integer(elig$LogLength),
          TopDiameter            = elig$TopDiameter,
          ButtDiameter           = elig$ButtDiameter,
          eligible               = elig$eligible,
          reason                 = elig$reason,
          VolumeDiameterCategory = prow$VolumeDiameterCategory,
          VolumeLengthCategory   = prow$VolumeLengthCategory,
          price_volume_m3        = round(pvc$price_volume_m3, 5),
          LengthClass_dm         = as.integer(log_len / 10L),
          PriceDiameterClass_cm  = as.integer(floor(elig$TopDiameter / 10)),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  generated <- do.call(rbind, cands_list[seq_len(cand_idx)])
  n_gen     <- nrow(generated)

  # Keep eligible with volume.
  elig_c <- generated[generated$eligible & !is.na(generated$price_volume_m3), ]

  # Price join.
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

  priced <- rbind(sawlog_c, nonsawlog_c)
  priced$value_nok <- priced$price_volume_m3 * priced$PriceNOKperM3

  n_unmatched   <- sum(is.na(priced$PriceNOKperM3))
  n_dm31        <- sum(!is.na(priced$LengthClass_dm) &
                         priced$LengthClass_dm == 31 &
                         is.na(priced$PriceNOKperM3))

  # DP input: priced and eligible.
  dp_in <- priced[!is.na(priced$PriceNOKperM3) &
                    !is.na(priced$value_nok) &
                    !is.na(priced$price_volume_m3), ]
  dp_in$StartPos <- as.integer(round(dp_in$StartPos))
  dp_in$StopPos  <- as.integer(round(dp_in$StopPos))
  n_priced <- nrow(dp_in)

  if (n_priced == 0) {
    return(list(tree_id = tree_id, species_group = sg,
                dbh_mm = pv$DBH, height_cm = pv$height_cm,
                n_generated = n_gen, n_priced = 0L,
                n_selected = 0L, total_value_nok = 0,
                total_volume_m3 = 0, total_length_cm = 0L,
                n_overlapping = 0L, n_unmatched_price = n_unmatched,
                n_dm31_unpriced = n_dm31, dp_best_value = 0,
                selected_logs = NULL, error = "No priced candidates"))
  }

  dp   <- run_dp_optimizer(dp_in)
  sel  <- if (length(dp$selected_rows) > 0) {
    s <- dp_in[dp$selected_rows, ]
    s[order(s$StartPos), ]
  } else {
    dp_in[integer(0), ]
  }

  # Overlap check.
  n_overlap <- 0L
  if (nrow(sel) > 1) {
    for (k in seq_len(nrow(sel) - 1L)) {
      if (sel$StopPos[k] > sel$StartPos[k + 1L]) n_overlap <- n_overlap + 1L
    }
  }

  list(
    tree_id          = tree_id,
    species_group    = sg,
    dbh_mm           = pv$DBH,
    height_cm        = pv$height_cm,
    n_generated      = n_gen,
    n_priced         = n_priced,
    n_selected       = nrow(sel),
    total_value_nok  = round(sum(sel$value_nok,      na.rm = TRUE), 2),
    total_volume_m3  = round(sum(sel$price_volume_m3, na.rm = TRUE), 5),
    total_length_cm  = sum(sel$LogLength,            na.rm = TRUE),
    n_overlapping    = n_overlap,
    n_unmatched_price = n_unmatched,
    n_dm31_unpriced  = n_dm31,
    dp_best_value    = round(dp$best_value, 2),
    selected_logs    = sel,
    error            = NA_character_
  )
}


# ---- run batch ----

wlog(sprintf("\n--- Running optimizer for %d trees ---", length(test_tree_ids)))

results_list <- vector("list", length(test_tree_ids))

for (i in seq_along(test_tree_ids)) {
  tid <- test_tree_ids[i]
  wlog(sprintf("  [%d/%d] treid = %s ...",
               i, length(test_tree_ids), tid))

  res <- tryCatch(
    optimize_one_tree(
      tree_id   = tid,
      profiles  = stem_profiles,
      pd        = ProductData,
      lc        = LengthClasses,
      pm_mat    = pm_matrix,
      pm_fl     = pm_flat,
      stump     = STUMP_HEIGHT_CM,
      step      = PROFILE_STEP_CM
    ),
    error = function(e) {
      wlog(sprintf("    ERROR: %s", conditionMessage(e)))
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
  results_list[[i]] <- res

  if (is.na(res$error)) {
    wlog(sprintf("    Species: %-6s  DBH: %5.0f mm  Height: %5.1f m",
                 res$species_group, res$dbh_mm, res$height_cm / 100))
    wlog(sprintf("    Generated: %d  Priced: %d  Selected: %d  Value: %.2f NOK",
                 res$n_generated, res$n_priced, res$n_selected,
                 res$total_value_nok))
    if (res$n_dm31_unpriced > 0) {
      wlog(sprintf("    REVIEW: %d unpriced 31 dm sawlog candidate(s) (known issue)",
                   res$n_dm31_unpriced))
    }
  }
}


# ---- assemble outputs ----

wlog("", "--- Assembling results ---")

# Per-tree summary table.
tree_summary <- do.call(rbind, lapply(results_list, function(r) {
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

# All selected logs (stacked from all trees).
sel_list  <- lapply(results_list, function(r) r$selected_logs)
sel_list  <- sel_list[!vapply(sel_list, is.null, logical(1))]
sel_list  <- sel_list[vapply(sel_list, nrow, integer(1)) > 0]
batch_selected <- if (length(sel_list) > 0) {
  do.call(rbind, sel_list)
} else {
  data.frame()
}

# Diagnostics.
n_trees_ok    <- sum(is.na(tree_summary$error))
n_trees_error <- sum(!is.na(tree_summary$error))
n_dm31_total  <- sum(tree_summary$n_dm31_unpriced, na.rm = TRUE)

diag_df <- data.frame(
  item  = c("stump_height_cm", "profile_step_cm", "price_scenario",
            "n_test_trees", "n_trees_completed", "n_trees_error",
            "n_selected_logs_total", "total_value_nok_all_trees",
            "total_volume_m3_all_trees", "n_unmatched_31dm_total"),
  value = c(as.character(STUMP_HEIGHT_CM), as.character(PROFILE_STEP_CM),
            PRICE_SCENARIO,
            length(test_tree_ids), n_trees_ok, n_trees_error,
            nrow(batch_selected),
            round(sum(tree_summary$total_value_nok, na.rm = TRUE), 2),
            round(sum(tree_summary$total_volume_m3, na.rm = TRUE), 5),
            n_dm31_total),
  stringsAsFactors = FALSE
)


# ---- save outputs ----

wlog("", "--- Saving outputs ---")

sum_csv  <- file.path(out_dir, "batch_tree_summary.csv")
sel_csv  <- file.path(out_dir, "batch_selected_logs.csv")
diag_csv <- file.path(out_dir, "batch_diagnostics.csv")

readr::write_csv(tree_summary,   sum_csv)
readr::write_csv(batch_selected, sel_csv)
readr::write_csv(diag_df,        diag_csv)

wlog(sprintf("  Saved: %s  (%d rows)", basename(sum_csv),  nrow(tree_summary)))
wlog(sprintf("  Saved: %s  (%d rows)", basename(sel_csv),  nrow(batch_selected)))
wlog(sprintf("  Saved: %s",             basename(diag_csv)))


# ---- diagnostic plots ----

wlog("", "--- Diagnostic plots ---")

ok_tab <- tree_summary[is.na(tree_summary$error), ]

if (nrow(ok_tab) > 0) {
  ok_tab$dbh_cm <- ok_tab$dbh_mm / 10

  # 1. Total value vs DBH.
  p_val <- ok_tab |>
    ggplot2::ggplot(ggplot2::aes(
      x      = dbh_cm,
      y      = total_value_nok,
      colour = species_group,
      label  = treid
    )) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(nudge_y = max(ok_tab$total_value_nok, na.rm = TRUE) * 0.02,
                       size = 2.5, hjust = 0) +
    ggplot2::labs(
      title  = sprintf("Total DP value vs DBH – %d batch trees (STUMP_HEIGHT = %d cm)",
                       nrow(ok_tab), STUMP_HEIGHT_CM),
      x      = "DBH (cm)",
      y      = "Total value_nok (NOK)",
      colour = "Species group"
    ) +
    ggplot2::theme_minimal()

  save_plot(p_val, "plot_batch_value_by_dbh.png")

  # 2. Number of selected logs vs DBH.
  p_nlogs <- ok_tab |>
    ggplot2::ggplot(ggplot2::aes(
      x      = dbh_cm,
      y      = n_selected,
      colour = species_group,
      label  = treid
    )) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(nudge_y = 0.15, size = 2.5, hjust = 0) +
    ggplot2::labs(
      title  = "Number of selected logs vs DBH – batch test",
      x      = "DBH (cm)",
      y      = "Number of selected logs",
      colour = "Species group"
    ) +
    ggplot2::scale_y_continuous(breaks = seq(0, max(ok_tab$n_selected + 1, na.rm = TRUE))) +
    ggplot2::theme_minimal()

  save_plot(p_nlogs, "plot_batch_n_logs_by_dbh.png")
}


# ---- final summary ----

wlog(
  "",
  "=============================================================",
  "  BATCH OPTIMIZER TEST SUMMARY",
  "=============================================================",
  sprintf("  STUMP_HEIGHT_CM          : %d cm", STUMP_HEIGHT_CM),
  sprintf("  Trees attempted          : %d", length(test_tree_ids)),
  sprintf("  Trees completed (no err) : %d", n_trees_ok),
  sprintf("  Trees with errors        : %d", n_trees_error),
  ""
)

if (n_trees_ok > 0) {
  ok_s <- tree_summary[is.na(tree_summary$error), ]
  wlog(
    "  Per-tree results:",
    capture.output(print(
      ok_s[, c("treid", "species_group", "dbh_mm", "n_generated",
                "n_priced", "n_selected", "total_value_nok",
                "total_volume_m3")],
      row.names = FALSE))
  )
  wlog(
    "",
    sprintf("  Total selected logs      : %d", nrow(batch_selected)),
    sprintf("  Total value (all trees)  : %.2f NOK",
            sum(ok_s$total_value_nok, na.rm = TRUE)),
    sprintf("  Total volume (all trees) : %.5f m3",
            sum(ok_s$total_volume_m3,  na.rm = TRUE))
  )
}

if (n_dm31_total > 0) {
  wlog(
    "",
    sprintf("  REVIEW: %d total unpriced 31 dm sawlog candidate(s) across all trees.",
            n_dm31_total),
    "  Known issue: sawlog price matrix starts at 34 dm."
  )
}

if (n_trees_error > 0) {
  err_s <- tree_summary[!is.na(tree_summary$error), ]
  wlog("", "  Trees with errors:")
  for (k in seq_len(nrow(err_s))) {
    wlog(sprintf("    treid %s: %s", err_s$treid[k], err_s$error[k]))
  }
}

wlog(
  "",
  sprintf("  Log: %s", log_file),
  ""
)

message(sprintf(
  "Script 07 complete. %d/%d trees, %d logs selected, total value = %.2f NOK.",
  n_trees_ok, length(test_tree_ids), nrow(batch_selected),
  sum(tree_summary$total_value_nok, na.rm = TRUE)
))
