# ==============================================================================
# R/06_test_one_tree_bucking_optimizer.R
#
# Purpose : Generate all optimizer candidate logs for one tree, value them,
#           and run a dynamic-programming bucking optimizer.
#
#           Candidate logs are generated directly from stem profiles,
#           ProductData, and LengthClasses (scripts 02–03).  Valuation uses
#           PriceMatrix_long from script 04.  This script does NOT depend on
#           pre-saved candidate-log tables from scripts 03 or 05.
#
#           This is a one-tree test only.  Not a production optimizer.
#           No bark correction.  No HPR / StanForD import.
#           ProductData, LengthClasses, and PriceMatrix_long are not modified.
#
# Positions are in cm above terrain.  The first bucking position is the assumed
# stump cut height.  Default 10 cm; later sensitivity tests can use
# 5, 10, 20, and 30 cm.
#
# Known issue carried forward:
#   Sawlog LengthClass_dm == 31 is absent from the price matrix (matrix starts
#   at 34 dm).  Those candidates have PriceNOKperM3 = NA and are excluded from
#   the DP input.  Reported as REVIEW, not silently fixed.
#
# Inputs  :
#   outputs/stem_profiles/stem_profiles_example_500trees.parquet
#   inputs/ProductData_spruce_pine_default.csv
#   inputs/LengthClasses_spruce_pine_default.csv
#   inputs/price_matrices/PriceMatrix_long_default_2021.csv
#
# Outputs :
#   outputs/optimizer_tests/one_tree_optimizer_generated_candidates.csv
#   outputs/optimizer_tests/one_tree_optimizer_priced_candidates.csv
#   outputs/optimizer_tests/one_tree_optimizer_selected_logs.csv
#   outputs/optimizer_tests/one_tree_optimizer_all_states.csv
#   outputs/optimizer_tests/one_tree_optimizer_summary.txt
#   outputs/optimizer_tests/one_tree_optimizer_diagnostics.csv
#   outputs/optimizer_tests/plot_one_tree_selected_logs.png  (optional)
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

root_dir   <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
inputs_dir <- file.path(root_dir, "inputs")
prof_dir   <- file.path(root_dir, "outputs", "stem_profiles")
opt_dir    <- file.path(root_dir, "outputs", "optimizer_tests")

dir.create(opt_dir, showWarnings = FALSE, recursive = TRUE)

log_file <- file.path(opt_dir, "one_tree_optimizer_summary.txt")
cat("", file = log_file)

wlog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = log_file, append = TRUE)
}

save_plot <- function(p, filename, width = 22, height = 12,
                      units = "cm", dpi = 150) {
  path <- file.path(opt_dir, filename)
  ggplot2::ggsave(path, plot = p, width = width, height = height,
                  units = units, dpi = dpi)
  wlog(sprintf("  Plot saved: %s", basename(path)))
}

wlog(
  "=============================================================",
  "  One-tree bucking optimizer – log",
  sprintf("  Run            : %s", Sys.time()),
  sprintf("  STUMP_HEIGHT_CM: %d cm", STUMP_HEIGHT_CM),
  sprintf("  PROFILE_STEP_CM: %d cm", PROFILE_STEP_CM),
  "============================================================="
)


# ---- volume / eligibility helpers (mirrors of script 03) ----
# These functions are duplicated from 03_define_products_and_volume.R so that
# script 06 is self-contained. If core logic changes in script 03, update here.

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

get_tree_profile_vectors <- function(profiles, tree_id) {
  tree_rows <- profiles |>
    dplyr::filter(treid == tree_id) |>
    dplyr::arrange(diameterPosition)
  if (nrow(tree_rows) == 0)
    stop(sprintf("No profile rows found for treid = '%s'.", tree_id))
  list(
    diameterPosition = tree_rows$diameterPosition,
    DiameterValue    = tree_rows$DiameterValue,
    DBH              = tree_rows$DBH[1],
    height_m         = tree_rows$height_m[1],
    height_cm        = round(tree_rows$height_m[1] * 100),
    species_group    = tree_rows$species_group[1],
    SpeciesGroupKey  = tree_rows$species_group[1],
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
                            StartPos, StopPos,
                            DiameterUnderBark = FALSE) {
  if (DiameterUnderBark) stop("DiameterUnderBark = TRUE not supported.")
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
  if (isTRUE(ProductRow$DiameterUnderBark))
    stop("DiameterUnderBark = TRUE not supported.")
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
    v <- VolumeCalc_nfi(diameterPosition, DiameterValue,
                        StartPos, eff_stoppos, DiameterUnderBark = FALSE)
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
    radius_mm <- top_d / 2
    v <- pi * radius_mm^2 * eff_length / 1e8
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
  tibble::tibble(
    eligible     = eligible,
    LogLength    = LogLength,
    TopDiameter  = round(TopDiameter, 1),
    ButtDiameter = round(ButtDiameter, 1),
    reason       = if (eligible) "OK" else paste(reasons, collapse = "; ")
  )
}


# ---- read inputs ----

wlog("", "--- Reading inputs ---")

# Stem profiles (prefer parquet, fall back to rds).
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

# Build optBuck-style helper columns and deduplicate.
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

# ProductData.
pd_csv <- file.path(inputs_dir, "ProductData_spruce_pine_default.csv")
if (!file.exists(pd_csv))
  stop("ProductData CSV not found: ", pd_csv,
       "\nRun 03_define_products_and_volume.R first.")
ProductData <- readr::read_csv(pd_csv, show_col_types = FALSE)
wlog(sprintf("  ProductData    : %d products from %s",
             nrow(ProductData), basename(pd_csv)))

# LengthClasses – reconstruct named list from long CSV.
lc_csv <- file.path(inputs_dir, "LengthClasses_spruce_pine_default.csv")
if (!file.exists(lc_csv))
  stop("LengthClasses CSV not found: ", lc_csv,
       "\nRun 03_define_products_and_volume.R first.")
lc_long <- readr::read_csv(lc_csv, show_col_types = FALSE)
LengthClasses <- split(as.integer(lc_long$LogLength), lc_long$ProductKey)
wlog(sprintf("  LengthClasses  : %d products from %s",
             length(LengthClasses), basename(lc_csv)))

# PriceMatrix_long.
pm_csv <- file.path(inputs_dir, "price_matrices", "PriceMatrix_long_default_2021.csv")
if (!file.exists(pm_csv))
  stop("PriceMatrix_long CSV not found: ", pm_csv,
       "\nRun 04_import_or_define_price_matrices.R first.")
PriceMatrix_long <- readr::read_csv(pm_csv, show_col_types = FALSE)
wlog(sprintf("  PriceMatrix    : %d rows from %s",
             nrow(PriceMatrix_long), basename(pm_csv)))


# ---- select test tree ----

wlog("", "--- Selecting test tree ---")

# Prefer treid 242251 (same tree used in prior tests) if available.
PREFERRED_TREE_ID <- "242251"
all_tree_ids <- as.character(unique(stem_profiles$treid))

if (PREFERRED_TREE_ID %in% all_tree_ids) {
  selected_tree_id <- PREFERRED_TREE_ID
  wlog(sprintf("  Using preferred tree: treid = %s", selected_tree_id))
} else {
  spruce_tab <- stem_profiles |>
    dplyr::filter(species_group == "spruce") |>
    dplyr::distinct(treid, dbh_cm) |>
    dplyr::arrange(dplyr::desc(dbh_cm))
  if (nrow(spruce_tab) == 0)
    stop("Preferred tree not found and no spruce trees available.")
  selected_tree_id <- as.character(spruce_tab$treid[1])
  wlog(sprintf("  Tree %s not found; using largest spruce: treid = %s",
               PREFERRED_TREE_ID, selected_tree_id))
}

pv <- get_tree_profile_vectors(stem_profiles, selected_tree_id)
sg <- pv$species_group

wlog(sprintf("  Species group  : %s",    sg))
wlog(sprintf("  DBH            : %.1f mm", pv$DBH))
wlog(sprintf("  Height         : %.1f m (%d cm)", pv$height_m, pv$height_cm))
wlog(sprintf("  Profile points : %d", length(pv$diameterPosition)))


# ---- generate all candidate logs ----

wlog("", "--- Generating candidate logs ---")
wlog(sprintf("  STUMP_HEIGHT_CM : %d cm (first StartPos)", STUMP_HEIGHT_CM))
wlog(sprintf("  PROFILE_STEP_CM : %d cm (grid step)", PROFILE_STEP_CM))

sp_products <- ProductData |> dplyr::filter(species_group == sg)

# Minimum log length across all species products (determines how close to the
# top the last StartPos can be).
min_log_len <- min(vapply(sp_products$ProductKey, function(pk) {
  if (pk %in% names(LengthClasses)) min(LengthClasses[[pk]]) else Inf
}, FUN.VALUE = numeric(1)))

# All StartPos values: 10 cm grid from stump to (tree_height - min_log_len).
start_positions <- seq(
  as.integer(STUMP_HEIGHT_CM),
  as.integer(pv$height_cm - min_log_len),
  by = as.integer(PROFILE_STEP_CM)
)

wlog(sprintf("  Start positions : %d (from %d to %d cm)",
             length(start_positions),
             min(start_positions), max(start_positions)))

# Pre-allocate list; fill sequentially.
cands_list <- vector("list",
  length(start_positions) * nrow(sp_products) * 65L)
cand_idx <- 0L

for (start_pos in start_positions) {
  for (j in seq_len(nrow(sp_products))) {
    prow   <- sp_products[j, ]
    pk     <- prow$ProductKey
    lc_vec <- LengthClasses[[pk]]

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
      cands_list[[cand_idx]] <- tibble::tibble(
        treid                  = as.character(selected_tree_id),
        plot_id                = pv$plot_id,
        species_group          = sg,
        ProductKey             = pk,
        ProductType            = prow$ProductType,
        StartPos               = as.integer(start_pos),
        StopPos                = as.integer(stop_pos),
        LogLength              = elig$LogLength,
        TopDiameter            = elig$TopDiameter,
        ButtDiameter           = elig$ButtDiameter,
        eligible               = elig$eligible,
        reason                 = elig$reason,
        VolumeDiameterCategory = prow$VolumeDiameterCategory,
        VolumeLengthCategory   = prow$VolumeLengthCategory,
        price_volume_m3        = round(pvc$price_volume_m3, 5),
        # Join keys for price matrix
        LengthClass_dm         = as.integer(log_len / 10L),
        PriceDiameterClass_cm  = as.integer(floor(elig$TopDiameter / 10))
      )
    }
  }
}

generated_cands <- dplyr::bind_rows(cands_list[seq_len(cand_idx)])

wlog(sprintf("  Total generated            : %d", nrow(generated_cands)))
wlog(sprintf("  Eligible                   : %d", sum(generated_cands$eligible)))
wlog(sprintf("  Unique StartPos values     : %d",
             dplyr::n_distinct(generated_cands$StartPos)))
wlog("  Generated by ProductKey:")
wlog(capture.output(print(
  as.data.frame(dplyr::count(generated_cands, ProductKey, eligible, name = "n")),
  row.names = FALSE)))


# ---- value eligible candidates ----

wlog("", "--- Valuing eligible candidates ---")

elig_cands <- generated_cands[
  generated_cands$eligible & !is.na(generated_cands$price_volume_m3), ]
wlog(sprintf("  Eligible candidates with volume: %d", nrow(elig_cands)))

# Detect price scenario (use first one found).
PRICE_SCENARIO <- unique(PriceMatrix_long$PriceScenario)[1]
wlog(sprintf("  Price scenario: %s", PRICE_SCENARIO))

# Sawlog price matrix (join on ProductKey + LengthClass_dm + PriceDiameterClass_cm).
pm_matrix <- PriceMatrix_long |>
  dplyr::filter(PriceScenario == PRICE_SCENARIO, PriceType == "matrix") |>
  dplyr::select(ProductKey, LengthClass_dm, PriceDiameterClass_cm,
                PriceNOKperM3) |>
  dplyr::mutate(LengthClass_dm        = as.integer(LengthClass_dm),
                PriceDiameterClass_cm = as.integer(PriceDiameterClass_cm))

# Flat price lookup (join on ProductKey only).
pm_flat <- PriceMatrix_long |>
  dplyr::filter(PriceScenario == PRICE_SCENARIO, PriceType == "flat") |>
  dplyr::select(ProductKey, PriceNOKperM3_flat = PriceNOKperM3)

sawlog_pks    <- unique(ProductData$ProductKey[ProductData$ProductType == "sawlog"])
nonsawlog_pks <- unique(ProductData$ProductKey[ProductData$ProductType != "sawlog"])

sawlog_cands <- elig_cands |>
  dplyr::filter(ProductKey %in% sawlog_pks) |>
  dplyr::left_join(pm_matrix,
                   by = c("ProductKey", "LengthClass_dm", "PriceDiameterClass_cm"))

nonsawlog_cands <- elig_cands |>
  dplyr::filter(ProductKey %in% nonsawlog_pks) |>
  dplyr::left_join(pm_flat, by = "ProductKey") |>
  dplyr::rename(PriceNOKperM3 = PriceNOKperM3_flat)

priced_cands <- dplyr::bind_rows(sawlog_cands, nonsawlog_cands)
priced_cands$value_nok <- priced_cands$price_volume_m3 * priced_cands$PriceNOKperM3

n_unmatched <- sum(is.na(priced_cands$PriceNOKperM3))
wlog(sprintf("  Candidates with price      : %d", nrow(priced_cands) - n_unmatched))
wlog(sprintf("  Candidates without price   : %d", n_unmatched))

if (n_unmatched > 0) {
  unmatched_rows <- priced_cands[is.na(priced_cands$PriceNOKperM3), ]
  unmatched_summary <- as.data.frame(
    table(ProductKey    = unmatched_rows$ProductKey,
          LengthClass_dm = as.character(unmatched_rows$LengthClass_dm)),
    stringsAsFactors = FALSE)
  unmatched_summary <- unmatched_summary[unmatched_summary$Freq > 0, ]
  wlog("  Unmatched breakdown (ProductKey / LengthClass_dm / count):")
  wlog(capture.output(print(unmatched_summary, row.names = FALSE)))

  # Specifically flag LengthClass_dm == 31 (known issue).
  n_dm31 <- sum(!is.na(unmatched_rows$LengthClass_dm) &
                  unmatched_rows$LengthClass_dm == 31)
  if (n_dm31 > 0) {
    wlog(sprintf(
      "  REVIEW: %d candidate(s) with LengthClass_dm == 31 have no price.",
      n_dm31))
    wlog("  Known issue: sawlog price matrix starts at 34 dm.")
    wlog("  These candidates are excluded from DP input (PriceNOKperM3 = NA).")
  }
}


# ---- filter to DP input ----

wlog("", "--- Filtering to DP input ---")

dp_cands <- priced_cands[
  !is.na(priced_cands$PriceNOKperM3) &
    !is.na(priced_cands$value_nok) &
    !is.na(priced_cands$price_volume_m3), ]

dp_cands$StartPos <- as.integer(round(dp_cands$StartPos))
dp_cands$StopPos  <- as.integer(round(dp_cands$StopPos))

n_excl_price <- nrow(priced_cands) - nrow(dp_cands)
n_unique_start_gen <- dplyr::n_distinct(generated_cands$StartPos)
n_unique_start_dp  <- dplyr::n_distinct(dp_cands$StartPos)

wlog(sprintf("  Eligible priced candidates    : %d", nrow(priced_cands)))
wlog(sprintf("  Excluded (NA price / volume)  : %d", n_excl_price))
wlog(sprintf("  DP input candidates           : %d", nrow(dp_cands)))
wlog(sprintf("  Unique StartPos (DP input)    : %d", n_unique_start_dp))

if (nrow(dp_cands) == 0)
  stop("No priced candidates available for DP optimizer.")

wlog("  DP candidates by ProductKey:")
wlog(capture.output(print(
  as.data.frame(table(ProductKey = dp_cands$ProductKey),
                stringsAsFactors = FALSE),
  row.names = FALSE)))
wlog(sprintf("  StartPos range : %d – %d cm",
             min(dp_cands$StartPos), max(dp_cands$StartPos)))
wlog(sprintf("  StopPos range  : %d – %d cm",
             min(dp_cands$StopPos), max(dp_cands$StopPos)))
wlog(sprintf("  value_nok range: %.2f – %.2f NOK",
             min(dp_cands$value_nok), max(dp_cands$value_nok)))


# ---- dynamic programming optimizer ----

wlog("", "--- Dynamic programming optimizer ---")

#' Position-based DP bucking optimizer.
#'
#' Maximises total value_nok over non-overlapping candidate logs.
#' State space = unique positions from all StartPos and StopPos values.
#'
#' @param cands  Data frame with integer StartPos, StopPos, value_nok.
#' @return List: dp_val, dp_log, all_positions, selected_rows.
run_dp_optimizer <- function(cands) {
  all_pos  <- sort(unique(c(cands$StartPos, cands$StopPos)))
  sentinel <- max(all_pos) + 1L
  all_pos  <- c(all_pos, sentinel)
  n_pos    <- length(all_pos)

  dp_val <- numeric(n_pos)
  dp_log <- integer(n_pos)

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

  # Traceback.
  selected_rows <- integer(0)
  i <- 1L
  while (i < n_pos) {
    if (dp_log[i] > 0L) {
      j             <- dp_log[i]
      selected_rows <- c(selected_rows, j)
      stop_i        <- match(cands$StopPos[j], all_pos)
      i             <- if (!is.na(stop_i)) stop_i else n_pos
    } else {
      i <- i + 1L
    }
  }

  list(dp_val        = dp_val,
       dp_log        = dp_log,
       all_positions = all_pos,
       selected_rows = selected_rows)
}

dp_result    <- run_dp_optimizer(dp_cands)
selected_idx <- dp_result$selected_rows

wlog(sprintf("  DP positions in state space: %d", length(dp_result$all_positions)))
wlog(sprintf("  Best total value (dp_val[1]): %.2f NOK", dp_result$dp_val[1]))
wlog(sprintf("  Selected logs               : %d", length(selected_idx)))


# ---- selected logs ----

wlog("", "--- Selected logs ---")

if (length(selected_idx) == 0) {
  wlog("  WARNING: No logs selected.")
  selected_logs <- dp_cands[integer(0), ]
} else {
  selected_logs <- dp_cands[selected_idx, ]
  selected_logs <- selected_logs[order(selected_logs$StartPos), ]
}

if (nrow(selected_logs) > 0) {
  wlog(capture.output(print(
    selected_logs[, c("ProductKey", "StartPos", "StopPos",
                      "LogLength", "price_volume_m3",
                      "PriceNOKperM3", "value_nok")],
    row.names = FALSE)))
}


# ---- build all-states table ----

dp_states <- data.frame(
  position      = dp_result$all_positions,
  dp_value      = dp_result$dp_val,
  log_row_index = dp_result$dp_log,
  stringsAsFactors = FALSE)
dp_states$log_ProductKey <- NA_character_
valid_log_idx <- dp_states$log_row_index > 0L &
  dp_states$log_row_index <= nrow(dp_cands) &
  dp_states$position < max(dp_result$all_positions)
dp_states$log_ProductKey[valid_log_idx] <-
  dp_cands$ProductKey[dp_states$log_row_index[valid_log_idx]]


# ---- diagnostics ----

wlog("", "--- Diagnostics ---")

total_val <- sum(selected_logs$value_nok,      na.rm = TRUE)
total_vol <- sum(selected_logs$price_volume_m3, na.rm = TRUE)
total_len <- sum(selected_logs$LogLength,       na.rm = TRUE)

n_overlap <- 0L
if (nrow(selected_logs) > 1) {
  for (k in seq_len(nrow(selected_logs) - 1L)) {
    if (selected_logs$StopPos[k] > selected_logs$StartPos[k + 1L])
      n_overlap <- n_overlap + 1L
  }
}
n_miss_price <- sum(is.na(selected_logs$PriceNOKperM3))
n_miss_val   <- sum(is.na(selected_logs$value_nok))

wlog(sprintf("  Selected logs              : %d", nrow(selected_logs)))
wlog(sprintf("  Total selected volume (m3) : %.4f", total_vol))
wlog(sprintf("  Total selected value (NOK) : %.2f", total_val))
wlog(sprintf("  Total selected length (cm) : %d",   total_len))
wlog(sprintf("  Overlapping log pairs      : %d",   n_overlap))
wlog(sprintf("  Missing price rows         : %d",   n_miss_price))
wlog(sprintf("  Missing value rows         : %d",   n_miss_val))

if (nrow(selected_logs) > 0) {
  wlog("", "  Value by ProductKey:")
  val_by_pk <- do.call(rbind, lapply(unique(selected_logs$ProductKey), function(pk) {
    r <- selected_logs[selected_logs$ProductKey == pk, ]
    data.frame(ProductKey = pk,
               n_logs     = nrow(r),
               volume_m3  = round(sum(r$price_volume_m3, na.rm = TRUE), 5),
               value_nok  = round(sum(r$value_nok,       na.rm = TRUE), 2),
               stringsAsFactors = FALSE)
  }))
  wlog(capture.output(print(val_by_pk, row.names = FALSE)))
}

if (nrow(dp_cands) > 0) {
  stem_span <- max(dp_cands$StopPos) - min(dp_cands$StartPos)
  pct_cov   <- if (stem_span > 0) 100 * total_len / stem_span else 0
  wlog(sprintf("  Candidate span (min start to max stop): %d – %d cm (%d cm total)",
               min(dp_cands$StartPos), max(dp_cands$StopPos), stem_span))
  wlog(sprintf("  Selected length covers %.0f%% of candidate span", pct_cov))
}

n_dm31_unpriced <- sum(!is.na(priced_cands$LengthClass_dm) &
                         priced_cands$LengthClass_dm == 31 &
                         is.na(priced_cands$PriceNOKperM3))

diag_df <- data.frame(
  item = c("tree_id", "species_group", "stump_height_cm",
           "n_generated_candidates", "n_unique_startpos_generated",
           "n_eligible_with_volume", "n_priced_dp_input",
           "n_unique_startpos_dp", "n_unmatched_price",
           "n_unmatched_31dm_sawlog",
           "n_selected_logs", "total_volume_m3", "total_value_nok",
           "total_length_cm", "n_overlapping_pairs",
           "n_missing_price_selected", "n_missing_value_selected",
           "dp_best_value", "n_dp_positions"),
  value = c(as.character(selected_tree_id), sg,
            as.character(STUMP_HEIGHT_CM),
            nrow(generated_cands), n_unique_start_gen,
            nrow(elig_cands), nrow(dp_cands),
            n_unique_start_dp, n_unmatched,
            n_dm31_unpriced,
            nrow(selected_logs),
            round(total_vol, 5),
            round(total_val, 2),
            total_len, n_overlap,
            n_miss_price, n_miss_val,
            round(dp_result$dp_val[1], 2),
            length(dp_result$all_positions)),
  stringsAsFactors = FALSE)


# ---- save outputs ----

wlog("", "--- Saving outputs ---")

gen_csv  <- file.path(opt_dir, "one_tree_optimizer_generated_candidates.csv")
pri_csv  <- file.path(opt_dir, "one_tree_optimizer_priced_candidates.csv")
sel_csv  <- file.path(opt_dir, "one_tree_optimizer_selected_logs.csv")
stat_csv <- file.path(opt_dir, "one_tree_optimizer_all_states.csv")
diag_csv <- file.path(opt_dir, "one_tree_optimizer_diagnostics.csv")

readr::write_csv(generated_cands, gen_csv)
readr::write_csv(priced_cands,    pri_csv)
readr::write_csv(selected_logs,   sel_csv)
readr::write_csv(dp_states,       stat_csv)
readr::write_csv(diag_df,         diag_csv)

wlog(sprintf("  Saved: %s  (%d rows)", basename(gen_csv),  nrow(generated_cands)))
wlog(sprintf("  Saved: %s  (%d rows)", basename(pri_csv),  nrow(priced_cands)))
wlog(sprintf("  Saved: %s  (%d rows)", basename(sel_csv),  nrow(selected_logs)))
wlog(sprintf("  Saved: %s  (%d rows)", basename(stat_csv), nrow(dp_states)))
wlog(sprintf("  Saved: %s",             basename(diag_csv)))


# ---- diagnostic plot ----

wlog("", "--- Diagnostic plot ---")

# Show non-zero value candidates as points coloured by ProductKey.
# Selected logs are highlighted with a larger solid point.
if (nrow(dp_cands) > 0) {
  dp_plot <- dp_cands[dp_cands$value_nok > 0, ]
  dp_plot$selected <- seq_len(nrow(dp_cands))[dp_cands$value_nok > 0] %in% selected_idx

  if (nrow(dp_plot) > 0) {
    p_bg <- dp_plot[!dp_plot$selected, ]
    p_fg <- dp_plot[dp_plot$selected,  ]

    p_sel <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = p_bg,
        ggplot2::aes(x = StartPos, y = value_nok, colour = ProductKey),
        size = 0.7, alpha = 0.25) +
      ggplot2::geom_segment(
        data = p_fg,
        ggplot2::aes(x = StartPos, xend = StopPos,
                     y = value_nok, yend = value_nok,
                     colour = ProductKey),
        linewidth = 1.4) +
      ggplot2::geom_point(
        data = p_fg,
        ggplot2::aes(x = StartPos, y = value_nok, colour = ProductKey),
        size = 3, shape = 21, fill = "white") +
      ggplot2::labs(
        title    = sprintf("One-tree bucking optimizer – tree %s (%s)",
                           selected_tree_id, sg),
        subtitle = sprintf(
          "Selected: %d log(s), value = %.0f NOK, volume = %.4f m3 | STUMP_HEIGHT = %d cm",
          nrow(selected_logs), total_val, total_vol, STUMP_HEIGHT_CM),
        x      = "StartPos (cm above terrain)",
        y      = "value_nok (NOK)",
        colour = "ProductKey") +
      ggplot2::theme_minimal()

    save_plot(p_sel, "plot_one_tree_selected_logs.png")
  }
}


# ---- final summary ----

wlog(
  "",
  "=============================================================",
  "  OPTIMIZER TEST SUMMARY",
  "=============================================================",
  sprintf("  Tree                           : %s", selected_tree_id),
  sprintf("  Species group                  : %s", sg),
  sprintf("  STUMP_HEIGHT_CM                : %d cm", STUMP_HEIGHT_CM),
  "",
  sprintf("  Generated candidates           : %d", nrow(generated_cands)),
  sprintf("  Unique StartPos (generated)    : %d", n_unique_start_gen),
  sprintf("  Eligible with volume           : %d", nrow(elig_cands)),
  sprintf("  Unmatched price (total)        : %d", n_unmatched),
  sprintf("  DP input (priced + eligible)   : %d", nrow(dp_cands)),
  sprintf("  Unique StartPos (DP input)     : %d", n_unique_start_dp),
  "",
  sprintf("  DP positions                   : %d", length(dp_result$all_positions)),
  sprintf("  Selected logs                  : %d", nrow(selected_logs)),
  sprintf("  Total selected volume (m3)     : %.4f", total_vol),
  sprintf("  Total selected value (NOK)     : %.2f", total_val),
  sprintf("  Total selected length (cm)     : %d",   total_len),
  sprintf("  Overlapping log pairs          : %d",   n_overlap),
  sprintf("  Missing price / value rows     : %d / %d",
          n_miss_price, n_miss_val),
  "",
  sprintf("  Log: %s", log_file),
  ""
)

if (n_unmatched > 0 && n_dm31_unpriced > 0) {
  wlog(sprintf(
    "  REVIEW: %d candidate(s) with LengthClass_dm == 31 had no price.",
    n_dm31_unpriced))
  wlog("  Known issue: sawlog price matrix starts at 34 dm, not 31 dm.")
}

message(sprintf(
  "Script 06 complete. Tree %s. Generated %d candidates (%d unique StartPos). Selected %d log(s), value = %.2f NOK.",
  selected_tree_id, nrow(generated_cands), n_unique_start_gen,
  nrow(selected_logs), total_val))
