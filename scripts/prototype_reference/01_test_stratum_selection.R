# ==============================================================================
# 01_test_stratum_selection.R
#
# Purpose : Test stratum selection from the prepared NFI 2020-2024 spruce/pine
#           dataset. Verify that users can select forest types based on species,
#           development class (hogstkl), site index (abonitet), county (fylnr),
#           and altitude (hoh) before moving on to taper profiles or bucking.
#
# Input   : data_processed/nfi_tree_plot_2024_spruce_pine_stratum_ready.parquet
#
# Outputs :
#   outputs/stratum_tests/example_*.parquet / .rds / .csv
#   outputs/stratum_tests/plot_*.png
#   outputs/stratum_tests/stratum_selection_summary.txt
#
# No Oracle connection. No taper modelling. No bucking. No Shiny.
# ==============================================================================


# ---- setup ----

library(dplyr)
library(arrow)
library(readr)
library(ggplot2)
library(tidyr)

root_dir    <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
output_dir  <- file.path(root_dir, "outputs", "stratum_tests")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

log_file <- file.path(output_dir, "stratum_selection_summary.txt")
cat("", file = log_file)  # reset on each run

input_file <- file.path(root_dir, "data_processed",
                        "nfi_tree_plot_2024_spruce_pine_stratum_ready.parquet")


# ---- helper functions ----

# Write to console and append to log file.
wlog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = log_file, append = TRUE)
}


# ---- read data ----

wlog(
  "=============================================================",
  "  NFI stratum selection test",
  sprintf("  Run: %s", Sys.time()),
  sprintf("  Input: %s", input_file),
  "============================================================="
)

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file,
       "\nRun 00_prepare_nfi_2024_data.R first.")
}

nfi <- arrow::read_parquet(input_file)

wlog(
  "",
  "--- Input dataset ---",
  sprintf("  Rows         : %d", nrow(nfi)),
  sprintf("  Columns      : %d", ncol(nfi)),
  sprintf("  Unique plots : %d", dplyr::n_distinct(nfi$plot_id, na.rm = TRUE)),
  sprintf("  Unique trees : %d", dplyr::n_distinct(nfi$treid,   na.rm = TRUE)),
  sprintf("  Columns      : %s", paste(names(nfi), collapse = ", "))
)


# ---- stratum selection function ----

#' Select a stratum from the NFI tree-plot dataset.
#'
#' All filter arguments are optional.  Pass NULL (default) to skip a filter.
#'
#' @param df           Data frame (the full stratum-ready NFI dataset).
#' @param species_group "spruce", "pine", or NULL for both.
#' @param hogstkl      Character vector of development-class codes, e.g.
#'                     c("41", "51"), or NULL.
#' @param abonitet     Numeric (or character) vector of site-index codes, or
#'                     NULL.
#' @param fylnr        Character vector of county codes, or NULL.
#' @param hoh_min      Minimum altitude in metres (inclusive), or NULL.
#' @param hoh_max      Maximum altitude in metres (inclusive), or NULL.
#' @param min_plots    Warn when the selected stratum has fewer than this many
#'                     unique plots.  Default 10.
#'
#' @return Filtered data frame.
select_nfi_stratum <- function(df,
                               species_group = NULL,
                               hogstkl       = NULL,
                               abonitet      = NULL,
                               fylnr         = NULL,
                               hoh_min       = NULL,
                               hoh_max       = NULL,
                               min_plots     = 10L) {

  result <- df

  # species_group
  if (!is.null(species_group)) {
    result <- result |> dplyr::filter(species_group %in% .env$species_group)
  }

  # hogstkl – compare as character to handle mixed storage types
  if (!is.null(hogstkl) && "hogstkl" %in% names(result)) {
    hkl_chr <- as.character(hogstkl)
    result   <- result |>
      dplyr::filter(as.character(hogstkl) %in% hkl_chr)
  }

  # abonitet – compare as character to handle numeric or character storage
  if (!is.null(abonitet) && "abonitet" %in% names(result)) {
    ab_chr <- as.character(abonitet)
    result  <- result |>
      dplyr::filter(as.character(abonitet) %in% ab_chr)
  }

  # fylnr – compare as character
  if (!is.null(fylnr) && "fylnr" %in% names(result)) {
    fy_chr <- as.character(fylnr)
    result  <- result |>
      dplyr::filter(as.character(fylnr) %in% fy_chr)
  }

  # altitude lower bound
  if (!is.null(hoh_min) && "hoh" %in% names(result)) {
    result <- result |> dplyr::filter(hoh >= hoh_min)
  }

  # altitude upper bound
  if (!is.null(hoh_max) && "hoh" %in% names(result)) {
    result <- result |> dplyr::filter(hoh <= hoh_max)
  }

  # Short summary
  n_trees_sel  <- nrow(result)
  n_plots_sel  <- dplyr::n_distinct(result$plot_id, na.rm = TRUE)
  species_info <- if (!is.null(species_group)) paste(species_group, collapse = "/") else "all"

  message(sprintf(
    "[select_nfi_stratum] %d trees | %d plots | species: %s",
    n_trees_sel, n_plots_sel, species_info
  ))

  if (n_plots_sel < min_plots) {
    warning(sprintf(
      "Stratum has only %d unique plots (threshold: %d). Results may be unreliable.",
      n_plots_sel, min_plots
    ))
  }

  result
}


# ---- summary function ----

#' Summarise a selected NFI stratum.
#'
#' @param df  Filtered tree-level data frame from select_nfi_stratum().
#' @return Named list of summary tables.
summarise_nfi_stratum <- function(df) {

  # Basal area per tree (m²)
  df <- df |> dplyr::mutate(ba_m2 = pi * (dbh_cm / 200)^2)

  # General
  general <- tibble::tibble(
    n_trees        = nrow(df),
    n_plots        = dplyr::n_distinct(df$plot_id, na.rm = TRUE),
    n_sesong       = dplyr::n_distinct(df$sesong,  na.rm = TRUE),
    mean_dbh_cm    = round(mean(df$dbh_cm,          na.rm = TRUE), 2),
    median_dbh_cm  = round(median(df$dbh_cm,        na.rm = TRUE), 2),
    mean_ht_m      = round(mean(df$beregnet_hoyde   / 10, na.rm = TRUE), 2),
    median_ht_m    = round(median(df$beregnet_hoyde / 10, na.rm = TRUE), 2)
  )

  # Trees by species_group
  by_species <- df |> dplyr::count(species_group, name = "n_trees")

  # Trees by treslag
  by_treslag <- df |> dplyr::count(treslag, name = "n_trees", sort = TRUE)

  # Plots by fylnr
  by_fylnr <- df |>
    dplyr::distinct(plot_id, fylnr) |>
    dplyr::count(fylnr, name = "n_plots", sort = TRUE)

  # Trees by hogstkl
  by_hogstkl <- if ("hogstkl" %in% names(df)) {
    df |> dplyr::count(hogstkl, name = "n_trees", sort = TRUE)
  } else {
    tibble::tibble(hogstkl = character(), n_trees = integer())
  }

  # Trees by abonitet
  by_abonitet <- if ("abonitet" %in% names(df)) {
    df |> dplyr::count(abonitet, name = "n_trees", sort = TRUE)
  } else {
    tibble::tibble(abonitet = character(), n_trees = integer())
  }

  # DBH summary by species
  dbh_by_species <- df |>
    dplyr::group_by(species_group) |>
    dplyr::summarise(
      n          = dplyr::n(),
      min_dbh_cm = round(min(dbh_cm,    na.rm = TRUE), 1),
      med_dbh_cm = round(median(dbh_cm, na.rm = TRUE), 1),
      max_dbh_cm = round(max(dbh_cm,    na.rm = TRUE), 1),
      .groups    = "drop"
    )

  # Height summary by species (beregnet_hoyde / 10 for display only)
  ht_by_species <- df |>
    dplyr::mutate(height_m_check = beregnet_hoyde / 10) |>
    dplyr::group_by(species_group) |>
    dplyr::summarise(
      n          = dplyr::n(),
      min_ht_m   = round(min(height_m_check,    na.rm = TRUE), 1),
      med_ht_m   = round(median(height_m_check, na.rm = TRUE), 1),
      max_ht_m   = round(max(height_m_check,    na.rm = TRUE), 1),
      .groups    = "drop"
    )

  # Plot-level summary: tree count, basal area per plot, basal area per ha
  plot_area_val <- if ("plot_area_m2" %in% names(df))
    unique(df$plot_area_m2[!is.na(df$plot_area_m2)])[1]
  else
    250

  plot_summary <- df |>
    dplyr::group_by(plot_id) |>
    dplyr::summarise(
      n_trees   = dplyr::n(),
      ba_plot   = round(sum(ba_m2, na.rm = TRUE), 4),
      ba_ha     = round(sum(ba_m2, na.rm = TRUE) * 10000 / plot_area_val, 2),
      .groups   = "drop"
    )

  list(
    general       = general,
    by_species    = by_species,
    by_treslag    = by_treslag,
    by_fylnr      = by_fylnr,
    by_hogstkl    = by_hogstkl,
    by_abonitet   = by_abonitet,
    dbh_by_species = dbh_by_species,
    ht_by_species  = ht_by_species,
    plot_summary   = plot_summary
  )
}


# ---- example selections ----

# Helper: run selection + summary, log results, save CSV
run_example <- function(label, df, ...) {
  wlog("", paste0("--- Example: ", label, " ---"))

  # Selection
  sel  <- select_nfi_stratum(df, ...)
  summ <- summarise_nfi_stratum(sel)

  wlog(
    sprintf("  Trees : %d", summ$general$n_trees),
    sprintf("  Plots : %d", summ$general$n_plots),
    "  Species breakdown:"
  )
  wlog(capture.output(print(summ$by_species)))
  wlog("  hogstkl breakdown:")
  wlog(capture.output(print(summ$by_hogstkl)))
  wlog("  abonitet breakdown:")
  wlog(capture.output(print(summ$by_abonitet)))
  wlog("  DBH by species:")
  wlog(capture.output(print(summ$dbh_by_species)))
  wlog("  Height by species (beregnet_hoyde / 10):")
  wlog(capture.output(print(summ$ht_by_species)))

  # Save CSV of general summary
  csv_path <- file.path(output_dir,
                        paste0("summary_", gsub("[^A-Za-z0-9]", "_", label), ".csv"))
  readr::write_csv(summ$general, csv_path)
  wlog(sprintf("  CSV saved: %s", basename(csv_path)))

  list(selection = sel, summary = summ)
}

# Example 1: spruce only
ex1 <- run_example("01_spruce_all",
  nfi,
  species_group = "spruce"
)

# Example 2: pine only
ex2 <- run_example("02_pine_all",
  nfi,
  species_group = "pine"
)

# Example 3: spruce, mature / older development classes (hogstkl 41 and 51)
ex3 <- run_example("03_spruce_hogstkl_41_51",
  nfi,
  species_group = "spruce",
  hogstkl       = c("41", "51")
)

# Example 4: spruce, site index 14 and 17
ex4 <- run_example("04_spruce_abonitet_14_17",
  nfi,
  species_group = "spruce",
  abonitet      = c(14, 17)
)

# Example 5: pine in Innlandet (fylnr 34) – warns if county not present
ex5 <- run_example("05_pine_fylnr_34",
  nfi,
  species_group = "pine",
  fylnr         = "34"
)

# Example 6: spruce between 200 and 500 m altitude
ex6 <- run_example("06_spruce_hoh_200_500",
  nfi,
  species_group = "spruce",
  hoh_min       = 200,
  hoh_max       = 500
)


# ---- save example datasets ----

wlog("", "--- Saving example datasets ---")

# Main example: spruce, hogstkl 41/51, abonitet 11/14/17
example_main <- select_nfi_stratum(
  nfi,
  species_group = "spruce",
  hogstkl       = c("41", "51"),
  abonitet      = c(11, 14, 17)
)

main_rds     <- file.path(output_dir, "example_spruce_hkl_41_51_bonitet_11_14_17.rds")
main_parquet <- file.path(output_dir, "example_spruce_hkl_41_51_bonitet_11_14_17.parquet")

saveRDS(example_main, main_rds)
arrow::write_parquet(example_main, main_parquet)
wlog(
  sprintf("  Full example: %d trees | %d plots",
          nrow(example_main),
          dplyr::n_distinct(example_main$plot_id, na.rm = TRUE)),
  sprintf("  Saved: %s", basename(main_rds)),
  sprintf("  Saved: %s", basename(main_parquet))
)

# 500-tree sample: sample whole plots
set.seed(2024L)

# Build a plot-level table with cumulative tree count.
# Sample plot IDs in random order until the cumulative count reaches >= 500,
# then keep all trees from those plots (may be slightly more than 500).
plot_sizes <- example_main |>
  dplyr::count(plot_id, name = "n_trees") |>
  dplyr::slice_sample(prop = 1)  # random order

plot_sizes$cumsum <- cumsum(plot_sizes$n_trees)
n_target         <- 500L
n_needed_plots   <- which(plot_sizes$cumsum >= n_target)[1]

if (is.na(n_needed_plots)) {
  # Dataset has fewer than 500 trees; keep everything
  selected_plot_ids <- plot_sizes$plot_id
  wlog("  NOTE: fewer than 500 trees available; keeping all trees for the sample.")
} else {
  selected_plot_ids <- plot_sizes$plot_id[seq_len(n_needed_plots)]
}

example_500 <- example_main |>
  dplyr::filter(plot_id %in% selected_plot_ids)

s500_rds     <- file.path(output_dir, "example_spruce_hkl_41_51_bonitet_11_14_17_500trees.rds")
s500_parquet <- file.path(output_dir, "example_spruce_hkl_41_51_bonitet_11_14_17_500trees.parquet")

saveRDS(example_500, s500_rds)
arrow::write_parquet(example_500, s500_parquet)
wlog(
  sprintf("  500-tree sample: %d trees | %d plots (whole-plot sampling, seed 2024)",
          nrow(example_500),
          dplyr::n_distinct(example_500$plot_id, na.rm = TRUE)),
  sprintf("  Saved: %s", basename(s500_rds)),
  sprintf("  Saved: %s", basename(s500_parquet))
)


# ---- diagnostic plots ----

wlog("", "--- Creating diagnostic plots ---")

# Convenience wrapper
save_plot <- function(p, filename, width = 16, height = 10, units = "cm", dpi = 150) {
  path <- file.path(output_dir, filename)
  ggplot2::ggsave(path, plot = p, width = width, height = height,
                  units = units, dpi = dpi)
  wlog(sprintf("  Plot saved: %s", basename(path)))
}

# 1. DBH distribution by species_group (full stratum-ready dataset)
p_dbh <- ggplot2::ggplot(nfi, ggplot2::aes(x = dbh_cm, fill = species_group)) +
  ggplot2::geom_histogram(binwidth = 2, colour = "white", alpha = 0.8,
                          position = "identity") +
  ggplot2::facet_wrap(~species_group, scales = "free_y") +
  ggplot2::labs(
    title = "DBH distribution by species group (full stratum-ready dataset)",
    x = "DBH (cm)", y = "Number of trees"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none")

save_plot(p_dbh, "plot_dbh_distribution.png")

# 2. Height distribution by species_group (beregnet_hoyde / 10)
nfi_ht <- nfi |> dplyr::mutate(height_m = beregnet_hoyde / 10)

p_ht <- ggplot2::ggplot(nfi_ht, ggplot2::aes(x = height_m, fill = species_group)) +
  ggplot2::geom_histogram(binwidth = 1, colour = "white", alpha = 0.8,
                          position = "identity") +
  ggplot2::facet_wrap(~species_group, scales = "free_y") +
  ggplot2::labs(
    title = "Height distribution by species group (beregnet_hoyde / 10)",
    x = "Height (m)", y = "Number of trees"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none")

save_plot(p_ht, "plot_height_distribution.png")

# 3. Tree count by hogstkl (full stratum-ready dataset)
if ("hogstkl" %in% names(nfi)) {
  p_hkl <- nfi |>
    dplyr::count(hogstkl, species_group) |>
    ggplot2::ggplot(ggplot2::aes(x = as.character(hogstkl), y = n,
                                  fill = species_group)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(
      title = "Number of trees by hogstkl",
      x = "hogstkl", y = "Number of trees", fill = "Species"
    ) +
    ggplot2::theme_minimal()

  save_plot(p_hkl, "plot_trees_by_hogstkl.png")
}

# 4. Tree count by abonitet (full stratum-ready dataset)
if ("abonitet" %in% names(nfi)) {
  p_ab <- nfi |>
    dplyr::count(abonitet, species_group) |>
    ggplot2::ggplot(ggplot2::aes(x = as.character(abonitet), y = n,
                                  fill = species_group)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(
      title = "Number of trees by abonitet",
      x = "abonitet", y = "Number of trees", fill = "Species"
    ) +
    ggplot2::theme_minimal()

  save_plot(p_ab, "plot_trees_by_abonitet.png")
}

# 5. Basal area per ha distribution for the main spruce example stratum
if (nrow(example_main) > 0) {
  ba_ha_data <- example_main |>
    dplyr::mutate(ba_m2 = pi * (dbh_cm / 200)^2) |>
    dplyr::group_by(plot_id) |>
    dplyr::summarise(
      ba_ha = sum(ba_m2, na.rm = TRUE) * 10000 /
              unique(dplyr::coalesce(plot_area_m2, 250L))[1],
      .groups = "drop"
    )

  p_ba <- ggplot2::ggplot(ba_ha_data, ggplot2::aes(x = ba_ha)) +
    ggplot2::geom_histogram(binwidth = 2, fill = "steelblue",
                            colour = "white", alpha = 0.85) +
    ggplot2::labs(
      title = "Basal area per ha – spruce, hogstkl 41/51, abonitet 11/14/17",
      x = expression(paste("Basal area (m"^2, "/ha)")),
      y = "Number of plots"
    ) +
    ggplot2::theme_minimal()

  save_plot(p_ba, "plot_ba_ha_spruce_example.png")
}


# ---- write log ----

wlog(
  "",
  "--- Files written ---",
  sprintf("  %s", basename(main_rds)),
  sprintf("  %s", basename(main_parquet)),
  sprintf("  %s", basename(s500_rds)),
  sprintf("  %s", basename(s500_parquet))
)
wlog("", sprintf("  Log: %s", log_file), "", "Done.")
