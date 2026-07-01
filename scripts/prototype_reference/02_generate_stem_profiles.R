# ==============================================================================
# 02_generate_stem_profiles.R
#
# Purpose : Generate taper-based stem profiles for selected NFI trees using
#           the taperNOR package (Hansen et al. 2023, Scand. J. For. Res.).
#           This is the step after stratum selection. The output is a long-form
#           table of stem diameters at regular height intervals, ready for
#           bucking optimisation.
#
# Input   : outputs/stratum_tests/
#             example_spruce_hkl_41_51_bonitet_11_14_17_500trees.parquet
#
# Outputs :
#   outputs/stem_profiles/stem_profiles_example_500trees.parquet / .rds
#   outputs/stem_profiles/stem_profile_tree_summary_example_500trees.csv / .rds
#   outputs/stem_profiles/trees_taper_input_example_500trees.parquet / .rds
#   outputs/stem_profiles/stem_profile_failed_trees_example_500trees.csv
#   outputs/stem_profiles/plot_*.png
#   outputs/stem_profiles/stem_profile_generation_summary.txt
#
# taperNOR function used : taperNOR()
#   taperNOR(h, dbh, h_top, sp, with_bark)
#     h        : heights above ground (m) at which to predict diameter
#     dbh      : diameter at breast height, over bark (cm)
#     h_top    : total tree height (m)
#     sp       : "spruce" | "pine" | "birch"
#     with_bark: logical (default TRUE)
#   Returns    : numeric vector of predicted diameters (cm) at each h
#
# No Oracle connection. No bucking. No price matrices. No Shiny.
# ==============================================================================


# ---- setup ----

library(dplyr)
library(arrow)
library(purrr)
library(tibble)
library(tidyr)
library(readr)
library(ggplot2)

# Check taperNOR is available before proceeding.
if (!requireNamespace("taperNOR", quietly = TRUE)) {
  stop(
    "Package 'taperNOR' is not installed.\n",
    "Install with:\n",
    "  devtools::install_git(\"https://github.com/SmartForest-no/taperNOR\")"
  )
}
library(taperNOR)

root_dir    <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
output_dir  <- file.path(root_dir, "outputs", "stem_profiles")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

log_file <- file.path(output_dir, "stem_profile_generation_summary.txt")
cat("", file = log_file)  # reset on each run

input_file <- file.path(
  root_dir, "outputs", "stratum_tests",
  "example_spruce_hkl_41_51_bonitet_11_14_17_500trees.parquet"
)

# Height-interval step for profile generation (metres).
PROFILE_STEP_M <- 0.10

# Minimum acceptable values for the taper function.
MIN_DBH_CM    <- 5.0
MIN_HEIGHT_M  <- 4.5   # 45 dm in the original NFI unit


# ---- helper functions ----

# Write to console and append to log file.
wlog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = log_file, append = TRUE)
}


# ---- read input trees ----

wlog(
  "=============================================================",
  "  Stem profile generation – summary log",
  sprintf("  Run   : %s", Sys.time()),
  sprintf("  Input : %s", input_file),
  "============================================================="
)

if (!file.exists(input_file)) {
  stop(
    "Input file not found:\n  ", input_file,
    "\nRun 01_test_stratum_selection.R first."
  )
}

trees_raw <- arrow::read_parquet(input_file)

wlog(
  "",
  "--- Input dataset ---",
  sprintf("  Rows         : %d", nrow(trees_raw)),
  sprintf("  Unique plots : %d", dplyr::n_distinct(trees_raw$plot_id,  na.rm = TRUE)),
  sprintf("  Unique trees : %d", dplyr::n_distinct(trees_raw$treid,    na.rm = TRUE)),
  sprintf("  Columns      : %s", paste(names(trees_raw), collapse = ", "))
)


# ---- prepare taper input ----

wlog("", "--- Preparing taper input ---")

# Required columns
required_cols <- c("treid", "plot_id", "sesong", "species_group",
                   "dbh_cm", "beregnet_hoyde")
missing_req <- setdiff(required_cols, names(trees_raw))
if (length(missing_req) > 0) {
  stop("Required columns missing from input: ",
       paste(missing_req, collapse = ", "))
}

# Convert beregnet_hoyde (dm) to height_m (m).
# beregnet_hoyde is stored in decimetres in the NFI system; divide by 10.
trees_taper <- trees_raw |>
  dplyr::mutate(
    height_m = as.numeric(beregnet_hoyde) / 10,
    dbh_cm   = as.numeric(dbh_cm)
  )

# Filter: minimum DBH and minimum height for taper function validity.
n_before_filter <- nrow(trees_taper)

trees_taper <- trees_taper |>
  dplyr::filter(!is.na(dbh_cm),    dbh_cm   >= MIN_DBH_CM) |>
  dplyr::filter(!is.na(height_m),  height_m >= MIN_HEIGHT_M)

n_after_filter  <- nrow(trees_taper)
n_removed_filter <- n_before_filter - n_after_filter

wlog(
  sprintf("  Input trees              : %d", n_before_filter),
  sprintf("  Trees after taper filter : %d", n_after_filter),
  sprintf("  Removed (dbh or height)  : %d", n_removed_filter)
)
if (n_removed_filter > 0) {
  wlog(sprintf(
    "  WARNING: %d trees removed by taper-input filter. Check dbh_cm and beregnet_hoyde.",
    n_removed_filter
  ))
}
if (n_after_filter == 0) {
  stop("No trees remain after taper-input filtering. Cannot continue.")
}

# Save the clean taper-input tree table.
taper_input_parquet <- file.path(output_dir, "trees_taper_input_example_500trees.parquet")
taper_input_rds     <- file.path(output_dir, "trees_taper_input_example_500trees.rds")
arrow::write_parquet(trees_taper, taper_input_parquet)
saveRDS(trees_taper,              taper_input_rds)
wlog(
  sprintf("  Saved: %s", basename(taper_input_parquet)),
  sprintf("  Saved: %s", basename(taper_input_rds))
)


# ---- taper wrapper ----

#' Generate a stem-diameter profile for one tree using taperNOR().
#'
#' Uses the Kozak-type taper equation from Hansen et al. (2023) as implemented
#' in taperNOR::taperNOR().
#'
#' @param treid       Tree identifier (character/integer).
#' @param plot_id     Plot identifier (character).
#' @param sesong      Measurement season.
#' @param dbh_cm      DBH over bark (cm).
#' @param height_m    Total tree height (m).
#' @param species_group "spruce" or "pine" (as stored in the NFI dataset).
#' @param step_m      Height step for the profile (m). Default 0.10.
#'
#' @return Tibble with columns: treid, plot_id, sesong, species_group,
#'         dbh_cm, height_m, z_m, diameter_cm.
#'         Returns NULL on failure (caller logs the error).
generate_profile_one_tree <- function(treid, plot_id, sesong,
                                      dbh_cm, height_m, species_group,
                                      step_m = 0.10) {

  # Heights at which to evaluate the profile (metres above ground).
  # Start at step_m (not 0) to avoid the undefined base.
  z_vec <- seq(step_m, height_m, by = step_m)

  # taperNOR() expects:
  #   h      = heights (m)
  #   dbh    = DBH over bark (cm)
  #   h_top  = total height (m)
  #   sp     = species string
  d_vec <- taperNOR::taperNOR(
    h      = z_vec,
    dbh    = dbh_cm,
    h_top  = height_m,
    sp     = species_group,
    with_bark = TRUE
  )

  tibble::tibble(
    treid        = treid,
    plot_id      = plot_id,
    sesong       = sesong,
    species_group = species_group,
    dbh_cm       = dbh_cm,
    height_m     = height_m,
    z_m          = z_vec,
    diameter_cm  = d_vec
  )
}


# ---- generate profiles ----

wlog("", "--- Generating stem profiles ---")

profile_list  <- vector("list", nrow(trees_taper))
failed_rows   <- list()

for (i in seq_len(nrow(trees_taper))) {
  row <- trees_taper[i, ]

  result <- tryCatch(
    generate_profile_one_tree(
      treid        = row$treid,
      plot_id      = row$plot_id,
      sesong       = row$sesong,
      dbh_cm       = row$dbh_cm,
      height_m     = row$height_m,
      species_group = row$species_group,
      step_m       = PROFILE_STEP_M
    ),
    error = function(e) {
      failed_rows[[length(failed_rows) + 1L]] <<- tibble::tibble(
        treid        = as.character(row$treid),
        plot_id      = as.character(row$plot_id),
        dbh_cm       = row$dbh_cm,
        height_m     = row$height_m,
        species_group = row$species_group,
        error_message = conditionMessage(e)
      )
      NULL
    }
  )

  profile_list[[i]] <- result
}

# Combine successful profiles.
stem_profiles <- dplyr::bind_rows(purrr::compact(profile_list))

n_profile_trees <- dplyr::n_distinct(stem_profiles$treid, na.rm = TRUE)
n_profile_rows  <- nrow(stem_profiles)
n_failed        <- length(failed_rows)

wlog(
  sprintf("  Profile rows generated   : %d", n_profile_rows),
  sprintf("  Unique trees in profiles : %d", n_profile_trees),
  sprintf("  Trees that failed        : %d", n_failed)
)

# Write failed trees to CSV.
failed_csv <- file.path(output_dir, "stem_profile_failed_trees_example_500trees.csv")
if (n_failed > 0) {
  failed_df <- dplyr::bind_rows(failed_rows)
  readr::write_csv(failed_df, failed_csv)
  wlog(sprintf("  WARNING: %d trees failed. IDs written to: %s",
               n_failed, basename(failed_csv)))
  wlog(capture.output(print(failed_df)))
} else {
  # Write an empty CSV so the file always exists.
  readr::write_csv(
    tibble::tibble(treid = character(), error_message = character()),
    failed_csv
  )
  wlog("  No failed trees.")
}

if (n_profile_rows == 0) {
  stop("No usable diameter predictions were produced. Check taperNOR inputs.")
}


# ---- quality checks ----

wlog("", "--- Quality checks ---")

# Zero / negative diameters.
# A diameter of exactly 0 at z_m == height_m is expected (tree top).
# Zero or negative diameters below the top are not expected and warrant a warning.
n_zero_top        <- sum(stem_profiles$diameter_cm <= 0 &
                           abs(stem_profiles$z_m - stem_profiles$height_m) < 1e-6,
                         na.rm = TRUE)
n_zero_below_top  <- sum(stem_profiles$diameter_cm <= 0 &
                           stem_profiles$z_m < stem_profiles$height_m - 1e-6,
                         na.rm = TRUE)

wlog(
  sprintf("  Zero diameter at tree top (expected)     : %d", n_zero_top),
  sprintf("  Zero/negative diameter below tree top    : %d", n_zero_below_top)
)

if (n_zero_below_top > 0) {
  wlog(sprintf(
    "  WARNING: %d profile points have diameter_cm <= 0 below the tree top. These will be set to 0.",
    n_zero_below_top
  ))
}

# Clamp all non-positive diameters to 0 regardless.
stem_profiles <- stem_profiles |>
  dplyr::mutate(diameter_cm = pmax(diameter_cm, 0))

# Add helper columns.
stem_profiles <- stem_profiles |>
  dplyr::mutate(
    rel_height  = z_m / height_m,
    diameter_mm = diameter_cm * 10
  )

# Diameter at breast height vs observed DBH.
bh_check <- stem_profiles |>
  dplyr::filter(abs(z_m - 1.3) == min(abs(z_m - 1.3)), .by = treid) |>
  dplyr::slice_head(n = 1, by = treid) |>
  dplyr::select(treid, diameter_at_bh_cm = diameter_cm)

dbh_check <- trees_taper |>
  dplyr::select(treid, dbh_cm_obs = dbh_cm) |>
  dplyr::left_join(bh_check, by = "treid") |>
  dplyr::mutate(
    dbh_diff = round(diameter_at_bh_cm - dbh_cm_obs, 3)
  )

wlog(
  "",
  "  Diameter at BH vs observed DBH:",
  sprintf("    Mean diff (profile - obs) : %.3f cm", mean(dbh_check$dbh_diff, na.rm = TRUE)),
  sprintf("    SD   diff                 : %.3f cm", sd(dbh_check$dbh_diff,   na.rm = TRUE)),
  sprintf("    Max  |diff|               : %.3f cm", max(abs(dbh_check$dbh_diff), na.rm = TRUE))
)

n_large_diff <- sum(abs(dbh_check$dbh_diff) > 2, na.rm = TRUE)
if (n_large_diff > 0) {
  wlog(sprintf(
    "  WARNING: %d trees have |profile_d_at_BH - dbh_cm| > 2 cm.",
    n_large_diff
  ))
}

# Profile point counts per tree.
pts_per_tree <- stem_profiles |> dplyr::count(treid, name = "n_pts")
wlog(
  "",
  "  Profile points per tree:",
  sprintf("    Min    : %d", min(pts_per_tree$n_pts)),
  sprintf("    Median : %.0f", median(pts_per_tree$n_pts)),
  sprintf("    Max    : %d", max(pts_per_tree$n_pts))
)
wlog(sprintf("  New columns added: rel_height, diameter_mm"))


# ---- tree-level profile summary ----

stem_profile_tree_summary <- stem_profiles |>
  dplyr::group_by(treid, plot_id, sesong, species_group, dbh_cm, height_m) |>
  dplyr::summarise(
    n_profile_points  = dplyr::n(),
    min_diameter_cm   = round(min(diameter_cm,    na.rm = TRUE), 2),
    max_diameter_cm   = round(max(diameter_cm,    na.rm = TRUE), 2),
    .groups           = "drop"
  ) |>
  dplyr::left_join(bh_check, by = "treid") |>
  dplyr::mutate(
    diameter_at_bh_cm = round(diameter_at_bh_cm, 2),
    dbh_difference_cm = round(diameter_at_bh_cm - dbh_cm, 3)
  )

wlog(
  "",
  "  Tree-level profile summary (first 5 rows):"
)
wlog(capture.output(print(head(stem_profile_tree_summary, 5))))


# ---- save outputs ----

wlog("", "--- Saving outputs ---")

# Stem profiles (long table)
profiles_parquet <- file.path(output_dir, "stem_profiles_example_500trees.parquet")
profiles_rds     <- file.path(output_dir, "stem_profiles_example_500trees.rds")
arrow::write_parquet(stem_profiles, profiles_parquet)
saveRDS(stem_profiles,              profiles_rds)
wlog(
  sprintf("  Saved: %s", basename(profiles_parquet)),
  sprintf("  Saved: %s", basename(profiles_rds))
)

# Tree-level summary
summary_csv <- file.path(output_dir, "stem_profile_tree_summary_example_500trees.csv")
summary_rds <- file.path(output_dir, "stem_profile_tree_summary_example_500trees.rds")
readr::write_csv(stem_profile_tree_summary, summary_csv)
saveRDS(stem_profile_tree_summary,           summary_rds)
wlog(
  sprintf("  Saved: %s", basename(summary_csv)),
  sprintf("  Saved: %s", basename(summary_rds))
)


# ---- diagnostic plots ----

wlog("", "--- Creating diagnostic plots ---")

save_plot <- function(p, filename, width = 16, height = 10,
                      units = "cm", dpi = 150) {
  path <- file.path(output_dir, filename)
  ggplot2::ggsave(path, plot = p, width = width, height = height,
                  units = units, dpi = dpi)
  wlog(sprintf("  Plot saved: %s", basename(path)))
}

# 1. DBH observed vs profile diameter at breast height
p_bh <- ggplot2::ggplot(
    stem_profile_tree_summary |> dplyr::filter(!is.na(diameter_at_bh_cm)),
    ggplot2::aes(x = dbh_cm, y = diameter_at_bh_cm, colour = species_group)
  ) +
  ggplot2::geom_point(alpha = 0.5, size = 1) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ggplot2::labs(
    title  = "DBH observed vs profile diameter at breast height (1.3 m)",
    x      = "Observed DBH (cm)",
    y      = "Profile diameter at 1.3 m (cm)",
    colour = "Species"
  ) +
  ggplot2::theme_minimal()

save_plot(p_bh, "plot_dbh_vs_profile_bh.png")

# 2. Histogram of dbh_difference_cm
p_diff <- ggplot2::ggplot(
    stem_profile_tree_summary |> dplyr::filter(!is.na(dbh_difference_cm)),
    ggplot2::aes(x = dbh_difference_cm, fill = species_group)
  ) +
  ggplot2::geom_histogram(binwidth = 0.1, colour = "white", alpha = 0.8,
                           position = "identity") +
  ggplot2::facet_wrap(~species_group, scales = "free_y") +
  ggplot2::labs(
    title = "Difference: profile diameter at BH minus observed DBH (cm)",
    x     = "dbh_difference_cm",
    y     = "Number of trees"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "none")

save_plot(p_diff, "plot_dbh_difference_histogram.png")

# 3. Example stem profiles – 20 randomly selected trees
set.seed(42L)
sample_ids <- stem_profiles |>
  dplyr::distinct(treid) |>
  dplyr::slice_sample(n = min(20L, dplyr::n_distinct(stem_profiles$treid)))

p_profiles <- stem_profiles |>
  dplyr::semi_join(sample_ids, by = "treid") |>
  dplyr::mutate(treid = as.character(treid)) |>
  ggplot2::ggplot(ggplot2::aes(x = diameter_cm, y = z_m,
                                group = treid, colour = species_group)) +
  ggplot2::geom_line(alpha = 0.7, linewidth = 0.4) +
  ggplot2::labs(
    title  = "Stem profiles – 20 randomly selected trees",
    x      = "Diameter (cm)",
    y      = "Height above ground (m)",
    colour = "Species"
  ) +
  ggplot2::theme_minimal()

save_plot(p_profiles, "plot_example_stem_profiles_20trees.png",
          width = 14, height = 18)

# 4. Stem profiles for the 10 largest trees by DBH
top10_ids <- stem_profile_tree_summary |>
  dplyr::slice_max(dbh_cm, n = 10, with_ties = FALSE) |>
  dplyr::select(treid)

p_top10 <- stem_profiles |>
  dplyr::semi_join(top10_ids, by = "treid") |>
  dplyr::mutate(treid = as.character(treid)) |>
  ggplot2::ggplot(ggplot2::aes(x = diameter_cm, y = z_m,
                                group = treid, colour = treid)) +
  ggplot2::geom_line(linewidth = 0.6) +
  ggplot2::labs(
    title  = "Stem profiles – 10 largest trees by DBH",
    x      = "Diameter (cm)",
    y      = "Height above ground (m)",
    colour = "treid"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.key.size = ggplot2::unit(0.4, "cm"))

save_plot(p_top10, "plot_stem_profiles_top10_dbh.png",
          width = 16, height = 18)

# 5. Distribution of number of profile points per tree
p_npts <- stem_profiles |>
  dplyr::count(treid, name = "n_pts") |>
  ggplot2::ggplot(ggplot2::aes(x = n_pts)) +
  ggplot2::geom_histogram(binwidth = 5, fill = "steelblue",
                           colour = "white", alpha = 0.85) +
  ggplot2::labs(
    title = "Distribution of number of profile points per tree",
    x     = "Number of profile points",
    y     = "Number of trees"
  ) +
  ggplot2::theme_minimal()

save_plot(p_npts, "plot_profile_points_per_tree.png")


# ---- write log ----

wlog(
  "",
  "=============================================================",
  "  SUMMARY",
  "=============================================================",
  sprintf("  Input trees              : %d", n_before_filter),
  sprintf("  Trees after filter       : %d", n_after_filter),
  sprintf("  Trees with profiles      : %d", n_profile_trees),
  sprintf("  Failed trees             : %d", n_failed),
  sprintf("  Total profile rows       : %d", n_profile_rows),
  "",
  "  Output files:",
  sprintf("    %s", basename(profiles_parquet)),
  sprintf("    %s", basename(profiles_rds)),
  sprintf("    %s", basename(summary_csv)),
  sprintf("    %s", basename(summary_rds)),
  sprintf("    %s", basename(taper_input_parquet)),
  sprintf("    %s", basename(taper_input_rds)),
  sprintf("    %s", basename(failed_csv)),
  "",
  sprintf("  Log: %s", log_file),
  "",
  "Done."
)
