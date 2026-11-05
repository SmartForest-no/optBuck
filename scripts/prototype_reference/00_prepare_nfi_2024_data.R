# ==============================================================================
# 00_prepare_nfi_2024_data.R
#
# Purpose : Read NFI (National Forest Inventory) tree and plot data from the
#           Oracle database for the 2024 period (full 2020-2024 cycle), clean
#           and filter the data, and export it for use in the nfiBuck bucking
#           simulation tool.
#
#           NORGE_TRE_2024_V and NORGE_FLA_2024_V cover the full 2020-2024
#           cycle.  All seasons in those tables are kept – no sesong filter.
#
#           Height source: beregnet_hoyde from SKOG.NORGE_TRE_2024_V.
#           No join to SKOG.LSKOV_TRE_BEREGNET is performed.
#           Original Norwegian NFI variable names are preserved.
#
# Species : Spruce and pine only (initial scope).
#
# Outputs :
#   data_processed/nfi_trees_2024_spruce_pine.parquet / .rds
#   data_processed/nfi_plots_2024.parquet              / .rds
#   data_processed/nfi_tree_plot_2024_spruce_pine.parquet / .rds
#   data_processed/nfi_tree_plot_2024_spruce_pine_stratum_ready.parquet / .rds
#   logs/nfi_2024_data_summary.txt
#
# ==============================================================================


# ---- setup ----

library(DBI)
library(ROracle)
library(keyring)
library(dplyr)
library(arrow)
library(janitor)

root_dir           <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
data_processed_dir <- file.path(root_dir, "data_processed")
logs_dir           <- file.path(root_dir, "logs")

dir.create(file.path(root_dir, "data_raw"), showWarnings = FALSE, recursive = TRUE)
dir.create(data_processed_dir,              showWarnings = FALSE, recursive = TRUE)
dir.create(logs_dir,                        showWarnings = FALSE, recursive = TRUE)

log_file <- file.path(logs_dir, "nfi_2024_data_summary.txt")
cat("", file = log_file)  # reset log on each run


# ---- helper functions ----

# Write to console and append to log file.
write_log <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = log_file, append = TRUE)
}

# Apply filter_fn(df) only when col_name exists in df.
# Skips with a warning if the column is absent.
safe_filter <- function(df, col_name, filter_fn) {
  if (col_name %in% names(df)) {
    filter_fn(df)
  } else {
    msg <- sprintf("WARNING: column '%s' not found – filter skipped.", col_name)
    warning(msg)
    write_log(msg)
    df
  }
}

# Return first name from `candidates` present in df, or NULL.
first_col <- function(df, candidates) {
  found <- candidates[candidates %in% names(df)]
  if (length(found) == 0L) NULL else found[[1L]]
}

is_spruce_code <- function(x) x %in% c(1L, 2L, 3L, 21L, 29L)
is_pine_code   <- function(x) x %in% c(10L, 11L, 12L, 20L)


# ---- database connection ----

drv <- dbDriver("Oracle")
con <- dbConnect(
  drv,
  dbname   = "ORACLE_SERVICE",
  username = "ORACLE_USER",
  password = keyring::key_get("ORACLE_SERVICE", "ORACLE_USER")
)

# Always close the connection, even if the script errors.
on.exit(tryCatch(DBI::dbDisconnect(con), error = function(e) NULL), add = TRUE)

write_log(
  "=============================================================",
  "  NFI 2024 data preparation  –  summary log",
  sprintf("  Run: %s", Sys.time()),
  "============================================================="
)


# ---- read tree data ----

write_log("", "--- Reading SKOG.NORGE_TRE_2024_V ---")

trees_raw <- DBI::dbGetQuery(con, "SELECT * FROM SKOG.NORGE_TRE_2024_V")
trees_raw <- janitor::clean_names(trees_raw)

n_trees_raw <- nrow(trees_raw)
write_log(
  sprintf("  Raw tree rows : %d", n_trees_raw),
  sprintf("  Columns       : %s", paste(names(trees_raw), collapse = ", "))
)

if ("sesong" %in% names(trees_raw)) {
  write_log("", "  Tree rows by sesong:")
  write_log(capture.output(print(dplyr::count(trees_raw, sesong, name = "n"))))
}


# ---- read plot data ----

write_log("", "--- Reading SKOG.NORGE_FLA_2024_V ---")

plots_raw <- DBI::dbGetQuery(con, "SELECT * FROM SKOG.NORGE_FLA_2024_V")
plots_raw <- janitor::clean_names(plots_raw)

n_plots_raw <- nrow(plots_raw)
write_log(
  sprintf("  Raw plot rows : %d", n_plots_raw),
  sprintf("  Columns       : %s", paste(names(plots_raw), collapse = ", "))
)

if ("sesong" %in% names(plots_raw)) {
  write_log("", "  Plot rows by sesong:")
  write_log(capture.output(print(dplyr::count(plots_raw, sesong, name = "n"))))
}


# ---- clean tree data ----

write_log("", "--- Cleaning tree data ---")

trees <- trees_raw

# -- require treslag --
if (!"treslag" %in% names(trees)) {
  stop("Column 'treslag' not found in tree table.")
}

# -- species_group helper column (treslag kept as-is) --
trees <- trees |>
  dplyr::mutate(
    treslag       = as.integer(treslag),
    species_group = dplyr::case_when(
      is_spruce_code(treslag) ~ "spruce",
      is_pine_code(treslag)   ~ "pine",
      TRUE                    ~ NA_character_
    )
  ) |>
  dplyr::filter(species_group %in% c("spruce", "pine"))

n_trees_sp <- nrow(trees)
write_log(sprintf("  After spruce/pine filter           : %d", n_trees_sp))

# -- living trees: tilstand == 11 --
trees <- safe_filter(trees, "tilstand",
  function(df) dplyr::filter(df, tilstand == 11L)
)
n_trees_alive <- nrow(trees)
write_log(sprintf("  After tilstand == 11 filter        : %d", n_trees_alive))

# -- brysth_dia: non-missing and positive --
if (!"brysth_dia" %in% names(trees)) {
  stop("Column 'brysth_dia' not found in tree table.")
}
trees <- trees |>
  dplyr::mutate(brysth_dia = as.numeric(brysth_dia)) |>
  dplyr::filter(!is.na(brysth_dia), brysth_dia > 0)

n_trees_dbh <- nrow(trees)
write_log(sprintf("  After brysth_dia filter (> 0)      : %d", n_trees_dbh))

# -- dbh_cm helper column (brysth_dia kept as-is) --
dbh_is_mm <- median(trees$brysth_dia, na.rm = TRUE) > 100
trees <- trees |>
  dplyr::mutate(dbh_cm = if (dbh_is_mm) brysth_dia / 10 else brysth_dia)
write_log(sprintf(
  "  brysth_dia unit guess: %s  ->  dbh_cm added",
  if (dbh_is_mm) "mm (divided by 10)" else "cm (kept as-is)"
))

# -- beregnet_hoyde: non-missing and positive --
if (!"beregnet_hoyde" %in% names(trees)) {
  stop("Column 'beregnet_hoyde' not found in tree table.")
}
trees <- trees |>
  dplyr::mutate(beregnet_hoyde = as.numeric(beregnet_hoyde))

n_before_ht <- nrow(trees)
trees       <- trees |> dplyr::filter(!is.na(beregnet_hoyde), beregnet_hoyde > 0)
n_trees_ht  <- nrow(trees)
n_lost_ht   <- n_before_ht - n_trees_ht
pct_lost_ht <- if (n_before_ht > 0) n_lost_ht / n_before_ht * 100 else 0

write_log(sprintf(
  "  After beregnet_hoyde filter (> 0)  : %d  (removed %d, %.1f%%)",
  n_trees_ht, n_lost_ht, pct_lost_ht
))
if (pct_lost_ht > 5) {
  msg <- sprintf(
    "WARNING: %.1f%% of trees removed by beregnet_hoyde filter (%d rows). Check data.",
    pct_lost_ht, n_lost_ht
  )
  warning(msg)
  write_log(msg)
}

# -- plot_id helper column --
if (!"flateid" %in% names(trees)) stop("Column 'flateid' not found in tree table.")
trees <- trees |>
  dplyr::mutate(
    plot_id = as.character(flateid),
    sesong  = as.integer(sesong)
  )

# -- select final tree columns; keep original NFI names --
tree_keep_cols <- c(
  "plot_id", "flateid", "flatenr", "hel_delt_flate", "sesong", "treid",
  "treslag", "species_group",
  "brysth_dia", "dbh_cm", "trehoyde", "beregnet_hoyde",
  "tilstand", "tslgr", "komnr", "fylnr",
  "arealtype", "areal_anv", "abonitet_tre", "abonitet", "hogstkl",
  "volm", "volu", "tilv5"
)

trees_clean <- trees |> dplyr::select(dplyr::any_of(tree_keep_cols))

absent_tree <- setdiff(tree_keep_cols, names(trees_clean))
if (length(absent_tree) > 0) {
  write_log(sprintf(
    "  NOTE: requested tree columns not found: %s",
    paste(absent_tree, collapse = ", ")
  ))
}
write_log(
  sprintf("  Final tree rows  : %d", nrow(trees_clean)),
  sprintf("  Final tree cols  : %s", paste(names(trees_clean), collapse = ", "))
)


# ---- clean plot data ----

write_log("", "--- Cleaning plot data ---")

plots <- plots_raw

# -- domain filters (skipped with warning if column absent) --
plots <- safe_filter(plots, "arealtype",
  function(df) dplyr::filter(df, arealtype %in% c(1L, 12L))
)
write_log(sprintf("  After arealtype filter             : %d", nrow(plots)))

plots <- safe_filter(plots, "areal_anv",
  function(df) dplyr::filter(df, areal_anv %in% c(1L, 5L, 9L))
)
write_log(sprintf("  After areal_anv filter             : %d", nrow(plots)))

plots <- safe_filter(plots, "hel_delt_flate",
  function(df) dplyr::filter(df, hel_delt_flate == 0L)
)
write_log(sprintf("  After hel_delt_flate == 0 filter   : %d", nrow(plots)))

# -- plot_id helper column --
if (!"flateid" %in% names(plots)) stop("Column 'flateid' not found in plot table.")
plots <- plots |>
  dplyr::mutate(
    plot_id      = as.character(flateid),
    sesong       = as.integer(sesong),
    plot_area_m2 = 250L
  )

# -- select final plot columns; keep original NFI names --
plot_keep_cols <- c(
  "plot_id", "flateid", "flatenr", "hel_delt_flate", "sesong",
  "komnr", "fylnr", "hoh",
  "arealtype", "areal_anv",
  "abonitet_tre", "abonitet",
  "best_tresl", "gran_pr", "furu_pr", "bjork_pr",
  "hogstkl", "lsk_dato",
  "plot_area_m2"
)

plots_clean <- plots |> dplyr::select(dplyr::any_of(plot_keep_cols))

absent_plot <- setdiff(plot_keep_cols, names(plots_clean))
if (length(absent_plot) > 0) {
  write_log(sprintf(
    "  NOTE: requested plot columns not found: %s",
    paste(absent_plot, collapse = ", ")
  ))
}
n_plots_filtered <- nrow(plots_clean)
write_log(
  sprintf("  Final plot rows  : %d", n_plots_filtered),
  sprintf("  Final plot cols  : %s", paste(names(plots_clean), collapse = ", "))
)


# ---- join tree and plot data ----

write_log("", "--- Joining trees to plots (inner_join) ---")

# Join keys: plot_id + sesong, plus hel_delt_flate if present in both tables.
join_keys <- c("plot_id", "sesong")
if ("hel_delt_flate" %in% names(trees_clean) &&
    "hel_delt_flate" %in% names(plots_clean)) {
  join_keys <- c(join_keys, "hel_delt_flate")
}
write_log(sprintf("  Join keys: %s", paste(join_keys, collapse = ", ")))

# Ensure consistent key types.
trees_clean <- trees_clean |>
  dplyr::mutate(plot_id = as.character(plot_id), sesong = as.integer(sesong))
plots_clean <- plots_clean |>
  dplyr::mutate(plot_id = as.character(plot_id), sesong = as.integer(sesong))

# Columns present in both tables (excluding keys) where we prefer plot values.
plot_strata <- c("fylnr", "komnr", "hogstkl", "abonitet", "hoh",
                 "best_tresl", "arealtype", "areal_anv", "abonitet_tre")
shared <- intersect(plot_strata, intersect(names(trees_clean), names(plots_clean)))

if (length(shared) > 0) {
  write_log(sprintf(
    "  Using plot-level values for shared columns (dropped from tree table): %s",
    paste(shared, collapse = ", ")
  ))
  trees_clean <- trees_clean |> dplyr::select(-dplyr::all_of(shared))
}

# inner_join: keep only trees whose plot passed the domain filters.
nfi_tree_plot_2024_spruce_pine <- dplyr::inner_join(
  trees_clean,
  plots_clean,
  by = join_keys
)

# Resolve flateid.x/flateid.y and flatenr.x/flatenr.y.
# These arise because flateid and flatenr appear in both tree and plot tables.
# Verify that tree- and plot-level values are identical, warn if not, then
# keep the plot-level value and drop the tree-level duplicate.
for (base_col in c("flateid", "flatenr")) {
  col_x <- paste0(base_col, ".x")
  col_y <- paste0(base_col, ".y")
  if (col_x %in% names(nfi_tree_plot_2024_spruce_pine) &&
      col_y %in% names(nfi_tree_plot_2024_spruce_pine)) {
    n_diff <- sum(
      nfi_tree_plot_2024_spruce_pine[[col_x]] != nfi_tree_plot_2024_spruce_pine[[col_y]],
      na.rm = TRUE
    )
    if (n_diff > 0) {
      write_log(sprintf(
        "  WARNING: %s and %s differ in %d rows – using plot-level value (%s).",
        col_x, col_y, n_diff, col_y
      ))
    }
    nfi_tree_plot_2024_spruce_pine <- nfi_tree_plot_2024_spruce_pine |>
      dplyr::rename(!!base_col := !!col_y) |>
      dplyr::select(-dplyr::all_of(col_x))
  }
}

# Check for any remaining unexpected .x/.y suffixes.
xy_cols <- grep("\\.(x|y)$", names(nfi_tree_plot_2024_spruce_pine), value = TRUE)
if (length(xy_cols) > 0) {
  write_log(sprintf(
    "  WARNING: unexpected duplicate-suffix columns remain after join: %s",
    paste(xy_cols, collapse = ", ")
  ))
}

write_log(
  sprintf("  Trees in joined table  : %d", nrow(nfi_tree_plot_2024_spruce_pine)),
  sprintf("  Unique plots in joined : %d",
          dplyr::n_distinct(nfi_tree_plot_2024_spruce_pine$plot_id, na.rm = TRUE))
)


# ---- create stratum-ready dataset ----

# Keep only rows where the four main stratum variables are all present.
# The full joined table is preserved unchanged.
nfi_tree_plot_2024_spruce_pine_stratum_ready <- nfi_tree_plot_2024_spruce_pine
for (v in c("hogstkl", "abonitet", "fylnr", "hoh")) {
  if (v %in% names(nfi_tree_plot_2024_spruce_pine_stratum_ready)) {
    nfi_tree_plot_2024_spruce_pine_stratum_ready <-
      nfi_tree_plot_2024_spruce_pine_stratum_ready |>
      dplyr::filter(!is.na(.data[[v]]))
  }
}
n_trees_removed_stratum <- nrow(nfi_tree_plot_2024_spruce_pine) -
                            nrow(nfi_tree_plot_2024_spruce_pine_stratum_ready)


# ---- quality checks and log ----

write_log(
  "",
  "=============================================================",
  "  QUALITY CHECKS",
  "============================================================="
)

write_log(
  "",
  "  Row counts:",
  sprintf("    Raw tree rows                     : %d", n_trees_raw),
  sprintf("    Raw plot rows                     : %d", n_plots_raw),
  sprintf("    Trees after spruce/pine filter    : %d", n_trees_sp),
  sprintf("    Trees after tilstand == 11 filter : %d", n_trees_alive),
  sprintf("    Trees after brysth_dia filter     : %d", n_trees_dbh),
  sprintf("    Trees after beregnet_hoyde filter : %d", n_trees_ht),
  sprintf("    Plots after domain filters        : %d", n_plots_filtered),
  sprintf("    Trees in full joined table        : %d", nrow(nfi_tree_plot_2024_spruce_pine)),
  sprintf("    Plots in full joined table        : %d",
          dplyr::n_distinct(nfi_tree_plot_2024_spruce_pine$plot_id, na.rm = TRUE)),
  sprintf("    Trees in stratum-ready table      : %d", nrow(nfi_tree_plot_2024_spruce_pine_stratum_ready)),
  sprintf("    Plots in stratum-ready table      : %d",
          dplyr::n_distinct(nfi_tree_plot_2024_spruce_pine_stratum_ready$plot_id, na.rm = TRUE)),
  sprintf("    Trees removed (missing strata)    : %d", n_trees_removed_stratum)
)

# Trees by species_group
write_log("", "  Trees by species_group (joined table):")
write_log(capture.output(print(
  dplyr::count(nfi_tree_plot_2024_spruce_pine, species_group, name = "n_trees")
)))

# DBH summary
write_log("", "  brysth_dia / dbh_cm summary by species_group:")
write_log(capture.output(print(
  nfi_tree_plot_2024_spruce_pine |>
    dplyr::group_by(species_group) |>
    dplyr::summarise(
      n          = dplyr::n(),
      min_dbh_cm = round(min(dbh_cm, na.rm = TRUE), 1),
      med_dbh_cm = round(median(dbh_cm, na.rm = TRUE), 1),
      max_dbh_cm = round(max(dbh_cm, na.rm = TRUE), 1),
      .groups    = "drop"
    )
)))

# beregnet_hoyde summary
write_log("", "  beregnet_hoyde summary by species_group (original units):")
write_log(capture.output(print(
  nfi_tree_plot_2024_spruce_pine |>
    dplyr::group_by(species_group) |>
    dplyr::summarise(
      n      = dplyr::n(),
      min_ht = round(min(beregnet_hoyde, na.rm = TRUE), 1),
      med_ht = round(median(beregnet_hoyde, na.rm = TRUE), 1),
      max_ht = round(max(beregnet_hoyde, na.rm = TRUE), 1),
      .groups = "drop"
    )
)))

# hogstkl counts
if ("hogstkl" %in% names(nfi_tree_plot_2024_spruce_pine)) {
  write_log("", "  Trees by hogstkl:")
  write_log(capture.output(print(
    dplyr::count(nfi_tree_plot_2024_spruce_pine, hogstkl, sort = TRUE)
  )))
}

# abonitet counts
if ("abonitet" %in% names(nfi_tree_plot_2024_spruce_pine)) {
  write_log("", "  Trees by abonitet:")
  write_log(capture.output(print(
    dplyr::count(nfi_tree_plot_2024_spruce_pine, abonitet, sort = TRUE)
  )))
}

# fylnr plot counts
if ("fylnr" %in% names(nfi_tree_plot_2024_spruce_pine)) {
  write_log("", "  Unique plots by fylnr:")
  write_log(capture.output(print(
    nfi_tree_plot_2024_spruce_pine |>
      dplyr::distinct(plot_id, fylnr) |>
      dplyr::count(fylnr, sort = TRUE)
  )))
}

# hogstkl and abonitet counts in stratum-ready table
if ("hogstkl" %in% names(nfi_tree_plot_2024_spruce_pine_stratum_ready)) {
  write_log("", "  Trees by hogstkl (stratum-ready table):")
  write_log(capture.output(print(
    dplyr::count(nfi_tree_plot_2024_spruce_pine_stratum_ready, hogstkl, sort = TRUE)
  )))
}
if ("abonitet" %in% names(nfi_tree_plot_2024_spruce_pine_stratum_ready)) {
  write_log("", "  Trees by abonitet (stratum-ready table):")
  write_log(capture.output(print(
    dplyr::count(nfi_tree_plot_2024_spruce_pine_stratum_ready, abonitet, sort = TRUE)
  )))
}

# Missing value report
write_log("", "  Missing values (joined table):")
check_cols <- c(
  "flateid", "treid", "treslag", "species_group",
  "brysth_dia", "dbh_cm", "beregnet_hoyde",
  "hogstkl", "abonitet", "fylnr", "hoh"
)
for (col in check_cols) {
  if (col %in% names(nfi_tree_plot_2024_spruce_pine)) {
    n_miss <- sum(is.na(nfi_tree_plot_2024_spruce_pine[[col]]))
    write_log(sprintf("    %-22s: %d missing", col, n_miss))
  } else {
    write_log(sprintf("    %-22s: column not present", col))
  }
}

# Hard stops on essential columns
essential <- c("plot_id", "treid", "treslag", "species_group", "dbh_cm", "beregnet_hoyde")
for (col in essential) {
  if (!col %in% names(nfi_tree_plot_2024_spruce_pine))
    stop(sprintf("Essential column '%s' missing from final table.", col))
  if (all(is.na(nfi_tree_plot_2024_spruce_pine[[col]])))
    stop(sprintf("Essential column '%s' is entirely NA.", col))
}
write_log("", "  Essential column checks: PASSED")


# ---- save outputs ----

write_log("", "--- Saving outputs ---")

saveRDS(trees_clean,
        file.path(data_processed_dir, "nfi_trees_2024_spruce_pine.rds"))
arrow::write_parquet(trees_clean,
        file.path(data_processed_dir, "nfi_trees_2024_spruce_pine.parquet"))
write_log("  Saved: nfi_trees_2024_spruce_pine (.rds + .parquet)")

saveRDS(plots_clean,
        file.path(data_processed_dir, "nfi_plots_2024.rds"))
arrow::write_parquet(plots_clean,
        file.path(data_processed_dir, "nfi_plots_2024.parquet"))
write_log("  Saved: nfi_plots_2024 (.rds + .parquet)")

saveRDS(nfi_tree_plot_2024_spruce_pine,
        file.path(data_processed_dir, "nfi_tree_plot_2024_spruce_pine.rds"))
arrow::write_parquet(nfi_tree_plot_2024_spruce_pine,
        file.path(data_processed_dir, "nfi_tree_plot_2024_spruce_pine.parquet"))
write_log("  Saved: nfi_tree_plot_2024_spruce_pine (.rds + .parquet)")

saveRDS(nfi_tree_plot_2024_spruce_pine_stratum_ready,
        file.path(data_processed_dir, "nfi_tree_plot_2024_spruce_pine_stratum_ready.rds"))
arrow::write_parquet(nfi_tree_plot_2024_spruce_pine_stratum_ready,
        file.path(data_processed_dir, "nfi_tree_plot_2024_spruce_pine_stratum_ready.parquet"))
write_log("  Saved: nfi_tree_plot_2024_spruce_pine_stratum_ready (.rds + .parquet)")

write_log("", sprintf("  Log written to: %s", log_file), "", "Done.")


# ---- disconnect ----

# on.exit() above handles the actual close; explicit call here is harmless.
DBI::dbDisconnect(con)
message("Oracle connection closed.")

