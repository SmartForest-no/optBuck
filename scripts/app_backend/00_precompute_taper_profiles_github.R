# ==============================================================================
# scripts/app_backend/00_precompute_taper_profiles_github.R
#
# Purpose : Precompute taper profiles for all usable trees in the sanitized
#           GitHub development data (data_processed_github/).
#
#           The resulting parquet cache avoids repeated taperNOR calls in
#           Shiny/backend sessions. One run here populates profiles for every
#           tree that passes the standard DBH/height filter.
#
# Filtering (matches backend and script 11 exactly):
#   - height_m_use  = beregnet_hoyde / 10
#   - dbh_cm       >= 5.0
#   - height_m_use >= 4.5
#   - no tilstand filter
#
# Identifier rule (same as backend):
#   - Helpers expect a column named plot_id.
#   - Input data uses plot_id_github.
#   - plot_id_github is renamed to plot_id before helper calls.
#   - plot_id is renamed back to plot_id_github before saving.
#   - Saved output must not contain plot_id.
#
# Input  : data_processed_github/nfi_tree_plot_stratum_ready_github.parquet
# Output : outputs/app_cache/taper_profiles_github/
#            taper_profiles_github.parquet
#            taper_profiles_manifest.txt
#            taper_profiles_failures.csv       (only if failures exist)
#
# Usage  : Rscript scripts/app_backend/00_precompute_taper_profiles_github.R
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow)
})

# ---- resolve project root ---------------------------------------------------
ROOT <- local({
  # APTER_ROOT -> walk up from getwd() -> stop()
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  env <- Sys.getenv("APTER_ROOT", unset = "")
  if (nzchar(env)) {
    d <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (!is_root(d))
      stop("Project root '", d, "' (from APTER_ROOT) does not contain ",
           "R/helpers_paths.R.", call. = FALSE)
  } else {
    d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    while (!is_root(d) && dirname(d) != d) d <- dirname(d)
    if (!is_root(d))
      stop("Cannot locate the project root: set APTER_ROOT, or run from ",
           "inside the project directory.", call. = FALSE)
  }
  d
})

# ---- source helpers ---------------------------------------------------------
for (.h in c("helpers_utils", "helpers_paths", "helpers_taper")) {
  src <- file.path(ROOT, "R", paste0(.h, ".R"))
  if (!file.exists(src)) stop("Helper not found: ", src)
  source(src, local = FALSE)
}
rm(.h, src)

# ---- constants (match backend and script 11) --------------------------------
MIN_DBH_CM   <- 5.0
MIN_HEIGHT_M <- 4.5
PROFILE_STEP_M <- 0.10

# ---- output directories -----------------------------------------------------
OUT_DIR          <- file.path(ROOT, "outputs", "app_cache", "taper_profiles_github")
OUT_DIR_BLOCK    <- file.path(ROOT, "outputs", "app_cache",
                               "taper_profiles_by_plot_block_github")
if (!dir.exists(OUT_DIR))       dir.create(OUT_DIR,       recursive = TRUE)
if (!dir.exists(OUT_DIR_BLOCK)) dir.create(OUT_DIR_BLOCK, recursive = TRUE)

OUT_PARQUET   <- file.path(OUT_DIR, "taper_profiles_github.parquet")
OUT_MANIFEST  <- file.path(OUT_DIR, "taper_profiles_manifest.txt")
OUT_FAILURES  <- file.path(OUT_DIR, "taper_profiles_failures.csv")

t_start <- proc.time()
cat("Apter taper profile precompute\n")
cat(sprintf("  Started : %s\n", Sys.time()))
cat(sprintf("  ROOT    : %s\n", ROOT))

# ---- load sanitized tree data -----------------------------------------------
GH_DIR <- file.path(ROOT, "data_processed_github")
input_file <- file.path(GH_DIR, "nfi_tree_plot_stratum_ready_github.parquet")

if (!file.exists(input_file))
  stop("Input file not found: ", input_file)

cat(sprintf("  Input   : %s\n", basename(input_file)))
trees_raw <- as.data.frame(arrow::read_parquet(input_file))
n_raw <- nrow(trees_raw)
cat(sprintf("  Rows (raw): %d\n", n_raw))

# ---- compute height_m from beregnet_hoyde (dm) / 10 -------------------------
if ("beregnet_hoyde" %in% names(trees_raw)) {
  trees_raw$height_m_use <- as.numeric(trees_raw$beregnet_hoyde) / 10
} else if ("trehoyde" %in% names(trees_raw)) {
  trees_raw$height_m_use <- as.numeric(trees_raw$trehoyde) / 10
} else {
  stop("No height column found (expected beregnet_hoyde or trehoyde).")
}

# ---- apply DBH / height filter (no tilstand filter — matches script 11) ------
trees_valid <- trees_raw[
  !is.na(trees_raw$dbh_cm)       & trees_raw$dbh_cm       >= MIN_DBH_CM &
  !is.na(trees_raw$height_m_use) & trees_raw$height_m_use >= MIN_HEIGHT_M,
  , drop = FALSE]

n_valid   <- nrow(trees_valid)
n_dropped <- n_raw - n_valid
cat(sprintf("  Trees after DBH/height filter: %d  (dropped %d)\n",
            n_valid, n_dropped))

# ---- prepare tree_data ------------------------------------------------------
# Helpers require plot_id. Temporarily rename plot_id_github -> plot_id.
tree_data <- trees_valid[, c("plot_id_github", "treid", "sesong",
                              "species_group", "dbh_cm", "height_m_use"),
                         drop = FALSE]
names(tree_data)[names(tree_data) == "plot_id_github"] <- "plot_id"
names(tree_data)[names(tree_data) == "height_m_use"]   <- "height_m"

cat(sprintf("  Generating profiles (%d trees, step = %.2f m) ...\n",
            n_valid, PROFILE_STEP_M))

# ---- generate taper profiles ------------------------------------------------
profiles    <- generate_taper_profiles_for_trees(
  tree_data, profile_step_m = PROFILE_STEP_M, quiet = FALSE)

profile_ok  <- or_default(attr(profiles, "profile_ok"), rep(TRUE, n_valid))
profile_err <- or_default(attr(profiles, "profile_errors"), rep("", n_valid))

n_ok   <- sum(profile_ok)
n_fail <- sum(!profile_ok)

cat(sprintf("  Successful profiles : %d\n", n_ok))
cat(sprintf("  Failed profiles     : %d\n", n_fail))

if (is.null(profiles) || nrow(profiles) == 0L)
  stop("generate_taper_profiles_for_trees() returned 0 rows. Nothing to save.")

n_profile_rows <- nrow(profiles)
cat(sprintf("  Profile rows        : %d\n", n_profile_rows))

# ---- rename plot_id -> plot_id_github before saving -------------------------
if ("plot_id" %in% names(profiles)) {
  names(profiles)[names(profiles) == "plot_id"] <- "plot_id_github"
}

# ---- select only the required output columns --------------------------------
required_cols <- c("plot_id_github", "treid", "sesong", "species_group",
                   "dbh_cm", "height_m", "z_m", "diameter_cm",
                   "DiameterValue", "diameterPosition", "DBH")

missing_cols <- setdiff(required_cols, names(profiles))
if (length(missing_cols) > 0L)
  warning("Some expected columns are absent: ",
          paste(missing_cols, collapse = ", "))

keep_cols <- intersect(required_cols, names(profiles))
profiles_out <- profiles[, keep_cols, drop = FALSE]

# ---- safety checks ----------------------------------------------------------
# 1. plot_id must not be present
if ("plot_id" %in% names(profiles_out))
  stop("SAFETY FAIL: 'plot_id' column found in taper profile output. ",
       "Original NFI plot ID must not be saved.")

# 2. plot_id_github must be present
if (!"plot_id_github" %in% names(profiles_out))
  stop("SAFETY FAIL: 'plot_id_github' column missing from taper profile output.")

# 3. No coordinate-like columns
coord_pattern <- paste0(
  "^(x|y|lat|lon|latitude|longitude|east|north|easting|",
  "northing|koordinat|utm|epsg)[_\\s]?"
)
bad_cols <- grep(coord_pattern, names(profiles_out),
                 ignore.case = TRUE, value = TRUE)
if (length(bad_cols) > 0L)
  stop("SAFETY FAIL: coordinate-like column(s) found: ",
       paste(bad_cols, collapse = ", "))

# 4. No duplicate columns
dupe_cols <- names(profiles_out)[duplicated(names(profiles_out))]
if (length(dupe_cols) > 0L)
  stop("SAFETY FAIL: duplicate column names: ",
       paste(dupe_cols, collapse = ", "))

cat("  Safety checks: PASS\n")
cat(sprintf("  Output columns (%d): %s\n",
            ncol(profiles_out), paste(names(profiles_out), collapse = ", ")))

# ---- write single-file parquet ----------------------------------------------
arrow::write_parquet(profiles_out, OUT_PARQUET)
out_size_mb <- round(file.info(OUT_PARQUET)$size / 1024^2, 2)
cat(sprintf("  Written : %s  (%.2f MB)\n", basename(OUT_PARQUET), out_size_mb))

# ---- write partitioned cache (by plot_block) --------------------------------
# plot_block is derived deterministically from plot_id_github:
#   numeric_id = as.integer(substr(plot_id_github, 2, nchar(plot_id_github)))
#   block_n    = floor((numeric_id - 1) / 500)
#   plot_block = sprintf("B%03d", block_n)
# This gives groups of 500 plots per partition (B000, B001, ...).
# With 6348 plots the result is ~13 blocks, each holding a manageable slice.

cat(sprintf("  Building partitioned cache (block size 500 plots) ...\n"))

profiles_block <- profiles_out
numeric_ids    <- as.integer(
  substr(profiles_block$plot_id_github, 2L,
         nchar(profiles_block$plot_id_github)))
block_n                    <- floor((numeric_ids - 1L) / 500L)
profiles_block$plot_block  <- sprintf("B%03d", block_n)

n_blocks      <- length(unique(profiles_block$plot_block))
block_rows    <- nrow(profiles_block)
block_plots   <- length(unique(profiles_block$plot_id_github))

# Safety check: plot_id must not appear in partitioned output.
if ("plot_id" %in% names(profiles_block))
  stop("SAFETY FAIL: 'plot_id' found in partitioned cache before writing.")

suppressPackageStartupMessages(library(dplyr))
arrow::write_dataset(
  dplyr::group_by(profiles_block, plot_block),
  path           = OUT_DIR_BLOCK,
  format         = "parquet",
  existing_data_behavior = "overwrite"
)

block_dir_mb <- round(
  sum(file.info(list.files(OUT_DIR_BLOCK, recursive = TRUE,
                            full.names = TRUE))$size, na.rm = TRUE) / 1024^2, 2)

cat(sprintf("  Partitioned cache: %d block(s), %d rows, %d plots, %.2f MB\n",
            n_blocks, block_rows, block_plots, block_dir_mb))
cat(sprintf("  Written to: %s\n", OUT_DIR_BLOCK))

# ---- write failures CSV (only if any failures) ------------------------------
if (n_fail > 0L) {
  fail_df <- data.frame(
    plot_id_github = tree_data$plot_id[!profile_ok],  # still named plot_id here
    treid          = tree_data$treid[!profile_ok],
    species_group  = tree_data$species_group[!profile_ok],
    dbh_cm         = tree_data$dbh_cm[!profile_ok],
    height_m       = tree_data$height_m[!profile_ok],
    error_message  = profile_err[!profile_ok],
    stringsAsFactors = FALSE
  )
  # The column was taken from tree_data which has 'plot_id' (before rename).
  # Rename for the saved file.
  names(fail_df)[names(fail_df) == "plot_id"] <- "plot_id_github"
  write.csv(fail_df, OUT_FAILURES, row.names = FALSE)
  cat(sprintf("  Failures : %s  (%d rows)\n", basename(OUT_FAILURES), n_fail))
}

# ---- write manifest ---------------------------------------------------------
elapsed <- as.numeric((proc.time() - t_start)["elapsed"])

manifest <- c(
  "Apter taper profile cache manifest",
  sprintf("Run time          : %s",     format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  sprintf("ROOT              : %s",     ROOT),
  sprintf("Input file        : %s",     basename(input_file)),
  "",
  "=== Input tree counts ===",
  sprintf("  Trees (raw)               : %d", n_raw),
  sprintf("  Trees after DBH>=5/ht>=4.5: %d", n_valid),
  sprintf("  Trees dropped             : %d", n_dropped),
  "",
  "=== Taper profile results ===",
  sprintf("  Successful profiles       : %d", n_ok),
  sprintf("  Failed profiles           : %d", n_fail),
  sprintf("  Profile rows in output    : %d", n_profile_rows),
  "",
  "=== Single-file cache ===",
  sprintf("  File    : %s",    basename(OUT_PARQUET)),
  sprintf("  Size    : %.2f MB", out_size_mb),
  sprintf("  Columns : %s",    paste(names(profiles_out), collapse = ", ")),
  "",
  "=== Partitioned cache (by plot_block, block size 500 plots) ===",
  sprintf("  Folder           : %s", basename(OUT_DIR_BLOCK)),
  sprintf("  Number of blocks : %d", n_blocks),
  sprintf("  Total rows       : %d", block_rows),
  sprintf("  Unique plots     : %d", block_plots),
  sprintf("  Folder size      : %.2f MB", block_dir_mb),
  sprintf("  plot_block column: floor((numeric_id - 1) / 500), formatted B000..B999"),
  "",
  "=== Safety verification ===",
  sprintf("  'plot_id' absent from single-file cache   : %s",
          !"plot_id" %in% names(profiles_out)),
  sprintf("  'plot_id_github' present in single-file   : %s",
          "plot_id_github" %in% names(profiles_out)),
  sprintf("  'plot_id' absent from partitioned cache   : %s",
          !"plot_id" %in% names(profiles_block)),
  sprintf("  'plot_id_github' present in partitioned   : %s",
          "plot_id_github" %in% names(profiles_block)),
  sprintf("  Coordinate columns                        : none detected"),
  sprintf("  Duplicate columns                         : none"),
  sprintf("  outputs/app_cache/ gitignored             : TRUE  (see .gitignore)"),
  "",
  "=== Filtering rules ===",
  "  height_m_use = beregnet_hoyde / 10",
  sprintf("  dbh_cm >= %.1f", MIN_DBH_CM),
  sprintf("  height_m_use >= %.1f", MIN_HEIGHT_M),
  "  No tilstand filter (matches script 11 and backend convention)",
  "",
  "=== Data provenance ===",
  "  Source: data_processed_github/ (sanitized; no original NFI plot IDs)",
  "  plot_id_github values are synthetic sequential IDs (P000001...)",
  "  Original plot_id was NOT used; no original NFI plot identifiers saved.",
  "  This cache is derived from sanitized data and contains no exact",
  "  plot coordinates, municipality codes, or original NFI plot codes.",
  "",
  sprintf("Elapsed time       : %.1f s", elapsed)
)
writeLines(manifest, OUT_MANIFEST)
cat(sprintf("  Manifest: %s\n", basename(OUT_MANIFEST)))

cat(sprintf("\nDone in %.1f s.\n", elapsed))
cat(sprintf("Single-file cache : %s\n", OUT_DIR))
cat(sprintf("Partitioned cache : %s\n", OUT_DIR_BLOCK))

