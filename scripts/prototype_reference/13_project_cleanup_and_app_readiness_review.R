# ==============================================================================
# R/13_project_cleanup_and_app_readiness_review.R
#
# Purpose : Project review and app-readiness assessment for nfiBuck.
#           Generates file inventories, script dependency overview,
#           stratum availability summary, and a project review report.
#
#           This script is READ-ONLY with respect to project files.
#           It does NOT delete, move, or modify any existing files.
#           It does NOT rerun taper, candidate generation, or optimization.
#
# Outputs (outputs/project_review/):
#   project_file_inventory.csv
#   output_file_inventory.csv
#   script_dependency_overview.csv
#   stratum_availability_by_species_hogstkl_abonitet.csv
#   stratum_availability_by_species_hogstkl_abonitet_fylnr.csv
#   stratum_availability_summary.txt
#   project_review_summary.txt
# ==============================================================================


# ---- setup ----

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(arrow)
})

root_dir <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
out_dir  <- file.path(root_dir, "outputs", "project_review")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

review_txt <- file.path(out_dir, "project_review_summary.txt")
cat("", file = review_txt)

rlog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = review_txt, append = TRUE)
}

rlog(
  "=============================================================",
  "  nfiBuck -- Project cleanup and app-readiness review",
  sprintf("  Generated    : %s", Sys.time()),
  sprintf("  Root         : %s", root_dir),
  "============================================================="
)


# ==============================================================================
# Part 1: File inventory
# ==============================================================================

rlog("", "=== PART 1: FILE INVENTORY ===")

# Helper: classify file by path/extension
classify_file <- function(path, fname, ext) {
  rel <- sub(paste0(root_dir, "/?"), "", path, fixed = TRUE)
  if (grepl("^R/", rel) && grepl("\\.(R|r)$", fname)) return("script")
  if (grepl("^audits/", rel)) return("audit")
  if (grepl("^data_processed/", rel)) return("input")
  if (grepl("^data_raw/", rel)) return("input")
  if (grepl("^inputs/", rel)) return("input")
  if (grepl("^docs/", rel)) return("doc")
  if (grepl("^logs/", rel)) return("log")
  if (grepl("\\.(png|pdf|svg)$", fname, ignore.case = TRUE)) return("plot")
  if (grepl("\\.(csv|txt|md)$", fname, ignore.case = TRUE)) return("output")
  if (grepl("\\.(parquet|rds)$", fname, ignore.case = TRUE)) return("output")
  return("unknown")
}

build_inventory <- function(search_dir, label) {
  all_files <- list.files(search_dir, recursive = TRUE,
                          full.names = TRUE, all.files = FALSE)
  if (length(all_files) == 0L) return(data.frame())

  info_list <- lapply(all_files, function(fp) {
    fi  <- file.info(fp)
    fn  <- basename(fp)
    ext <- tolower(tools::file_ext(fn))
    rel <- sub(paste0(normalizePath(root_dir, winslash = "/"), "/"), "", 
               normalizePath(fp, winslash = "/"), fixed = TRUE)
    data.frame(
      path            = rel,
      file_name       = fn,
      extension       = ext,
      directory       = dirname(rel),
      size_mb         = round(fi$size / 1048576, 4),
      modified_time   = format(fi$mtime, "%Y-%m-%d %H:%M:%S"),
      likely_category = classify_file(fp, fn, ext),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, info_list)
}

rlog("  Building full project file inventory...")
full_inv <- build_inventory(root_dir, "project")
full_inv <- full_inv[order(full_inv$directory, full_inv$file_name), ]

rlog(sprintf("  Total files found: %d", nrow(full_inv)))

# Category breakdown
cat_tbl <- sort(table(full_inv$likely_category), decreasing = TRUE)
rlog("  By category:")
for (nm in names(cat_tbl)) {
  rlog(sprintf("    %-12s : %d", nm, cat_tbl[[nm]]))
}

readr::write_csv(full_inv, file.path(out_dir, "project_file_inventory.csv"))
rlog(sprintf("  Saved: project_file_inventory.csv (%d files)", nrow(full_inv)))


# Outputs-only inventory
out_inv <- full_inv[grepl("^outputs/", full_inv$path), ]
readr::write_csv(out_inv, file.path(out_dir, "output_file_inventory.csv"))
rlog(sprintf("  Saved: output_file_inventory.csv (%d files)", nrow(out_inv)))


# Folder size summary for outputs/
if (nrow(out_inv) > 0L) {
  out_inv$top_folder <- sub("/.*", "", sub("^outputs/", "", out_inv$path))
  folder_sizes <- out_inv %>%
    group_by(top_folder) %>%
    summarise(n_files   = n(),
              total_mb  = round(sum(size_mb, na.rm = TRUE), 3),
              .groups = "drop") %>%
    arrange(desc(total_mb))
  rlog("", "  Output folder sizes (MB):")
  for (i in seq_len(nrow(folder_sizes))) {
    rlog(sprintf("    %-40s  %4d files  %8.3f MB",
                 folder_sizes$top_folder[i],
                 folder_sizes$n_files[i],
                 folder_sizes$total_mb[i]))
  }
}


# ==============================================================================
# Part 2: Script dependency overview
# ==============================================================================

rlog("", "=== PART 2: SCRIPT DEPENDENCY OVERVIEW ===")

scripts_dir <- file.path(root_dir, "R")
audits_dir  <- file.path(root_dir, "audits")

script_files <- c(
  list.files(scripts_dir, pattern = "\\.R$", full.names = TRUE),
  list.files(audits_dir,  pattern = "\\.R$", full.names = TRUE)
)

extract_patterns <- function(lines, pattern) {
  m <- regmatches(lines, gregexpr(pattern, lines, perl = TRUE))
  unique(unlist(m[vapply(m, length, integer(1L)) > 0L]))
}

dep_rows <- lapply(script_files, function(fp) {
  lines <- tryCatch(readLines(fp, warn = FALSE), error = function(e) character(0))

  # Libraries
  lib_lines <- grep("^library\\(", lines, value = TRUE)
  libs <- gsub("library\\(|\\)|\"", "", lib_lines)
  libs <- unique(trimws(libs))

  # source() calls
  src_lines <- grep("source\\(", lines, value = TRUE)
  srcs <- regmatches(src_lines, regexpr("[\"'][^\"']+[\"']", src_lines))
  srcs <- gsub("[\"']", "", srcs)

  # Input mentions: parquet, rds, csv reads
  inp_pat <- "[\"'][^\"']*\\.(parquet|rds|csv|xlsx|txt)[\"']"
  inp_all <- extract_patterns(lines, inp_pat)
  inp_all <- gsub("[\"']", "", inp_all)
  # Try to keep file names only that look like actual file refs
  inp_files <- inp_all[grepl("(data_processed|inputs|outputs|stratum)", inp_all)]

  # Output mentions: write_csv, write_parquet, ggsave, saveRDS, write_rds
  out_lines <- grep("(write_csv|write_parquet|ggsave|saveRDS|write_rds|write\\()", lines, value = TRUE)
  out_pat   <- "[\"'][^\"']*\\.(csv|parquet|rds|png|pdf|txt)[\"']"
  out_files <- extract_patterns(out_lines, out_pat)
  out_files <- gsub("[\"']", "", out_files)
  out_files <- basename(out_files)

  fn <- basename(fp)
  rel <- sub(paste0(normalizePath(root_dir, winslash = "/"), "/"), "",
             normalizePath(fp, winslash = "/"), fixed = TRUE)

  data.frame(
    script_path       = rel,
    script_name       = fn,
    libraries         = paste(libs,      collapse = "; "),
    source_calls      = paste(srcs,      collapse = "; "),
    likely_inputs     = paste(unique(basename(inp_files)),  collapse = "; "),
    likely_outputs    = paste(unique(out_files), collapse = "; "),
    stringsAsFactors  = FALSE
  )
})

dep_df <- do.call(rbind, dep_rows)
dep_df <- dep_df[order(dep_df$script_path), ]

readr::write_csv(dep_df, file.path(out_dir, "script_dependency_overview.csv"))
rlog(sprintf("  Saved: script_dependency_overview.csv (%d scripts/audits)",
             nrow(dep_df)))


# ==============================================================================
# Part 3: Stratum availability overview
# ==============================================================================

rlog("", "=== PART 3: STRATUM AVAILABILITY OVERVIEW ===")

strat_avail_txt <- file.path(out_dir, "stratum_availability_summary.txt")
cat("", file = strat_avail_txt)

slog <- function(...) {
  lines <- c(...)
  cat(lines, sep = "\n")
  cat(lines, sep = "\n", file = strat_avail_txt, append = TRUE)
  cat(lines, sep = "\n", file = review_txt, append = TRUE)
}

strat_file <- file.path(root_dir, "data_processed",
                         "nfi_tree_plot_2024_spruce_pine_stratum_ready.parquet")

if (!file.exists(strat_file)) {
  slog("  ERROR: Stratum file not found:", strat_file)
  stop("Stratum file missing: ", strat_file)
}

slog("  Reading stratum file:", basename(strat_file))
strat <- as.data.frame(arrow::read_parquet(strat_file))
slog(sprintf("  Rows: %d  Columns: %d", nrow(strat), ncol(strat)))
slog(sprintf("  Columns: %s", paste(names(strat), collapse = ", ")))

# Require plot_id
if (!"plot_id" %in% names(strat)) {
  slog("  ERROR: plot_id column not found in stratum file.")
  stop("plot_id is required but missing from stratum file.")
}

# Require key stratum columns
required_cols <- c("species_group", "hogstkl", "abonitet")
missing_cols  <- setdiff(required_cols, names(strat))
if (length(missing_cols) > 0L) {
  slog(sprintf("  ERROR: Missing required columns: %s",
               paste(missing_cols, collapse = ", ")))
  stop("Missing columns: ", paste(missing_cols, collapse = ", "))
}

has_fylnr <- "fylnr" %in% names(strat)
slog(sprintf("  fylnr column present: %s", has_fylnr))

# ---- 3a. By species_group x hogstkl x abonitet ----

slog("", "  -- 3a. By species_group x hogstkl x abonitet --")

agg1 <- strat %>%
  group_by(species_group, hogstkl, abonitet) %>%
  summarise(
    n_trees            = n(),
    n_plots            = n_distinct(plot_id),
    enough_plots_10    = n_distinct(plot_id) >= 10L,
    enough_plots_25    = n_distinct(plot_id) >= 25L,
    enough_plots_50    = n_distinct(plot_id) >= 50L,
    .groups = "drop"
  ) %>%
  arrange(species_group, hogstkl, abonitet)

readr::write_csv(agg1, file.path(out_dir,
  "stratum_availability_by_species_hogstkl_abonitet.csv"))

n_combos     <- nrow(agg1)
n_ge10       <- sum(agg1$enough_plots_10)
n_ge25       <- sum(agg1$enough_plots_25)
n_ge50       <- sum(agg1$enough_plots_50)

slog(
  sprintf("  Total species x hogstkl x abonitet combinations : %d", n_combos),
  sprintf("  Combinations with >= 10 plots                    : %d (%.0f%%)",
          n_ge10,  n_ge10  / n_combos * 100),
  sprintf("  Combinations with >= 25 plots                    : %d (%.0f%%)",
          n_ge25,  n_ge25  / n_combos * 100),
  sprintf("  Combinations with >= 50 plots                    : %d (%.0f%%)",
          n_ge50,  n_ge50  / n_combos * 100),
  sprintf("  Saved: stratum_availability_by_species_hogstkl_abonitet.csv (%d rows)",
          n_combos)
)

# Top 10 by n_plots
top10 <- head(agg1[order(-agg1$n_plots), ], 10L)
slog("", "  Top 10 combinations by n_plots:")
slog(sprintf("  %-14s %-8s %-9s %7s %7s",
             "species_group", "hogstkl", "abonitet", "n_plots", "n_trees"))
for (i in seq_len(nrow(top10))) {
  slog(sprintf("  %-14s %-8s %-9s %7d %7d",
               top10$species_group[i],
               as.character(top10$hogstkl[i]),
               as.character(top10$abonitet[i]),
               top10$n_plots[i],
               top10$n_trees[i]))
}

# ---- 3b. By species_group x hogstkl x abonitet x fylnr ----

if (has_fylnr) {
  slog("", "  -- 3b. By species_group x hogstkl x abonitet x fylnr --")

  agg2 <- strat %>%
    group_by(species_group, hogstkl, abonitet, fylnr) %>%
    summarise(
      n_trees         = n(),
      n_plots         = n_distinct(plot_id),
      enough_plots_10 = n_distinct(plot_id) >= 10L,
      enough_plots_25 = n_distinct(plot_id) >= 25L,
      enough_plots_50 = n_distinct(plot_id) >= 50L,
      .groups = "drop"
    ) %>%
    arrange(species_group, hogstkl, abonitet, fylnr)

  readr::write_csv(agg2, file.path(out_dir,
    "stratum_availability_by_species_hogstkl_abonitet_fylnr.csv"))

  n2_combos <- nrow(agg2)
  n2_ge10   <- sum(agg2$enough_plots_10)
  n2_ge25   <- sum(agg2$enough_plots_25)
  n2_ge50   <- sum(agg2$enough_plots_50)

  slog(
    sprintf("  Total species x hogstkl x abonitet x fylnr combinations : %d", n2_combos),
    sprintf("  Combinations with >= 10 plots                             : %d (%.0f%%)",
            n2_ge10,  n2_ge10  / n2_combos * 100),
    sprintf("  Combinations with >= 25 plots                             : %d (%.0f%%)",
            n2_ge25,  n2_ge25  / n2_combos * 100),
    sprintf("  Combinations with >= 50 plots                             : %d (%.0f%%)",
            n2_ge50,  n2_ge50  / n2_combos * 100),
    sprintf("  Saved: stratum_availability_by_species_hogstkl_abonitet_fylnr.csv (%d rows)",
            n2_combos)
  )
} else {
  slog("  fylnr column not present; skipping county-level aggregation.")
}

# ---- 3c. Overall totals ----

slog("", "  -- 3c. Overall dataset totals --")
n_sp <- length(unique(strat$species_group))
n_hk <- length(unique(strat$hogstkl))
n_ab <- length(unique(strat$abonitet))
n_pl <- n_distinct(strat$plot_id)
n_tr <- nrow(strat)

slog(
  sprintf("  Distinct species_group : %d (%s)",
          n_sp, paste(sort(unique(strat$species_group)), collapse = ", ")),
  sprintf("  Distinct hogstkl       : %d (%s)",
          n_hk, paste(sort(unique(strat$hogstkl)), collapse = ", ")),
  sprintf("  Distinct abonitet      : %d (%s)",
          n_ab, paste(sort(unique(strat$abonitet)), collapse = ", ")),
  sprintf("  Total plots            : %d", n_pl),
  sprintf("  Total trees            : %d", n_tr)
)


# ==============================================================================
# Part 4: Project review summary and decisions needed
# ==============================================================================

rlog(
  "",
  "=== PART 4: RECOMMENDATIONS ===",
  "",
  "--- Cleanup recommendations ---",
  "  1. Extract shared functions into R/helpers_*.R (8 helper files proposed).",
  "     Priority: helpers_dp_optimizer.R, helpers_candidate_generation.R,",
  "     helpers_valuation.R — these are the largest blocks of duplicated code.",
  "  2. Extract audit utilities (record, alog, diag_val, %||%) into",
  "     R/helpers_audit.R and source from all audit scripts.",
  "  3. Move outputs/_archive/ candidate folders after confirmed no longer needed:",
  "     batch_optimizer_tests, optimizer_tests, valuation_tests,",
  "     price_matrix_tests, product_volume_tests.",
  "  4. scripts 08-12 should remain as accepted reference runs.",
  "     Do not refactor them until helpers are tested independently.",
  "",
  "--- App design recommendations ---",
  "  1. Build v1 as sampled-plot mode only (default 10 plots).",
  "     Full-stratum runs offline only.",
  "  2. Implement price-matrix validation as a standalone function in",
  "     helpers_valuation.R before building the Shiny module.",
  "  3. v1: spruce and pine only; uploaded CSV/Excel matrices.",
  "  4. All estimates clearly labelled as sampled-plot approximations.",
  "  5. Do not apply NFI expansion factors in v1.",
  "",
  "=== DECISIONS NEEDED ===",
  "",
  "The following questions require user decisions before app development begins:",
  "",
  "  1. DEFAULT PLOT SAMPLE SIZE",
  "     Should the app default to 10 plots per stratum sample?",
  "     (Current prototype used 21,238 trees across 1,239 plots for the full run.)",
  "",
  "  2. ALLOWED SAMPLE SIZES",
  "     Should users be allowed to choose 25 or 50 plots in addition to 10?",
  "     Should a minimum plot count be enforced (e.g., refuse if < 5 plots available)?",
  "",
  "  3. MISSING PRICE CLASSES — DEFAULT BEHAVIOUR",
  "     Should missing price classes be excluded by default",
  "     (candidates remain unpriced, not selected)?",
  "     Or should missing prices default to zero (treated as waste)?",
  "",
  "  4. ZERO-PRICE OVERRIDE",
  "     Should users be allowed to check a box to treat missing prices as zero?",
  "     If yes, all outputs must be clearly flagged as 'zero-price override active'.",
  "",
  "  5. SPECIES SCOPE",
  "     Should the first app version be spruce and pine only?",
  "     (No other species in the current prototype.)",
  "",
  "  6. PRICE MATRIX INPUT",
  "     Should the first app allow only uploaded Excel/CSV matrices?",
  "     Or also allow manual editing of a price table in the browser UI?",
  "     (Manual editing is harder to validate.)",
  "",
  "  7. FULL-STRATUM RUNS",
  "     Should full-stratum runs (all plots in a stratum) be offline only,",
  "     or should the app optionally support them with a time warning?",
  "",
  "  8. COUNTY FILTER",
  "     Should the app support fylnr (county) as a stratum filter in v1?",
  ""
)


# ==============================================================================
# Done
# ==============================================================================

rlog(
  "=============================================================",
  "  Script 13 complete.",
  sprintf("  Outputs written to: %s", out_dir),
  "============================================================="
)

cat("Script 13 complete.\n")
