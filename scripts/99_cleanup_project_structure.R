# ==============================================================================
# scripts/99_cleanup_project_structure.R
#
# Purpose : Reorganise the nfiBuck repository after the prototype phase.
#           Numbered R scripts and old audits are moved to prototype_reference/
#           subfolders.  Old prototype output folders are moved to
#           outputs/_archive/.  Nothing is permanently deleted.
#
# Usage   :
#   DRY_RUN = TRUE  (default) — prints all planned moves, writes manifest CSV.
#   DRY_RUN = FALSE            — executes the moves.
#
# Safety rules:
#   - safe_move() never overwrites an existing destination.
#   - safe_move() only moves; it never deletes source without confirming copy.
#   - This script does NOT modify file contents.
#   - This script does NOT touch: inputs/, data_processed/, helpers_*.R,
#     audit_14, audit_15, script 14, script 15, docs/, or price_matrices/.
#
# Outputs :
#   outputs/project_review/post_cleanup_file_inventory.csv
#   outputs/project_review/post_cleanup_summary.txt
#   docs/CLEANUP_MANIFEST.md  (written separately)
#
# To execute actual cleanup, change DRY_RUN to FALSE and re-run.
# ==============================================================================

DRY_RUN      <- FALSE
ARCHIVE_DATE <- format(Sys.Date(), "%Y%m%d")

root_dir     <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"
archive_root <- file.path("_archive", paste0("prototype_", ARCHIVE_DATE))

# Paths resolved from root
full_archive <- file.path(root_dir, "outputs", archive_root)
review_dir   <- file.path(root_dir, "outputs", "project_review")

cat(sprintf("=== nfiBuck Cleanup Script ===\n"))
cat(sprintf("DRY_RUN      : %s\n", DRY_RUN))
cat(sprintf("ARCHIVE_DATE : %s\n", ARCHIVE_DATE))
cat(sprintf("root_dir     : %s\n", root_dir))
cat(sprintf("archive_root : outputs/%s\n\n", archive_root))


# ==============================================================================
# Manifest accumulator
# ==============================================================================

.manifest <- data.frame(
  from_rel  = character(0),
  to_rel    = character(0),
  type      = character(0),
  action    = character(0),
  status    = character(0),
  note      = character(0),
  stringsAsFactors = FALSE
)

manifest_add <- function(from_rel, to_rel, type, status, note = "") {
  .manifest <<- rbind(.manifest, data.frame(
    from_rel  = from_rel,
    to_rel    = to_rel,
    type      = type,
    action    = "move",
    status    = status,
    note      = note,
    stringsAsFactors = FALSE
  ))
}


# ==============================================================================
# safe_move: move a file or folder, record in manifest, never overwrite
# ==============================================================================

safe_move <- function(from_rel, to_rel, note = "") {
  from <- file.path(root_dir, from_rel)
  to   <- file.path(root_dir, to_rel)

  # Determine type
  src_exists  <- file.exists(from) || dir.exists(from)
  is_dir      <- dir.exists(from)
  type        <- if (is_dir) "folder" else "file"

  if (!src_exists) {
    msg <- sprintf("  [SKIP  ] %-55s (source missing)", from_rel)
    cat(msg, "\n")
    manifest_add(from_rel, to_rel, type, "SKIP", "source not found")
    return(invisible(FALSE))
  }

  # Check destination does not already exist
  dest_exists <- file.exists(to) || dir.exists(to)
  if (dest_exists) {
    msg <- sprintf("  [SKIP  ] %-55s (dest exists)", from_rel)
    cat(msg, "\n")
    manifest_add(from_rel, to_rel, type, "SKIP", "destination already exists")
    return(invisible(FALSE))
  }

  # Ensure parent directory exists (or announce it in dry-run)
  to_parent <- dirname(to)
  if (!dir.exists(to_parent)) {
    if (!DRY_RUN) dir.create(to_parent, recursive = TRUE, showWarnings = FALSE)
  }

  if (DRY_RUN) {
    msg <- sprintf("  [PLAN  ] %-55s -> %s", from_rel, to_rel)
    cat(msg, "\n")
    manifest_add(from_rel, to_rel, type, "planned", note)
    return(invisible(TRUE))
  }

  # Attempt rename (atomic on same volume)
  ok <- file.rename(from, to)

  if (!ok) {
    # Fall-back: copy + remove (needed across volumes or if rename fails)
    if (is_dir) {
      ok_copy <- all(file.copy(from, to_parent, recursive = TRUE,
                               overwrite = FALSE))
      if (ok_copy) {
        ok_del <- unlink(from, recursive = TRUE) == 0L
        ok     <- ok_copy && ok_del
      }
    } else {
      ok_copy <- file.copy(from, to, overwrite = FALSE)
      if (ok_copy) {
        ok_del <- file.remove(from)
        ok     <- ok_copy && ok_del
      }
    }
  }

  status <- if (ok) "MOVED" else "ERROR"
  msg    <- sprintf("  [%-5s ] %-55s -> %s", status, from_rel, to_rel)
  cat(msg, "\n")
  manifest_add(from_rel, to_rel, type, status, note)
  invisible(ok)
}


# ==============================================================================
# Count files before cleanup (for summary)
# ==============================================================================

count_files_in_dir <- function(rel_dir) {
  full <- file.path(root_dir, rel_dir)
  if (!dir.exists(full)) return(0L)
  length(list.files(full, recursive = TRUE, all.files = FALSE))
}

before_R      <- count_files_in_dir("R")
before_audits <- count_files_in_dir("audits")
before_out    <- count_files_in_dir("outputs")
before_logs   <- count_files_in_dir("logs")
before_total  <- before_R + before_audits + before_out + before_logs +
                 count_files_in_dir("docs")

cat(sprintf("Files before cleanup:\n"))
cat(sprintf("  R/              : %d\n", before_R))
cat(sprintf("  audits/         : %d\n", before_audits))
cat(sprintf("  outputs/        : %d\n", before_out))
cat(sprintf("  logs/           : %d\n", before_logs))
cat(sprintf("  docs/           : %d\n", count_files_in_dir("docs")))
cat(sprintf("  Total (these)   : %d\n\n", before_total))


# ==============================================================================
# Section 1: R/ — move numbered prototype scripts to scripts/prototype_reference/
# ==============================================================================

cat("--- Section 1: R/ numbered scripts → scripts/prototype_reference/ ---\n")

# Numbered scripts to move (00-15, keeping helpers_*.R in place)
numbered_scripts <- list.files(
  file.path(root_dir, "R"),
  pattern     = "^[0-9]+_.*\\.R$",
  full.names  = FALSE
)

for (fname in numbered_scripts) {
  safe_move(
    from_rel = file.path("R", fname),
    to_rel   = file.path("scripts", "prototype_reference", fname),
    note     = "prototype script"
  )
}
cat("\n")


# ==============================================================================
# Section 2: audits/ — move prototype audits to audits/prototype_reference/
#            Keep audit_14 and audit_15 in place (current audits)
# ==============================================================================

cat("--- Section 2: audits/ prototype audits → audits/prototype_reference/ ---\n")

# Keep these visible
current_audit_patterns <- c("^audit_14_", "^audit_15_")

all_audits <- list.files(
  file.path(root_dir, "audits"),
  pattern    = "^audit_.*\\.R$",
  full.names = FALSE
)

for (fname in all_audits) {
  is_current <- any(vapply(current_audit_patterns, function(p)
    grepl(p, fname), logical(1)))
  if (!is_current) {
    safe_move(
      from_rel = file.path("audits", fname),
      to_rel   = file.path("audits", "prototype_reference", fname),
      note     = "prototype audit"
    )
  } else {
    cat(sprintf("  [KEEP  ] audits/%s (current audit)\n", fname))
  }
}
cat("\n")


# ==============================================================================
# Section 3: outputs/ — archive old prototype output folders
#
# SAFE TO ARCHIVE (not referenced by any current script):
#   batch_optimizer_tests/
#   example_sample_bucking/
#   pine_smoke_bucking/
#   price_matrix_tests/
#   product_volume_tests/
#   valuation_tests/
#   selected_stratum_bucking/
#
# UNCERTAIN — keep in place (actively used by script 15 / audit 15):
#   optimizer_tests/            → used by audit_15 (reference selected_logs)
#   mixed_species_example_bucking/ → used by script 15 (reference + input trees)
#   stem_profiles/              → used by script 15 (tree 242251 profile inputs)
#   stratum_tests/              → used by archived scripts 10/11 (parquet data)
#
# KEEP VISIBLE (current outputs):
#   project_review/             → planning artifacts
#   helper_validation/          → current audit 14 outputs
#   core_engine_helpers_regression/ → current audit 15 outputs
#   selected_stratum_summary/   → final accepted stratum summary
# ==============================================================================

cat("--- Section 3: outputs/ → outputs/_archive/prototype_outputs/ ---\n")

outputs_to_archive <- c(
  "outputs/batch_optimizer_tests",
  "outputs/example_sample_bucking",
  "outputs/pine_smoke_bucking",
  "outputs/price_matrix_tests",
  "outputs/product_volume_tests",
  "outputs/valuation_tests",
  "outputs/selected_stratum_bucking"
)

outputs_uncertain <- c(
  "outputs/optimizer_tests",
  "outputs/mixed_species_example_bucking",
  "outputs/stem_profiles",
  "outputs/stratum_tests"
)

for (odir in outputs_to_archive) {
  base <- basename(odir)
  safe_move(
    from_rel = odir,
    to_rel   = file.path("outputs", archive_root, base),
    note     = "prototype output"
  )
}

cat("\n")
cat("  [UNCERTAIN] The following output folders are kept in place because\n")
cat("  they are referenced by script 15 / audit 15 or by archived scripts.\n")
cat("  Move them manually only after verifying no active script depends on them:\n")
for (ud in outputs_uncertain) {
  cat(sprintf("    KEPT UNCERTAIN: %s\n", ud))
  manifest_add(ud, ud, "folder", "KEPT_UNCERTAIN",
    "actively referenced by script 15/audit 15 or archived scripts")
}
cat("\n")


# ==============================================================================
# Section 4: data_raw/ — empty folder, add README
# ==============================================================================

cat("--- Section 4: data_raw/ — add README.md to empty folder ---\n")

data_raw_path  <- file.path(root_dir, "data_raw")
data_raw_readme <- file.path(data_raw_path, "README.md")

if (!dir.exists(data_raw_path)) {
  cat("  [SKIP  ] data_raw/ not found\n")
} else if (length(list.files(data_raw_path)) == 0L) {
  readme_text <- paste0(
    "# data_raw/\n\n",
    "This folder is intentionally empty in the repository.\n\n",
    "Raw NFI data are read directly from the Oracle database\n",
    "via `R/00_prepare_nfi_2024_data.R` and are not stored here.\n\n",
    "Processed data live in `data_processed/`.\n"
  )
  if (!DRY_RUN) {
    writeLines(readme_text, data_raw_readme)
    cat(sprintf("  [WRITE ] data_raw/README.md\n"))
  } else {
    cat(sprintf("  [PLAN  ] Create data_raw/README.md (folder is empty)\n"))
  }
  manifest_add("data_raw/", "data_raw/README.md", "file",
    if (DRY_RUN) "planned" else "WRITTEN", "README for empty raw data folder")
} else {
  cat(sprintf("  [SKIP  ] data_raw/ not empty; no README added\n"))
}
cat("\n")


# ==============================================================================
# Section 5: logs/ — archive old logs (keep logs/_archive/)
# ==============================================================================

cat("--- Section 5: logs/ → logs/_archive/ ---\n")

all_logs <- list.files(file.path(root_dir, "logs"),
                       full.names = FALSE, recursive = FALSE)

# Keep only non-hidden, non-directory log files
log_files <- all_logs[!grepl("^_", all_logs) &
                       !dir.exists(file.path(root_dir, "logs", all_logs))]

for (lf in log_files) {
  safe_move(
    from_rel = file.path("logs", lf),
    to_rel   = file.path("logs", "_archive", lf),
    note     = "old prototype log"
  )
}
cat("\n")


# ==============================================================================
# Summary of planned/executed moves
# ==============================================================================

n_planned <- sum(.manifest$status == "planned", na.rm = TRUE)
n_moved   <- sum(.manifest$status == "MOVED",   na.rm = TRUE)
n_skipped <- sum(.manifest$status %in% c("SKIP", "KEEP_UNCERTAIN"), na.rm = TRUE)
n_errors  <- sum(.manifest$status == "ERROR",   na.rm = TRUE)
n_uncertain <- sum(.manifest$status == "KEPT_UNCERTAIN", na.rm = TRUE)

cat("=== SUMMARY ===\n")
if (DRY_RUN) {
  cat(sprintf("  Files/folders planned to move : %d\n", n_planned))
  cat(sprintf("  Uncertain (kept in place)     : %d\n", n_uncertain))
  cat(sprintf("  Skipped (source missing/dest  : %d\n", n_skipped))
} else {
  cat(sprintf("  Files/folders moved           : %d\n", n_moved))
  cat(sprintf("  Errors                        : %d\n", n_errors))
  cat(sprintf("  Uncertain (kept in place)     : %d\n", n_uncertain))
  cat(sprintf("  Skipped                       : %d\n", n_skipped))
}

cat("\n")
cat("  Folders that would become cleaner:\n")
cat("    R/          -- only helpers_*.R remain\n")
cat("    audits/     -- only audit_14 and audit_15 remain\n")
cat("    outputs/    -- archive candidates moved under outputs/_archive/\n")
cat("    logs/       -- moved to logs/_archive/\n")

cat("\n")
cat("  Folders with archived content:\n")
cat(sprintf("    outputs/%s/\n", archive_root))
cat("    audits/prototype_reference/\n")
cat("    scripts/prototype_reference/\n")
cat("    logs/_archive/\n")

if (DRY_RUN) {
  cat("\n")
  cat("  *** DRY RUN COMPLETE — no files were moved. ***\n")
  cat("  To run the actual cleanup, set DRY_RUN <- FALSE and re-run.\n")
}


# ==============================================================================
# Write manifest CSV
# ==============================================================================

dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)

manifest_path <- file.path(review_dir, "cleanup_manifest.csv")
write.csv(.manifest, manifest_path, row.names = FALSE, na = "")
cat(sprintf("\n  Manifest written: %s\n", manifest_path))


# ==============================================================================
# Post-cleanup inventory (if not dry run, show final counts)
# ==============================================================================

write_post_cleanup_summary <- function() {
  after_R      <- count_files_in_dir("R")
  after_audits <- count_files_in_dir("audits")
  after_out    <- count_files_in_dir("outputs")
  after_scripts <- count_files_in_dir("scripts")

  lines <- c(
    "=== Post-Cleanup Summary ===",
    sprintf("Date          : %s", Sys.time()),
    sprintf("DRY_RUN       : %s", DRY_RUN),
    sprintf("ARCHIVE_DATE  : %s", ARCHIVE_DATE),
    "",
    "--- File counts ---",
    sprintf("R/              before: %d  after: %d  (helpers_*.R only)", before_R, after_R),
    sprintf("audits/         before: %d  after: %d  (audit_14, audit_15 only)", before_audits, after_audits),
    sprintf("outputs/        before: %d  after: %d", before_out, after_out),
    sprintf("scripts/        created: %d  (prototype_reference/ only)", after_scripts),
    "",
    "--- Move counts ---",
    sprintf("Planned moves   : %d", n_planned),
    sprintf("Executed moves  : %d", n_moved),
    sprintf("Errors          : %d", n_errors),
    sprintf("Uncertain (kept): %d", n_uncertain),
    sprintf("Skipped         : %d", n_skipped),
    "",
    "--- Visible folders after cleanup ---",
    "  R/                           -- helpers only",
    "  scripts/prototype_reference/ -- numbered scripts 00-15",
    "  audits/                      -- audit_14, audit_15",
    "  audits/prototype_reference/  -- audits 05-12",
    "  outputs/project_review/      -- planning inventory",
    "  outputs/helper_validation/   -- audit 14 outputs",
    "  outputs/core_engine_helpers_regression/ -- audit 15 outputs",
    "  outputs/selected_stratum_summary/       -- final accepted stratum summary",
    "",
    "--- Archived folders ---",
    sprintf("  outputs/%s/", archive_root),
    "    batch_optimizer_tests/",
    "    example_sample_bucking/",
    "    pine_smoke_bucking/",
    "    price_matrix_tests/",
    "    product_volume_tests/",
    "    valuation_tests/",
    "    selected_stratum_bucking/",
    "  logs/_archive/",
    "",
    "--- Uncertain / kept in place ---",
    "  outputs/optimizer_tests/           (used by script 15 / audit 15)",
    "  outputs/mixed_species_example_bucking/  (used by script 15 / audit 15)",
    "  outputs/stem_profiles/             (used by script 15 / audit 15)",
    "  outputs/stratum_tests/             (used by archived scripts 10/11)",
    "",
    "--- Safety checks ---",
    sprintf("  helpers_*.R present in R/          : %s",
            all(file.exists(file.path(root_dir, "R",
                    c("helpers_paths.R","helpers_products.R","helpers_valuation.R",
                      "helpers_audit.R","helpers_taper.R",
                      "helpers_candidate_generation.R","helpers_dp_optimizer.R"))))),
    sprintf("  audit_14 present in audits/        : %s",
            length(list.files(file.path(root_dir,"audits"),
                              "^audit_14")) > 0),
    sprintf("  audit_15 present in audits/        : %s",
            length(list.files(file.path(root_dir,"audits"),
                              "^audit_15")) > 0),
    sprintf("  prototype scripts in scripts/prototype_reference/: %s",
            length(list.files(file.path(root_dir,"scripts","prototype_reference"),
                              "^[0-9]+_.*\\.R$")) > 0),
    sprintf("  inputs/ unmodified                 : %s",
            dir.exists(file.path(root_dir, "inputs"))),
    sprintf("  data_processed/ unmodified         : %s",
            dir.exists(file.path(root_dir, "data_processed")))
  )

  out_path <- file.path(review_dir, "post_cleanup_summary.txt")
  writeLines(lines, out_path)
  cat(sprintf("  Summary written: %s\n", out_path))

  # Post-cleanup file inventory
  all_files <- list.files(root_dir, recursive = TRUE,
                          full.names = FALSE, all.files = FALSE)
  inv <- data.frame(
    path      = all_files,
    directory = dirname(all_files),
    file_name = basename(all_files),
    stringsAsFactors = FALSE
  )
  inv_path <- file.path(review_dir, "post_cleanup_file_inventory.csv")
  write.csv(inv, inv_path, row.names = FALSE)
  cat(sprintf("  Inventory written: %s  (%d files)\n", inv_path, nrow(inv)))
}

write_post_cleanup_summary()

cat("\nScript 99 complete.\n")
