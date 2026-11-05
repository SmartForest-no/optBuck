# ==============================================================================
# scripts/run_prototype_00_prepare_nfi_data.R
#
# Purpose : Run the frozen Oracle extraction script
#             scripts/prototype_reference/00_prepare_nfi_2024_data.R
#           under the current project root. That script hardcodes the old
#           S:/.../optBuck/nfiBuck root and may not be edited (audit 14
#           checks 19-20 assert the prototype scripts are unmodified). This
#           wrapper reads its text, replaces only the `root_dir <- "..."`
#           line with the resolved Apter root, and evaluates the result. The
#           frozen file and its modification time are left untouched.
#
# Requires: Oracle access (ROracle, keyring entry ORACLE_SERVICE /
#           ORACLE_USER). Writes data_processed/ and logs/ under the root.
#
# Usage   : Rscript scripts/run_prototype_00_prepare_nfi_data.R            (run)
#           Rscript scripts/run_prototype_00_prepare_nfi_data.R --dry-run  (show
#           the substituted line and exit, no Oracle needed)
#           Run from the project root, or set APTER_ROOT.
# ==============================================================================

root_dir <- local({
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

dry_run <- "--dry-run" %in% commandArgs(trailingOnly = TRUE)

frozen_path <- file.path(root_dir, "scripts", "prototype_reference",
                         "00_prepare_nfi_2024_data.R")
if (!file.exists(frozen_path)) stop("Frozen script not found: ", frozen_path)

src <- readLines(frozen_path, encoding = "UTF-8", warn = FALSE)

root_line <- grep('^root_dir\\s*<-\\s*".*"\\s*$', src)
if (length(root_line) != 1L)
  stop("Expected exactly one `root_dir <- \"...\"` line in ", frozen_path,
       ", found ", length(root_line), ". The frozen script has changed.")

new_line <- sprintf('root_dir <- "%s"', root_dir)
cat("Frozen script :", frozen_path, "\n")
cat(sprintf("Line %d       : %s\n", root_line, src[root_line]))
cat("Replaced with :", new_line, "\n")
src[root_line] <- new_line

# The old root must not survive anywhere in code (comments are fine).
code <- sub("#.*$", "", src)
if (any(grepl("optBuck/nfiBuck", code, fixed = TRUE)))
  stop("The old nfiBuck root still appears in code after substitution.")

if (dry_run) {
  cat("Dry run: not executed.\n")
} else {
  eval(parse(text = src, encoding = "UTF-8"), envir = globalenv())
}
