# ==============================================================================
# audits/audit_39_name_and_paths.R  (2026-11-06)
#
# Purpose : (1) The tool name. The working name used before 2026-11-06 is gone from every tracked file:
#           the only matches of the old name left are the Norwegian/Swedish words (aptering, apteringsgrad,
#           Apteringsanalyse, apteres, ...); the remote is SmartForest-no/optBuck, so no address is exempt.
#           (2) Paths. The package never needs an environment variable or a working directory: the
#           read-only files come from system.file() (extdata_path(), app_dir()), the writable ones from
#           tools::R_user_dir() (nfi_cache_dir()); the root helper is not set from the app.
#
# Checks  :
#  1.  No tracked file name contains the old name
#  2.  No tracked text file contains the old name except the allowed words (list printed)
#  3.  No environment variable, option or function of the old name remains (OPTBUCK_*, optbuck.* only)
#  4.  inst/app and R/ do not set OPTBUCK_ROOT, and inst/app calls get_root_dir() once (app.R)
#  5.  Read-only paths: extdata_path() and app_dir() default to system.file() (resolve without a root);
#      writable path: nfi_cache_dir() is under tools::R_user_dir() when no development cache exists
#  6.  Every function with a `root` argument defaulting to get_root_dir() is a data reader (the list)
#
# Verdict : PASS / FAIL (helpers_audit.R)
# Outputs : outputs/name_and_paths/audit_39_name_and_paths.{csv,txt}
# Usage   : Rscript audits/audit_39_name_and_paths.R
# ==============================================================================

root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  env <- Sys.getenv("OPTBUCK_ROOT", unset = "")
  if (nzchar(env)) {
    d <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (!is_root(d)) stop("Project root '", d, "' (from OPTBUCK_ROOT) does not contain R/helpers_paths.R.", call. = FALSE)
  } else {
    d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    while (!is_root(d) && dirname(d) != d) d <- dirname(d)
    if (!is_root(d)) stop("Cannot locate the project root: set OPTBUCK_ROOT, or run from inside the project directory.", call. = FALSE)
  }
  d
})
Sys.setenv(OPTBUCK_ROOT = root_dir)

suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)
source(file.path(root_dir, "audits", "helpers_audit.R"))

out_dir <- path_output("name_and_paths", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_39_name_and_paths.txt")
audit_csv <- file.path(out_dir, "audit_39_name_and_paths.csv")
init_audit_log(audit_txt)

E <- new.env(parent = emptyenv())
rec <- function(check, result, detail = "") record_check(E, check, result, detail, log_path = audit_txt)
PASS <- "PASS"; FAIL <- "FAIL"
ok <- function(x) if (isTRUE(x)) PASS else FAIL
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)
cat("  Audit: tool name and paths (2026-11-06)\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

shorten <- function(x, n = 8L) if (length(x) == 0L) "none" else
  paste(c(utils::head(x, n), if (length(x) > n) sprintf("... (%d)", length(x))), collapse = " | ")

# The old name is spelled in pieces so that this file does not contain it.
OLD <- paste0("ap", "ter")
# Allowed: Norwegian words that begin with it (aptering..., apteres, apterte, aptere, apterer, aptert) ...
ALLOWED_WORD <- paste0("(?i)", OLD, "(?:ing[A-Za-zæøå_]*|ende|es|te|t|er|e)(?![A-Za-zæøå0-9_])")

git_files <- function() {
  f <- tryCatch(system2("git", c("-c", "safe.directory=*", "-C", shQuote(root_dir), "ls-files"), stdout = TRUE, stderr = FALSE),
                error = function(e) character(0), warning = function(w) character(0))
  if (length(f) == 0L) f <- list.files(root_dir, recursive = TRUE, all.files = FALSE)   # no git: every file
  f
}
files <- git_files()
files <- files[file.exists(file.path(root_dir, files))]

# ---- 1 file names ---------------------------------------------------------------------------
hit_names <- files[grepl(OLD, files, ignore.case = TRUE) & !grepl(ALLOWED_WORD, files, perl = TRUE)]
rec("Check 01: no tracked file name contains the old tool name", ok(length(hit_names) == 0L),
    sprintf("%d tracked files; offending: %s", length(files), shorten(hit_names)))

# ---- 2 file contents --------------------------------------------------------------------------
is_text <- function(raw) !any(raw[seq_len(min(8000L, length(raw)))] == as.raw(0L))
residual <- character(0); allowed_seen <- character(0)
for (f in files) {
  p <- file.path(root_dir, f)
  if (file.size(p) == 0L || file.size(p) > 20e6) next
  raw <- readBin(p, "raw", file.size(p))
  if (!is_text(raw)) next
  txt <- rawToChar(raw); Encoding(txt) <- "UTF-8"
  if (!isTRUE(grepl(OLD, txt, ignore.case = TRUE, useBytes = TRUE))) next
  lines <- strsplit(txt, "\r?\n")[[1L]]
  for (i in which(grepl(OLD, lines, ignore.case = TRUE, useBytes = TRUE))) {
    rest <- gsub(ALLOWED_WORD, "", lines[i], perl = TRUE, useBytes = FALSE)
    if (grepl(OLD, rest, ignore.case = TRUE)) residual <- c(residual, sprintf("%s:%d", f, i))
    else allowed_seen <- c(allowed_seen, f)
  }
}
rec("Check 02: no tracked text file contains the old name except Norwegian words",
    ok(length(residual) == 0L), sprintf("allowed lines in %d files; offending: %s", length(unique(allowed_seen)), shorten(residual)))

# ---- 3 option, variable, function names ----------------------------------------------------------
src <- unlist(lapply(list.files(file.path(root_dir, c("R", "inst/app")), pattern = "[.](R|js|css)$", full.names = TRUE, recursive = TRUE),
                     function(f) readLines(f, warn = FALSE, encoding = "UTF-8")))
needed <- c("OPTBUCK_WORKERS", "optbuck.workers", "optbuck_workers", "OPTBUCK_LANG")
have <- vapply(needed, function(k) any(grepl(k, src, fixed = TRUE)), NA)
envs <- unique(unlist(regmatches(src, gregexpr("Sys\\.(?:getenv|setenv)\\(\\s*\"?[A-Z_]+", src, perl = TRUE))))
rec("Check 03: options, variables and functions carry the package name (optbuck.*, OPTBUCK_*, optbuck_*)",
    ok(all(have)), sprintf("present: %s | environment variables read or set in R/ and inst/app: %s", paste(names(have)[have], collapse = ", "),
                           shorten(sub(".*\\(\\s*\"?", "", envs), 12L)))

# ---- 4 no OPTBUCK_ROOT set by the package ---------------------------------------------------------
setters <- unlist(lapply(list.files(file.path(root_dir, c("R", "inst/app")), pattern = "[.]R$", full.names = TRUE, recursive = TRUE), function(f) {
  l <- readLines(f, warn = FALSE, encoding = "UTF-8"); i <- grep("Sys\\.setenv\\([^)]*OPTBUCK_ROOT", l)
  if (length(i)) sprintf("%s:%d", basename(f), i) else character(0) }))
app_uses <- unlist(lapply(list.files(file.path(root_dir, "inst/app"), pattern = "[.]R$", full.names = TRUE, recursive = TRUE), function(f) {
  l <- readLines(f, warn = FALSE, encoding = "UTF-8"); l <- l[!grepl("^\\s*#", l)]; i <- grep("get_root_dir\\(", l)
  if (length(i)) rep(basename(f), length(i)) else character(0) }))
rec("Check 04: the package sets no OPTBUCK_ROOT; inst/app calls get_root_dir() once (app.R)",
    ok(length(setters) == 0L && identical(app_uses, "app.R")), sprintf("setters: %s | get_root_dir() in inst/app: %s", shorten(setters), shorten(app_uses)))

# ---- 5 read-only and writable paths ---------------------------------------------------------------
ext <- extdata_path("inputs")
app <- app_dir()
cache <- nfi_cache_dir(root = tempdir())               # a root without outputs/app_cache = the installed case
rec("Check 05: extdata_path() / app_dir() come from system.file(); the NFI cache of an installed package is under R_user_dir()",
    ok(identical(normalizePath(ext, winslash = "/"), normalizePath(file.path(system.file("extdata", package = "optBuck"), "inputs"), winslash = "/")) &&
         identical(normalizePath(app, winslash = "/"), normalizePath(system.file("app", package = "optBuck"), winslash = "/")) &&
         startsWith(normalizePath(cache, winslash = "/", mustWork = FALSE),
                    normalizePath(tools::R_user_dir("optBuck", "cache"), winslash = "/", mustWork = FALSE))),
    sprintf("extdata %s | app %s | cache %s", ext, app, cache))

# ---- 6 readers that default to get_root_dir() ------------------------------------------------------
defs <- unlist(lapply(list.files(file.path(root_dir, "R"), pattern = "[.]R$", full.names = TRUE), function(f) {
  l <- readLines(f, warn = FALSE, encoding = "UTF-8"); i <- grep("(root|root_dir)\\s*=\\s*get_root_dir\\(\\)", l)
  if (length(i)) sprintf("%s:%d", basename(f), i) else character(0) }))
rec("Check 06: the arguments that default to get_root_dir() are listed (data readers and the NFI cache, no writers of other kinds)",
    ok(length(defs) > 0L && !any(grepl("^(helpers_hpr_export|helpers_pin_export|helpers_result_tables)", defs))),
    sprintf("%d arguments in %s", length(defs), paste(sort(unique(sub(":.*", "", defs))), collapse = ", ")))

write_audit_outputs(E$results, audit_csv, audit_txt)
