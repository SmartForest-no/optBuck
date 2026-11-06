# ==============================================================================
# R/helpers_paths.R
#
# Purpose : Path helpers of the optBuck package (roadmap 8, step 8, 2026-11-05).
#           "root" is the package root: the installed package folder, or in development
#           (pkgload::load_all(), scripts, audits) the repository root with inst/. Data files
#           live in inst/extdata/ (inputs/, nfi/, demo/) and the app in inst/app/; after
#           installation they are in extdata/ and app/ of the package folder.
#
# Usage   : root <- get_root_dir()
#           pd_path <- path_input("ProductData_spruce_pine_default.csv")
#           out_dir <- path_output("selected_stratum_bucking")   # development only (outputs/)
# ==============================================================================

# The folder of the installed package, or the source folder under pkgload::load_all().
pkg_root <- function() {
  d <- tryCatch(find.package("optBuck", quiet = TRUE), error = function(e) character(0))
  if (length(d) == 0L || !nzchar(d[1L])) return(NA_character_)
  normalizePath(d[1L], winslash = "/", mustWork = FALSE)
}

# TRUE when `d` is a package root or a repository root of optBuck.
.is_root <- function(d)
  file.exists(file.path(d, "extdata", "inputs")) || file.exists(file.path(d, "inst", "extdata", "inputs"))

# Return the root directory.
#
# Installed use needs no setting: the package folder is the root (read-only data via
# system.file(); writable files, such as the NFI profile cache, go to tools::R_user_dir()).
# Resolution order (first hit wins):
#   1. Environment variable  OPTBUCK_ROOT  (development only: the repository root)
#   2. R option              optbuck.root   (development only)
#   3. The package folder (installed package, or the source folder under load_all())
#   4. Walk up from getwd() to the first folder with inst/extdata/inputs (scripts run from the repository)
# A root from 1-2 must hold the data files; otherwise the function stops with a clear message.
get_root_dir <- function() {
  explicit <- c(Sys.getenv("OPTBUCK_ROOT", unset = ""), getOption("optbuck.root", default = ""))
  explicit <- explicit[nzchar(explicit)]
  if (length(explicit) > 0L) {
    root <- normalizePath(explicit[1L], winslash = "/", mustWork = FALSE)
    if (!.is_root(root))
      stop("Project root '", root, "' (from OPTBUCK_ROOT or option optbuck.root) does not contain inst/extdata/inputs.", call. = FALSE)
    return(root)
  }
  p <- pkg_root()
  if (!is.na(p) && .is_root(p)) return(p)
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  repeat {
    if (.is_root(d)) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  stop("Cannot locate the optBuck data files: reinstall the package, or (development) run from inside the repository.", call. = FALSE)
}

# A path under the data folder (read-only): by default the installed package's extdata (system.file();
# under load_all() inst/extdata), else the data folder of `root_dir` (inst/extdata or extdata).
extdata_path <- function(..., root_dir = NULL) {
  base <- if (is.null(root_dir)) system.file("extdata", package = "optBuck") else ""
  if (!nzchar(base)) {
    root_dir <- if (is.null(root_dir)) get_root_dir() else root_dir
    dev <- file.path(root_dir, "inst", "extdata")
    base <- if (dir.exists(dev)) dev else file.path(root_dir, "extdata")
  }
  file.path(base, ...)
}

# The shiny app folder (read-only): the installed package's app (inst/app in development).
app_dir <- function(root_dir = NULL) {
  base <- if (is.null(root_dir)) system.file("app", package = "optBuck") else ""
  if (nzchar(base)) return(base)
  root_dir <- if (is.null(root_dir)) get_root_dir() else root_dir
  dev <- file.path(root_dir, "inst", "app")
  if (dir.exists(dev)) dev else file.path(root_dir, "app")
}

# A path under the inputs folder (default products, prices, translations, test fixtures).
path_input <- function(..., root_dir = NULL) extdata_path("inputs", ..., root_dir = root_dir)

# A writable per-user folder of the package, created on demand ("cache": the NFI profiles;
# "data": files that should survive; never inside the installed package folder).
user_dir <- function(which = c("cache", "data")) {
  d <- tools::R_user_dir("optBuck", match.arg(which))
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# A path under the repository's outputs/ folder (audits and scripts only; the folder exists in development).
path_output <- function(..., root_dir = get_root_dir()) file.path(root_dir, "outputs", ...)

# Ensure a directory exists, creating it (recursively) if needed.
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}
