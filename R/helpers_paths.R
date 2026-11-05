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
# Resolution order (first hit wins):
#   1. Environment variable  APTER_ROOT  (development: the repository root)
#   2. R option              apter.root
#   3. The package folder (installed package, or the source folder under load_all())
#   4. Walk up from getwd() to the first folder with inst/extdata/inputs (scripts run from the repository)
# A root from 1-2 must hold the data files; otherwise the function stops with a clear message.
get_root_dir <- function() {
  explicit <- c(Sys.getenv("APTER_ROOT", unset = ""), getOption("apter.root", default = ""))
  explicit <- explicit[nzchar(explicit)]
  if (length(explicit) > 0L) {
    root <- normalizePath(explicit[1L], winslash = "/", mustWork = FALSE)
    if (!.is_root(root))
      stop("Project root '", root, "' (from APTER_ROOT or option apter.root) does not contain inst/extdata/inputs.", call. = FALSE)
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
  stop("Cannot locate the optBuck data files: install the package, set APTER_ROOT, or run from inside the repository.", call. = FALSE)
}

# A path under the data folder (inst/extdata in development, extdata when installed).
extdata_path <- function(..., root_dir = get_root_dir()) {
  dev <- file.path(root_dir, "inst", "extdata")
  file.path(if (dir.exists(dev)) dev else file.path(root_dir, "extdata"), ...)
}

# The shiny app folder (inst/app in development, app when installed).
app_dir <- function(root_dir = get_root_dir()) {
  dev <- file.path(root_dir, "inst", "app")
  if (dir.exists(dev)) dev else file.path(root_dir, "app")
}

# A path under the inputs folder (default products, prices, translations, test fixtures).
path_input <- function(..., root_dir = get_root_dir()) extdata_path("inputs", ..., root_dir = root_dir)

# A path under the repository's outputs/ folder (audits and scripts only; the folder exists in development).
path_output <- function(..., root_dir = get_root_dir()) file.path(root_dir, "outputs", ...)

# Ensure a directory exists, creating it (recursively) if needed.
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}
