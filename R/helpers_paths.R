# ==============================================================================
# R/helpers_paths.R
#
# Purpose : Centralised path helpers for the nfiBuck project.
#           All scripts should resolve the project root through get_root_dir()
#           rather than hardcoding paths inline.
#
# Usage   : source("R/helpers_paths.R")
#           root <- get_root_dir()
#           pd_path <- path_input("ProductData_spruce_pine_default.csv")
#           out_dir <- path_output("selected_stratum_bucking")
#           ensure_dir(out_dir)
# ==============================================================================


#' Return the project root directory.
#'
#' Resolution order (first hit wins):
#'   1. Environment variable  APTER_ROOT
#'   2. Environment variable  NFIBUCK_ROOT  (legacy, kept until the Phase 1 rename)
#'   3. R option              apter.root    (options(apter.root = "..."))
#'   4. R option              nfibuck.root  (legacy)
#'   5. Walk up from getwd() to the first directory containing R/helpers_paths.R
#' A root from 1-4 must contain R/helpers_paths.R; otherwise, or if 5 finds
#' nothing, the function stops with a clear message. No path is hardcoded.
#'
#' @return Character scalar — absolute path, no trailing slash.
get_root_dir <- function() {
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))

  explicit <- c(Sys.getenv("APTER_ROOT", unset = ""),
                Sys.getenv("NFIBUCK_ROOT", unset = ""),
                getOption("apter.root", default = ""),
                getOption("nfibuck.root", default = ""))
  explicit <- explicit[nzchar(explicit)]
  if (length(explicit) > 0L) {
    root <- normalizePath(explicit[1L], winslash = "/", mustWork = FALSE)
    if (!is_root(root))
      stop("Project root '", root, "' (from APTER_ROOT / NFIBUCK_ROOT / ",
           "options) does not contain R/helpers_paths.R.", call. = FALSE)
    return(root)
  }

  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  repeat {
    if (is_root(d)) return(d)
    parent <- dirname(d)
    if (identical(parent, d)) break
    d <- parent
  }
  stop("Cannot locate the project root: set APTER_ROOT, or run from inside ",
       "the project directory.", call. = FALSE)
}


#' Build a path under the project inputs/ directory.
#'
#' @param ...      Path components passed to file.path().
#' @param root_dir Project root (default: get_root_dir()).
#' @return Character scalar path.
path_input <- function(..., root_dir = get_root_dir()) {
  file.path(root_dir, "inputs", ...)
}


#' Build a path under the project outputs/ directory.
#'
#' @param ...      Path components passed to file.path().
#' @param root_dir Project root (default: get_root_dir()).
#' @return Character scalar path.
path_output <- function(..., root_dir = get_root_dir()) {
  file.path(root_dir, "outputs", ...)
}


#' Build a path under the project data_processed/ directory.
#'
#' @param ...      Path components passed to file.path().
#' @param root_dir Project root (default: get_root_dir()).
#' @return Character scalar path.
path_data <- function(..., root_dir = get_root_dir()) {
  file.path(root_dir, "data_processed", ...)
}


#' Ensure a directory exists, creating it (recursively) if needed.
#'
#' @param path Directory path to create.
#' @return Invisibly returns the path.
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}
