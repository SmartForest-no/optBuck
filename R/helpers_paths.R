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


#' Return the nfiBuck project root directory.
#'
#' Resolution order:
#'   1. Environment variable  NFIBUCK_ROOT  (useful for CI or different machines)
#'   2. R option              nfibuck.root  (set via options(nfibuck.root = "..."))
#'   3. Hardcoded default     S:/Prosjekter/.../nfiBuck
#'
#' @return Character scalar — absolute path, no trailing slash.
get_root_dir <- function() {
  env_val <- Sys.getenv("NFIBUCK_ROOT", unset = "")
  if (nchar(env_val) > 0L) return(normalizePath(env_val, winslash = "/", mustWork = FALSE))

  opt_val <- getOption("nfibuck.root", default = NULL)
  if (!is.null(opt_val) && nchar(opt_val) > 0L)
    return(normalizePath(opt_val, winslash = "/", mustWork = FALSE))

  normalizePath(
    "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck",
    winslash = "/", mustWork = FALSE
  )
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
