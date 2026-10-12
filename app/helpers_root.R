# app/helpers_root.R
#
# Purpose : Resolve the project root for the Shiny app. Sourced by app.R
#           before anything else, so it must not depend on R/ helpers.


#' Resolve the project root for the app.
#'
#' Order: APTER_ROOT -> NFIBUCK_ROOT (legacy, until the Phase 1 rename) ->
#' parent of the app directory. The result must contain R/helpers_paths.R,
#' otherwise stop() with a clear message.
#'
#' @param app_dir The app directory (runApp() sets the working directory to it).
#' @return Character scalar: absolute root path, forward slashes.
resolve_app_root <- function(app_dir = getwd()) {
  env <- Sys.getenv(c("APTER_ROOT", "NFIBUCK_ROOT"), unset = "")
  env <- env[nzchar(env)]
  source_lbl <- if (length(env) > 0L) names(env)[1L] else "app directory parent"
  root <- if (length(env) > 0L) env[[1L]] else file.path(app_dir, "..")
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  if (!file.exists(file.path(root, "R", "helpers_paths.R")))
    stop("Project root '", root, "' (from ", source_lbl, ") does not contain ",
         "R/helpers_paths.R. Set APTER_ROOT to the project directory, or run ",
         "shiny::runApp(\"app\") from the project root.", call. = FALSE)
  root
}
