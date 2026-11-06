# inst/app/app.R
#
# Purpose : Shiny app of the optBuck package (the bucking simulator; roadmap 8 made the repository the
#           package). Tabs: 1 Stems, 2 Assortment specifications and prices (Scenarios A and B),
#           3 Simulator (compares A and B when B differs), 4 Price tuning, 5 Bucking analysis, then Help.
#
# Run     : optBuck::run_app()
#           In development (the repository): pkgload::load_all(); optBuck::run_app(), or
#           shiny::runApp("inst/app") from the repository root.
#
# Package : the app uses the package's internal functions, so they are put on the search path
#           (attach of the namespace's objects). In development the package is loaded from the source
#           folder with pkgload::load_all().
#
# Files   : setup.R               data, default assortments and prices, choices
#           theme.R               design tokens, bslib (Bootstrap 5) theme
#           helpers_ui.R          cards, read-only fields, reactable and plot styling
#           www/                  optbuck.css, icon, self-hosted fonts (OFL)
#           helpers_validation.R  inputs -> backend params, stratum counts
#           helpers_upload.R      price files for runs, uploads to A or B
#           helpers_jobs.R        runs as background jobs (ExtendedTask, workers)
#           tuning.R, check.R, check_report.R, help.R   the tabs 4 and 5 and the Help tab
#           app_ui.R / app_server.R  `ui` and `server` (not ui.R/server.R,
#                                    which Shiny treats as a separate app format)
#
# Data    : the package's extdata/nfi (sanitized, no original plot IDs)

library(shiny)
suppressPackageStartupMessages({
  library(bslib)
  library(reactable)
  library(rhandsontable)
  library(arrow)
})

# runApp() sets the working directory to the app directory.
APP_DIR <- normalizePath(getwd(), winslash = "/")

# ---- the package ---------------------------------------------------------------------
if (!isNamespaceLoaded("optBuck")) {
  src_root <- normalizePath(file.path(APP_DIR, "..", ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(src_root, "DESCRIPTION")) && requireNamespace("pkgload", quietly = TRUE))
    suppressPackageStartupMessages(pkgload::load_all(src_root, quiet = TRUE))      # development
  else loadNamespace("optBuck")
}
if (!"optBuck_internals" %in% search()) {
  ns_objects <- as.list(asNamespace("optBuck"), all.names = TRUE)
  ns_objects <- ns_objects[!startsWith(names(ns_objects), ".__") & names(ns_objects) != ".packageName"]   # not the namespace's bookkeeping
  attach(ns_objects, name = "optBuck_internals", warn.conflicts = FALSE)
}
ROOT <- get_root_dir()      # the package folder (installed) or the repository root (development)

# ---- app files ---------------------------------------------------------------
for (.f in c("setup.R", "theme.R", "helpers_ui.R", "helpers_validation.R",
             "helpers_upload.R", "helpers_jobs.R", "tuning.R", "check_report.R", "check.R", "help.R", "app_ui.R", "server_scenarios.R",
             "app_server.R")) {
  source(file.path(APP_DIR, .f), local = TRUE)
}
rm(.f)

# ---- launch -----------------------------------------------------------------
shinyApp(ui, server)
