# app/app.R
#
# Purpose : Shiny app for Apter, the bucking simulator.
#           v1 scope: spruce/pine, default 2021 price matrix, sampled plots.
#
# Run     : from the project root,
#             shiny::runApp("app")   or   shiny::runApp("app/app.R")
#           From elsewhere, set APTER_ROOT to the project root first.
#
# Root    : APTER_ROOT -> parent of app/
#           (app/helpers_root.R). The resolved root is exported as APTER_ROOT
#           so the backend resolves the same one.
#
# Files   : helpers_root.R        project root resolution
#           setup.R               data loading, choice mappings, label, CSS
#           helpers_validation.R  inputs -> backend params, stratum counts
#           helpers_upload.R      Scenario 2 price matrix (multipliers/upload)
#           app_ui.R / app_server.R  `ui` and `server` (not ui.R/server.R,
#                                    which Shiny treats as a separate app format)
#
# Data    : data_processed_github/ (sanitized, no original plot IDs)
# Backend : scripts/app_backend/01_run_sampled_plot_bucking.R

library(shiny)
suppressPackageStartupMessages(library(arrow))

# runApp() sets the working directory to the app directory.
APP_DIR <- normalizePath(getwd(), winslash = "/")

# ---- project root -----------------------------------------------------------
source(file.path(APP_DIR, "helpers_root.R"), local = TRUE)
ROOT <- resolve_app_root(APP_DIR)
Sys.setenv(APTER_ROOT = ROOT)

# ---- source backend (also sources all 7 helpers) ----------------------------
source(file.path(ROOT, "scripts", "app_backend",
                 "01_run_sampled_plot_bucking.R"))

# ---- app files ---------------------------------------------------------------
for (.f in c("setup.R", "helpers_validation.R", "helpers_upload.R",
             "app_ui.R", "app_server.R")) {
  source(file.path(APP_DIR, .f), local = TRUE)
}
rm(.f)

# ---- launch -----------------------------------------------------------------
shinyApp(ui, server)
