# app/helpers_upload.R
#
# Purpose : Scenario price matrix for the Scenario 2 run (Phase 4a). The
#           scenario is edited on the Prices and assortments page
#           (R/helpers_price_matrix.R does the grids, upload and validation);
#           here it is written to the file the backend reads.


#' Write the scenario price matrix to a temporary long CSV for the backend.
#' The caller removes the file after the run.
write_scenario_pm <- function(pm) {
  path <- tempfile(pattern = "apter_scenario_", fileext = ".csv")
  pm_export(pm, path)
  path
}
