# app/helpers_upload.R
#
# Purpose : Scenario 2 price-matrix helpers: multiplier matrix and the
#           price_matrix_path passed to run_sampled_plot_bucking().
#           Uploaded matrices are not converted; the backend validates them
#           against the PriceMatrix_long schema.


#' Apply sawlog / pulpwood / waste multipliers to a PriceMatrix_long table.
.make_s2_pm <- function(base_pm, mult_saw, mult_pulp, mult_wst) {
  pm2 <- base_pm
  pk  <- pm2$ProductKey
  pm2$PriceNOKperM3 <- ifelse(
    grepl("sawlog",  pk, ignore.case = TRUE), pm2$PriceNOKperM3 * mult_saw,
    ifelse(grepl("pulp",  pk, ignore.case = TRUE), pm2$PriceNOKperM3 * mult_pulp,
    ifelse(grepl("waste", pk, ignore.case = TRUE), pm2$PriceNOKperM3 * mult_wst,
           pm2$PriceNOKperM3)))
  pm2
}


#' Resolve the Scenario 2 price_matrix_path.
#'
#' @param mode    "same", "multiplier" or "upload".
#' @param upload  fileInput value (data.frame with datapath) or NULL.
#' @param mults   Numeric c(saw = , pulp = , wst = ) multipliers.
#' @param base_pm Base PriceMatrix_long table (APP_PRICE_LONG).
#' @return "default_2021" or a file path. In "multiplier" mode this is a
#'   tempfile the caller must remove after use. In "upload" mode a missing
#'   upload stops the reactive via req().
resolve_s2_price_path <- function(mode, upload, mults, base_pm) {
  switch(mode,
    "same"       = "default_2021",
    "multiplier" = {
      pm2 <- .make_s2_pm(base_pm, mults[["saw"]], mults[["pulp"]], mults[["wst"]])
      tmp <- tempfile(fileext = ".csv")
      readr::write_csv(pm2, tmp)
      tmp
    },
    "upload" = {
      req(!is.null(upload))
      upload$datapath
    },
    "default_2021"
  )
}
