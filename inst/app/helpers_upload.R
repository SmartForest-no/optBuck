# app/helpers_upload.R
#
# Purpose : Price files for the runs and uploads to price set A or B
#           (Phase 4c/4d). R/helpers_price_matrix.R reads, validates and writes
#           price matrices; here a price set is written to the file the
#           backend reads, and an upload is merged into one scenario.


#' Write a price matrix to a temporary long CSV for the backend. The caller
#' removes the file after the run.
write_price_file <- function(pm) {
  path <- tempfile(pattern = "optbuck_prices_", fileext = ".csv")
  pm_export(pm_prices(pm), path)       # never the hidden Distribution column
  path
}


#' Merge an uploaded price file into one scenario and check the result.
#'
#' A wide upload replaces only the matrices it contains (and the flat prices
#' of the products in its flat_prices sheet); a long upload replaces the
#' scenario's whole price set. Scenarios A and B are independent (Phase 4d):
#' the same function serves both.
#'
#' @param defs,pm The scenario's assortment table and prices.
#' @return list(ok, pm (the scenario's new prices), layout, ids, errors,
#'   warnings).
upload_price_set <- function(path, filename, defs, pm) {
  keys <- assortment_table_keys(defs)
  cur  <- pm_minimal(pm)
  up <- read_price_matrix_upload(path, filename, cur,
                                 matrix_keys = keys[defs$PriceType == "matrix"], alias = key_aliases(defs))
  out <- list(ok = FALSE, pm = NULL, layout = up$layout, ids = up$ids,
              errors = up$errors, warnings = character(0))
  if (length(up$errors) > 0L) return(out)
  cand <- if (identical(up$layout, "long")) up$long else {
    flat_keys <- up$long$ProductKey[is.na(up$long$LengthClass_dm)]
    drop <- (cur$ProductKey %in% up$ids & !is.na(cur$LengthClass_dm)) |
            (is.na(cur$LengthClass_dm) & cur$ProductKey %in% flat_keys)
    pm_bind(cur[!drop, , drop = FALSE], up$long[, PM_REQUIRED_COLS, drop = FALSE])
  }
  unknown <- setdiff(unique(as.character(up$long$ProductKey)), keys)
  if (length(unknown) > 0L) {
    out$errors <- sprintf("Not in this scenario: %s (add the assortment first).",
                          paste(unknown, collapse = ", "))
    return(out)
  }
  v <- check_price_set(defs, cand)
  out$ok <- isTRUE(v$ok); out$pm <- v$pm
  out$errors <- v$errors; out$warnings <- v$warnings
  out
}


#' Upload prices for one assortment (the detail panel's upload): the
#' file's rows for `key` replace that assortment's prices; rows for other
#' assortments are ignored and listed in `ignored`.
#'
#' @return list(ok, pm, ignored, errors, warnings).
upload_assortment_prices <- function(path, filename, defs, pm, key) {
  keys <- assortment_table_keys(defs)
  cur  <- pm_minimal(pm)
  up <- read_price_matrix_upload(path, filename, cur,
                                 matrix_keys = keys[defs$PriceType == "matrix"], alias = key_aliases(defs))
  out <- list(ok = FALSE, pm = NULL, ignored = character(0),
              errors = up$errors, warnings = character(0))
  if (length(up$errors) > 0L) return(out)
  long <- up$long
  if (!all(PM_REQUIRED_COLS %in% names(long))) {
    out$errors <- sprintf("Long price files need the columns %s.", paste(PM_REQUIRED_COLS, collapse = ", "))
    return(out)
  }
  long <- long[, PM_REQUIRED_COLS, drop = FALSE]
  out$ignored <- setdiff(unique(as.character(long$ProductKey)), key)
  mine <- long[long$ProductKey %in% key, , drop = FALSE]
  if (nrow(mine) == 0L) {
    out$errors <- sprintf("The file has no prices for %s.", key)
    return(out)
  }
  v <- check_price_set(defs, pm_bind(cur[cur$ProductKey != key, , drop = FALSE], mine))
  out$ok <- isTRUE(v$ok); out$pm <- v$pm
  out$errors <- v$errors; out$warnings <- v$warnings
  out
}
