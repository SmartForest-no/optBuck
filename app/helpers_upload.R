# app/helpers_upload.R
#
# Purpose : Price files for the runs and uploads to price set A or B
#           (Phase 4c). R/helpers_price_matrix.R reads, validates and writes
#           price matrices; here a price set is written to the file the
#           backend reads, and an upload is merged into A or B.


#' Write a price matrix to a temporary long CSV for the backend. The caller
#' removes the file after the run.
write_price_file <- function(pm) {
  path <- tempfile(pattern = "apter_prices_", fileext = ".csv")
  pm_export(pm, path)
  path
}


#' Merge an uploaded price file into price set A or B and check the result.
#'
#' A wide upload replaces only the matrices it contains (and the flat prices
#' of the products in its flat_prices sheet); a long upload replaces the
#' whole set. In B the uploaded products get "Own prices"; after a long
#' upload, products missing from the file are "Not used" in B.
#'
#' @param side "A" or "B".
#' @param defs Assortment table; pm_a, pm_b, state: the current price sets.
#' @return list(ok, pm (the side's new prices), state (B states), layout,
#'   ids, errors, warnings).
upload_price_set <- function(path, filename, side = c("A", "B"), defs, pm_a, pm_b, state) {
  side <- match.arg(side)
  keys <- assortment_table_keys(defs)
  cur  <- if (side == "A") pm_minimal(pm_a) else pm_b_effective(pm_a, pm_b, state)
  up <- read_price_matrix_upload(path, filename, cur,
                                 matrix_keys = keys[defs$PriceType == "matrix"])
  out <- list(ok = FALSE, pm = NULL, state = state, layout = up$layout, ids = up$ids,
              errors = up$errors, warnings = character(0))
  if (length(up$errors) > 0L) return(out)

  long <- identical(up$layout, "long")
  new_keys <- unique(as.character(up$long$ProductKey))
  cand <- if (long) up$long else {
    keep <- pm_minimal(if (side == "A") pm_a else pm_b)
    flat_keys <- up$long$ProductKey[is.na(up$long$LengthClass_dm)]
    drop <- (keep$ProductKey %in% up$ids & !is.na(keep$LengthClass_dm)) |
            (is.na(keep$LengthClass_dm) & keep$ProductKey %in% flat_keys)
    rbind(keep[!drop, , drop = FALSE], up$long[, PM_REQUIRED_COLS, drop = FALSE])
  }
  unknown <- setdiff(new_keys, keys)
  if (length(unknown) > 0L) {
    out$errors <- paste("Not in the assortment table:", paste(unknown, collapse = ", "),
                        "(add the assortment first).")
    return(out)
  }

  if (side == "A") {
    v <- check_price_set(defs, cand)
  } else {
    st <- state
    st[intersect(new_keys, names(st))] <- "own"
    if (long) st[setdiff(names(st), new_keys)] <- "none"
    # The file's own rows first (non-numeric cells etc.), then B as a whole.
    v <- check_price_set(defs[keys %in% new_keys, , drop = FALSE],
                         cand[cand$ProductKey %in% new_keys, , drop = FALSE])
    if (!isTRUE(v$ok)) { out$errors <- v$errors; return(out) }
    cand_b <- pm_minimal(cand)
    b <- b_set(defs, pm_a, cand_b, st)
    v <- check_price_set(b$defs, b$pm)
    if (v$ok) { v$pm <- cand_b; out$state <- st }
  }
  out$ok <- isTRUE(v$ok); out$pm <- v$pm
  out$errors <- v$errors; out$warnings <- v$warnings
  out
}
