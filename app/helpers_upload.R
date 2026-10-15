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


#' Merge an uploaded price file into scenario A or B and check the result.
#'
#' A wide upload replaces only the matrices it contains (and the flat prices
#' of the products in its flat_prices sheet); a long upload replaces the
#' whole set. In B the uploaded products get "Own prices"; after a long
#' upload, A's products missing from the file are not used in B.
#'
#' @param side "A" or "B".
#' @param defs_a A's assortment table; pm_a, pm_b, state: prices and B's
#'   price states; defs_b_own, b_asm: B's own rows and assortment states
#'   (Phase 4d; b_defs()).
#' @return list(ok, pm (the side's new prices), state, b_asm, layout, ids,
#'   errors, warnings).
upload_price_set <- function(path, filename, side = c("A", "B"), defs_a, pm_a, pm_b, state,
                             defs_b_own = NULL, b_asm = NULL) {
  side <- match.arg(side)
  if (is.null(b_asm)) b_asm <- setNames(rep("same", nrow(defs_a)), assortment_table_keys(defs_a))
  defs <- if (side == "A") defs_a else b_defs(defs_a, defs_b_own, b_asm)
  keys <- assortment_table_keys(defs)
  cur  <- if (side == "A") pm_minimal(pm_a)
          else b_set(defs_a, pm_a, pm_b, state, defs_b_own, b_asm)$pm
  up <- read_price_matrix_upload(path, filename, cur,
                                 matrix_keys = keys[defs$PriceType == "matrix"])
  out <- list(ok = FALSE, pm = NULL, state = state, b_asm = b_asm, layout = up$layout,
              ids = up$ids, errors = up$errors, warnings = character(0))
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
    out$errors <- sprintf("Not in Scenario %s: %s (add the assortment first).", side,
                          paste(unknown, collapse = ", "))
    return(out)
  }

  if (side == "A") {
    v <- check_price_set(defs, cand)
  } else {
    st <- state; asm <- b_asm
    st[new_keys] <- "own"
    if (long) {
      gone <- setdiff(keys, new_keys)
      b_only <- setdiff(gone, assortment_table_keys(defs_a))
      if (length(b_only) > 0L) {
        out$errors <- paste("Missing prices for assortments only in Scenario B:",
                            paste(b_only, collapse = ", "))
        return(out)
      }
      st[gone] <- "none"; asm[gone] <- "none"
    }
    # The file's own rows first (non-numeric cells etc.), then B as a whole.
    v <- check_price_set(defs[keys %in% new_keys, , drop = FALSE],
                         cand[cand$ProductKey %in% new_keys, , drop = FALSE])
    if (!isTRUE(v$ok)) { out$errors <- v$errors; return(out) }
    cand_b <- pm_minimal(cand)
    own <- if (is.null(defs_b_own)) NULL else
      defs_b_own[!assortment_table_keys(defs_b_own) %in% names(asm)[asm == "none"], , drop = FALSE]
    b <- b_set(defs_a, pm_a, cand_b, st, own, asm)
    v <- check_price_set(b$defs, b$pm)
    if (v$ok) { v$pm <- cand_b; out$state <- st; out$b_asm <- asm }
  }
  out$ok <- isTRUE(v$ok); out$pm <- v$pm
  out$errors <- v$errors; out$warnings <- v$warnings
  out
}
