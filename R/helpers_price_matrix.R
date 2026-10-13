# ==============================================================================
# R/helpers_price_matrix.R
#
# Purpose : Price-matrix scenarios (Phase 4a): editable grids, differences
#           from the default, reset, upload (long or wide layout, detected
#           automatically) with validation, templates and export.
#
# Layouts :
#   - Long  : PriceMatrix_long rows; the four columns Apter needs are
#             ProductKey, LengthClass_dm, PriceDiameterClass_cm (sawlogs),
#             PriceNOKperM3. Flat products (pulpwood, waste) have empty
#             LengthClass_dm / PriceDiameterClass_cm. A long upload replaces
#             the whole scenario. Export of a full scenario is long CSV.
#   - Wide  : the source layout of PriceMatrixGran.xlsx / PriceMatrixFuru.xlsx
#             and of the editor: rows = top-diameter classes (cm), columns =
#             length classes (dm), top-left cell = the matrix (ProductKey,
#             e.g. spruce_sawlog). xlsx: one sheet per sawlog matrix (sheet
#             name = ProductKey) plus an optional "flat_prices" sheet
#             (ProductKey, PriceNOKperM3). CSV: one grid. A wide upload
#             replaces only the matrices it contains.
#   Both layouts go through validate_scenario_pm(). Units are never converted.
#
# Matrix ids: one per matrix-priced ProductKey (sawlogs and matrix custom
#             assortments), plus "flat_prices" (all flat-priced products:
#             pulpwood, waste and flat custom assortments, all species).
#
# Custom assortments (Phase 4b, 2026-10-13): definitions are rows of
#   ASSORTMENT_COLS (app units: cm), turned into ProductData by
#   custom_assortment_rows() (helpers_products.R). pm_new_assortment_prices()
#   gives a new assortment's starting prices; assortments_export() /
#   assortments_import() move definitions and their default and scenario
#   prices as one xlsx.
# ==============================================================================

#' Matrix id of the flat-price table.
PM_FLAT_ID <- "flat_prices"

#' Required long-layout columns.
PM_REQUIRED_COLS <- c("ProductKey", "LengthClass_dm", "PriceDiameterClass_cm",
                      "PriceNOKperM3")

#' Columns of a custom assortment definition (one row per assortment).
ASSORTMENT_COLS <- c("Name", "Class", "Species", "MinTopDiameter_cm",
                     "MaxTopDiameter_cm", "MinButtDiameter_cm", "MaxButtDiameter_cm",
                     "AllowedLengths_cm", "Priority", "DiameterUnderBark",
                     "DiameterTopPosition_cm", "VolumeDiameterCategory",
                     "VolumeDiameterAdjustment", "PriceType", "UseIn")


#' The four columns Apter uses, typed, from any PriceMatrix_long.
pm_minimal <- function(pm) {
  out <- data.frame(
    ProductKey            = as.character(pm$ProductKey),
    LengthClass_dm        = suppressWarnings(as.integer(pm$LengthClass_dm)),
    PriceDiameterClass_cm = suppressWarnings(as.integer(pm$PriceDiameterClass_cm)),
    PriceNOKperM3         = suppressWarnings(as.numeric(pm$PriceNOKperM3)),
    stringsAsFactors = FALSE)
  out <- out[order(out$ProductKey, out$LengthClass_dm, out$PriceDiameterClass_cm,
                   na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.is_matrix_row <- function(pm) !is.na(pm$LengthClass_dm)

#' Editable matrix ids: sawlog ProductKeys (spruce before pine) + flat.
pm_matrix_ids <- function(pm) {
  saw <- unique(as.character(pm$ProductKey[.is_matrix_row(pm)]))
  sp  <- match(sub("_.*$", "", saw), c("spruce", "pine"))
  c(saw[order(sp, saw, na.last = TRUE)], PM_FLAT_ID)
}

#' Human label for a matrix id ("spruce_sawlog" -> "Spruce sawlog").
pm_matrix_label <- function(id) {
  if (identical(id, PM_FLAT_ID)) return("Flat prices")
  s <- gsub("_", " ", id)
  paste0(toupper(substring(s, 1, 1)), substring(s, 2))
}


#' One matrix as a grid in the source layout.
#'
#' Sawlog: rows = PriceDiameterClass_cm (row names), columns = LengthClass_dm
#' (column names), values = PriceNOKperM3 (NA where the matrix has no price).
#' Flat: rows = ProductKey (row names), one column "PriceNOKperM3".
pm_to_grid <- function(pm, id) {
  if (identical(id, PM_FLAT_ID)) {
    f <- pm[!.is_matrix_row(pm), , drop = FALSE]
    f <- f[order(f$ProductKey), , drop = FALSE]
    g <- data.frame(PriceNOKperM3 = as.numeric(f$PriceNOKperM3))
    rownames(g) <- f$ProductKey
    return(g)
  }
  s <- pm[pm$ProductKey == id & .is_matrix_row(pm), , drop = FALSE]
  if (nrow(s) == 0L) stop("No sawlog matrix '", id, "' in the price matrix.")
  lens  <- sort(unique(s$LengthClass_dm))
  diams <- sort(unique(s$PriceDiameterClass_cm))
  m <- matrix(NA_real_, length(diams), length(lens),
              dimnames = list(as.character(diams), as.character(lens)))
  m[cbind(match(s$PriceDiameterClass_cm, diams), match(s$LengthClass_dm, lens))] <-
    as.numeric(s$PriceNOKperM3)
  as.data.frame(m, check.names = FALSE)
}

#' Long rows for one grid (inverse of pm_to_grid()).
.grid_to_long <- function(grid, id) {
  if (identical(id, PM_FLAT_ID))
    return(data.frame(ProductKey = rownames(grid), LengthClass_dm = NA_integer_,
                      PriceDiameterClass_cm = NA_integer_,
                      PriceNOKperM3 = as.numeric(grid[[1L]]),
                      stringsAsFactors = FALSE))
  m <- as.matrix(grid)
  d <- expand.grid(PriceDiameterClass_cm = as.integer(rownames(grid)),
                   LengthClass_dm = as.integer(colnames(grid)),
                   KEEP.OUT.ATTRS = FALSE)
  out <- data.frame(ProductKey = id, LengthClass_dm = d$LengthClass_dm,
                    PriceDiameterClass_cm = d$PriceDiameterClass_cm,
                    PriceNOKperM3 = as.numeric(m), stringsAsFactors = FALSE)
  out[!is.na(out$PriceNOKperM3), , drop = FALSE]
}

#' Write a grid back into a (minimal) long price matrix, replacing that
#' matrix's rows. For the flat table only the products in the grid change.
pm_from_grid <- function(grid, id, pm) {
  pm  <- pm_minimal(pm)
  new <- .grid_to_long(grid, id)
  drop <- if (identical(id, PM_FLAT_ID))
    !.is_matrix_row(pm) & pm$ProductKey %in% new$ProductKey
  else pm$ProductKey == id & .is_matrix_row(pm)
  pm_minimal(rbind(pm[!drop, , drop = FALSE], new))
}

#' Logical matrix (grid shape): TRUE where scenario differs from default.
#' In the flat table, products with no default price (custom assortments
#' used in the scenario only) have nothing to differ from: FALSE.
pm_diff_mask <- function(default, scenario, id) {
  a <- as.matrix(pm_to_grid(default, id)); b <- as.matrix(pm_to_grid(scenario, id))
  if (!identical(dim(a), dim(b)) || !identical(dimnames(a), dimnames(b))) {
    # Different classes: compare on the scenario's grid.
    a <- a[rownames(b)[rownames(b) %in% rownames(a)],
           colnames(b)[colnames(b) %in% colnames(a)], drop = FALSE]
    out <- matrix(TRUE, nrow(b), ncol(b), dimnames = dimnames(b))
    if (identical(id, PM_FLAT_ID)) out[!rownames(b) %in% rownames(a), ] <- FALSE
    out[rownames(a), colnames(a)] <- !mapply(.same_price, a, b[rownames(a), colnames(a)])
    return(out)
  }
  matrix(!mapply(.same_price, a, b), nrow(a), ncol(a), dimnames = dimnames(a))
}

#' The default grid laid over the scenario grid's rows and columns, for the
#' editor's highlighting (cells are matched by class, not position). Cells
#' the default lacks are NA (so a filled scenario cell shows as differing);
#' flat rows without a default take the scenario value (no highlight). With
#' no default matrix at all, the scenario grid itself (nothing highlighted).
pm_default_aligned <- function(default, scenario, id) {
  b <- as.matrix(pm_to_grid(scenario, id))
  if (!identical(id, PM_FLAT_ID) &&
      !id %in% default$ProductKey[.is_matrix_row(default)]) return(b)
  a <- as.matrix(pm_to_grid(default, id))
  out <- matrix(NA_real_, nrow(b), ncol(b), dimnames = dimnames(b))
  r <- intersect(rownames(b), rownames(a)); k <- intersect(colnames(b), colnames(a))
  out[r, k] <- a[r, k]
  if (identical(id, PM_FLAT_ID)) {
    new <- !rownames(b) %in% rownames(a)
    out[new, ] <- b[new, ]
  }
  out
}

.same_price <- function(x, y) (is.na(x) && is.na(y)) ||
  (!is.na(x) && !is.na(y) && abs(x - y) <= 1e-9)

#' Number of cells that differ between scenario and default, over the
#' matrices both have (a custom assortment priced in one run only has no
#' default to differ from).
pm_n_diff <- function(default, scenario) {
  ids <- intersect(pm_matrix_ids(default), pm_matrix_ids(scenario))
  sum(vapply(ids, function(id)
    sum(pm_diff_mask(default, scenario, id)), numeric(1)))
}

#' Reset one matrix of the scenario to the default.
pm_reset <- function(scenario, default, id) {
  pm_from_grid(pm_to_grid(default, id), id, scenario)
}


# ---- validation -----------------------------------------------------------------

#' Validate a scenario price matrix (long layout).
#'
#' Errors block use; warnings do not. Uses the same product checks as the
#' backend (validate_price_matrix_against_products()).
#'
#' @param pm Long price matrix (any columns; the required four are checked).
#' @param pd ProductData; lc LengthClasses (with LogLength_dm).
#' @param default Optional default matrix: missing cells relative to it are
#'   reported as a warning.
#' @return list(ok, errors, warnings, pm = pm_minimal(pm) or NULL).
validate_scenario_pm <- function(pm, pd, lc, default = NULL) {
  errors <- character(0); warnings <- character(0)
  miss <- setdiff(PM_REQUIRED_COLS, names(pm))
  if (length(miss) > 0L)
    return(list(ok = FALSE, warnings = warnings, pm = NULL,
                errors = paste("Missing column(s):", paste(miss, collapse = ", "))))

  raw_price <- pm$PriceNOKperM3
  num_price <- suppressWarnings(as.numeric(as.character(raw_price)))
  bad_num   <- is.na(num_price) & !is.na(raw_price) & nzchar(trimws(as.character(raw_price)))
  if (any(bad_num))
    errors <- c(errors, sprintf("Non-numeric PriceNOKperM3 in %d row(s), e.g. '%s'.",
                                sum(bad_num), as.character(raw_price[bad_num][1L])))
  for (col in c("LengthClass_dm", "PriceDiameterClass_cm")) {
    raw <- pm[[col]]
    num <- suppressWarnings(as.numeric(as.character(raw)))
    bad <- is.na(num) & !is.na(raw) & nzchar(trimws(as.character(raw)))
    if (any(bad))
      errors <- c(errors, sprintf("Non-numeric %s in %d row(s), e.g. '%s'.",
                                  col, sum(bad), as.character(raw[bad][1L])))
  }
  m <- pm_minimal(pm)
  if (any(m$PriceNOKperM3 < 0, na.rm = TRUE))
    errors <- c(errors, sprintf("Negative price in %d row(s).",
                                sum(m$PriceNOKperM3 < 0, na.rm = TRUE)))
  unknown <- setdiff(unique(m$ProductKey), pd$ProductKey)
  if (length(unknown) > 0L)
    errors <- c(errors, paste("Unknown ProductKey:", paste(unknown, collapse = ", ")))

  saw <- m[.is_matrix_row(m), , drop = FALSE]
  if (nrow(saw) > 0L) {
    if (any(is.na(saw$PriceDiameterClass_cm)))
      errors <- c(errors, "Sawlog row(s) without PriceDiameterClass_cm.")
    ok_len <- paste(lc$ProductKey, lc$LogLength_dm)
    bad_len <- unique(saw[!paste(saw$ProductKey, saw$LengthClass_dm) %in% ok_len &
                            saw$ProductKey %in% pd$ProductKey,
                          c("ProductKey", "LengthClass_dm")])
    if (nrow(bad_len) > 0L)
      errors <- c(errors, paste("Length class(es) not in LengthClasses:",
                                paste(paste0(bad_len$ProductKey, " ", bad_len$LengthClass_dm, " dm"),
                                      collapse = ", ")))
    # Classes below the product's minimum top diameter are never used.
    min_cls <- floor(as.numeric(pd$MinTopDiameter[match(saw$ProductKey, pd$ProductKey)]) / 10)
    below <- !is.na(min_cls) & saw$PriceDiameterClass_cm < min_cls
    if (any(below, na.rm = TRUE))
      warnings <- c(warnings, sprintf(
        "%d matrix row(s) with diameter classes below the product's minimum top diameter; they are never used in pricing.",
        sum(below, na.rm = TRUE)))
    # The largest class is the product's price-diameter cap (Phase 4b).
    if (!is.null(default)) {
      dm  <- pm_minimal(default); dm <- dm[.is_matrix_row(dm), , drop = FALSE]
      cap_new <- tapply(saw$PriceDiameterClass_cm, saw$ProductKey, max, na.rm = TRUE)
      cap_def <- tapply(dm$PriceDiameterClass_cm, dm$ProductKey, max, na.rm = TRUE)
      k <- intersect(names(cap_new), names(cap_def))
      k <- k[cap_new[k] != cap_def[k]]
      for (pk in k)
        warnings <- c(warnings, sprintf(
          "%s: largest diameter class is %d cm (default %d cm); larger logs are priced at %d cm.",
          pk, cap_new[[pk]], cap_def[[pk]], cap_new[[pk]]))
    }
  }
  key <- paste(m$ProductKey, m$LengthClass_dm, m$PriceDiameterClass_cm)
  if (any(duplicated(key)))
    errors <- c(errors, sprintf("Duplicated price key(s): %d, e.g. %s.",
                                sum(duplicated(key)), key[duplicated(key)][1L]))

  if (length(errors) == 0L) {
    v <- validate_price_matrix_against_products(pd, lc, m)$checks
    fails <- v[v$result == "FAIL", , drop = FALSE]
    if (nrow(fails) > 0L)
      errors <- c(errors, paste0(fails$check, ": ", fails$detail))
    rev <- v[v$result == "REVIEW", , drop = FALSE]
    # Zero-priced waste is an accepted assumption (CLAUDE.md); not a warning.
    zero_rows <- m[!is.na(m$PriceNOKperM3) & m$PriceNOKperM3 == 0, , drop = FALSE]
    waste_keys <- pd$ProductKey[pd$ProductType == "waste"]
    if (nrow(zero_rows) > 0L && all(zero_rows$ProductKey %in% waste_keys))
      rev <- rev[!grepl("zero", rev$check, ignore.case = TRUE), , drop = FALSE]
    if (nrow(rev) > 0L) warnings <- c(warnings, paste0(rev$check, ": ", rev$detail))
    if (!is.null(default)) {
      d <- pm_minimal(default)
      d <- d[d$ProductKey %in% pd$ProductKey, , drop = FALSE]   # this run's products
      dk <- paste(d$ProductKey, d$LengthClass_dm, d$PriceDiameterClass_cm)
      n_missing <- sum(!dk %in% key)
      if (n_missing > 0L)
        warnings <- c(warnings, sprintf(
          "%d price cell(s) of the default are missing; candidates there stay unpriced.",
          n_missing))
    }
  }
  list(ok = length(errors) == 0L, errors = errors, warnings = warnings,
       pm = if (length(errors) == 0L) m else NULL)
}


# ---- upload -----------------------------------------------------------------------

#' Parse one wide grid (raw character table incl. header row) to long rows.
#'
#' @param raw   data.frame/matrix of character, header row included.
#' @param id    Matrix id (ProductKey); if NULL, read from the top-left cell.
#' @param valid_ids Allowed sawlog ProductKeys.
#' @return list(long = data.frame or NULL, id, errors).
.parse_wide_grid <- function(raw, id = NULL, valid_ids) {
  raw <- as.matrix(raw)
  raw[is.na(raw)] <- ""
  errs <- character(0)
  corner <- unname(trimws(as.character(raw[1L, 1L])))
  if (is.null(id)) id <- corner
  id <- unname(as.character(id))
  if (!id %in% valid_ids)
    return(list(long = NULL, id = id, errors = sprintf(
      "Unknown matrix '%s' (expected one of: %s).", id, paste(valid_ids, collapse = ", "))))
  if (nzchar(corner) && corner != id)
    errs <- c(errs, sprintf("Top-left cell '%s' does not match sheet '%s'.", corner, id))
  hdr  <- unname(trimws(as.character(raw[1L, -1L])))
  keep <- nzchar(hdr)
  lens <- suppressWarnings(as.numeric(hdr[keep]))
  if (any(is.na(lens) | lens != round(lens)))
    errs <- c(errs, sprintf("%s: header must be numeric length classes (dm); found '%s'.",
                            id, paste(hdr[keep][is.na(lens) | lens != round(lens)], collapse = "', '")))
  rows <- raw[-1L, , drop = FALSE]
  rows <- rows[nzchar(trimws(rows[, 1L])), , drop = FALSE]
  diams <- suppressWarnings(as.numeric(trimws(rows[, 1L])))
  if (any(is.na(diams) | diams != round(diams)))
    errs <- c(errs, sprintf("%s: row labels must be numeric diameter classes (cm); found '%s'.",
                            id, paste(rows[is.na(diams) | diams != round(diams), 1L], collapse = "', '")))
  if (any(duplicated(lens)))  errs <- c(errs, sprintf("%s: duplicated length column(s).", id))
  if (any(duplicated(diams))) errs <- c(errs, sprintf("%s: duplicated diameter row(s).", id))
  if (length(errs) > 0L) return(list(long = NULL, id = id, errors = errs))

  cells <- rows[, -1L, drop = FALSE][, keep, drop = FALSE]
  vals  <- suppressWarnings(matrix(as.numeric(trimws(cells)), nrow(cells)))
  bad   <- is.na(vals) & nzchar(trimws(cells))
  if (any(bad)) {
    i <- which(bad, arr.ind = TRUE)[1L, ]
    return(list(long = NULL, id = id, errors = sprintf(
      "%s: non-numeric price in %d cell(s), e.g. '%s' at %s cm / %s dm.",
      id, sum(bad), trimws(cells[i[1], i[2]]), diams[i[1]], lens[i[2]])))
  }
  d <- expand.grid(r = seq_along(diams), c = seq_along(lens))
  long <- data.frame(ProductKey = id, LengthClass_dm = as.integer(lens[d$c]),
                     PriceDiameterClass_cm = as.integer(diams[d$r]),
                     PriceNOKperM3 = vals[cbind(d$r, d$c)], stringsAsFactors = FALSE)
  list(long = long[!is.na(long$PriceNOKperM3), , drop = FALSE], id = id, errors = character(0))
}

#' Read an uploaded price file; detect long or wide layout.
#'
#' @param path     File on disk (e.g. fileInput datapath).
#' @param filename Original name (for the extension); defaults to path.
#' @param default  Default/current price matrix (gives the matrix ids).
#' @param matrix_keys Further matrix ProductKeys accepted in a wide upload
#'   (matrix-priced custom assortments).
#' @return list(layout = "long"|"wide", long = data.frame (raw long rows, for
#'   validation), ids = matrices contained (wide) or all ids (long), errors).
read_price_matrix_upload <- function(path, filename = path, default,
                                     matrix_keys = character(0)) {
  ext <- tolower(tools::file_ext(filename))
  saw_ids <- union(setdiff(pm_matrix_ids(default), PM_FLAT_ID), matrix_keys)
  fail <- function(msg) list(layout = NA_character_, long = NULL, ids = character(0),
                             errors = msg)

  if (ext == "rds") {
    x <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.data.frame(x)) return(fail("RDS file does not contain a data frame."))
    return(list(layout = "long", long = as.data.frame(x), ids = pm_matrix_ids(default),
                errors = character(0)))
  }

  if (ext == "xlsx") {
    sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) NULL)
    if (is.null(sheets)) return(fail("Cannot read the xlsx file."))
    longs <- list(); ids <- character(0); errs <- character(0)
    for (sh in sheets) {
      raw <- as.data.frame(readxl::read_excel(path, sheet = sh, col_names = FALSE,
                                              col_types = "text", .name_repair = "minimal"))
      if (identical(sh, PM_FLAT_ID)) {
        hdr <- trimws(as.character(unlist(raw[1L, ])))
        if (!all(c("ProductKey", "PriceNOKperM3") %in% hdr)) {
          errs <- c(errs, "flat_prices: needs columns ProductKey and PriceNOKperM3.")
          next
        }
        body <- raw[-1L, , drop = FALSE]; names(body) <- hdr
        longs[[sh]] <- data.frame(ProductKey = trimws(body$ProductKey),
                                  LengthClass_dm = NA, PriceDiameterClass_cm = NA,
                                  PriceNOKperM3 = trimws(body$PriceNOKperM3),
                                  stringsAsFactors = FALSE)
        ids <- c(ids, PM_FLAT_ID)
      } else {
        p <- .parse_wide_grid(raw, id = sh, valid_ids = saw_ids)
        errs <- c(errs, p$errors)
        if (!is.null(p$long)) { longs[[sh]] <- p$long; ids <- c(ids, sh) }
      }
    }
    if (length(errs) > 0L) return(fail(errs))
    if (length(longs) == 0L) return(fail("No price sheets found in the xlsx file."))
    return(list(layout = "wide", long = do.call(rbind, longs), ids = ids,
                errors = character(0)))
  }

  if (ext == "csv") {
    raw <- tryCatch(utils::read.csv(path, header = FALSE, colClasses = "character",
                                    check.names = FALSE, na.strings = character(0)),
                    error = function(e) NULL)
    if (is.null(raw) || nrow(raw) < 2L) return(fail("Cannot read the CSV file."))
    hdr <- trimws(as.character(unlist(raw[1L, ])))
    if ("ProductKey" %in% hdr) {   # long layout; missing columns are reported by validation
      body <- raw[-1L, , drop = FALSE]; names(body) <- hdr
      body[] <- lapply(body, function(x) { x <- trimws(x); x[x %in% c("", "NA")] <- NA; x })
      return(list(layout = "long", long = body, ids = pm_matrix_ids(default),
                  errors = character(0)))
    }
    if (hdr[1L] %in% saw_ids) {
      p <- .parse_wide_grid(raw, id = NULL, valid_ids = saw_ids)
      if (length(p$errors) > 0L) return(fail(p$errors))
      return(list(layout = "wide", long = p$long, ids = p$id, errors = character(0)))
    }
    return(fail(paste0(
      "Unrecognised layout: expected long columns (ProductKey, LengthClass_dm, ",
      "PriceDiameterClass_cm, PriceNOKperM3) or a wide grid with a ",
      "matrix name (", paste(saw_ids, collapse = " / "), ") in the top-left cell.")))
  }
  fail(sprintf("Unsupported file type '.%s' (use .csv, .xlsx or .rds).", ext))
}

#' Apply an upload to the scenario: read, merge (wide replaces only its
#' matrices), validate. Returns list(ok, pm, layout, ids, errors, warnings).
apply_price_upload <- function(path, filename, scenario, default, pd, lc) {
  mk <- if ("PriceType" %in% names(pd)) pd$ProductKey[pd$PriceType %in% "matrix"]
        else character(0)
  up <- read_price_matrix_upload(path, filename, default, matrix_keys = mk)
  if (length(up$errors) > 0L)
    return(list(ok = FALSE, pm = NULL, layout = up$layout, ids = up$ids,
                errors = up$errors, warnings = character(0)))
  candidate <- if (identical(up$layout, "long")) up$long else {
    keep <- pm_minimal(scenario)
    drop <- (keep$ProductKey %in% up$ids & .is_matrix_row(keep)) |
            (PM_FLAT_ID %in% up$ids & !.is_matrix_row(keep) &
               keep$ProductKey %in% up$long$ProductKey[is.na(up$long$LengthClass_dm)])
    rbind(keep[!drop, , drop = FALSE],
          up$long[, PM_REQUIRED_COLS, drop = FALSE])
  }
  v <- validate_scenario_pm(candidate, pd, lc, default = default)
  list(ok = v$ok, pm = v$pm, layout = up$layout, ids = up$ids,
       errors = v$errors, warnings = v$warnings)
}


# ---- templates and export ---------------------------------------------------------

#' Long CSV of a full price matrix: the four required columns.
pm_export <- function(pm, path) {
  readr::write_csv(pm_minimal(pm), path, na = "")
  invisible(path)
}

#' Long-layout template (CSV): the default prices, four columns.
pm_template_long <- function(default, path) pm_export(default, path)

#' Wide-layout template (xlsx): one sheet per sawlog matrix in the source
#' layout (top-left cell = ProductKey, header = lengths dm, rows = diameter
#' classes cm) plus a flat_prices sheet; filled with the default prices.
pm_template_wide <- function(default, path) {
  d <- pm_minimal(default)
  sheets <- lapply(setdiff(pm_matrix_ids(d), PM_FLAT_ID), function(id) {
    g <- pm_to_grid(d, id)
    out <- cbind(setNames(data.frame(as.integer(rownames(g))), id), g)
    names(out) <- c(id, colnames(g))
    out
  })
  names(sheets) <- setdiff(pm_matrix_ids(d), PM_FLAT_ID)
  f <- pm_to_grid(d, PM_FLAT_ID)
  sheets[[PM_FLAT_ID]] <- data.frame(ProductKey = rownames(f),
                                     PriceNOKperM3 = f$PriceNOKperM3,
                                     stringsAsFactors = FALSE)
  writexl::write_xlsx(sheets, path)
  invisible(path)
}


# ---- custom assortments (Phase 4b) --------------------------------------------------

#' An empty assortment-definition table.
empty_assortment_defs <- function() {
  d <- as.data.frame(setNames(replicate(length(ASSORTMENT_COLS), character(0), simplify = FALSE),
                              ASSORTMENT_COLS), stringsAsFactors = FALSE)
  .type_assortment_defs(d)
}

# Column types of a definition table (numbers numeric, flags logical, rest
# character; AllowedLengths_cm stays a "310;340" string).
.type_assortment_defs <- function(d) {
  for (n in c("MinTopDiameter_cm", "MaxTopDiameter_cm", "MinButtDiameter_cm",
              "MaxButtDiameter_cm", "Priority", "DiameterTopPosition_cm"))
    d[[n]] <- suppressWarnings(as.numeric(d[[n]]))
  d$DiameterUnderBark <- as.logical(d$DiameterUnderBark)
  for (n in c("Name", "Class", "Species", "AllowedLengths_cm", "VolumeDiameterCategory",
              "VolumeDiameterAdjustment", "PriceType", "UseIn"))
    d[[n]] <- as.character(d[[n]])
  d[, ASSORTMENT_COLS, drop = FALSE]
}


#' Check custom assortment definitions against the base products and each
#' other. Product rules are those of validate_products().
#'
#' @param defs Definition table (ASSORTMENT_COLS).
#' @param base_pd,base_lc Base ProductData / LengthClasses.
#' @return list(ok, errors, warnings).
validate_assortment_defs <- function(defs, base_pd, base_lc) {
  err <- character(0)
  miss <- setdiff(ASSORTMENT_COLS, names(defs))
  if (length(miss) > 0L)
    return(list(ok = FALSE, warnings = character(0),
                errors = paste("Assortment definitions missing column(s):",
                               paste(miss, collapse = ", "))))
  if (nrow(defs) == 0L) return(list(ok = TRUE, errors = err, warnings = character(0)))
  slug <- vapply(defs$Name, assortment_slug, character(1))
  if (any(!nzchar(slug))) err <- c(err, "Every assortment needs a name with letters or digits.")
  if (any(duplicated(slug[nzchar(slug)])))
    err <- c(err, paste("Duplicated assortment name:",
                        paste(unique(defs$Name[duplicated(slug)]), collapse = ", ")))
  bad_sp <- !defs$Species %in% c(PRODUCT_SPECIES, "both")
  if (any(bad_sp)) err <- c(err, sprintf("Species must be spruce, pine or both: %s",
                                         paste(defs$Name[bad_sp], collapse = ", ")))
  bad_use <- !defs$UseIn %in% ASSORTMENT_USE_IN
  if (any(bad_use)) err <- c(err, sprintf("UseIn must be one of %s: %s",
                                          paste(ASSORTMENT_USE_IN, collapse = "/"),
                                          paste(defs$Name[bad_use], collapse = ", ")))
  no_len <- vapply(defs$AllowedLengths_cm, function(x) {
    v <- parse_lengths_cm(x); length(v) == 0L || anyNA(v)
  }, logical(1))
  if (any(no_len)) err <- c(err, sprintf("Allowed lengths must be whole cm, e.g. 310;340: %s",
                                         paste(defs$Name[no_len], collapse = ", ")))
  if (length(err) > 0L) return(list(ok = FALSE, errors = err, warnings = character(0)))

  all_defs <- defs; all_defs$UseIn <- "both"
  p <- products_for_run(base_pd, base_lc, all_defs, "default")
  clash <- intersect(p$pd$ProductKey[-seq_len(nrow(base_pd))], base_pd$ProductKey)
  if (length(clash) > 0L)
    err <- c(err, paste("Assortment name clashes with a base product:",
                        paste(clash, collapse = ", ")))
  v <- validate_products(p$pd, p$lc)
  list(ok = length(err) == 0L && v$ok, errors = c(err, v$errors), warnings = v$warnings)
}


#' Starting prices for one custom assortment, long minimal rows.
#'
#' Matrix: diameter classes from the minimum top diameter up to the maximum
#' top diameter (not capped at 50 cm; the largest class is the assortment's
#' price-diameter cap) x the allowed lengths, all at `price`. A maximum of
#' Inf needs `max_class_cm`. Flat: one row per species product.
#'
#' @param def One definition row (ASSORTMENT_COLS).
#' @param price Starting price, NOK/m3.
#' @param max_class_cm Largest diameter class (cm) when MaxTopDiameter_cm is
#'   Inf or NA.
pm_new_assortment_prices <- function(def, price, max_class_cm = NA) {
  r <- custom_assortment_rows(def)
  keys <- r$pd$ProductKey
  if (identical(as.character(def$PriceType[1L]), "flat"))
    return(pm_minimal(data.frame(ProductKey = keys, LengthClass_dm = NA,
                                 PriceDiameterClass_cm = NA, PriceNOKperM3 = price)))
  lo <- floor(r$pd$MinTopDiameter[1L] / 10)
  hi <- if (is.finite(r$pd$MaxTopDiameter[1L])) floor(r$pd$MaxTopDiameter[1L] / 10)
        else as.numeric(max_class_cm)
  if (is.na(hi) || hi < lo)
    stop("Give the largest diameter class for an assortment without a maximum top diameter.")
  lens <- sort(unique(r$lc$LogLength_dm))
  g <- expand.grid(PriceDiameterClass_cm = as.integer(seq(lo, hi)),
                   LengthClass_dm = as.integer(lens), ProductKey = keys,
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  pm_minimal(data.frame(ProductKey = g$ProductKey, LengthClass_dm = g$LengthClass_dm,
                        PriceDiameterClass_cm = g$PriceDiameterClass_cm,
                        PriceNOKperM3 = price, stringsAsFactors = FALSE))
}


#' Custom ProductKeys of a definition table.
assortment_keys <- function(defs) {
  if (is.null(defs) || nrow(defs) == 0L) return(character(0))
  unlist(lapply(seq_len(nrow(defs)), function(i)
    custom_assortment_rows(defs[i, , drop = FALSE])$pd$ProductKey))
}


#' Write custom assortments with their prices to one xlsx: sheets
#' "assortments" (definitions), "default_prices" and "scenario_prices" (long
#' layout, custom ProductKeys only).
assortments_export <- function(defs, default_pm, scenario_pm, path) {
  keys <- assortment_keys(defs)
  pick <- function(pm) { m <- pm_minimal(pm); m[m$ProductKey %in% keys, , drop = FALSE] }
  writexl::write_xlsx(list(assortments = defs[, ASSORTMENT_COLS, drop = FALSE],
                           default_prices = pick(default_pm),
                           scenario_prices = pick(scenario_pm)), path)
  invisible(path)
}


#' Read an assortments xlsx (assortments_export() layout) and merge it into
#' the default and scenario price matrices. The upload replaces all custom
#' assortments; base prices are kept.
#'
#' @return list(ok, defs, default_pm, scenario_pm, errors, warnings).
assortments_import <- function(path, base_pd, base_lc, default_pm, scenario_pm) {
  fail <- function(msg) list(ok = FALSE, defs = NULL, default_pm = NULL,
                             scenario_pm = NULL, errors = msg, warnings = character(0))
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) NULL)
  if (is.null(sheets)) return(fail("Cannot read the xlsx file."))
  need <- c("assortments", "default_prices", "scenario_prices")
  if (!all(need %in% sheets))
    return(fail(paste("The xlsx needs sheets:", paste(need, collapse = ", "))))
  rd <- function(sh) as.data.frame(readxl::read_excel(path, sheet = sh, col_types = "text"))
  defs <- rd("assortments")
  if (length(setdiff(ASSORTMENT_COLS, names(defs))) > 0L)
    return(fail(paste("assortments sheet missing column(s):",
                      paste(setdiff(ASSORTMENT_COLS, names(defs)), collapse = ", "))))
  defs <- .type_assortment_defs(defs)
  v <- validate_assortment_defs(defs, base_pd, base_lc)
  if (!v$ok) return(fail(v$errors))

  base_keys <- base_pd$ProductKey
  merge_pm <- function(pm, new) {
    keep <- pm_minimal(pm); keep <- keep[keep$ProductKey %in% base_keys, , drop = FALSE]
    pm_minimal(rbind(keep, new[, PM_REQUIRED_COLS, drop = FALSE]))
  }
  errs <- character(0); warns <- v$warnings; out <- list()
  for (run in c("default", "scenario")) {
    p   <- products_for_run(base_pd, base_lc, defs, run)
    new <- rd(paste0(run, "_prices"))
    new <- new[new$ProductKey %in% p$pd$ProductKey, , drop = FALSE]
    pm  <- merge_pm(if (run == "default") default_pm else scenario_pm, new)
    vv  <- validate_scenario_pm(pm, p$pd, p$lc)
    if (!vv$ok) errs <- c(errs, paste0(run, " prices: ", vv$errors))
    warns <- c(warns, if (length(vv$warnings)) paste0(run, " prices: ", vv$warnings))
    out[[run]] <- vv$pm
  }
  if (length(errs) > 0L) return(fail(errs))
  list(ok = TRUE, defs = defs, default_pm = out$default, scenario_pm = out$scenario,
       errors = character(0), warnings = warns)
}
