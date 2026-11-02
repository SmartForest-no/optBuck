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


#' Optional hidden column of a price matrix (Phase 7a, 2026-10-30): the
#' target distribution (percent) of a cell, read from .hpr/.pin files and
#' written back on export. It is not used by the optimizer. The column exists
#' only when some cell has a value; NA = the cell has none.
PM_DISTRIBUTION_COL <- "Distribution"

#' Second optional hidden column (2026-10-30): the lower limit (mm) of the
#' .hpr/.pin diameter class a whole-cm row was spread from (e.g. 142). Kept
#' like Distribution; used by price tuning only; never in a run or a price
#' file; not in the scenario xlsx. NA = not from a file / a new row.
PM_FILE_CLASS_COL <- "FileDiameterClass_mm"
PM_HIDDEN_COLS <- c(PM_DISTRIBUTION_COL, PM_FILE_CLASS_COL)

#' The four columns Apter uses, typed, from any PriceMatrix_long. A hidden
#' Distribution column is kept when the matrix has one (see pm_prices()).
pm_minimal <- function(pm) {
  out <- data.frame(
    ProductKey            = as.character(pm$ProductKey),
    LengthClass_dm        = suppressWarnings(as.integer(pm$LengthClass_dm)),
    PriceDiameterClass_cm = suppressWarnings(as.integer(pm$PriceDiameterClass_cm)),
    PriceNOKperM3         = suppressWarnings(as.numeric(pm$PriceNOKperM3)),
    stringsAsFactors = FALSE)
  for (h in intersect(PM_HIDDEN_COLS, names(pm)))
    out[[h]] <- suppressWarnings(as.numeric(pm[[h]]))
  out <- out[order(out$ProductKey, out$LengthClass_dm, out$PriceDiameterClass_cm,
                   na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' The price columns only (no hidden Distribution): what runs, the backend and
#' price files see.
pm_prices <- function(pm) {
  out <- pm_minimal(pm)
  out[, PM_REQUIRED_COLS, drop = FALSE]
}

#' rbind of price matrices where a hidden Distribution column may be on one
#' side only: the other side gets NA.
pm_bind <- function(...) {
  l <- Filter(function(x) !is.null(x), list(...))
  for (h in PM_HIDDEN_COLS)
    if (any(vapply(l, function(x) h %in% names(x), logical(1))))
      l <- lapply(l, function(x) { if (!h %in% names(x)) x[[h]] <- rep(NA_real_, nrow(x)); x })
  do.call(rbind, l)
}

.is_matrix_row <- function(pm) !is.na(pm$LengthClass_dm)

#' Editable matrix ids: sawlog ProductKeys (spruce before pine) + flat.
pm_matrix_ids <- function(pm) {
  saw <- unique(as.character(pm$ProductKey[.is_matrix_row(pm)]))
  sp  <- match(sub("_.*$", "", saw), c("spruce", "pine"))
  c(saw[order(sp, saw, na.last = TRUE)], PM_FLAT_ID)
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
  pm_drop_empty_cells(out)
}

#' Drop the empty (NA-price) cells of matrix rows, except where a whole
#' diameter class is empty: that class keeps its rows with NA prices (an
#' "empty row", e.g. a class forbidden in an .hpr file), so the classes stay
#' consecutive and the class is never chosen (its lookup is NA, unpriced).
#' A class missing altogether is still a gap (validate_scenario_pm()).
pm_drop_empty_cells <- function(long) {
  na <- is.na(long$PriceNOKperM3)
  if (!any(na)) return(long)
  cls <- paste(long$ProductKey, long$PriceDiameterClass_cm)
  all_na <- tapply(na, cls, all)
  long[!na | unname(all_na[cls]), , drop = FALSE]
}

#' Write a grid back into a (minimal) long price matrix, replacing that
#' matrix's rows. For the flat table only the products in the grid change.
pm_from_grid <- function(grid, id, pm) {
  pm  <- pm_minimal(pm)
  new <- .grid_to_long(grid, id)
  drop <- if (identical(id, PM_FLAT_ID))
    !.is_matrix_row(pm) & pm$ProductKey %in% new$ProductKey
  else pm$ProductKey == id & .is_matrix_row(pm)
  # The hidden Distribution of a cell stays with the cell (a price edit never
  # changes it); cells new to the grid have none.
  # The file diameter class likewise stays with the row.
  hid <- intersect(PM_HIDDEN_COLS, names(pm))
  if (length(hid)) {
    old <- pm[drop, , drop = FALSE]
    hit <- match(paste(new$ProductKey, new$LengthClass_dm, new$PriceDiameterClass_cm),
                 paste(old$ProductKey, old$LengthClass_dm, old$PriceDiameterClass_cm))
    for (h in hid) new[[h]] <- old[[h]][hit]
  }
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


# ---- rows and columns (Phase 4c) ---------------------------------------------------
# A matrix's columns are its allowed lengths (dm) and its rows its
# top-diameter classes (cm). Classes are consecutive whole cm, so rows are
# added and removed at the ends only; a new row or column copies its
# neighbour's prices.

.matrix_rows <- function(pm, id) pm[pm$ProductKey == id & .is_matrix_row(pm), , drop = FALSE]

#' Add a diameter class below the smallest ("low") or above the largest
#' ("high") class of matrix `id`.
#'
#' @param max_top_cm The assortment's max top diameter (cm): a class above
#'   floor(max_top_cm) is never used (a log's price class is its top
#'   diameter rounded down), so it is not added (2026-10-22). Inf: no limit.
pm_add_class <- function(pm, id, end = c("low", "high"), max_top_cm = Inf) {
  end <- match.arg(end); pm <- pm_minimal(pm); s <- .matrix_rows(pm, id)
  if (nrow(s) == 0L) stop("No matrix '", id, "'.")
  from <- if (end == "low") min(s$PriceDiameterClass_cm) else max(s$PriceDiameterClass_cm)
  to   <- from + if (end == "low") -1L else 1L
  if (to < 1L) stop("The smallest diameter class is 1 cm.")
  if (end == "high" && isTRUE(to > floor(max_top_cm)))
    stop(sprintf("The largest diameter class is %d cm: the max top diameter is %s cm.",
                 as.integer(floor(max_top_cm)), format(max_top_cm)))
  new <- s[s$PriceDiameterClass_cm == from, , drop = FALSE]
  new$PriceDiameterClass_cm <- as.integer(to)
  for (h in intersect(PM_HIDDEN_COLS, names(new))) new[[h]] <- NA_real_   # a new cell has none
  pm_minimal(rbind(pm, new))
}

#' Remove the smallest ("low") or largest ("high") diameter class of matrix `id`.
pm_remove_class <- function(pm, id, end = c("low", "high")) {
  end <- match.arg(end); pm <- pm_minimal(pm); s <- .matrix_rows(pm, id)
  cls <- unique(s$PriceDiameterClass_cm)
  if (length(cls) <= 1L) stop("A matrix needs at least one diameter class.")
  drop <- if (end == "low") min(cls) else max(cls)
  pm_minimal(pm[!(pm$ProductKey == id & .is_matrix_row(pm) & pm$PriceDiameterClass_cm == drop), ,
                drop = FALSE])
}

#' Add length `dm` (whole dm) to matrix `id`, copying the nearest length's
#' prices (the shorter one on a tie).
pm_add_length <- function(pm, id, dm) {
  pm <- pm_minimal(pm); s <- .matrix_rows(pm, id)
  dm <- suppressWarnings(as.numeric(dm))
  if (length(dm) != 1L || is.na(dm) || dm <= 0 || dm != round(dm))
    stop("A length is a positive whole number of dm.")
  lens <- sort(unique(s$LengthClass_dm))
  if (dm %in% lens) stop(sprintf("Length %d dm is already in the matrix.", as.integer(dm)))
  from <- lens[which.min(abs(lens - dm))]
  new <- s[s$LengthClass_dm == from, , drop = FALSE]
  new$LengthClass_dm <- as.integer(dm)
  for (h in intersect(PM_HIDDEN_COLS, names(new))) new[[h]] <- NA_real_   # a new cell has none
  pm_minimal(rbind(pm, new))
}

#' Remove length `dm` from matrix `id`: that length is no longer allowed.
pm_remove_length <- function(pm, id, dm) {
  pm <- pm_minimal(pm); s <- .matrix_rows(pm, id)
  lens <- unique(s$LengthClass_dm)
  if (!as.integer(dm) %in% lens) stop(sprintf("Length %s dm is not in the matrix.", dm))
  if (length(lens) <= 1L) stop("A matrix needs at least one length.")
  pm_minimal(pm[!(pm$ProductKey == id & .is_matrix_row(pm) & pm$LengthClass_dm == as.integer(dm)), ,
                drop = FALSE])
}

# ---- bulk edits (Phase 4) ----------------------------------------------------------
# Prices of a range or of the whole matrix changed at once. Empty (forbidden)
# cells stay empty; results are rounded to 2 decimals and never below 0.

.pm_matrix_grid <- function(pm, id) {
  if (identical(id, PM_FLAT_ID) || nrow(.matrix_rows(pm_minimal(pm), id)) == 0L)
    stop("No matrix '", id, "'.")
  pm_to_grid(pm_minimal(pm), id)
}

.pm_bulk_price <- function(x) pmax(round(x, 2), 0)

#' Add `value` NOK/m3 ("amount") or `value` % ("percent") to the priced cells
#' of rows `rows` x columns `cols` (grid indices) of matrix `id`.
pm_bulk_adjust <- function(pm, id, rows, cols, mode = c("amount", "percent"), value) {
  mode <- match.arg(mode)
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || !is.finite(value)) stop("Give the amount or percentage as a number.")
  g <- .pm_matrix_grid(pm, id)
  rows <- intersect(as.integer(rows), seq_len(nrow(g)))
  cols <- intersect(as.integer(cols), seq_len(ncol(g)))
  if (length(rows) == 0L || length(cols) == 0L) stop("Select cells in the grid first.")
  m <- as.matrix(g)
  v <- m[rows, cols, drop = FALSE]
  new <- .pm_bulk_price(if (mode == "amount") v + value else v * (1 + value / 100))
  m[rows, cols] <- ifelse(is.na(v), NA_real_, new)
  g[] <- m
  pm_from_grid(g, id, pm)
}

#' Fill matrix `id`: every priced cell = the price of its diameter class
#' (`row_prices`, named by class in cm) + the correction of its length class
#' (`length_corr`, named by dm). Empty cells stay empty.
pm_bulk_fill <- function(pm, id, row_prices, length_corr) {
  g <- .pm_matrix_grid(pm, id)
  rp <- suppressWarnings(as.numeric(row_prices[rownames(g)]))
  lc <- suppressWarnings(as.numeric(length_corr[colnames(g)]))
  lc[is.na(lc)] <- 0
  m <- as.matrix(g)
  need <- rowSums(!is.na(m)) > 0L
  if (any(is.na(rp) & need) || any(!is.finite(rp[!is.na(rp)])))
    stop("Give a price for every diameter class.")
  new <- .pm_bulk_price(outer(rp, lc, "+"))
  m[] <- ifelse(is.na(m), NA_real_, new)
  g[] <- m
  pm_from_grid(g, id, pm)
}

#' Each diameter class's first priced cell (the fill dialog's starting prices);
#' NA for an empty row.
pm_row_first_prices <- function(pm, id) {
  m <- as.matrix(.pm_matrix_grid(pm, id))
  stats::setNames(apply(m, 1L, function(r) if (all(is.na(r))) NA_real_ else r[!is.na(r)][1L]),
                  rownames(m))
}

#' Matrices whose diameter classes have gaps (classes must be consecutive cm).
.pm_class_gaps <- function(pm) {
  s <- pm[.is_matrix_row(pm) & !is.na(pm$PriceDiameterClass_cm), , drop = FALSE]
  ids <- unique(s$ProductKey)
  ids[vapply(ids, function(id) {
    cls <- sort(unique(s$PriceDiameterClass_cm[s$ProductKey == id]))
    length(cls) > 1L && any(diff(cls) != 1L)
  }, logical(1))]
}


# ---- assortment table prices (Phase 4c) ---------------------------------------------

#' Starting prices of a new product: a grid of diameter classes
#' class_lo..class_hi (cm) x lengths (cm, whole dm), or one flat price.
pm_start_prices <- function(key, price_type, price, class_lo = NA, class_hi = NA,
                            lengths_cm = numeric(0)) {
  if (identical(price_type, "flat"))
    return(pm_minimal(data.frame(ProductKey = key, LengthClass_dm = NA,
                                 PriceDiameterClass_cm = NA, PriceNOKperM3 = price)))
  lo <- suppressWarnings(as.integer(class_lo)); hi <- suppressWarnings(as.integer(class_hi))
  if (is.na(lo) || is.na(hi) || lo < 1L || hi < lo)
    stop("Give the smallest and largest diameter class (whole cm, smallest >= 1).")
  dm <- as.numeric(lengths_cm) / 10
  if (length(dm) == 0L || any(is.na(dm) | dm != round(dm) | dm <= 0))
    stop("A price grid needs lengths in whole dm (multiples of 10 cm).")
  g <- expand.grid(PriceDiameterClass_cm = seq(lo, hi), LengthClass_dm = as.integer(dm),
                   KEEP.OUT.ATTRS = FALSE)
  pm_minimal(data.frame(ProductKey = key, LengthClass_dm = g$LengthClass_dm,
                        PriceDiameterClass_cm = g$PriceDiameterClass_cm,
                        PriceNOKperM3 = price, stringsAsFactors = FALSE))
}

#' Give product `from`'s prices to `to` (an assortment renamed or moved to
#' another species).
pm_rename_key <- function(pm, from, to) {
  pm <- pm_minimal(pm)
  pm$ProductKey[pm$ProductKey == from] <- to
  pm_minimal(pm)
}

#' Write one scenario (Phase 4d) to an xlsx: sheet "assortments" (the
#' assortment table, cm; Type is the hidden product type, keep it as it is)
#' and its prices in the wide layout of pm_template_wide() (one sheet per
#' matrix, "flat_prices").
# Sheets of a scenario xlsx that carry no price grid.
SCENARIO_META_SHEETS <- c("assortments", "scenario", "file_classes")
.is_target_sheet <- function(x) grepl("^target_[0-9]+$", x)

scenario_export <- function(defs, pm, path) {
  # The target distribution (restructure R2.5): the four fields as columns of
  # the assortment table (only when some assortment has them), one sheet
  # target_<n> per assortment with a target (the price sheet's layout; the
  # corner cell is the ProductKey) and one sheet file_classes (matrix id,
  # diameter row in cm, file diameter class in mm) for rows that have one.
  has_fields <- any(!is.na(unlist(defs[, DISTRIBUTION_DEF_COLS, drop = FALSE])))
  cols <- if (has_fields) ASSORTMENT_TABLE_COLS else setdiff(ASSORTMENT_TABLE_COLS, DISTRIBUTION_DEF_COLS)
  sheets <- c(list(assortments = defs[, cols, drop = FALSE]),
              .pm_wide_sheets(pm), .pm_target_sheets(pm), .pm_file_class_sheet(pm))
  writexl::write_xlsx(sheets, path)
  invisible(path)
}

#' Read a scenario_export() xlsx (assortments plus prices) and check it.
#'
#' @return list(ok, defs, pm, errors, warnings).
scenario_import <- function(path) {
  fail <- function(msg) list(ok = FALSE, errors = msg, warnings = character(0))
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) NULL)
  if (is.null(sheets)) return(fail("Cannot read the xlsx file."))
  if (!"assortments" %in% sheets)
    return(fail("The xlsx needs a sheet 'assortments' and the price sheets."))
  defs <- as.data.frame(readxl::read_excel(path, sheet = "assortments", col_types = "text"))
  miss <- setdiff(setdiff(ASSORTMENT_TABLE_COLS, c("Type", "PriceLengths_cm", "PermittedGrades", "ReferenceGrades", "HprProductKey", DISTRIBUTION_DEF_COLS)), names(defs))
  if (length(miss) > 0L)
    return(fail(paste("assortments sheet missing column(s):", paste(miss, collapse = ", "))))
  defs <- defs[!is.na(defs$Name) & nzchar(trimws(defs$Name)), , drop = FALSE]
  defs <- .type_assortment_table(defs)
  keys <- assortment_table_keys(defs)
  empty <- data.frame(ProductKey = character(0), LengthClass_dm = numeric(0),
                      PriceDiameterClass_cm = numeric(0), PriceNOKperM3 = numeric(0))
  up <- read_price_matrix_upload(path, path, empty,
                                 matrix_keys = keys[defs$PriceType == "matrix"])
  if (length(up$errors) > 0L) return(fail(up$errors))
  v <- check_price_set(defs, up$long)
  if (!v$ok) return(fail(v$errors))
  v$pm <- .pm_attach_hidden(v$pm, path, sheets, keys[defs$PriceType == "matrix"])
  # An older file may have a 'scenario' sheet with a name; it is ignored.
  list(ok = TRUE, defs = defs, pm = v$pm, errors = character(0), warnings = v$warnings)
}

#' Put the target (sheets target_<n>) and the file diameter classes (sheet
#' file_classes) of a scenario xlsx back on the price matrix as its hidden
#' columns. A value with no priced cell to hold it is ignored.
.pm_attach_hidden <- function(pm, path, sheets, matrix_keys) {
  pm <- pm_minimal(pm)
  key <- function(p) paste(p$ProductKey, p$LengthClass_dm, p$PriceDiameterClass_cm)
  for (sh in sheets[.is_target_sheet(sheets)]) {
    raw <- as.data.frame(readxl::read_excel(path, sheet = sh, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
    p <- .parse_wide_grid(raw, id = NULL, valid_ids = matrix_keys)
    if (is.null(p$long)) next
    if (!PM_DISTRIBUTION_COL %in% names(pm)) pm[[PM_DISTRIBUTION_COL]] <- NA_real_
    i <- match(key(pm), key(p$long))
    hit <- !is.na(i) & .is_matrix_row(pm) & !is.na(pm$PriceNOKperM3)
    pm[[PM_DISTRIBUTION_COL]][hit] <- p$long$PriceNOKperM3[i[hit]]
  }
  if ("file_classes" %in% sheets) {
    fc <- as.data.frame(readxl::read_excel(path, sheet = "file_classes", col_types = "text"))
    if (all(c("ProductKey", "PriceDiameterClass_cm", PM_FILE_CLASS_COL) %in% names(fc))) {
      kk <- function(k, c) paste(trimws(k), suppressWarnings(as.integer(c)))
      i <- match(kk(pm$ProductKey, pm$PriceDiameterClass_cm), kk(fc$ProductKey, fc$PriceDiameterClass_cm))
      hit <- !is.na(i) & .is_matrix_row(pm)
      pm[[PM_FILE_CLASS_COL]] <- NA_real_
      pm[[PM_FILE_CLASS_COL]][hit] <- suppressWarnings(as.numeric(fc[[PM_FILE_CLASS_COL]][i[hit]]))
    }
  }
  pm_minimal(pm)
}

#' Check a price set: the assortment table with the price matrix it runs
#' with (validate_assortment_table(), then validate_scenario_pm() against the
#' products it gives).
#'
#' @return list(ok, errors, warnings, pm = minimal matrix or NULL).
check_price_set <- function(defs, pm) {
  m <- if (all(PM_REQUIRED_COLS %in% names(pm))) suppressWarnings(pm_minimal(pm)) else pm
  v <- validate_assortment_table(defs, m)
  if (!v$ok) return(list(ok = FALSE, errors = v$errors, warnings = v$warnings, pm = NULL))
  p <- products_from_defs(defs, m)
  vv <- validate_scenario_pm(pm, p$pd, p$lc)
  vv$warnings <- c(v$warnings, vv$warnings)
  vv
}

# ---- two independent scenarios (Phase 4d) -------------------------------------------
# Scenarios A and B each hold their own assortment table and price matrix;
# B starts as a copy of A and nothing is linked. These helpers compare the
# two and copy an assortment or prices from one to the other.

# A scenario's prices of the given keys, sorted (for comparisons).
.scenario_rows <- function(pm, keys) {
  m <- pm_prices(pm)          # the hidden Distribution never counts as a difference
  m <- m[m$ProductKey %in% keys, , drop = FALSE]
  m <- m[order(m$ProductKey, m$LengthClass_dm, m$PriceDiameterClass_cm, na.last = TRUE), , drop = FALSE]
  rownames(m) <- NULL
  m
}

#' Row-by-row comparison of one scenario's assortment table (`defs`) with
#' the other's (`other`).
#'
#' @return list(status = per row of `defs` "same" / "changed" / "only" (not
#'   in the other scenario), diff = logical matrix (rows of `defs` x
#'   ASSORTMENT_TABLE_COLS) of fields that differ, other_only = keys only in
#'   the other scenario).
assortment_diff <- function(defs, other) {
  k <- assortment_table_keys(defs); ko <- assortment_table_keys(other)
  cols <- setdiff(ASSORTMENT_TABLE_COLS, DISTRIBUTION_DEF_COLS)   # hidden, no effect on runs
  diff <- matrix(FALSE, nrow(defs), length(cols), dimnames = list(k, cols))
  status <- rep("only", nrow(defs))
  for (i in seq_len(nrow(defs))) {
    j <- match(k[i], ko)
    if (is.na(j)) { diff[i, ] <- TRUE; next }
    for (n in cols) {
      x <- other[[n]][j]; y <- defs[[n]][i]
      diff[i, n] <- !((is.na(x) && is.na(y)) || (!is.na(x) && !is.na(y) && x == y))
    }
    status[i] <- if (any(diff[i, ])) "changed" else "same"
  }
  list(status = status, diff = diff, other_only = setdiff(ko, k))
}

#' TRUE when two assortment tables hold the same rows (order ignored).
same_assortments <- function(a, b) {
  if (nrow(a) != nrow(b)) return(FALSE)
  if (!setequal(assortment_table_keys(a), assortment_table_keys(b))) return(FALSE)
  all(assortment_diff(a, b)$status == "same")
}

#' TRUE when the two scenarios' prices of `key` are identical (cells, rows
#' and columns; flat or matrix).
same_prices <- function(pm_a, pm_b, key)
  isTRUE(all.equal(.scenario_rows(pm_a, key), .scenario_rows(pm_b, key), check.attributes = FALSE))

#' TRUE when Scenarios A and B differ: in their assortments (membership or
#' any field), in the prices of their assortments, or in their stem-grade
#' options (Phase 6; NULL = both off). Then a run compares them.
scenarios_differ <- function(defs_a, pm_a, defs_b, pm_b, opts_a = NULL, opts_b = NULL) {
  if (!identical(normalize_grade_opts(opts_a), normalize_grade_opts(opts_b))) return(TRUE)
  if (!same_assortments(defs_a, defs_b)) return(TRUE)
  keys <- assortment_table_keys(defs_a)
  !isTRUE(all.equal(.scenario_rows(pm_a, keys), .scenario_rows(pm_b, keys), check.attributes = FALSE))
}

#' Copy assortment `key` (its definition and prices) from one scenario to
#' the other: it replaces the target's row in place, or is added at the end.
#'
#' @return list(defs, pm): the target scenario after the copy.
copy_assortment <- function(from_defs, from_pm, to_defs, to_pm, key) {
  i <- match(key, assortment_table_keys(from_defs))
  if (is.na(i)) stop("Assortment ", key, " is not in the scenario copied from.")
  j <- match(key, assortment_table_keys(to_defs))
  if (is.na(j)) to_defs <- rbind(to_defs, from_defs[i, , drop = FALSE])
  else to_defs[j, ] <- from_defs[i, ]
  rownames(to_defs) <- NULL
  f <- pm_minimal(from_pm); t <- pm_minimal(to_pm)
  list(defs = to_defs,
       pm = pm_minimal(pm_bind(t[t$ProductKey != key, , drop = FALSE], f[f$ProductKey == key, , drop = FALSE])))
}

#' Copy the prices of `keys` from one scenario to the other, where the
#' assortment is in both and priced the same way (both matrix or both flat).
#'
#' @return list(pm (the target's prices), copied, skipped (keys not copied:
#'   not in both or priced differently)).
copy_prices <- function(from_defs, from_pm, to_defs, to_pm, keys) {
  kf <- assortment_table_keys(from_defs); kt <- assortment_table_keys(to_defs)
  ok <- keys %in% kf & keys %in% kt
  ok[ok] <- from_defs$PriceType[match(keys[ok], kf)] == to_defs$PriceType[match(keys[ok], kt)]
  copied <- keys[ok]
  f <- pm_minimal(from_pm); t <- pm_minimal(to_pm)
  list(pm = pm_minimal(pm_bind(t[!t$ProductKey %in% copied, , drop = FALSE],
                               f[f$ProductKey %in% copied, , drop = FALSE])),
       copied = copied, skipped = keys[!ok])
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
    gaps <- .pm_class_gaps(saw)
    if (length(gaps) > 0L)
      errors <- c(errors, paste("Diameter classes must be consecutive whole cm (no gaps):",
                                paste(gaps, collapse = ", ")))
    # Logs between the minimum top diameter and the smallest class are unpriced.
    lo_cls <- tapply(saw$PriceDiameterClass_cm, saw$ProductKey, min, na.rm = TRUE)
    k <- intersect(names(lo_cls), pd$ProductKey)
    mt <- floor(as.numeric(pd$MinTopDiameter[match(k, pd$ProductKey)]) / 10)
    for (j in which(!is.na(mt) & mt < lo_cls[k]))
      warnings <- c(warnings, sprintf(
        "%s: the smallest diameter class is %d cm but the minimum top diameter is %g cm; logs below %d cm are unpriced.",
        k[j], lo_cls[[k[j]]], as.numeric(pd$MinTopDiameter[match(k[j], pd$ProductKey)]) / 10,
        lo_cls[[k[j]]]))
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
  list(long = pm_drop_empty_cells(long), id = id, errors = character(0))
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
    for (sh in setdiff(sheets, SCENARIO_META_SHEETS)[!.is_target_sheet(setdiff(sheets, SCENARIO_META_SHEETS))]) {   # a scenario file's other sheets are not prices
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
    pm_bind(keep[!drop, , drop = FALSE],
            up$long[, PM_REQUIRED_COLS, drop = FALSE])
  }
  v <- validate_scenario_pm(candidate, pd, lc, default = default)
  list(ok = v$ok, pm = v$pm, layout = up$layout, ids = up$ids,
       errors = v$errors, warnings = v$warnings)
}


# ---- templates and export ---------------------------------------------------------

#' Long CSV of a full price matrix: the four required columns.
pm_export <- function(pm, path) {
  readr::write_csv(pm_prices(pm), path, na = "")
  invisible(path)
}

#' Long-layout template (CSV): the default prices, four columns.
pm_template_long <- function(default, path) pm_export(default, path)

#' Wide-layout template (xlsx): one sheet per sawlog matrix in the source
#' layout (top-left cell = ProductKey, header = lengths dm, rows = diameter
#' classes cm) plus a flat_prices sheet; filled with the default prices.
pm_template_wide <- function(default, path) {
  writexl::write_xlsx(.pm_wide_sheets(default), path)
  invisible(path)
}

# A matrix's hidden column (Distribution or FileDiameterClass_mm) as a grid in
# the layout of pm_to_grid(): NA where the cell has no value.
.pm_hidden_grid <- function(pm, id, col) {
  g <- pm_to_grid(pm, id)
  m <- matrix(NA_real_, nrow(g), ncol(g), dimnames = dimnames(g))
  s <- pm[pm$ProductKey == id & .is_matrix_row(pm) & !is.na(pm$PriceNOKperM3), , drop = FALSE]
  if (nrow(s) && col %in% names(s))
    m[cbind(match(s$PriceDiameterClass_cm, as.integer(rownames(g))), match(s$LengthClass_dm, as.integer(colnames(g))))] <- s[[col]]
  as.data.frame(m, check.names = FALSE)
}

# Sheets target_1, target_2, ...: the Distribution of every matrix with a value.
.pm_target_sheets <- function(pm) {
  d <- pm_minimal(pm)
  if (!PM_DISTRIBUTION_COL %in% names(d)) return(list())
  ids <- setdiff(pm_matrix_ids(d), PM_FLAT_ID)
  ids <- ids[vapply(ids, function(id) any(!is.na(d[[PM_DISTRIBUTION_COL]][d$ProductKey == id & .is_matrix_row(d)])), logical(1))]
  out <- lapply(ids, function(id) {
    g <- .pm_hidden_grid(d, id, PM_DISTRIBUTION_COL)
    o <- cbind(setNames(data.frame(as.integer(rownames(g))), id), g)
    names(o) <- c(id, colnames(g)); o
  })
  setNames(out, if (length(ids)) paste0("target_", seq_along(ids)) else character(0))
}

# Sheet file_classes: the file diameter class (mm) of every matrix row that has one.
.pm_file_class_sheet <- function(pm) {
  d <- pm_minimal(pm)
  if (!PM_FILE_CLASS_COL %in% names(d)) return(list())
  r <- d[.is_matrix_row(d) & !is.na(d[[PM_FILE_CLASS_COL]]), c("ProductKey", "PriceDiameterClass_cm", PM_FILE_CLASS_COL), drop = FALSE]
  r <- r[!duplicated(r[, 1:2]), , drop = FALSE]
  if (nrow(r) == 0L) return(list())
  rownames(r) <- NULL
  list(file_classes = r)
}

# The sheets of the wide layout: one per matrix, then "flat_prices".
.pm_wide_sheets <- function(pm) {
  d <- pm_minimal(pm)
  sheets <- lapply(setdiff(pm_matrix_ids(d), PM_FLAT_ID), function(id) {
    g <- pm_to_grid(d, id)
    out <- cbind(setNames(data.frame(as.integer(rownames(g))), id), g)
    names(out) <- c(id, colnames(g))
    out
  })
  names(sheets) <- setdiff(pm_matrix_ids(d), PM_FLAT_ID)
  f <- pm_to_grid(d, PM_FLAT_ID)
  if (nrow(f) > 0L)
    sheets[[PM_FLAT_ID]] <- data.frame(ProductKey = rownames(f),
                                       PriceNOKperM3 = f$PriceNOKperM3,
                                       stringsAsFactors = FALSE)
  sheets
}


# ---- custom assortments (Phase 4b) --------------------------------------------------

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
  # Custom assortments are commercial: "waste" is the base waste products' class.
  if (any(as.character(defs$Class) %in% "waste"))
    err <- c(err, sprintf("ProductType 'waste' is reserved for the base waste products: %s",
                          paste(defs$Name[as.character(defs$Class) %in% "waste"], collapse = ", ")))
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


#' Keep existing prices when an assortment is edited.
#'
#' `new` holds the edited assortment's cells at the starting price
#' (pm_new_assortment_prices()). Every cell that existed before the edit (same
#' species, length class and diameter class; for a flat price, same species)
#' takes its price from `pm`; new cells keep the starting price. Cells that no
#' longer exist are not in `new`, so they are dropped. Matching is by species,
#' so renaming an assortment keeps its prices.
#'
#' @param new     Long minimal rows of the edited assortment.
#' @param pm      The run's current price matrix (default or scenario).
#' @param old_def,new_def The definition before and after the edit.
#' @return `new` with carried-over prices; attribute "n_kept" = cells kept.
pm_keep_assortment_prices <- function(new, pm, old_def, new_def) {
  old_r <- custom_assortment_rows(old_def)$pd
  new_r <- custom_assortment_rows(new_def)$pd
  old_key <- setNames(old_r$ProductKey, old_r$species_group)[
    new_r$species_group[match(new$ProductKey, new_r$ProductKey)]]
  old <- pm_minimal(pm)
  hit <- match(paste(old_key, new$LengthClass_dm, new$PriceDiameterClass_cm),
               paste(old$ProductKey, old$LengthClass_dm, old$PriceDiameterClass_cm))
  new$PriceNOKperM3[!is.na(hit)] <- old$PriceNOKperM3[hit[!is.na(hit)]]
  attr(new, "n_kept") <- sum(!is.na(hit))
  new
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