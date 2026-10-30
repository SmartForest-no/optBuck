# ==============================================================================
# R/helpers_result_tables.R
#
# Purpose : Result tables derived from a run's selected logs and trees
#           (2026-10-22): the log tally per price matrix (stocknota) and
#           value by DBH class. Pure functions on result tables; the app
#           renders them (Selected logs and Per m3 tabs) and offers downloads.
#
# Tally   : commercial logs (not waste) per price-diameter class x length
#           class of each matrix-priced assortment, laid out like its price
#           grid (pm_to_grid(): the scenario's own axes; cells without a price
#           are NA, priced cells without logs 0). Logs above the largest class
#           count in it (PriceDiameterClass_cm_used, as priced). Flat-priced
#           assortments: one row per length class. Volume = the logs'
#           commercial volume under bark (the NOK/m3 denominator), so the
#           grand total equals the run's commercial volume. Matrix vs flat
#           comes from the price matrix (rows with a length class), never
#           from key names; waste from ProductType.
#
# DBH     : trees, commercial volume, value and NOK/m3 per species and 5 cm
#           DBH class (..., 5-9.9, ..., 55-59.9, >= 60 cm), plus a total per
#           species.
# ==============================================================================

if (!exists("or_default", mode = "function"))   # helpers_utils.R
  source(file.path(dirname(if (is.null(sys.frame(1L)$ofile)) "." else sys.frame(1L)$ofile),
                   "helpers_utils.R"))

#' Tally measures: column in log_tally() -> label.
TALLY_MEASURES <- c(logs = "Logs", volume = "Volume (m³ u.b.)")

#' DBH classes (cm): width, the first lower limit and the open top class.
#' Classes start at 0: NFI trees and tree lists start at 5 cm, but .hpr stems
#' have no minimum DBH.
DBH_CLASS_WIDTH_CM <- 5
DBH_CLASS_FROM_CM  <- 0
DBH_CLASS_OPEN_CM  <- 60


# ---- log tally -------------------------------------------------------------------------

# ProductKeys priced on a matrix in `pm` (rows with a length class).
.tally_matrix_keys <- function(pm)
  if (is.null(pm)) character(0) else unique(as.character(pm$ProductKey[!is.na(pm$LengthClass_dm)]))

#' The log tally of one run, long: one row per assortment, diameter class
#' (NA for flat-priced) and length class, with logs and commercial volume.
#'
#' @param selected_logs A run's selected_logs_all.
#' @param pm            The run's price matrix (long; pm_minimal() layout).
#' @return data.frame(ProductKey, ProductType, species_group, priced
#'   ("matrix"/"flat"), diameter_class_cm, length_class_dm, logs, volume_m3),
#'   ordered by species (spruce, pine), type (sawlog, pulpwood, other), key,
#'   diameter and length.
log_tally <- function(selected_logs, pm) {
  sl <- selected_logs[as.character(selected_logs$ProductType) != "waste", , drop = FALSE]
  empty <- data.frame(ProductKey = character(0), ProductType = character(0),
                      species_group = character(0), priced = character(0),
                      diameter_class_cm = integer(0), length_class_dm = integer(0),
                      logs = integer(0), volume_m3 = numeric(0), stringsAsFactors = FALSE)
  if (nrow(sl) == 0L) return(empty)
  key <- as.character(sl$ProductKey)
  mat <- key %in% .tally_matrix_keys(pm)
  d <- ifelse(mat, as.integer(sl$PriceDiameterClass_cm_used), NA_integer_)
  l <- as.integer(sl$LengthClass_dm)
  g <- paste(key, d, l, sep = "\r")
  s <- rowsum(cbind(logs = 1, volume_m3 = as.numeric(sl$commercial_volume_m3)), g, reorder = FALSE)
  i <- match(rownames(s), g)
  out <- data.frame(ProductKey = key[i], ProductType = as.character(sl$ProductType[i]),
                    species_group = as.character(sl$species_group[i]),
                    priced = ifelse(mat[i], "matrix", "flat"),
                    diameter_class_cm = d[i], length_class_dm = l[i],
                    logs = as.integer(s[, "logs"]), volume_m3 = unname(s[, "volume_m3"]),
                    stringsAsFactors = FALSE)
  rank_sp <- match(out$species_group, c("spruce", "pine")); rank_sp[is.na(rank_sp)] <- 3L
  rank_ty <- match(out$ProductType, c("sawlog", "pulpwood")); rank_ty[is.na(rank_ty)] <- 3L
  out <- out[order(rank_sp, rank_ty, out$ProductKey, out$diameter_class_cm, out$length_class_dm), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Logs per length class and type group (Sawlog, Pulpwood, Other; waste left
#' out, as in log_tally()), for the Logs tab's length figure. The counts equal
#' the log tally's Total row summed over the non-waste assortments.
#'
#' @param selected_logs A run's selected_logs_all.
#' @return data.frame(length_class_dm, type (factor, ASSORTMENT_TYPE_GROUPS),
#'   logs), ordered by length class and type.
log_length_distribution <- function(selected_logs) {
  g <- type_group(selected_logs$ProductType)
  k <- !is.na(g) & !is.na(selected_logs$LengthClass_dm)
  empty <- data.frame(length_class_dm = integer(0),
                      type = factor(character(0), levels = ASSORTMENT_TYPE_GROUPS),
                      logs = integer(0))
  if (!any(k)) return(empty)
  d <- as.data.frame(table(length_class_dm = as.integer(selected_logs$LengthClass_dm[k]),
                           type = factor(g[k], levels = ASSORTMENT_TYPE_GROUPS)),
                     stringsAsFactors = FALSE)
  d$length_class_dm <- as.integer(d$length_class_dm)
  d$type <- factor(d$type, levels = ASSORTMENT_TYPE_GROUPS)
  d <- d[d$Freq > 0L, , drop = FALSE]
  names(d)[names(d) == "Freq"] <- "logs"
  d <- d[order(d$length_class_dm, d$type), , drop = FALSE]
  rownames(d) <- NULL
  d
}

#' Assortments in one or two tallies, in tally order (A's first).
log_tally_keys <- function(tally_a, tally_b = NULL)
  unique(c(as.character(tally_a$ProductKey), as.character(tally_b$ProductKey)))

# The cells of one assortment in one scenario: data.frame(d, l, value,
# priced). Matrix-priced: every cell of the price grid (priced = has a
# price) plus the tally; `collapse` sums over diameters (d = NA).
.tally_cells <- function(tally, pm, key, measure, collapse = FALSE) {
  t <- if (is.null(tally)) tally else tally[tally$ProductKey == key, , drop = FALSE]
  cells <- data.frame(d = as.integer(t$diameter_class_cm), l = as.integer(t$length_class_dm),
                      value = as.numeric(if (measure == "logs") t$logs else t$volume_m3),
                      priced = rep(TRUE, NROW(t)))
  if (!collapse && key %in% .tally_matrix_keys(pm)) {
    g <- pm_to_grid(pm, key)
    grid <- expand.grid(d = as.integer(rownames(g)), l = as.integer(names(g)))
    grid$value <- 0
    grid$priced <- !is.na(unlist(g, use.names = FALSE))
    cells <- rbind(grid, cells)
  }
  if (collapse) cells$d <- NA_integer_
  .tally_merge(cells)
}

# Sum values and OR the priced flags of cells with the same (d, l).
.tally_merge <- function(cells) {
  if (nrow(cells) == 0L) return(cells)
  k <- paste(cells$d, cells$l)
  first <- !duplicated(k)
  data.frame(d = cells$d[first], l = cells$l[first],
             value = unname(tapply(cells$value, factor(k, levels = k[first]), sum)),
             priced = unname(tapply(cells$priced, factor(k, levels = k[first]), any)))
}

# A grid from cells: first column the diameter class (or "All diameters"),
# one column per length class (dm), Total; a Total row. NA = no price (and
# no logs), 0 = priced, no logs.
.tally_grid <- function(cells, flat, rows = NULL, cols = NULL) {
  cols <- or_default(cols, sort(unique(cells$l)))
  rows <- if (flat) NA_integer_ else or_default(rows, sort(unique(cells$d)))
  m <- matrix(NA_real_, length(rows), length(cols))
  if (nrow(cells) > 0L) {
    i <- if (flat) rep(1L, nrow(cells)) else match(cells$d, rows)
    j <- match(cells$l, cols)
    ok <- !is.na(i) & !is.na(j) & (cells$priced | cells$value != 0)
    m[cbind(i, j)[ok, , drop = FALSE]] <- cells$value[ok]
  }
  out <- data.frame(label = if (flat) "All diameters" else as.character(rows), m,
                    Total = rowSums(m, na.rm = TRUE), check.names = FALSE, stringsAsFactors = FALSE)
  names(out) <- c("Diameter (cm)", as.character(cols), "Total")
  tot <- data.frame(label = "Total", t(colSums(m, na.rm = TRUE)), Total = sum(m, na.rm = TRUE),
                    check.names = FALSE, stringsAsFactors = FALSE)
  names(tot) <- names(out)
  rbind(out, tot)
}

#' One assortment's tally grid in one scenario, laid out like its price grid.
#'
#' @param tally   log_tally() of the run.
#' @param pm      The run's price matrix.
#' @param key     ProductKey.
#' @param measure "logs" or "volume".
#' @return data.frame: "Diameter (cm)" (or "All diameters" for a flat-priced
#'   assortment), one column per length class (dm), "Total"; a "Total" row.
#'   NA: the cell has no price; 0: priced, no logs.
log_tally_grid <- function(tally, pm, key, measure = c("logs", "volume")) {
  measure <- match.arg(measure)
  flat <- !key %in% .tally_matrix_keys(pm)
  .tally_grid(.tally_cells(tally, pm, key, measure), flat)
}

# Whether B - A of `key` can use the diameter rows: it is matrix-priced in
# every scenario that has it.
.tally_diff_flat <- function(key, pm_a, pm_b, tally_a, tally_b) {
  has <- function(t, pm) key %in% t$ProductKey || key %in% pm$ProductKey
  m_a <- key %in% .tally_matrix_keys(pm_a); m_b <- key %in% .tally_matrix_keys(pm_b)
  !((m_a || !has(tally_a, pm_a)) && (m_b || !has(tally_b, pm_b)) && (m_a || m_b))
}

#' B - A of one assortment's tally: the union of both grids' axes, signed
#' values. An assortment only one scenario has is shown against zero; one
#' priced on a matrix in one scenario and flat in the other is compared per
#' length class.
log_tally_diff_grid <- function(tally_a, pm_a, tally_b, pm_b, key, measure = c("logs", "volume")) {
  measure <- match.arg(measure)
  flat <- .tally_diff_flat(key, pm_a, pm_b, tally_a, tally_b)
  a <- .tally_cells(tally_a, pm_a, key, measure, collapse = flat)
  b <- .tally_cells(tally_b, pm_b, key, measure, collapse = flat)
  a$value <- -a$value
  .tally_grid(.tally_merge(rbind(a, b)), flat)
}

#' The tallies as one long table (CSV download).
#'
#' @param tallies Named list of log_tally() results (e.g. A, B).
log_tally_long <- function(tallies) {
  out <- do.call(rbind, lapply(names(tallies), function(nm)
    if (nrow(tallies[[nm]])) cbind(Scenario = nm, tallies[[nm]], stringsAsFactors = FALSE)))
  if (is.null(out)) return(data.frame())
  names(out) <- c("Scenario", "Assortment", "Type", "Species", "Pricing", "Diameter class (cm)",
                  "Length class (dm)", "Logs", "Volume (m3 u.b.)")
  out
}

#' Write the tallies to an xlsx file: one sheet per assortment, stacking the
#' grids (logs and volume per scenario, then B - A) on common axes, and a
#' sheet "long" with every cell.
#'
#' @param tallies Named list of log_tally() results: A, and B for a comparison.
#' @param pms     Named list of the matching price matrices.
#' @param label   function(key) -> sheet title (e.g. "Spruce sawlog").
#' @param scenario_keys Optional list(A = , B = ) of each scenario's
#'   assortment keys: an assortment only one scenario has gets only that
#'   scenario's grids, without B - A (2026-10-22). NULL: every assortment
#'   gets every scenario's grids.
#' @param lang    "en" (as before) or "no": headers, table titles, values and
#'   the "long" sheet's name through the translation table (helpers_i18n.R);
#'   `label` gives the sheet titles in that language.
log_tally_xlsx <- function(path, tallies, pms, label = identity, scenario_keys = NULL, lang = "en") {
  keys <- do.call(log_tally_keys, unname(tallies))
  sheets <- lapply(keys, function(key) {
    has <- if (is.null(scenario_keys)) rep(TRUE, length(tallies))
           else vapply(names(tallies), function(n) key %in% scenario_keys[[n]], logical(1))
    if (!any(has)) has[] <- TRUE
    use <- which(has); two <- length(use) == 2L
    flat <- if (two) .tally_diff_flat(key, pms[[1]], pms[[2]], tallies[[1]], tallies[[2]])
            else !key %in% .tally_matrix_keys(pms[[use]])
    views <- list()
    for (ms in names(TALLY_MEASURES)) {
      cells <- lapply(use, function(s)
        .tally_cells(tallies[[s]], pms[[s]], key, ms, collapse = flat))
      if (two) { a <- cells[[1]]; a$value <- -a$value; cells[[3]] <- .tally_merge(rbind(a, cells[[2]])) }
      nms <- c(names(tallies)[use], if (two) "B - A")
      views[paste(nms, ms)] <- cells
    }
    all_cells <- do.call(rbind, views)
    rows <- sort(unique(all_cells$d)); cols <- sort(unique(all_cells$l))
    order_v <- c(paste(names(tallies)[use], "logs"), paste(names(tallies)[use], "volume"),
                 if (two) c("B - A logs", "B - A volume"))
    do.call(rbind, lapply(order_v, function(v) {
      g <- .tally_grid(views[[v]], flat, rows, cols)
      what <- sub(" (logs|volume)$", "", v); ms <- sub("^.* ", "", v)
      cbind(Table = sprintf("%s: %s", what, TALLY_MEASURES[[ms]]), g, stringsAsFactors = FALSE)
    }))
  })
  nm <- substr(gsub("[\\[\\]:*?/\\\\]", " ", vapply(keys, label, ""), perl = TRUE), 1L, 31L)
  names(sheets) <- make.unique(nm, sep = " ")
  sheets$long <- log_tally_long(tallies)
  if (!identical(lang, "en")) {
    sheets <- lapply(sheets, i18n_df, lang = lang)
    names(sheets)[length(sheets)] <- tr("sheet_long", lang = lang)
  }
  writexl::write_xlsx(sheets, path)
  invisible(path)
}


# ---- per assortment (Products tab) ----------------------------------------------------

#' Logs, price volume, value, NOK/m3 contribution and share of commercial
#' volume per assortment, for one run or A and B (2026-10-23, Products tab).
#' Sums of the selected logs (product_summary's columns, unrounded). The
#' contribution is the assortment's value / the run's total commercial
#' volume, so the contributions add up to the run's NOK/m3; the share is
#' its commercial volume / the total.
#'
#' @param r_a,r_b Run results (r_b NULL: one run).
#' @param keys    Optional list(A = , B = ) of each scenario's assortment
#'   keys: an assortment a scenario does not have is NA there (shown "–");
#'   one it has without logs is 0. NULL: missing means 0.
#' @return data.frame: key, then logs, price_vol, value, contrib, share with
#'   suffix _a (and _b); rows in order of first appearance (A, then B).
assortment_summary <- function(r_a, r_b = NULL, keys = NULL) {
  one <- function(r) {
    l <- r$selected_logs_all; k <- unique(as.character(l$ProductKey))
    tot <- sum(l$commercial_volume_m3, na.rm = TRUE)
    s <- function(x) as.numeric(tapply(x, factor(l$ProductKey, levels = k), sum, na.rm = TRUE, default = 0))
    v <- s(l$value_nok); cv <- s(l$commercial_volume_m3)
    data.frame(key = k, logs = as.integer(table(factor(l$ProductKey, levels = k))),
               price_vol = s(l$price_volume_m3), value = v,
               contrib = if (tot > 0) v / tot else NA_real_,
               share = if (tot > 0) cv / tot else NA_real_, stringsAsFactors = FALSE)
  }
  runs <- c(list(a = one(r_a)), if (!is.null(r_b)) list(b = one(r_b)))
  all_keys <- unique(unlist(lapply(runs, `[[`, "key")))
  out <- data.frame(key = all_keys, stringsAsFactors = FALSE)
  for (s in names(runs)) {
    x <- runs[[s]]; i <- match(all_keys, x$key)
    has <- if (is.null(keys)) rep(TRUE, length(all_keys)) else all_keys %in% keys[[toupper(s)]]
    for (col in c("logs", "price_vol", "value", "contrib", "share")) {
      v <- x[[col]][i]; v[is.na(i)] <- 0; v[!has] <- NA
      out[[paste(col, s, sep = "_")]] <- v
    }
  }
  out
}


# ---- value by DBH class ----------------------------------------------------------------

#' DBH class lower limit (cm) and label for dbh values (cm).
dbh_class <- function(dbh_cm, width = DBH_CLASS_WIDTH_CM, from = DBH_CLASS_FROM_CM,
                      open_from = DBH_CLASS_OPEN_CM) {
  lo <- pmin(pmax(floor((dbh_cm - from) / width) * width + from, from), open_from)
  list(lo = lo, label = ifelse(lo >= open_from, sprintf("≥ %g", open_from),
                               sprintf("%g–%.1f", lo, lo + width - 0.1)))
}

#' Trees, commercial volume, value and NOK/m3 per species and DBH class,
#' plus a total per species.
#'
#' @param tree_summary A run's tree_summary.
#' @return data.frame(species_group, dbh_class (label; "All" for the totals),
#'   class_lo (NA for the totals), trees, commercial_volume_m3, value_nok,
#'   nok_per_m3), species spruce, pine; classes ascending, then "All".
dbh_class_summary <- function(tree_summary, width = DBH_CLASS_WIDTH_CM, from = DBH_CLASS_FROM_CM,
                              open_from = DBH_CLASS_OPEN_CM) {
  ts <- tree_summary
  cl <- dbh_class(ts$dbh_cm, width, from, open_from)
  sp <- as.character(ts$species_group)
  one <- function(keep, label, lo) {
    v <- sum(ts$total_value_nok[keep], na.rm = TRUE); m3 <- sum(ts$commercial_volume_m3[keep], na.rm = TRUE)
    data.frame(dbh_class = label, class_lo = lo, trees = sum(keep), commercial_volume_m3 = m3,
               value_nok = v, nok_per_m3 = if (m3 > 0) v / m3 else NA_real_, stringsAsFactors = FALSE)
  }
  species <- intersect(c("spruce", "pine"), unique(sp))
  out <- do.call(rbind, lapply(species, function(s) {
    los <- sort(unique(cl$lo[sp == s]))
    rows <- lapply(los, function(lo) one(sp == s & cl$lo == lo, cl$label[match(lo, cl$lo)], lo))
    cbind(species_group = s, do.call(rbind, c(rows, list(one(sp == s, "All", NA_real_)))),
          stringsAsFactors = FALSE)
  }))
  if (is.null(out)) out <- data.frame(species_group = character(0), dbh_class = character(0),
                                      class_lo = numeric(0), trees = integer(0),
                                      commercial_volume_m3 = numeric(0), value_nok = numeric(0),
                                      nok_per_m3 = numeric(0))
  rownames(out) <- NULL
  out
}

#' Stems per species for the Stems tab (restructure R1.2): the stems that will
#' be bucked (count, mean DBH in cm, mean height in m) and, where the source
#' reports them, the stems left out, per species (spruce first, then pine).
#' Plain numbers only; a species with no stems and no exclusions is not listed.
#'
#' @param species,dbh_cm,height_m Per used stem.
#' @param excluded_species Species of the excluded stems (any other value, such
#'   as "none", is not counted).
#' @return data.frame(species, n_stems, mean_dbh_cm, mean_height_m, n_excluded).
stem_species_summary <- function(species, dbh_cm, height_m, excluded_species = character(0)) {
  sp <- intersect(c("spruce", "pine"), unique(c(species, excluded_species)))
  do.call(rbind, lapply(sp, function(s) {
    i <- species == s
    data.frame(species = s, n_stems = sum(i),
               mean_dbh_cm = if (any(i)) mean(dbh_cm[i]) else NA_real_,
               mean_height_m = if (any(i)) mean(height_m[i], na.rm = TRUE) else NA_real_,
               n_excluded = sum(excluded_species == s), stringsAsFactors = FALSE)
  })) -> out
  if (is.null(out)) out <- data.frame(species = character(0), n_stems = integer(0), mean_dbh_cm = numeric(0),
                                      mean_height_m = numeric(0), n_excluded = integer(0))
  out
}
