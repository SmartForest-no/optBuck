# ==============================================================================
# R/helpers_distribution_edit.R
#
# Purpose : The target distribution editor (restructure R2, 2026-10-30): pure
#           functions on the price matrix and the assortment table. The target
#           is the hidden price-matrix column Distribution (percent per cell,
#           PM_DISTRIBUTION_COL) and the four hidden assortment fields
#           DISTRIBUTION_DEF_COLS. Runs never read them; price tuning does.
#
# Rules   : * A cell can only carry a target where the assortment has a price
#             there (an unavailable cell has no pm row).
#           * An edit of one cell sets the same value in every whole-cm row of
#             the same file diameter class (FileDiameterClass_mm), the way the
#             .hpr import spread it; a row without a file class is its own class.
#           * Price edits never change Distribution and target edits never
#             change prices (the functions here touch only the Distribution
#             column).
#           * Even / Longer logs use tuning_target()'s rules (R/helpers_price_tuning.R).
#
# Needs   : helpers_price_matrix.R (pm_minimal(), pm_to_grid()),
#           helpers_products.R (assortment_table_keys(), DISTRIBUTION_DEF_COLS),
#           helpers_price_tuning.R (tuning_target()).
# ==============================================================================

DIST_CATEGORIES <- c("Number of logs", "Volume of logs")
DIST_TOL <- 1e-6

# pm with a Distribution column (NA where it had none).
.dist_ensure <- function(pm) {
  pm <- pm_minimal(pm)
  if (!PM_DISTRIBUTION_COL %in% names(pm)) pm[[PM_DISTRIBUTION_COL]] <- NA_real_
  pm
}

# The column is only there when some cell has a value.
.dist_tidy <- function(pm) {
  if (PM_DISTRIBUTION_COL %in% names(pm) && all(is.na(pm[[PM_DISTRIBUTION_COL]])))
    pm[[PM_DISTRIBUTION_COL]] <- NULL
  pm
}

# The priced cells of one matrix-priced assortment: pm row indices.
.dist_rows <- function(pm, id)
  which(pm$ProductKey == id & !is.na(pm$LengthClass_dm) & !is.na(pm$PriceNOKperM3))

# TRUE where the assortment has a price (grid shape of pm_to_grid()).
dist_available <- function(pm, id) {
  g <- as.matrix(pm_to_grid(pm_minimal(pm), id))
  !is.na(g)
}

# The target as a grid in the layout of the price grid (rows = diameter cm,
# columns = length dm): the percentage of a cell, NA where the cell has no
# price or no target.
dist_grid <- function(pm, id) {
  pm <- .dist_ensure(pm)
  g <- pm_to_grid(pm, id)
  m <- matrix(NA_real_, nrow(g), ncol(g), dimnames = dimnames(g))
  i <- .dist_rows(pm, id)
  m[cbind(match(pm$PriceDiameterClass_cm[i], as.integer(rownames(g))),
          match(pm$LengthClass_dm[i], as.integer(colnames(g))))] <- pm[[PM_DISTRIBUTION_COL]][i]
  as.data.frame(m, check.names = FALSE)
}

# Does the assortment have a target (some cell above 0)?
dist_has_target <- function(pm, id) {
  pm <- pm_minimal(pm)
  PM_DISTRIBUTION_COL %in% names(pm) &&
    any(pm[[PM_DISTRIBUTION_COL]][pm$ProductKey == id & !is.na(pm$LengthClass_dm)] > 0, na.rm = TRUE)
}

# Sum of a row's targets (a row with none sums to 0).
dist_row_sums <- function(grid) unname(rowSums(as.matrix(grid), na.rm = TRUE))

# TRUE for rows whose sum is neither 0 nor 100 (shown in bold).
dist_row_flag <- function(sums) abs(sums) > DIST_TOL & abs(sums - 100) > 1e-4

# Set the target of one cell. Every priced row of the same length that has
# the same non-NA FileDiameterClass_mm as the edited row gets the value (only
# the edited row when it has none). NA clears the cells. Unavailable cells
# (no price) cannot be edited: an error.
dist_set_cell <- function(pm, id, diam_cm, length_dm, value) {
  if (!is.na(value) && (value < 0 || value > 100)) stop("A target is between 0 and 100 percent.")
  pm <- .dist_ensure(pm)
  i <- .dist_rows(pm, id)
  at <- i[pm$PriceDiameterClass_cm[i] == diam_cm & pm$LengthClass_dm[i] == length_dm]
  if (length(at) != 1L) stop("The cell has no price: no target can be set there.")
  fc <- if (PM_FILE_CLASS_COL %in% names(pm)) pm[[PM_FILE_CLASS_COL]][at] else NA_real_
  rows <- if (is.na(fc)) at else
    i[pm$LengthClass_dm[i] == length_dm & !is.na(pm[[PM_FILE_CLASS_COL]][i]) & pm[[PM_FILE_CLASS_COL]][i] == fc]
  pm[[PM_DISTRIBUTION_COL]][rows] <- as.numeric(value)
  .dist_tidy(pm)
}

# Even ("neutral") or Longer logs ("long") target for every diameter row of
# the assortment, from tuning_target()'s rules applied to the cells that have
# a price. Rows with no price get none. Replaces the assortment's target.
dist_apply_target <- function(pm, id, type = c("neutral", "long")) {
  type <- match.arg(type)
  pm <- .dist_ensure(pm)
  av <- dist_available(pm, id)                      # diameter x length
  grid <- list(price = ifelse(t(av), 1, NA_real_))  # tuning_target(): length x diameter
  dimnames(grid$price) <- list(colnames(av), rownames(av))
  w <- tuning_target(grid, type)                    # length x diameter
  i <- .dist_rows(pm, id)
  pm[[PM_DISTRIBUTION_COL]][pm$ProductKey == id & !is.na(pm$LengthClass_dm)] <- NA_real_
  pm[[PM_DISTRIBUTION_COL]][i] <- w[cbind(match(pm$LengthClass_dm[i], as.integer(rownames(w))),
                                          match(pm$PriceDiameterClass_cm[i], as.integer(colnames(w))))]
  .dist_tidy(pm)
}

# Remove the assortment's target (the column goes when no assortment has one).
dist_clear <- function(pm, id) {
  pm <- .dist_ensure(pm)
  pm[[PM_DISTRIBUTION_COL]][pm$ProductKey == id & !is.na(pm$LengthClass_dm)] <- NA_real_
  .dist_tidy(pm)
}

# The row of an assortment in the table.
.dist_def_row <- function(defs, id) {
  i <- match(id, assortment_table_keys(defs))
  if (is.na(i)) stop("No assortment '", id, "' in the table.")
  i
}

# The four distribution fields when an assortment gets a target for the first
# time (all four NA): NOT allowed (distribution bucking unticked; roadmap 8 R5),
# "Volume of logs", within product, MAXDeviation 0 (hidden in the app until
# the box is ticked). Fields from a file are kept as they are.
dist_init_fields <- function(defs, id) {
  i <- .dist_def_row(defs, id)
  if (all(is.na(unlist(defs[i, DISTRIBUTION_DEF_COLS, drop = FALSE])))) {
    defs$DistributionAllowed[i] <- FALSE
    defs$DistributionCategory[i] <- "Volume of logs"
    defs$DistributionWithinProduct[i] <- TRUE
    defs$MAXDeviation[i] <- 0
  }
  defs
}

# Remove the four fields (the assortment has no target any more).
dist_clear_fields <- function(defs, id) {
  i <- .dist_def_row(defs, id)
  defs$DistributionAllowed[i] <- NA; defs$DistributionCategory[i] <- NA_character_
  defs$DistributionWithinProduct[i] <- NA; defs$MAXDeviation[i] <- NA_real_
  defs
}

# Set DistributionAllowed (the harvester also steers towards the target:
# apportionment bucking). Only written to exports; Apter's own bucking is pure
# value bucking.
dist_set_allowed <- function(defs, id, allowed) {
  defs$DistributionAllowed[.dist_def_row(defs, id)] <- isTRUE(allowed)
  defs
}

# Set MAXDeviation (the largest value loss, in percent, that apportionment
# bucking may accept).
dist_set_maxdev <- function(defs, id, x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || is.na(x) || x < 0 || x > 100) stop("MAXDeviation is between 0 and 100 percent.")
  defs$MAXDeviation[.dist_def_row(defs, id)] <- x
  defs
}

# Set DistributionCategory ("Number of logs" or "Volume of logs").
dist_set_category <- function(defs, id, category) {
  if (!category %in% DIST_CATEGORIES) stop("DistributionCategory is 'Number of logs' or 'Volume of logs'.")
  defs$DistributionCategory[.dist_def_row(defs, id)] <- category
  defs
}
