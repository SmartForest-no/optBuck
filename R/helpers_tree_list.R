# ==============================================================================
# R/helpers_tree_list.R
#
# Purpose : Uploaded tree lists (Phase 5, 2026-10-13): read, validate, impute
#           heights (helpers_heights.R) and check taperNOR's range. The
#           validation layer's tree-list part (docs/plans/PLAN.md): errors
#           block a run, warnings do not; units are never converted.
#
# Format  : one row per tree, CSV or xlsx (first sheet); column names
#           case-insensitive.
#             tree_id  required  unique within plot_id (or overall)
#             species  required  spruce or pine
#             dbh      required  cm, over bark
#             height   optional  m; empty where not measured
#             plot_id  optional  groups trees for the height model
#
# Trees   : the internal key (treid) is "plot_id/tree_id" when plot IDs are
#           given, else tree_id. Results show the user's tree_id and plot_id;
#           the plot goes in the plot_id_github column used by the backend
#           and the comparison.
# ==============================================================================

#' Tree-list columns.
TREE_LIST_REQUIRED <- c("tree_id", "species", "dbh")
TREE_LIST_OPTIONAL <- c("height", "plot_id")

#' taperNOR's range (as for NFI trees; helpers_taper.R).
TREE_LIST_MIN_DBH_CM   <- 5.0
TREE_LIST_MIN_HEIGHT_M <- 4.5
#' Beyond these the taper and height models extrapolate: warning.
TREE_LIST_MAX_DBH_CM   <- 80
TREE_LIST_MAX_HEIGHT_M <- 40
#' Plausible height/diameter ratio (height m / dbh m).
TREE_LIST_HD_RANGE <- c(30, 150)


#' Read an uploaded tree list (all columns as text, names lower-cased).
#'
#' @return list(data = data.frame or NULL, errors).
read_tree_list <- function(path, filename = path) {
  ext <- tolower(tools::file_ext(filename))
  df <- tryCatch(switch(ext,
    csv  = utils::read.csv(path, colClasses = "character", check.names = FALSE,
                           na.strings = c("", "NA"), strip.white = TRUE,
                           fileEncoding = "UTF-8-BOM"),
    xlsx = as.data.frame(readxl::read_excel(path, sheet = 1, col_types = "text")),
    NULL), error = function(e) e)
  if (inherits(df, "error"))
    return(list(data = NULL, errors = paste("Cannot read the file:", conditionMessage(df))))
  if (is.null(df))
    return(list(data = NULL, errors = sprintf("Unsupported file type '.%s' (use .csv or .xlsx).", ext)))
  names(df) <- tolower(trimws(names(df)))
  list(data = df, errors = character(0))
}


#' Validate a tree list, impute heights and check taperNOR's range.
#'
#' @param df   data.frame from read_tree_list()$data (text columns).
#' @param min_measured,timeout_s Passed to impute_heights().
#' @return list(ok, errors, warnings, trees, heights = impute_heights()
#'   summary, excluded). `trees` (all valid rows): treid, tree_id, plot_id,
#'   species_group, dbh_cm, height_measured_m, height_m, height_source,
#'   in_range, excluded_reason.
prepare_tree_list <- function(df, min_measured = HEIGHT_MIN_MEASURED_PER_SPECIES,
                              timeout_s = HEIGHT_FIT_TIMEOUT_S) {
  err <- character(0); warn <- character(0)
  fail <- function() list(ok = FALSE, errors = err, warnings = warn, trees = NULL,
                          heights = NULL, excluded = NULL)
  if (is.null(df) || nrow(df) == 0L) { err <- "The tree list is empty."; return(fail()) }
  miss <- setdiff(TREE_LIST_REQUIRED, names(df))
  if (length(miss) > 0L) {
    err <- paste("Missing required column(s):", paste(miss, collapse = ", "))
    return(fail())
  }
  extra <- setdiff(names(df), c(TREE_LIST_REQUIRED, TREE_LIST_OPTIONAL))
  if (length(extra) > 0L) warn <- c(warn, paste("Ignored column(s):", paste(extra, collapse = ", ")))

  txt <- function(x) { x <- trimws(as.character(x)); x[x %in% c("", "NA")] <- NA; x }
  n <- nrow(df); row <- seq_len(n) + 1L          # file row numbers (header = 1)
  rows_of <- function(i) paste(head(row[i], 5), collapse = ", ")
  tree_id <- txt(df$tree_id)
  plot_id <- if ("plot_id" %in% names(df)) txt(df$plot_id) else rep(NA_character_, n)
  species <- tolower(txt(df$species))
  num <- function(col) {
    raw <- if (col %in% names(df)) txt(df[[col]]) else rep(NA_character_, n)
    v <- suppressWarnings(as.numeric(raw))
    bad <- !is.na(raw) & is.na(v)
    if (any(bad)) err <<- c(err, sprintf("Non-numeric %s in %d row(s) (rows %s), e.g. '%s'.",
                                         col, sum(bad), rows_of(bad), raw[bad][1L]))
    v
  }
  dbh <- num("dbh"); height <- num("height")

  if (any(is.na(tree_id)))
    err <- c(err, sprintf("Missing tree_id in %d row(s) (rows %s).", sum(is.na(tree_id)),
                          rows_of(is.na(tree_id))))
  bad_sp <- is.na(species) | !species %in% c("spruce", "pine")
  if (any(bad_sp))
    err <- c(err, sprintf("Unknown species in %d row(s): %s (use spruce or pine).", sum(bad_sp),
                          paste(unique(ifelse(is.na(species), "<empty>", species[bad_sp])),
                                collapse = ", ")))
  no_dbh <- is.na(txt(df$dbh))
  if (any(no_dbh))
    err <- c(err, sprintf("Missing dbh in %d row(s) (rows %s).", sum(no_dbh), rows_of(no_dbh)))
  if (any(dbh <= 0, na.rm = TRUE))
    err <- c(err, sprintf("dbh must be > 0: %d row(s) (rows %s).", sum(dbh <= 0, na.rm = TRUE),
                          rows_of(which(dbh <= 0))))
  if (any(height <= 0, na.rm = TRUE))
    err <- c(err, sprintf("height must be > 0: %d row(s) (rows %s).", sum(height <= 0, na.rm = TRUE),
                          rows_of(which(height <= 0))))
  has_plot <- any(!is.na(plot_id))
  key <- if (has_plot) paste(ifelse(is.na(plot_id), "", plot_id), tree_id, sep = "/") else tree_id
  dup <- !is.na(tree_id) & duplicated(key)
  if (any(dup))
    err <- c(err, sprintf("Duplicated tree_id%s: %s.", if (has_plot) " within plot_id" else "",
                          paste(unique(head(key[dup], 5)), collapse = ", ")))
  if (length(err) > 0L) return(fail())

  # Likely unit errors (reported, never converted).
  if (stats::median(dbh) > 100)
    warn <- c(warn, sprintf("The median dbh is %.0f; dbh must be in cm (this looks like mm).", stats::median(dbh)))
  if (any(!is.na(height)) && stats::median(height, na.rm = TRUE) > 60)
    warn <- c(warn, sprintf("The median height is %.0f; height must be in m (this looks like dm).",
                            stats::median(height, na.rm = TRUE)))
  hd <- height / (dbh / 100)
  odd <- !is.na(hd) & (hd < TREE_LIST_HD_RANGE[1] | hd > TREE_LIST_HD_RANGE[2])
  if (any(odd))
    warn <- c(warn, sprintf("%d tree(s) with an implausible height/diameter ratio (outside %d-%d), e.g. tree %s: %.0f.",
                            sum(odd), TREE_LIST_HD_RANGE[1], TREE_LIST_HD_RANGE[2],
                            key[odd][1L], hd[odd][1L]))

  trees <- data.frame(treid = key, tree_id = tree_id, plot_id = plot_id,
                      species_group = species, dbh_cm = dbh, height_measured_m = height,
                      stringsAsFactors = FALSE)
  imp <- impute_heights(data.frame(species_group = species, dbh_cm = dbh,
                                   height_m = height, plot_id = plot_id),
                        min_measured = min_measured, timeout_s = timeout_s)
  trees$height_m <- imp$height_m
  trees$height_source <- imp$height_source
  warn <- c(warn, imp$warnings)

  # taperNOR's range: trees outside are excluded, as for NFI trees.
  reason <- ifelse(trees$dbh_cm < TREE_LIST_MIN_DBH_CM,
                   sprintf("dbh below %.1f cm", TREE_LIST_MIN_DBH_CM),
            ifelse(trees$height_m < TREE_LIST_MIN_HEIGHT_M,
                   sprintf("height below %.1f m", TREE_LIST_MIN_HEIGHT_M), NA_character_))
  trees$in_range <- is.na(reason)
  trees$excluded_reason <- reason
  if (any(!trees$in_range)) {
    tab <- table(reason[!trees$in_range])
    warn <- c(warn, sprintf("%d tree(s) outside taperNOR's range are excluded: %s.",
                            sum(!trees$in_range),
                            paste(sprintf("%d with %s", as.integer(tab), names(tab)), collapse = ", ")))
  }
  if (!any(trees$in_range)) {
    err <- "No tree is within taperNOR's range (dbh >= 5 cm and height >= 4.5 m)."
    return(fail())
  }
  big <- trees$in_range & (trees$dbh_cm > TREE_LIST_MAX_DBH_CM | trees$height_m > TREE_LIST_MAX_HEIGHT_M)
  if (any(big))
    warn <- c(warn, sprintf("%d tree(s) beyond %d cm dbh or %d m height: the taper and height models extrapolate there.",
                            sum(big), TREE_LIST_MAX_DBH_CM, TREE_LIST_MAX_HEIGHT_M))

  list(ok = TRUE, errors = character(0), warnings = warn, trees = trees,
       heights = imp$summary, excluded = trees[!trees$in_range, , drop = FALSE])
}


#' Example tree list (CSV): spruce with 4 measured heights (calibrated
#' FinnHoyde), pine without heights (uncalibrated FinnHoyde), two plots.
tree_list_template <- function(path) {
  t <- data.frame(
    tree_id = c(1:7, 1:5),
    species = c(rep("spruce", 5), "pine", "pine", rep("spruce", 4), "pine"),
    dbh     = c(32.5, 28.1, 24.0, 19.6, 35.2, 30.4, 26.8, 22.3, 29.0, 18.4, 33.7, 27.5),
    height  = c(20.0, NA, 16.8, NA, NA, NA, NA, 16.0, NA, 14.0, NA, NA),
    plot_id = c(rep("A", 7), rep("B", 5)),
    stringsAsFactors = FALSE)
  utils::write.csv(t, path, row.names = FALSE, na = "")
  invisible(path)
}
