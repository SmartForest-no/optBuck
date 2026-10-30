# ==============================================================================
# R/helpers_utils.R
#
# Purpose : Small utilities shared by all of Apter: the app, the backend, its
#           parallel workers and the audits source this file (directly or
#           through the helper that needs it).
#
# or_default() is Apter's "value or default" rule. It has its own name so it
# never masks, and is never confused with, base R's `%||%` (R >= 4.4), which
# only replaces NULL. Apter code uses or_default(); base `%||%` is left alone.
# ==============================================================================

#' x, unless it is NULL or a single NA; then default.
#'
#' @param x       Value.
#' @param default Fallback (evaluated only when used).
#' @return x or default.
or_default <- function(x, default) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) default else x
}

#' A scenario's display name (restructure R1.8): trimmed, at most 20
#' characters, `fallback` ("A" or "B") when empty or missing. Used in result
#' tables, figure legends and comparison headers; internal ids stay A and B.
scenario_label <- function(x, fallback) {
  x <- if (length(x) == 1L && !is.na(x)) trimws(gsub("[[:cntrl:]]", " ", as.character(x))) else ""
  if (!nzchar(x)) return(fallback)
  substr(x, 1L, SCENARIO_NAME_MAX)
}
SCENARIO_NAME_MAX <- 20L

#' The same name as a file-name part: letters, digits, "_" and "-" only
#' (Norwegian letters transliterated, spaces become "_"), `fallback` if
#' nothing is left.
scenario_file_tag <- function(x, fallback) {
  x <- chartr("\u00e6\u00f8\u00e5\u00c6\u00d8\u00c5", "aoaAOA", as.character(x))
  x <- gsub("[^A-Za-z0-9_-]+", "_", trimws(x))
  x <- gsub("^_+|_+$", "", x)
  if (nzchar(x)) x else fallback
}
