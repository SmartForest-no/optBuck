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
