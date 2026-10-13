# ==============================================================================
# scripts/performance_tree_lists.R
#
# Purpose : Seeded synthetic tree lists for the Performance phase (baseline,
#           profiling, benchmarks, audit 22). Spruce and pine mixed, DBH
#           10-40 cm, every height measured (on a Naslund curve with a plot
#           effect, as audit 21's generator), so no height model is fitted
#           and the lists are identical on every machine.
#
# Usage   : source("scripts/performance_tree_lists.R")
#           up <- perf_tree_list_upload(1000)        # upload format
#           p  <- perf_tree_list(1000)               # prepare_tree_list() result
#           Needs R/helpers_tree_list.R (sourced by the backend script).
# ==============================================================================

#' Synthetic tree list in the upload format (tree_id, species, dbh, height,
#' plot_id), 20 trees per plot, about 60 % spruce.
#'
#' @param n    Number of trees.
#' @param seed RNG seed (the lists are reproducible).
perf_tree_list_upload <- function(n, seed = 2026L) {
  old <- if (exists(".Random.seed", envir = globalenv()))
    get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (!is.null(old)) assign(".Random.seed", old, envir = globalenv()))
  set.seed(seed)
  per_plot <- 20L
  n_plots  <- ceiling(n / per_plot)
  plots    <- sprintf("P%04d", seq_len(n_plots))
  plot     <- rep(plots, each = per_plot)[seq_len(n)]
  u        <- stats::rnorm(n_plots, 0, 0.12)[match(plot, plots)]
  species  <- ifelse(stats::runif(n) < 0.6, "spruce", "pine")
  d        <- round(stats::runif(n, 10, 40), 1)
  h        <- round(1.3 + (d / (1.6 + u + 0.33 * d))^3 + stats::rnorm(n, 0, 0.6), 1)
  data.frame(tree_id = as.character(seq_len(n)), species = species,
             dbh = as.character(d), height = as.character(h), plot_id = plot,
             stringsAsFactors = FALSE)
}

#' The same list, validated and prepared (prepare_tree_list()).
perf_tree_list <- function(n, seed = 2026L) {
  p <- prepare_tree_list(perf_tree_list_upload(n, seed))
  if (!isTRUE(p$ok)) stop("Synthetic tree list failed validation: ",
                          paste(p$errors, collapse = "; "))
  p
}
