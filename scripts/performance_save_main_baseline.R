# ==============================================================================
# scripts/performance_save_main_baseline.R
#
# Purpose : Snapshot of backend results on main before the Performance phase
#           (commit 3fa0a9d). Audit 22 asserts that the optimised backend
#           (shared geometry, cache, workers, chunking) gives the same results.
#
# Cases   : nfi_spruce, nfi_pine, nfi_both   the three strata of audits 16/20,
#                                             default 2021 prices
#           tl100, tl1000                    synthetic tree lists
#                                             (scripts/performance_tree_lists.R),
#                                             default prices
#           tl1000_scn                       tl1000 under a changed matrix
#                                             (spruce sawlog +10 %, pine pulpwood
#                                             +50 NOK/m3)
#           tl100_custom_default,            tl100 with three custom assortments,
#           tl100_custom_scenario            UseIn default / scenario / both,
#                                             products from products_for_run()
#                                             as in the app
#
# Output  : outputs/performance/baseline_main_3fa0a9d.rds, a list with one
#           element per case (selected_logs_all, tree_summary, per_m3_summary,
#           per_m3_by_assortment, product_summary, unpriced_summary,
#           zero_log_trees) plus $meta (git commit, date) and $inputs (the
#           price matrices and assortment definitions used).
#
# Usage   : Rscript scripts/performance_save_main_baseline.R
#           Run once, on 3fa0a9d; do not regenerate it on later code.
# ==============================================================================

root_dir <- local({
  # APTER_ROOT -> walk up from getwd() -> stop()
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  env <- Sys.getenv("APTER_ROOT", unset = "")
  if (nzchar(env)) {
    d <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (!is_root(d))
      stop("Project root '", d, "' (from APTER_ROOT) does not contain ",
           "R/helpers_paths.R.", call. = FALSE)
  } else {
    d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    while (!is_root(d) && dirname(d) != d) d <- dirname(d)
    if (!is_root(d))
      stop("Cannot locate the project root: set APTER_ROOT, or run from ",
           "inside the project directory.", call. = FALSE)
  }
  d
})
Sys.setenv(APTER_ROOT = root_dir)
source(file.path(root_dir, "scripts", "app_backend", "01_run_sampled_plot_bucking.R"))
source(file.path(root_dir, "scripts", "performance_tree_lists.R"))
source(file.path(root_dir, "scripts", "performance_cases.R"))

keep <- c("selected_logs_all", "tree_summary", "per_m3_summary",
          "per_m3_by_assortment", "product_summary", "unpriced_summary",
          "zero_log_trees")

cases <- perf_cases(root_dir)
out <- list()
for (nm in names(cases)) {
  t0 <- proc.time()
  r  <- perf_run_case(cases[[nm]], root_dir)
  out[[nm]] <- r[keep]
  cat(sprintf("%-24s %6d trees  %7.1f s\n", nm, nrow(r$tree_summary),
              (proc.time() - t0)[["elapsed"]]))
}

commit <- tryCatch(system2("git", c("-c", "safe.directory=*", "-C", shQuote(root_dir),
                                    "rev-parse", "HEAD"), stdout = TRUE),
                   error = function(e) NA_character_)
out$meta   <- list(commit = commit, date = Sys.time(), R = R.version.string)
out$inputs <- perf_case_inputs(root_dir)

dir.create(file.path(root_dir, "outputs", "performance"), showWarnings = FALSE,
           recursive = TRUE)
path <- file.path(root_dir, "outputs", "performance", "baseline_main_3fa0a9d.rds")
if (file.exists(path)) stop("Baseline exists; do not regenerate it: ", path)
saveRDS(out, path)
cat("Saved", path, "\n")
