# ==============================================================================
# scripts/phase4b_save_default_baseline.R
#
# Purpose : Snapshot of default-product backend results, taken on the code
#           before Phase 4b (custom assortments, per-product volume methods,
#           per-product price-diameter cap). Audit 20 (check 8) asserts that
#           the default products still give identical results.
#
# Strata  : the three of audit 16 (spruce, pine, both; HKL 41/42, bonitet
#           14/17), default 2021 prices.
#
# Output  : outputs/assortments/baseline_phase4b_default.rds, a list with
#           one element per stratum (selected_logs_all, tree_summary,
#           per_m3_summary, per_m3_by_assortment, product_summary,
#           unpriced_summary, sampled_plot_ids) plus $meta (git commit, date).
#
# Usage   : Rscript scripts/phase4b_save_default_baseline.R
#           Run once, on the pre-Phase-4b commit; do not regenerate it on
#           later code, or check 8 compares the code against itself.
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
suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)

PHASE4B_BASELINE_STRATA <- list(
  spruce = list(species_group = "spruce", hogstkl = c(41L, 42L),
                abonitet = c(14L, 17L), n_plots = 25L, seed = 42L),
  pine   = list(species_group = "pine",   hogstkl = c(41L, 42L),
                abonitet = c(14L, 17L), n_plots = 25L, seed = 42L),
  both   = list(species_group = "both",   hogstkl = c(41L, 42L),
                abonitet = c(14L, 17L), n_plots = 10L, seed = 7L))

keep <- c("selected_logs_all", "tree_summary", "per_m3_summary",
          "per_m3_by_assortment", "product_summary", "unpriced_summary",
          "sampled_plot_ids")
out <- lapply(PHASE4B_BASELINE_STRATA, function(s) {
  r <- run_sampled_plot_bucking(params = s, price_matrix_path = "default_2021",
                                root = root_dir)
  r[keep]
})
commit <- tryCatch(system2("git", c("-c", "safe.directory=*", "-C", shQuote(root_dir),
                                    "rev-parse", "HEAD"), stdout = TRUE),
                   error = function(e) NA_character_)
out$meta <- list(commit = commit, created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                 strata = PHASE4B_BASELINE_STRATA)

dir.create(file.path(root_dir, "outputs", "assortments"), showWarnings = FALSE,
           recursive = TRUE)
path <- file.path(root_dir, "outputs", "assortments", "baseline_phase4b_default.rds")
saveRDS(out, path)
for (n in names(PHASE4B_BASELINE_STRATA))
  cat(sprintf("%-6s trees %4d  logs %5d  value %12.2f NOK\n", n,
              nrow(out[[n]]$tree_summary), nrow(out[[n]]$selected_logs_all),
              sum(out[[n]]$selected_logs_all$value_nok)))
cat("Saved", path, "\ncommit", commit, "\n")
