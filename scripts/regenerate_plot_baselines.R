# ==============================================================================
# scripts/regenerate_plot_baselines.R  (roadmap 8, answer 1, 2026-11-04)
#
# Purpose : After the NFI plot IDs were permuted (scripts/permute_plot_ids.R) the seeded plot
#           samples (n_plots, seed) of the three NFI strata pick other plots, so the three NFI cases
#           of the two baselines can no longer be translated, only regenerated. This does it
#           deliberately, once:
#             outputs/performance/baseline_main_3fa0a9d_newids.rds   nfi_spruce, nfi_pine, nfi_both
#             outputs/assortments/baseline_phase4b_default_newids.rds spruce, pine, both
#           The tree-list cases and everything else in the files are kept. The original baselines
#           (baseline_main_3fa0a9d.rds, baseline_phase4b_default.rds) are not touched. That the
#           relabelling changes nothing else was shown with all plots before and after
#           (scripts/verify_plot_permutation.R). The meta list records what was regenerated.
# Usage   : Rscript scripts/regenerate_plot_baselines.R   (after permute_plot_ids.R)
# ==============================================================================
root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  d <- normalizePath(Sys.getenv("APTER_ROOT", unset = getwd()), winslash = "/", mustWork = FALSE)
  while (!is_root(d) && dirname(d) != d) d <- dirname(d)
  d
})
Sys.setenv(APTER_ROOT = root_dir)
suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)
source(file.path(root_dir, "scripts", "performance_tree_lists.R"))
source(file.path(root_dir, "scripts", "performance_cases.R"))
commit <- tryCatch(system2("git", c("-c", "safe.directory=*", "-C", shQuote(root_dir), "rev-parse", "HEAD"), stdout = TRUE),
                   error = function(e) NA_character_)
note <- list(what = "NFI cases regenerated after the plot IDs were permuted (roadmap 8)", commit = commit, date = Sys.time())

# ---- performance baseline ---------------------------------------------------------------------
f_perf <- file.path(root_dir, "outputs", "performance", "baseline_main_3fa0a9d_newids.rds")
b <- readRDS(f_perf)
keep <- setdiff(names(b$nfi_spruce), character(0))
cases <- perf_cases(root_dir, limits = "pre_phase4c")
for (nm in grep("^nfi_", names(cases), value = TRUE)) {
  r <- perf_run_case(cases[[nm]], root_dir)
  b[[nm]] <- r[keep]
  cat(sprintf("%-12s %6d trees, %d plots\n", nm, nrow(r$tree_summary), length(unique(r$tree_summary$plot_id_github))))
}
b$meta$regenerated <- note
saveRDS(b, f_perf)

# ---- Phase 4b default baseline ----------------------------------------------------------------
f_def <- file.path(root_dir, "outputs", "assortments", "baseline_phase4b_default_newids.rds")
g <- readRDS(f_def)
for (n in setdiff(names(g), "meta")) {
  r <- run_sampled_plot_bucking(g$meta$strata[[n]], "default_2021", root_dir,
                                products = pre_phase4c_limits(load_products(root_dir)))
  r$tree_summary$height_source <- NULL            # the baseline predates it (audit 20 check 08)
  g[[n]] <- r[names(g[[n]])]
  cat(sprintf("default %-7s %6d trees\n", n, nrow(r$tree_summary)))
}
g$meta$regenerated <- note
saveRDS(g, f_def)
cat("regenerated; commit", substr(commit, 1, 7), "\n")
