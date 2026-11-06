# ==============================================================================
# scripts/permute_plot_ids.R  (roadmap 8, answer 1, 2026-11-04)
#
# Purpose : The synthetic NFI plot IDs (P000001 ... P006348) were numbered in the alphabetical
#           order of the original plot codes, so their order leaks information about the original
#           codes. This replaces them by a random permutation (set.seed(20261105)), in
#           data_processed_github/ and in the taper cache (outputs/app_cache/, rebuilt by the
#           caller with scripts/app_backend/00_precompute_taper_profiles_github.R, or relabelled
#           here when the cache exists). The seeded plot sample (n_plots, seed) draws from the
#           sorted plot IDs, so it now picks other plots: the three sampled NFI baseline cases are
#           regenerated deliberately (scripts/regenerate_plot_baselines.R, PLAN.md).
#           The old -> new plot map is written ONLY to C:/optbuck_private/nfi_plot_id_map.rds.
# Usage   : Rscript scripts/permute_plot_ids.R   (run once; refuses to run twice)
# ==============================================================================
root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  d <- normalizePath(Sys.getenv("OPTBUCK_ROOT", unset = getwd()), winslash = "/", mustWork = FALSE)
  while (!is_root(d) && dirname(d) != d) d <- dirname(d)
  if (!is_root(d)) stop("Cannot locate the project root.", call. = FALSE)
  d
})
suppressPackageStartupMessages(library(arrow))
gh <- file.path(root_dir, "inst", "extdata", "nfi")
f_tree <- file.path(gh, "nfi_tree_plot_stratum_ready_github.parquet")
f_plot <- file.path(gh, "nfi_plots_github.parquet")
priv <- "C:/optbuck_private"; map_file <- file.path(priv, "nfi_plot_id_map.rds")
if (file.exists(map_file)) stop("Already permuted (", map_file, " exists).", call. = FALSE)
bak <- file.path(priv, "nfi_original_before_plot_permutation"); dir.create(bak, showWarnings = FALSE, recursive = TRUE)
stopifnot(file.copy(c(f_tree, f_plot), bak, overwrite = FALSE))

trees <- as.data.frame(read_parquet(f_tree)); plots <- as.data.frame(read_parquet(f_plot))
ids <- sort(unique(c(trees$plot_id_github, plots$plot_id_github)))
stopifnot(all(trees$plot_id_github %in% ids), all(grepl("^P[0-9]{6}$", ids)))
set.seed(20261105)
new <- sprintf("P%06d", sample.int(length(ids)))
map <- data.frame(plot_old = ids, plot_new = new, stringsAsFactors = FALSE)
trees$plot_id_github <- map$plot_new[match(trees$plot_id_github, map$plot_old)]
plots$plot_id_github <- map$plot_new[match(plots$plot_id_github, map$plot_old)]
trees <- trees[order(trees$plot_id_github, trees$treid), , drop = FALSE]; rownames(trees) <- NULL
plots <- plots[order(plots$plot_id_github), , drop = FALSE]; rownames(plots) <- NULL
saveRDS(map, map_file)
write_parquet(trees, f_tree); write_parquet(plots, f_plot)
cat(sprintf("%d plots relabelled; trees %d, plots %d. Map: %s (private)\n", length(ids), nrow(trees), nrow(plots), map_file))
cat("Rebuild the taper cache next: Rscript scripts/app_backend/00_precompute_taper_profiles_github.R\n")
