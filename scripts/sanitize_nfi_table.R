# ==============================================================================
# scripts/sanitize_nfi_table.R  (roadmap 8, step 2, 2026-11-04)
#
# Purpose : Prepare the NFI tables in data_processed_github/ for publication.
#           - drops the columns the app never reads (volm, volu, plot_area_m2);
#           - replaces the original NFI tree number `treid` by a random
#             permutation of 1..N (set.seed(20261104)), so a tree can no longer be
#             looked up in the NFI database;
#           - keeps `plot_id_github` as it is. It is a synthetic sequential ID and
#             the sampled-plot path of the backend (n_plots, seed) draws from the
#             SORTED plot IDs: relabelling the plots would change which plots a
#             seeded sample picks and with it the performance baseline. (Decision
#             2026-11-04, PLAN.md Roadmap 8; the rank order of the original plot IDs
#             stays recoverable in principle, which is accepted.)
#           The old -> new tree number map is written ONLY to
#           C:/apter_private/nfi_id_map.rds (never committed); the original
#           tables are copied to C:/apter_private/nfi_original/ first.
#
# Usage   : Rscript scripts/sanitize_nfi_table.R   (run once; refuses to run twice)
# ==============================================================================
root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  d <- normalizePath(Sys.getenv("APTER_ROOT", unset = getwd()), winslash = "/", mustWork = FALSE)
  while (!is_root(d) && dirname(d) != d) d <- dirname(d)
  if (!is_root(d)) stop("Cannot locate the project root.", call. = FALSE)
  d
})
suppressPackageStartupMessages(library(arrow))
gh <- file.path(root_dir, "data_processed_github")
f_tree <- file.path(gh, "nfi_tree_plot_stratum_ready_github.parquet")
f_plot <- file.path(gh, "nfi_plots_github.parquet")
priv <- "C:/apter_private"
bak <- file.path(priv, "nfi_original"); map_file <- file.path(priv, "nfi_id_map.rds")
if (file.exists(map_file)) stop("Already sanitized (", map_file, " exists).", call. = FALSE)

dir.create(bak, showWarnings = FALSE, recursive = TRUE)
stopifnot(file.copy(c(f_tree, f_plot), bak, overwrite = FALSE))

trees <- as.data.frame(read_parquet(f_tree)); plots <- as.data.frame(read_parquet(f_plot))
stopifnot(!anyDuplicated(trees$treid))
set.seed(20261104)
new_id <- sample.int(nrow(trees))                       # random permutation of 1..N
map <- data.frame(treid_old = trees$treid, treid_new = new_id)
trees$treid <- as.integer(new_id)   # integer: as.character(100000) is "1e+05" for a double
drop <- c("volm", "volu", "plot_area_m2")
trees <- trees[, setdiff(names(trees), drop), drop = FALSE]
plots <- plots[, setdiff(names(plots), drop), drop = FALSE]
trees <- trees[order(trees$plot_id_github, trees$treid), , drop = FALSE]   # not in the original tree order
rownames(trees) <- NULL
saveRDS(map, map_file)
write_parquet(trees, f_tree); write_parquet(plots, f_plot)
cat(sprintf("trees %d x %d, plots %d x %d; columns now: %s\nmap: %s (private)\n",
            nrow(trees), ncol(trees), nrow(plots), ncol(plots), paste(names(trees), collapse = ", "), map_file))
