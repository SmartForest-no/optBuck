# ==============================================================================
# scripts/verify_plot_permutation.R  (roadmap 8, answer 1, 2026-11-04)
#
# Purpose : Show that relabelling the NFI plot IDs (scripts/permute_plot_ids.R) changes nothing but
#           the plot_id_github column: the three NFI strata of audits 16/20 are run with
#           all_plots = TRUE (no sampling, so the plot labels cannot change which trees are
#           bucked) BEFORE and AFTER the relabelling; after translating the plot column back with
#           the private map the result tables must be identical.
# Usage   : Rscript scripts/verify_plot_permutation.R before    (writes the reference, private)
#           Rscript scripts/verify_plot_permutation.R after     (compares; needs nfi_plot_id_map.rds)
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
mode <- commandArgs(trailingOnly = TRUE)[1L]
ref_file <- "C:/apter_private/diag/plot_perm_before.rds"
tabs <- c("selected_logs_all", "tree_summary", "product_summary", "per_m3_summary")
run_all <- function() lapply(PERF_NFI_STRATA, function(s) {
  s$all_plots <- TRUE
  r <- run_sampled_plot_bucking(s, "default_2021", root_dir)
  r[tabs]
})
srt <- function(d) { k <- intersect(c("plot_id_github", "treid", "StartPos", "ProductKey", "scope"), names(d))
                     d <- d[do.call(order, unname(as.list(d[k]))), , drop = FALSE]; rownames(d) <- NULL; d }
if (identical(mode, "before")) {
  saveRDS(run_all(), ref_file); cat("reference written\n")
} else {
  b <- readRDS(ref_file); a <- run_all()
  map <- readRDS("C:/apter_private/nfi_plot_id_map.rds")
  ok <- TRUE
  for (n in names(a)) for (t in tabs) {
    x <- a[[n]][[t]]
    if ("plot_id_github" %in% names(x)) x$plot_id_github <- map$plot_old[match(x$plot_id_github, map$plot_new)]
    same <- isTRUE(all.equal(srt(x), srt(b[[n]][[t]]), tolerance = 1e-12, check.attributes = FALSE))
    ok <- ok && same
    cat(sprintf("%-8s %-18s %s (%d rows)\n", n, t, if (same) "identical" else "DIFFERS", nrow(x)))
  }
  cat(if (ok) "\nOK: relabelling the plots changed nothing but the plot IDs.\n" else "\nFAILED\n")
  quit(status = if (ok) 0L else 1L)
}
