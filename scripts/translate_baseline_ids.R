# ==============================================================================
# scripts/translate_baseline_ids.R  (roadmap 8, step 2, 2026-11-04)
#
# Purpose : The NFI tree numbers (`treid`) were replaced by a random permutation
#           (scripts/sanitize_nfi_table.R). This rewrites ONLY the tree ids of the NFI
#           cases of the main baseline (outputs/performance/baseline_main_3fa0a9d.rds)
#           to the new ids and writes baseline_main_3fa0a9d_newids.rds. Values are not
#           touched, so performance_check.R must still report "identical"; the
#           original baseline file is not changed. Tree-list cases (tl*) have their
#           own synthetic ids and are copied as they are.
# Needs   : C:/apter_private/nfi_id_map.rds (private; never committed).
# ==============================================================================
root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  d <- normalizePath(Sys.getenv("APTER_ROOT", unset = getwd()), winslash = "/", mustWork = FALSE)
  while (!is_root(d) && dirname(d) != d) d <- dirname(d)
  if (!is_root(d)) stop("Cannot locate the project root.", call. = FALSE)
  d
})
map <- readRDS("C:/apter_private/nfi_id_map.rds")
to_new <- setNames(sprintf("%d", as.integer(map$treid_new)), sprintf("%.0f", map$treid_old))
f_old <- file.path(root_dir, "outputs", "performance", "baseline_main_3fa0a9d.rds")
f_new <- file.path(root_dir, "outputs", "performance", "baseline_main_3fa0a9d_newids.rds")
b <- readRDS(f_old); n_changed <- 0L
for (case in grep("^nfi_", names(b), value = TRUE)) {
  for (tab in c("selected_logs_all", "tree_summary", "zero_log_trees")) {
    d <- b[[case]][[tab]]
    if (is.data.frame(d) && "treid" %in% names(d) && nrow(d)) {
      new <- unname(to_new[as.character(d$treid)])
      stopifnot(!anyNA(new))
      b[[case]][[tab]]$treid <- new; n_changed <- n_changed + nrow(d)
    }
  }
}
saveRDS(b, f_new)
cat(sprintf("%d tree-id cells translated in %s; written %s\n", n_changed, paste(grep("^nfi_", names(b), value = TRUE), collapse = ", "), basename(f_new)))
