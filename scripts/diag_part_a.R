# Part A numbers (roadmap 8, 2026-11-05): the bucking check with and without manual-only cells for the best
# possible bucking, and the stems left out as incomplete; private files, output only to C:/optbuck_private/diag/.
# Usage: Rscript scripts/diag_part_a.R file1 file4 file5
root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  d <- normalizePath(Sys.getenv("OPTBUCK_ROOT", unset = getwd()), winslash = "/", mustWork = FALSE)
  while (!is_root(d) && dirname(d) != d) d <- dirname(d)
  d
})
Sys.setenv(OPTBUCK_ROOT = root_dir)
suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)

dirp <- "C:/optbuck_private/hpr"
priv_hpr <- function(id) { m <- utils::read.csv("C:/optbuck_private/hpr_files.csv", stringsAsFactors = FALSE, encoding = "UTF-8"); file.path(dirp, m$file[match(id, m$id)]) }   # the real names are private (not in the repository)
cfg <- list(file1 = priv_hpr("file1"), file4 = priv_hpr("file4"), file5 = priv_hpr("file5"))
for (nm in intersect(names(cfg), commandArgs(TRUE))) for (mo in c(FALSE, TRUE)) {
  t0 <- Sys.time(); x <- bucking_check(cfg[[nm]], manual_ok = mo)
  saveRDS(x, sprintf("C:/optbuck_private/diag/%s_a_%s.rds", nm, if (mo) "after" else "before"))
  s <- x$stems; u <- s$use & s$value_opt > 0
  cat(sprintf("%s manual_ok=%s: stems %d, yield %.2f %% (rec %.0f / best %.0f), rec>best %d, yield 0 (rec 0) %d, incomplete %d, left out %d, %.0f s\n", nm, mo,
      sum(u), 100 * sum(s$value_rec[u]) / sum(s$value_opt[u]), sum(s$value_rec[u]), sum(s$value_opt[u]), sum(s$rec_gt_opt, na.rm = TRUE),
      sum(u & s$value_rec == 0), x$n_incomplete, x$n_left_out, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
