# ==============================================================================
# scripts/verify_vs_old_lite.R  (roadmap 8, step 5 LITE, 2026-11-04)
#
# Purpose : First comparison of the new recorded-log valuation (bucking_check()) with the saved
#           actual values of the old optBuck (C:/optbuck_private/optbuck_reference/<file>/per_stem.csv)
#           per stem, for file1, file3, file4 and file5 (patched copy). It reads the saved RDS/CSV only;
#           the old package is never run. NOT the full R3 verification: the "old-rules mode"
#           (a re-implementation of the old valuation on the new reader's data, B5-1) and the
#           one-rule-at-a-time attribution (B5-2) are not done yet (PLAN.md, Blocked items).
# Output  : C:/optbuck_private/optbuck_reference/r3_lite.csv and stem-level files r3_lite_<file>.csv
# Usage   : Rscript scripts/verify_vs_old_lite.R [file1 file3 ...]
# ==============================================================================
root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  d <- normalizePath(Sys.getenv("OPTBUCK_ROOT", unset = getwd()), winslash = "/", mustWork = FALSE)
  while (!is_root(d) && dirname(d) != d) d <- dirname(d)
  d
})
Sys.setenv(OPTBUCK_ROOT = root_dir)
suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)

dirp <- "C:/optbuck_private/hpr"; ref <- "C:/optbuck_private/optbuck_reference"
cfg <- list(file1 = c(list.files(dirp, "^REDACTED46_REDACTED45.*\\.hpr$", full.names = TRUE)[1L], "file1"),
            file3 = c(file.path(dirp, "REDACTED33"), "file3"),
            file4 = c(file.path(dirp, "REDACTED27"), "file4"),
            file5 = c(file.path(dirp, "REDACTED28"), "file5p"))
args <- commandArgs(trailingOnly = TRUE); if (length(args)) cfg <- cfg[names(cfg) %in% args]
out <- list()
for (nm in names(cfg)) {
  t0 <- Sys.time()
  x <- bucking_check(cfg[[nm]][1L])
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  old <- read.csv(file.path(ref, cfg[[nm]][2L], "per_stem.csv"), stringsAsFactors = FALSE)
  s <- x$stems[, c("StemKey", "species_group", "use", "value_rec", "value_opt", "n_logs_rec", "n_manual", "outside")]
  m <- merge(s, old[, c("StemKey", "actual_value", "n_logs_actual")], by.x = "StemKey", by.y = "StemKey")
  m$diff <- m$value_rec - m$actual_value
  m$rel <- ifelse(m$actual_value > 0, m$diff / m$actual_value, NA_real_)
  m$same_logs <- m$n_logs_rec == m$n_logs_actual
  write.csv(m, file.path(ref, sprintf("r3_lite_%s.csv", nm)), row.names = FALSE)
  eq <- abs(m$diff) < 0.01
  out[[nm]] <- data.frame(file = nm, seconds = round(secs), stems_new = nrow(s), stems_compared = nrow(m),
    equal_001_pct = round(100 * mean(eq), 1), within_1pct = round(100 * mean(abs(m$rel) < 0.01, na.rm = TRUE), 1),
    within_5pct = round(100 * mean(abs(m$rel) < 0.05, na.rm = TRUE), 1), same_n_logs_pct = round(100 * mean(m$same_logs), 1),
    old_total = round(sum(m$actual_value)), new_total = round(sum(m$value_rec)),
    total_diff_pct = round(100 * (sum(m$value_rec) / sum(m$actual_value) - 1), 2),
    median_rel_pct = round(100 * median(m$rel, na.rm = TRUE), 2),
    new_zero_old_pos = sum(m$value_rec == 0 & m$actual_value > 0), old_zero_new_pos = sum(m$actual_value == 0 & m$value_rec > 0))
  print(out[[nm]]); flush.console()
  rm(x); invisible(gc())
}
res <- do.call(rbind, out); write.csv(res, file.path(ref, "r3_lite.csv"), row.names = FALSE)
print(res)
