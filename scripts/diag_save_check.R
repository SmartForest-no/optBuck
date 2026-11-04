# Saves bucking_check() results of the owner's files to C:/apter_private/diag/ (private; never committed).
# Usage: Rscript scripts/diag_save_check.R file5 [file1 file4] [noreject]
root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  d <- normalizePath(Sys.getenv("APTER_ROOT", unset = getwd()), winslash = "/", mustWork = FALSE)
  while (!is_root(d) && dirname(d) != d) d <- dirname(d)
  d
})
Sys.setenv(APTER_ROOT = root_dir)
source(file.path(root_dir, "R", "helpers_paths.R"))
source(file.path(root_dir, "scripts", "app_backend", "01_run_sampled_plot_bucking.R"), local = FALSE)
dirp <- "C:/apter_private/hpr"
cfg <- list(file1 = list.files(dirp, "^REDACTED46_REDACTED45.*[.]hpr$", full.names = TRUE)[1L], file3 = file.path(dirp, "REDACTED33"),
            file4 = file.path(dirp, "REDACTED27"), file5 = file.path(dirp, "REDACTED28"))
args <- commandArgs(trailingOnly = TRUE)
rz <- !"noreject" %in% args; uo <- "usedonly" %in% args
for (nm in intersect(names(cfg), args)) {
  t0 <- Sys.time()
  x <- bucking_check(cfg[[nm]], reject_zones = rz, used_only = uo)
  saveRDS(x, sprintf("C:/apter_private/diag/%s_%s.rds", nm, if (uo) "used" else if (rz) "rz" else "norz"))
  cat(nm, rz, sprintf("%.0f s", as.numeric(difftime(Sys.time(), t0, units = "secs"))), "\n")
  print(x$key_figures)
}
