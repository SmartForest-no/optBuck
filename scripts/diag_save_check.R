# Saves bucking_check() results of the owner's files to C:/optbuck_private/diag/ (private; never committed).
# Usage: Rscript scripts/diag_save_check.R file5 [file1 file4] [noreject]
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
cfg <- list(file1 = priv_hpr("file1"), file3 = priv_hpr("file3"),
            file4 = priv_hpr("file4"), file5 = priv_hpr("file5"))
args <- commandArgs(trailingOnly = TRUE)
rz <- !"noreject" %in% args; uo <- "usedonly" %in% args
for (nm in intersect(names(cfg), args)) {
  t0 <- Sys.time()
  x <- bucking_check(cfg[[nm]], reject_zones = rz, used_only = uo)
  saveRDS(x, sprintf("C:/optbuck_private/diag/%s_%s.rds", nm, if (uo) "used" else if (rz) "rz" else "norz"))
  cat(nm, rz, sprintf("%.0f s", as.numeric(difftime(Sys.time(), t0, units = "secs"))), "\n")
  print(x$key_figures)
}
