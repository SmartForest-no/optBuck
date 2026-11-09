# ==============================================================================
# Capture the single-product tune_prices() result on the owner's file5 (spruce
# 140 Sagtømmer gran), with the code as it is when this script runs. Audit 33 check 14
# compares later versions of tune_prices() against it.
#
# The result is derived from a real buyer's price list, so it is written to
# outputs/restructure/tuning_single_expected.rds (gitignored), never into the
# repository. Run it once, BEFORE tune_prices() is changed.
#
# Usage: Rscript scripts/capture_tuning_single_baseline.R
# ==============================================================================

root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  env <- Sys.getenv("OPTBUCK_ROOT", unset = "")
  d <- if (nzchar(env)) normalizePath(env, winslash = "/", mustWork = FALSE) else
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!is_root(d) && dirname(d) != d) d <- dirname(d)
  if (!is_root(d)) stop("Cannot locate the project root.", call. = FALSE)
  d
})
Sys.setenv(OPTBUCK_ROOT = root_dir)
suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)

source(file.path(root_dir, "scripts", "roadmap7_helpers.R"))

f5 <- "C:/optbuck_private/hpr/REDACTED28"
if (!file.exists(f5)) stop("file5 not available: ", f5)

hp <- read_hpr(f5); stopifnot(isTRUE(hp$ok))
smap_h <- setNames(guess_hpr_species(hp$species$name, hp$species$user_id), hp$species$key)
sc <- hpr_to_scenario(hp, smap_h); stopifnot(isTRUE(sc$ok))
pm <- pm_minimal(sc$pm); pr <- products_from_defs(sc$defs, pm)
products <- list(pd = pr$pd, lc = pr$lc, pm = pm)
dst <- read_hpr_distribution(f5)
key <- products$pd$ProductKey[products$pd$species_group == "spruce" & products$pd$ProductType == "sawlog" &
                                grepl("140 .*sagt", products$pd$ProductName, ignore.case = TRUE)][1L]
hk <- sc$defs$HprProductKey[match(key, assortment_table_keys(sc$defs))]
hpr <- hpr_tuning_info(dst, hk)
st <- read_hpr_stems(f5); stopifnot(isTRUE(st$ok))
smap <- setNames(guess_hpr_species(st$species$name, st$species$user_id), st$species$key)
price_path <- tempfile(fileext = ".csv"); pm_export(pm, price_path)
job <- prepare_hpr_stems_job(st, smap, price_matrix_path = price_path, products = pr$pd, length_classes = pr$lc)
rm(st); invisible(gc())

t <- tune_prices(job, products, key, "real", n = 200, seed = 1, max_iter = 3, hpr = hpr, early_stop = FALSE)
expected <- list(
  trace      = t$trace[setdiff(names(t$trace), "elapsed_s")],
  best_price = t$best_price,
  best_iter  = t$best_iter,
  fg         = t$trace$fg,
  value      = t$trace$value_total
)
out <- file.path(root_dir, "outputs", "restructure")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
saveRDS(expected, file.path(out, "tuning_single_expected.rds"))
cat(sprintf("saved: iterations %d, best %d, FG %s\n", nrow(t$trace), t$best_iter,
            paste(round(t$trace$fg, 2), collapse = " -> ")))
