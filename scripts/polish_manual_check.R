# ==============================================================================
# scripts/polish_manual_check.R (R4, end of the session, 2026-11-02)
#
# The Price tuning tab's computation (tuning_run() of app/tuning.R, seed 1) on
# the built-in NFI stems, without the browser (D7): the selection of the R4
# task (Gran og furu, HKL 5, bonitet 11/14/17, fylke 11 Rogaland, 0-1000 m:
# 21 plots, 18 spruce and 131 pine trees), the default assortments and prices
# with an Even target on the main spruce sawlog assortment, 30 iterations.
# Reports the numbers and the runtime. The result goes to
# outputs/restructure/polish_manual_check.txt (gitignored).
#
# Usage: Rscript scripts/polish_manual_check.R
# ==============================================================================
root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!is_root(d) && dirname(d) != d) d <- dirname(d)
  if (!is_root(d)) stop("Cannot locate the project root.", call. = FALSE)
  d
})
Sys.setenv(APTER_ROOT = root_dir)
suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)

# tuning_run() and its constant, without the app's UI code
for (e in parse(file.path(root_dir, "inst", "app", "tuning.R"), keep.source = FALSE))
  if (is.call(e) && identical(as.character(e[[1L]]), "<-") && as.character(e[[2L]]) %in% c("TUNE_MIN_LOGS", "tuning_run")) eval(e)

out <- file.path(root_dir, "outputs", "restructure"); dir.create(out, recursive = TRUE, showWarnings = FALSE)
log <- file.path(out, "polish_manual_check.txt"); unlink(log)
say <- function(...) { x <- sprintf(...); cat(x, "\n"); cat(x, "\n", file = log, append = TRUE) }

defs0 <- defs_from_products(load_products()); pm0 <- pm_minimal(load_price_matrix_long())

run_case <- function(label, params, key) {
  pm <- dist_apply_target(pm0, key, "neutral")
  defs <- dist_init_fields(dist_clear_fields(defs0, key), key)
  pr <- products_from_defs(defs, pm)
  products <- list(pd = pr$pd, lc = pr$lc, pm = pm)
  price_path <- tempfile(fileext = ".csv"); pm_export(pm, price_path)
  t0 <- proc.time()[["elapsed"]]
  job <- prepare_sampled_job(params, price_matrix_path = price_path, products = pr$pd, length_classes = pr$lc)
  t_prep <- proc.time()[["elapsed"]] - t0
  say("%s: %d trees on %d plots; job prepared in %.1f s; Even target on %s", label, length(job$pvs), job$meta$n_plots_sampled, t_prep, key)
  t0 <- proc.time()[["elapsed"]]
  r <- tuning_run(job, products, defs, key, 30L, list(use_grades = FALSE, cut_at_changes = FALSE))
  secs <- proc.time()[["elapsed"]] - t0
  if (isTRUE(r$too_few)) {
    say("  too few logs to tune (fewer than %d logs in a diameter class) | %.0f s", TUNE_MIN_LOGS, secs)
  } else {
    for (k in r$ids)
      say("  %-24s logs %d | FG on all stems %.1f -> %.1f | tuned %s", k, as.integer(r$n_logs[[k]]), r$by[[k]]$fg_o, r$by[[k]]$fg_t, k %in% r$used)
    say("  value at original prices %.0f -> %.0f NOK (%+.2f %%) | sawlog volume %.1f -> %.1f m3 | sample %d of %d stems, best iteration %d | %.0f s",
        r$orig$value, r$tuned$value, 100 * (r$tuned$value - r$orig$value) / r$orig$value, r$orig$saw_vol, r$tuned$saw_vol,
        r$n, r$N, r$best_iter, secs)
  }
}
a1 <- list(species_group = "both", hogstkl = c(51L, 52L), abonitet = c(11L, 14L, 17L), fylnr = "11", hoh_max = 1000, all_plots = TRUE)
run_case("A1 selection (Gran og furu, HKL 5, bonitet 11/14/17, Rogaland, 0-1000 m)", a1, "spruce_sawlog")
run_case("A1 selection, the pine sawlog instead", a1, "pine_sawlog")
run_case("default stratum (spruce, HKL 4, bonitet 11/14/17, all counties)",
         list(species_group = "spruce", hogstkl = c(41L, 42L), abonitet = c(11L, 14L, 17L), all_plots = TRUE), "spruce_sawlog")
say("done; written to %s", log)
