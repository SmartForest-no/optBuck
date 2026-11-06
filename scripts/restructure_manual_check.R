# ==============================================================================
# scripts/restructure_manual_check.R (restructure, end of session, 2026-10-30)
#
# The tuning tab's computation (tuning_run() of app/tuning.R, 30 iterations,
# seed 1) on the owner's file5, without the browser:
#   1. spruce 140 SCA alone with its file target (as before the restructure:
#      FG on all stems about 83 -> 94, value change about -0.2 %),
#   2. spruce 140 SCA together with spruce 148 (Even target set with the
#      editor's helpers): FG per assortment, value change, runtime.
# file5 is read in place (C:/optbuck_private/hpr), the result goes to
# outputs/restructure/manual_check.txt (gitignored).
#
# Usage: Rscript scripts/restructure_manual_check.R
# ==============================================================================
root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!is_root(d) && dirname(d) != d) d <- dirname(d)
  if (!is_root(d)) stop("Cannot locate the project root.", call. = FALSE)
  d
})
Sys.setenv(OPTBUCK_ROOT = root_dir)
suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)

# tuning_run() and its constant, without the app's UI code
for (e in parse(file.path(root_dir, "inst", "app", "tuning.R"), keep.source = FALSE))
  if (is.call(e) && identical(as.character(e[[1L]]), "<-") && as.character(e[[2L]]) %in% c("TUNE_MIN_LOGS", "tuning_run")) eval(e)

f5 <- "C:/optbuck_private/hpr/REDACTED28"
stopifnot(file.exists(f5))
out <- file.path(root_dir, "outputs", "restructure"); dir.create(out, recursive = TRUE, showWarnings = FALSE)
log <- file.path(out, "manual_check.txt"); unlink(log)
say <- function(...) { x <- sprintf(...); cat(x, "\n"); cat(x, "\n", file = log, append = TRUE) }

h <- read_hpr(f5); stopifnot(isTRUE(h$ok))
sc <- hpr_to_scenario(h, setNames(guess_hpr_species(h$species$name, h$species$user_id), h$species$key)); stopifnot(isTRUE(sc$ok))
pm <- pm_minimal(sc$pm); pr <- products_from_defs(sc$defs, pm)
products <- list(pd = pr$pd, lc = pr$lc, pm = pm)
nm <- function(k) products$pd$ProductName[match(k, products$pd$ProductKey)]
k140 <- products$pd$ProductKey[products$pd$species_group == "spruce" & products$pd$ProductType == "sawlog" & grepl("140 sca", products$pd$ProductName, ignore.case = TRUE)][1L]
k148 <- products$pd$ProductKey[products$pd$species_group == "spruce" & products$pd$ProductType == "sawlog" & grepl("148", products$pd$ProductName, fixed = TRUE)][1L]
st <- read_hpr_stems(f5); stopifnot(isTRUE(st$ok))
smap <- setNames(guess_hpr_species(st$species$name, st$species$user_id), st$species$key)
price_path <- tempfile(fileext = ".csv"); pm_export(pm, price_path)
job <- prepare_hpr_stems_job(st, smap, price_matrix_path = price_path, products = pr$pd, length_classes = pr$lc)
rm(st); invisible(gc())
say("file5: %d stems; assortments %s and %s", length(job$pvs), nm(k140), nm(k148))

report <- function(r, secs) {
  for (k in r$used)
    say("  %-28s FG on all stems %.1f -> %.1f", nm(k), r$by[[k]]$fg_o, r$by[[k]]$fg_t)
  say("  value at original prices %.0f -> %.0f NOK (%+.2f %%) | sawlog volume %.1f -> %.1f m3 | sample %d stems, best iteration %d | %.0f s",
      r$orig$value, r$tuned$value, 100 * (r$tuned$value - r$orig$value) / r$orig$value, r$orig$saw_vol, r$tuned$saw_vol,
      r$n, r$best_iter, secs)
}
say("1. %s alone, its file target, 30 iterations", nm(k140))
t0 <- proc.time()[["elapsed"]]
r1 <- tuning_run(job, products, sc$defs, k140, 30L, list(use_grades = FALSE, cut_at_changes = FALSE))
report(r1, proc.time()[["elapsed"]] - t0)

say("2. %s (file target) together with %s (Even target), 30 iterations", nm(k140), nm(k148))
pm2 <- dist_apply_target(pm, k148, "neutral")
defs2 <- dist_init_fields(dist_clear_fields(sc$defs, k148), k148)
t0 <- proc.time()[["elapsed"]]
r2 <- tuning_run(job, list(pd = pr$pd, lc = pr$lc, pm = pm2), defs2, c(k140, k148), 30L, list(use_grades = FALSE, cut_at_changes = FALSE))
report(r2, proc.time()[["elapsed"]] - t0)
say("done; written to %s", log)
