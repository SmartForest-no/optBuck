# ==============================================================================
# scripts/grades_manual_check.R (R5 "Kvalitetskoder og sortimenttyper", 2026-11-03)
#
# The owner's private .hpr files (C:/apter_private/hpr, read in place, never
# copied) run the way the app runs them, without the browser:
#   stems from one file, assortments imported from another (or the same), the
#   same-instruction test (hpr_same_instruction()), the grade options with the
#   per-species grade files and the notes (grade_unused_notes()).
# Cases a-f of the R5 task: value, sawlog share (sawlog m3 / all commercial m3),
# whether the box would be disabled, the notes; f = file5 price tuning.
# The result goes to outputs/restructure/grades_manual_check.txt (gitignored).
#
# Usage: Rscript scripts/grades_manual_check.R [cases, e.g. a,b,c]
# ==============================================================================
root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!is_root(d) && dirname(d) != d) d <- dirname(d)
  if (!is_root(d)) stop("Cannot locate the project root.", call. = FALSE)
  d
})
Sys.setenv(APTER_ROOT = root_dir)
source(file.path(root_dir, "R", "helpers_paths.R"))
source(file.path(root_dir, "scripts", "app_backend", "01_run_sampled_plot_bucking.R"), local = FALSE)
source(file.path(root_dir, "R", "helpers_price_tuning.R"))
source(file.path(root_dir, "R", "helpers_distribution_edit.R"))
for (e in parse(file.path(root_dir, "app", "tuning.R"), keep.source = FALSE))
  if (is.call(e) && identical(as.character(e[[1L]]), "<-") && as.character(e[[2L]]) %in% c("TUNE_MIN_LOGS", "tuning_run")) eval(e)

args <- commandArgs(trailingOnly = TRUE)
cases <- if (length(args)) strsplit(args[1L], ",")[[1L]] else c("a", "b", "c", "d", "e", "f")
dirp <- "C:/apter_private/hpr"
F1 <- list.files(dirp, "^REDACTED46_REDACTED45.*\\.hpr$", full.names = TRUE)[1L]   # file1, REDACTED45
F3 <- file.path(dirp, "REDACTED33")                                          # file3, Gobakken
F5 <- file.path(dirp, "REDACTED28")                                       # file5
stopifnot(file.exists(F1), file.exists(F3), file.exists(F5))
out <- file.path(root_dir, "outputs", "restructure"); dir.create(out, recursive = TRUE, showWarnings = FALSE)
log <- file.path(out, "grades_manual_check.txt"); unlink(log)
say <- function(...) { x <- sprintf(...); cat(x, "\n"); cat(x, "\n", file = log, append = TRUE) }

stems_cache <- list()
read_stems <- function(path) {
  if (is.null(stems_cache[[path]])) {
    st <- read_hpr_stem_files(path)
    stopifnot(isTRUE(st$ok))
    stems_cache[[path]] <<- list(st = st, map = setNames(guess_hpr_species(st$species$name, st$species$user_id), st$species$key))
  }
  stems_cache[[path]]
}
scenario_of <- function(path) {
  h <- read_hpr(path); stopifnot(isTRUE(h$ok))
  sc <- hpr_to_scenario(h, setNames(guess_hpr_species(h$species$name, h$species$user_id), h$species$key))
  stopifnot(isTRUE(sc$ok)); sc
}

# One run the way the app does it: the box is disabled (and off) when no stem file has the
# scenario's instruction; otherwise the grade files are those with the same instruction.
run_case <- function(label, stem_path, defs, pm, use, cut) {
  s <- read_stems(stem_path); st <- s$st
  dist <- hpr_grade_lengths(st, s$map)
  same <- hpr_same_instruction(st$instruction, s$map, defs, files = sort(unique(dist$file)))
  disabled <- !any(same$same)
  u <- use && !disabled
  opts <- list(use_grades = u, cut_at_changes = cut)
  if (u || cut) opts$grade_files <- grade_files_of(same, TRUE)
  pm <- pm_minimal(pm); pr <- products_from_defs(defs, pm)
  price_path <- tempfile(fileext = ".csv"); pm_export(pm, price_path)
  job <- prepare_hpr_stems_job(st, s$map, price_matrix_path = price_path, products = pr$pd,
                               length_classes = pr$lc, grade_opts = opts)
  r <- finish_job(job, buck_job(job))
  l <- r$selected_logs_all
  value <- sum(l$value_nok, na.rm = TRUE); cv <- sum(l$commercial_volume_m3, na.rm = TRUE)
  saw <- sum(l$commercial_volume_m3[as.character(l$ProductType) %in% "sawlog"], na.rm = TRUE)
  tm <- tapply(l$commercial_volume_m3, type_group(l$ProductType), sum)
  nt <- if (use || disabled) grade_unused_notes(dist, same) else grade_unused_notes(NULL, NULL)
  say("%s: value %s NOK | sawlog %.1f %% | types %s | box disabled %s | same %s | notes %s",
      label, format(round(value), big.mark = " "), 100 * saw / cv,
      paste(sprintf("%s %.0f m3", names(tm), tm), collapse = ", "), disabled,
      paste(same$species, same$same, sep = "=", collapse = "/"),
      if (nrow(nt)) paste(sprintf("%s %s %%", nt$species, nt$share), collapse = ", ") else "none")
  invisible(list(value = value, saw = 100 * saw / cv, logs = r$selected_logs_all, disabled = disabled, notes = nt))
}

if ("a" %in% cases) {
  sc1 <- scenario_of(F1)
  a <- run_case("a. REDACTED45 stems + REDACTED45 assortments, grades + cut on", F1, sc1$defs, sc1$pm, TRUE, TRUE)
}
if ("b" %in% cases) {
  sc3 <- scenario_of(F3)
  b <- run_case("b. Gobakken stems + Gobakken assortments, grades + cut on", F3, sc3$defs, sc3$pm, TRUE, TRUE)
}
if ("c" %in% cases) {
  sc3 <- scenario_of(F3)
  c1 <- run_case("c. REDACTED45 stems + Gobakken assortments, grades + cut on", F1, sc3$defs, sc3$pm, TRUE, TRUE)
  c0 <- run_case("c. (reference) the same with both options off", F1, sc3$defs, sc3$pm, FALSE, FALSE)
  say("   c equals both-off: %s", isTRUE(all.equal(c1$value, c0$value)) && identical(nrow(c1$logs), nrow(c0$logs)))
}
if ("d" %in% cases) {
  sc1 <- scenario_of(F1)
  d1 <- run_case("d. Gobakken stems + REDACTED45 assortments, grades + cut on", F3, sc1$defs, sc1$pm, TRUE, TRUE)
  d0 <- run_case("d. (reference) the same with both options off", F3, sc1$defs, sc1$pm, FALSE, FALSE)
  say("   d equals both-off: %s", isTRUE(all.equal(d1$value, d0$value)) && identical(nrow(d1$logs), nrow(d0$logs)))
  ty <- sc1$defs[, c("Name", "Species", "Type")]; ty$shown <- type_choice(ty$Type)
  say("   REDACTED45 types as scenario: %s", paste(sprintf("%s/%s=%s", ty$Name, ty$Species, ty$shown), collapse = "; "))
}
if ("e" %in% cases) {
  sc1 <- scenario_of(F1)
  # B: a copy of A with "SAG - REDACTED35" at a fixed price of 400 (the assortment form: flat pricing)
  defs <- sc1$defs; pm <- pm_minimal(sc1$pm)
  ks <- assortment_table_keys(defs)
  hit <- which(grepl("Mj\u00f8sbrukene", defs$Name))
  for (i in hit) {
    defs$PriceType[i] <- "flat"
    pm <- pm_minimal(pm_bind(pm[pm$ProductKey != ks[i], , drop = FALSE], pm_start_prices(ks[i], "flat", 400)))
  }
  v <- check_price_set(defs, pm); stopifnot(v$ok)
  e <- run_case("e. REDACTED45 stems + REDACTED45 assortments, SAG - Mj\u00f8sbrukene fixed 400 (scenario B), grades + cut on",
                F1, defs, v$pm, TRUE, TRUE)
}
if ("f" %in% cases) {
  h <- read_hpr(F5); sc <- hpr_to_scenario(h, setNames(guess_hpr_species(h$species$name, h$species$user_id), h$species$key))
  pm <- pm_minimal(sc$pm); pr <- products_from_defs(sc$defs, pm)
  products <- list(pd = pr$pd, lc = pr$lc, pm = pm)
  k140 <- products$pd$ProductKey[products$pd$species_group == "spruce" & products$pd$ProductType == "sawlog" & grepl("140 sca", products$pd$ProductName, ignore.case = TRUE)][1L]
  st <- read_hpr_stems(F5); stopifnot(isTRUE(st$ok))
  smap <- setNames(guess_hpr_species(st$species$name, st$species$user_id), st$species$key)
  price_path <- tempfile(fileext = ".csv"); pm_export(pm, price_path)
  job <- prepare_hpr_stems_job(st, smap, price_matrix_path = price_path, products = pr$pd, length_classes = pr$lc)
  rm(st); invisible(gc())
  t0 <- proc.time()[["elapsed"]]
  r1 <- tuning_run(job, products, sc$defs, k140, 30L, list(use_grades = FALSE, cut_at_changes = FALSE))
  say("f. file5 tuning, spruce 140 SCA alone: n %d | FG %.1f -> %.1f | value %.0f -> %.0f NOK (%+.2f %%) | %.0f s",
      as.integer(r1$n_logs[[k140]]), r1$by[[k140]]$fg_o, r1$by[[k140]]$fg_t, r1$orig$value, r1$tuned$value,
      100 * (r1$tuned$value - r1$orig$value) / r1$orig$value, proc.time()[["elapsed"]] - t0)
}
say("done; written to %s", log)
