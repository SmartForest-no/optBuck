# ==============================================================================
# scripts/butt_condition_report.R  (roadmap 8, step 3, 2026-11-04)
#
# Purpose : Before/after report for the ProductCondition rule ("Buck butt-log only" /
#           "Do not buck butt log") in the shared engine. For each harvester file (the
#           owner's five private files in C:/apter_private/hpr, read in place, and the
#           synthetic fixtures) the assortments of the file itself are imported and the
#           file's own stems bucked twice: "before" = every ButtCondition blanked (the
#           old behaviour), "after" = as in the file. Also price tuning of file5's
#           spruce 140 SCA before/after. Files appear as file1..file5 only.
# Output  : outputs/butt_condition/before_after.txt
# Usage   : Rscript scripts/butt_condition_report.R [skip_tuning] [only=file5]
# ==============================================================================
root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  d <- normalizePath(Sys.getenv("APTER_ROOT", unset = getwd()), winslash = "/", mustWork = FALSE)
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
skip_tuning <- "skip_tuning" %in% commandArgs(trailingOnly = TRUE)
only <- sub("^only=", "", grep("^only=", commandArgs(trailingOnly = TRUE), value = TRUE))   # e.g. only=file5

dirp <- "C:/apter_private/hpr"
files <- c(file1 = list.files(dirp, "^REDACTED46_REDACTED45.*\\.hpr$", full.names = TRUE)[1L],
           file2 = file.path(dirp, "REDACTED40"), file3 = file.path(dirp, "REDACTED33"),
           file4 = file.path(dirp, "REDACTED27"), file5 = file.path(dirp, "REDACTED28"))
fx <- list.files(file.path(root_dir, "inputs", "test_fixtures"), "\\.hpr$", full.names = TRUE)
names(fx) <- paste0("fixture:", sub("\\.hpr$", "", basename(fx)))
files <- c(files[file.exists(files)], fx)
if (length(only)) files <- files[names(files) %in% only]
out <- file.path(root_dir, "outputs", "butt_condition"); dir.create(out, recursive = TRUE, showWarnings = FALSE)
log <- file.path(out, if (length(only)) paste0("before_after_", only, ".txt") else "before_after.txt"); unlink(log)
say <- function(...) { x <- sprintf(...); cat(x, "\n"); cat(x, "\n", file = log, append = TRUE) }

run_one <- function(st, smap, defs, pm) {
  pm <- pm_minimal(pm); pr <- products_from_defs(defs, pm)
  price_path <- tempfile(fileext = ".csv"); pm_export(pm, price_path)
  job <- prepare_hpr_stems_job(st, smap, price_matrix_path = price_path, products = pr$pd, length_classes = pr$lc)
  r <- finish_job(job, buck_job(job)); l <- r$selected_logs_all
  list(value = sum(l$value_nok, na.rm = TRUE),
       saw = sum(l$commercial_volume_m3[as.character(l$ProductType) %in% "sawlog"], na.rm = TRUE),
       cv = sum(l$commercial_volume_m3, na.rm = TRUE), logs = nrow(l), job = job, pr = pr, pm = pm)
}
say("ProductCondition before/after (before = ButtCondition blanked), %s", format(Sys.time(), "%Y-%m-%d %H:%M"))
say("%-34s %6s %12s %12s %10s %10s %s", "file", "n_cond", "value before", "value after", "sawlog b", "sawlog a", "logs b/a")
for (nm in names(files)) {
  p <- files[[nm]]
  h <- read_hpr(p); if (!isTRUE(h$ok)) { say("%-34s read_hpr: %s", nm, h$errors[1L]); next }
  smap <- setNames(guess_hpr_species(h$species$name, h$species$user_id), h$species$key)
  sc <- hpr_to_scenario(h, smap); if (!isTRUE(sc$ok)) { say("%-34s scenario: %s", nm, sc$errors[1L]); next }
  st <- read_hpr_stem_files(p)
  if (!isTRUE(st$ok)) { say("%-34s stems: %s", nm, paste(st$errors, collapse = " ")); next }
  stmap <- setNames(guess_hpr_species(st$species$name, st$species$user_id), st$species$key)
  n_cond <- sum(!is.na(sc$defs$ButtCondition))
  after <- run_one(st, stmap, sc$defs, sc$pm)
  d0 <- sc$defs; d0$ButtCondition <- NA_character_
  before <- run_one(st, stmap, d0, sc$pm)
  say("%-34s %6d %12.0f %12.0f %9.1f%% %9.1f%% %d/%d%s", nm, n_cond, before$value, after$value,
      100 * before$saw / before$cv, 100 * after$saw / after$cv, before$logs, after$logs,
      if (n_cond == 0L) sprintf("  identical: %s", isTRUE(all.equal(before$value, after$value, tolerance = 0)) && before$logs == after$logs) else "")
  if (n_cond > 0L) say("   products with a condition: %s", paste(sprintf("%s/%s=%s", sc$defs$Name[!is.na(sc$defs$ButtCondition)],
      sc$defs$Species[!is.na(sc$defs$ButtCondition)], sc$defs$ButtCondition[!is.na(sc$defs$ButtCondition)]), collapse = "; "))
  if (identical(nm, "file5") && !skip_tuning) {
    for (lab in c("before", "after")) {
      defs <- sc$defs; if (lab == "before") defs$ButtCondition <- NA_character_
      pm <- pm_minimal(sc$pm); pr <- products_from_defs(defs, pm)
      products <- list(pd = pr$pd, lc = pr$lc, pm = pm)
      k140 <- products$pd$ProductKey[products$pd$species_group == "spruce" & products$pd$ProductType == "sawlog" &
                                       grepl("140 sca", products$pd$ProductName, ignore.case = TRUE)][1L]
      price_path <- tempfile(fileext = ".csv"); pm_export(pm, price_path)
      job <- prepare_hpr_stems_job(st, stmap, price_matrix_path = price_path, products = pr$pd, length_classes = pr$lc)
      t0 <- proc.time()[["elapsed"]]
      r1 <- tuning_run(job, products, defs, k140, 30L, list(use_grades = FALSE, cut_at_changes = FALSE))
      say("   tuning file5 spruce 140 SCA (%s): n %d | FG %.1f -> %.1f | value %.0f -> %.0f NOK (%+.2f %%) | %.0f s", lab,
          as.integer(r1$n_logs[[k140]]), r1$by[[k140]]$fg_o, r1$by[[k140]]$fg_t, r1$orig$value, r1$tuned$value,
          100 * (r1$tuned$value - r1$orig$value) / r1$orig$value, proc.time()[["elapsed"]] - t0)
    }
  }
  rm(st, before, after); invisible(gc())
}
say("done")
