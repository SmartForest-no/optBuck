# ==============================================================================
# audits/audit_21_tree_lists_and_heights.R
#
# Purpose : Audit of Phase 5 (2026-10-13): uploaded tree lists
#           (R/helpers_tree_list.R), height imputation (R/helpers_heights.R)
#           and the tree-list backend run (run_tree_list_bucking()).
#
# Checks  :
#  1.  FinnHoyde reference curves by hand, both sides of each breakpoint:
#      spruce d 10, 14.9, 15, 30 -> 7.300000, 10.240000, 10.363264,
#      16.642749 m; pine d 9.9, 10, 34.9, 35, 50 -> 7.780144, 7.845600,
#      17.160302, 17.095000, 18.700000 m
#  2.  Spruce-only and pine-only lists impute without error (guards
#      fieldPlot's absent-species failure, which optBuck's wrapper avoids)
#  3.  >= 10 measured heights -> fieldPlot ("fieldPlot (plot)" in plots with
#      measured heights, "fieldPlot (species curve)" in a plot without);
#      no heights -> "FinnHoyde (uncalibrated)"; measured heights unchanged
#      bit for bit; every tree has a height source
#  3b. Calibration by hand: spruce with 3 heights (d 20/25/30 cm, h 16/18/
#      19.5 m): ratio = 53.5 / (12.969480 + 14.991026 + 16.642749)
#      = 1.199464; a spruce of 22 cm gets 1.199464 x 13.832943 = 16.592107 m
#      and source "FinnHoyde (calibrated, n = 3)"; the ratio is in the
#      summary
#  3c. A ratio outside 0.7-1.3 gives the calibration warning, one inside
#      does not; the site-independence warning only for a species with no
#      heights
#  4.  Time limit: on 20,000 trees in 800 plots, a 0.2 s limit stops the
#      nlme fit sooner than the unrestricted fit takes and nls is used
#      (fieldPlot's own timeout never interrupts nlme); no NA heights
#  5.  Validation: one broken file per error and per warning, each with its
#      message
#  6.  NFI equivalence: trees of an NFI run uploaded as a tree list with
#      their heights give the same cutting patterns, values and NOK/m3
#  7.  End to end: a mixed list (missing heights, two plots, one tree out of
#      range) runs at default and scenario prices; the comparison works;
#      height_source per tree; the excluded tree is listed
#  8.  The template validates with only its expected height warning
#
# Verdict : PASS / ACCEPTED WITH REVIEW / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/tree_lists/audit_21_tree_lists_and_heights.csv
#   outputs/tree_lists/audit_21_tree_lists_and_heights.txt
#
# Usage   : Rscript audits/audit_21_tree_lists_and_heights.R
# ==============================================================================

suppressPackageStartupMessages(library(readr))

root_dir <- local({
  # OPTBUCK_ROOT -> walk up from getwd() -> stop()
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  env <- Sys.getenv("OPTBUCK_ROOT", unset = "")
  if (nzchar(env)) {
    d <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (!is_root(d))
      stop("Project root '", d, "' (from OPTBUCK_ROOT) does not contain ",
           "R/helpers_paths.R.", call. = FALSE)
  } else {
    d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    while (!is_root(d) && dirname(d) != d) d <- dirname(d)
    if (!is_root(d))
      stop("Cannot locate the project root: set OPTBUCK_ROOT, or run from ",
           "inside the project directory.", call. = FALSE)
  }
  d
})
Sys.setenv(OPTBUCK_ROOT = root_dir)

suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)
source(file.path(root_dir, "audits", "helpers_audit.R"))


out_dir <- path_output("tree_lists", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_21_tree_lists_and_heights.txt")
audit_csv <- file.path(out_dir, "audit_21_tree_lists_and_heights.csv")
init_audit_log(audit_txt)

E <- new.env(parent = emptyenv())
rec <- function(check, result, detail = "")
  record_check(E, check, result, detail, log_path = audit_txt)
PASS <- "PASS"; REVIEW <- "REVIEW"; FAIL <- "FAIL"
section <- function(title) {
  cat(sprintf("\n--- %s ---\n", title), file = audit_txt, append = TRUE)
  cat(sprintf("\n--- %s ---\n", title))
}
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)
cat("  Audit: tree lists and heights\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

TOL <- 1e-6

# Synthetic trees on a Naslund curve with a plot effect (heights in m).
mk <- function(species, n_plots, per_plot, frac_h, seed, plots = sprintf("P%d", seq_len(n_plots))) {
  set.seed(seed)
  plot <- rep(plots, each = per_plot)
  u <- stats::rnorm(length(plots), 0, 0.12)[match(plot, plots)]
  d <- round(stats::runif(length(plot), 10, 40), 1)
  h <- round(1.3 + (d / (1.6 + u + 0.33 * d))^3 + stats::rnorm(length(d), 0, 0.6), 1)
  h[stats::runif(length(d)) > frac_h] <- NA
  data.frame(species_group = species, dbh_cm = d, height_m = h, plot_id = plot,
             stringsAsFactors = FALSE)
}
as_upload <- function(t) data.frame(tree_id = as.character(seq_len(nrow(t))),
                                    species = t$species_group, dbh = as.character(t$dbh_cm),
                                    height = ifelse(is.na(t$height_m), NA, as.character(t$height_m)),
                                    plot_id = t$plot_id, stringsAsFactors = FALSE)


# ==============================================================================
section("01. FinnHoyde reference curves by hand")
fh_s <- finnhoyde_height(c(10, 14.9, 15, 30), "spruce")
fh_p <- finnhoyde_height(c(9.9, 10, 34.9, 35, 50), "pine")
want_s <- c(7.300000, 10.240000, 10.363264, 16.642749)
want_p <- c(7.780144, 7.845600, 17.160302, 17.095000, 18.700000)
rec("Check 01: FinnHoyde spruce (10, 14.9, 15, 30 cm) and pine (9.9, 10, 34.9, 35, 50 cm) equal the hand values",
    if (all(abs(fh_s - want_s) < TOL) && all(abs(fh_p - want_p) < TOL)) PASS else FAIL,
    sprintf("spruce %s | pine %s", paste(sprintf("%.6f", fh_s), collapse = ", "),
            paste(sprintf("%.6f", fh_p), collapse = ", ")))


# ==============================================================================
section("02. Single-species lists")
one <- lapply(c("spruce", "pine"), function(s) tryCatch(
  impute_heights(mk(s, 4, 15, 0.5, 11)), error = function(e) conditionMessage(e)))
ok2 <- all(vapply(one, function(x) is.list(x) && !anyNA(x$height_m) &&
                    identical(x$summary$method, "fieldPlot"), logical(1)))
rec("Check 02: Spruce-only and pine-only lists impute without error, with fieldPlot",
    if (ok2) PASS else FAIL,
    paste(vapply(one, function(x) if (is.list(x)) paste(x$summary$species, x$summary$model)
                 else paste("ERROR:", x), character(1)), collapse = " | "))


# ==============================================================================
section("03. Methods per species")
sp3 <- mk("spruce", 3, 15, 0.6, 21)                       # >= 10 heights, plots P1-P3
sp3 <- rbind(sp3, transform(mk("spruce", 1, 6, 0, 22), plot_id = "P9"))   # plot without heights
pi3 <- mk("pine", 2, 6, 0, 23)                            # no heights
t3 <- rbind(sp3, pi3)
i3 <- impute_heights(t3)
meas <- !is.na(t3$height_m)
src_sp_missing <- unique(i3$height_source[t3$species_group == "spruce" & !meas & t3$plot_id != "P9"])
src_p9 <- unique(i3$height_source[t3$plot_id == "P9"])
ok3 <- identical(i3$height_m[meas], t3$height_m[meas]) &&
  all(i3$height_source[meas] == "measured") && !anyNA(i3$height_source) && !anyNA(i3$height_m) &&
  identical(src_sp_missing, "fieldPlot (plot)") && identical(src_p9, "fieldPlot (species curve)") &&
  all(i3$height_source[t3$species_group == "pine"] == "FinnHoyde (uncalibrated)") &&
  all(abs(i3$height_m[t3$species_group == "pine"] -
            finnhoyde_height(t3$dbh_cm[t3$species_group == "pine"], "pine")) < TOL)
rec("Check 03: >= 10 heights -> fieldPlot (plot / species curve for a plot without heights); none -> uncalibrated FinnHoyde; measured heights unchanged; every tree has a source",
    if (ok3) PASS else FAIL,
    sprintf("spruce: %d measured, model %s; missing in measured plots: %s; plot P9: %s | pine: %s",
            sum(meas), i3$summary$model[i3$summary$species == "spruce"],
            paste(src_sp_missing, collapse = "/"), paste(src_p9, collapse = "/"),
            paste(unique(i3$height_source[t3$species_group == "pine"]), collapse = "/")))

t3b <- data.frame(species_group = "spruce", dbh_cm = c(20, 25, 30, 22),
                  height_m = c(16, 18, 19.5, NA), plot_id = "A")
i3b <- impute_heights(t3b)
ratio_hand <- 53.5 / (12.969480 + 14.991026 + 16.642749)
ok3b <- abs(ratio_hand - 1.199464) < 1e-6 &&
  abs(i3b$summary$ratio - 1.199464) < 1e-6 &&
  abs(i3b$height_m[4] - 16.592107) < 1e-6 &&
  identical(i3b$height_source[4], "FinnHoyde (calibrated, n = 3)") &&
  identical(i3b$summary$method, "FinnHoyde (calibrated, n = 3)") &&
  identical(i3b$height_m[1:3], c(16, 18, 19.5))
rec("Check 03b: Calibration by hand: ratio 53.5 / 44.603255 = 1.199464; a 22 cm spruce gets 16.592107 m, 'FinnHoyde (calibrated, n = 3)'",
    if (ok3b) PASS else FAIL,
    sprintf("ratio %.6f (summary %.6f), height %.6f, source '%s'", ratio_hand,
            i3b$summary$ratio, i3b$height_m[4], i3b$height_source[4]))

hi <- t3b; hi$height_m[1:3] <- hi$height_m[1:3] * 1.5        # ratio ~1.8
pine0 <- data.frame(species_group = "pine", dbh_cm = c(20, 30), height_m = NA, plot_id = "A")
w_hi  <- impute_heights(rbind(hi, pine0))$warnings
w_in  <- impute_heights(t3b)$warnings
ok3c <- any(grepl("Spruce: measured heights are on average 80 % above", w_hi, fixed = TRUE)) &&
  length(w_in) == 0L &&
  sum(grepl("independent of site", w_hi, fixed = TRUE)) == 1L &&
  any(grepl("Heights for 2 pine trees", w_hi, fixed = TRUE))
rec("Check 03c: Ratio 1.8 warns, ratio 1.2 does not; site-independence warning only for the species without heights",
    if (ok3c) PASS else FAIL,
    paste(c(w_hi, sprintf("(inside: %d warnings)", length(w_in))), collapse = " | "))


# ==============================================================================
section("04. Time limit on the nlme fit")
t4 <- mk("spruce", 800, 25, 0.8, 41)              # 20,000 trees: nlme takes ~0.5 s
t0 <- proc.time()[["elapsed"]]
i4full <- impute_heights(t4)                       # unrestricted: nlme to completion
el4full <- proc.time()[["elapsed"]] - t0
t0 <- proc.time()[["elapsed"]]
i4 <- impute_heights(t4, timeout_s = 0.2)
el4 <- proc.time()[["elapsed"]] - t0
rec("Check 04: A 0.2 s limit interrupts nlme (sooner than the full fit) and falls back to nls; no NA heights",
    if (identical(i4full$summary$model, "nlme") && identical(i4$summary$model, "nls") &&
        grepl("time limit", i4$summary$note) && el4 < el4full &&
        !anyNA(i4$height_m)) PASS else FAIL,
    sprintf("full fit: %s in %.2f s | limited: %s in %.2f s, note '%s'",
            i4full$summary$model, el4full, i4$summary$model, el4, i4$summary$note))


# ==============================================================================
section("05. Validation of broken tree lists")
good <- data.frame(tree_id = c("1", "2", "3"), species = c("spruce", "pine", "spruce"),
                   dbh = c("25", "30", "18.5"), height = c("19.5", NA, "15"),
                   plot_id = c("A", "A", "B"), stringsAsFactors = FALSE)
mod <- function(f) { d <- good; f(d) }
err_cases <- list(
  "missing column"     = list(d = good[, -3], pat = "Missing required column(s): dbh"),
  "empty"              = list(d = good[0, ], pat = "The tree list is empty"),
  "non-numeric dbh"    = list(d = mod(function(d) { d$dbh[2] <- "30,5"; d }), pat = "Non-numeric dbh"),
  "non-numeric height" = list(d = mod(function(d) { d$height[1] <- "tall"; d }), pat = "Non-numeric height"),
  "missing tree_id"    = list(d = mod(function(d) { d$tree_id[2] <- NA; d }), pat = "Missing tree_id"),
  "duplicate in plot"  = list(d = mod(function(d) { d$tree_id[2] <- "1"; d }), pat = "Duplicated tree_id within plot_id"),
  "duplicate, no plot" = list(d = mod(function(d) { d$plot_id <- NULL; d$tree_id[3] <- "1"; d }),
                              pat = "Duplicated tree_id: 1"),
  "unknown species"    = list(d = mod(function(d) { d$species[2] <- "birch"; d }), pat = "Unknown species in 1 row(s): birch"),
  "dbh <= 0"           = list(d = mod(function(d) { d$dbh[1] <- "0"; d }), pat = "dbh must be > 0"),
  "height <= 0"        = list(d = mod(function(d) { d$height[3] <- "-2"; d }), pat = "height must be > 0"),
  "missing dbh"        = list(d = mod(function(d) { d$dbh[3] <- NA; d }), pat = "Missing dbh"),
  "all out of range"   = list(d = mod(function(d) { d$dbh <- c("3", "4", "4.5"); d }),
                              pat = "No tree is within taperNOR's range"))
for (nm in names(err_cases)) {
  p <- prepare_tree_list(err_cases[[nm]]$d)
  rec(sprintf("Check 05: %s -> rejected", nm),
      if (!isTRUE(p$ok) && any(grepl(err_cases[[nm]]$pat, p$errors, fixed = TRUE))) PASS else FAIL,
      paste(p$errors, collapse = " / "))
}
f5 <- tempfile(fileext = ".txt"); writeLines("x", f5)
r5 <- read_tree_list(f5)
rec("Check 05: unsupported file type -> rejected",
    if (is.null(r5$data) && grepl("Unsupported file type", r5$errors)) PASS else FAIL, r5$errors)

warn_cases <- list(
  "dbh looks like mm"    = list(d = mod(function(d) { d$dbh <- c("250", "300", "185"); d$height <- NA; d }),
                                pat = "looks like mm"),
  "height looks like dm" = list(d = mod(function(d) { d$height <- c("195", "210", "150"); d }),
                                pat = "looks like dm"),
  "implausible h/d"      = list(d = mod(function(d) { d$height[1] <- "6"; d }),
                                pat = "implausible height/diameter ratio"),
  "out of range"         = list(d = mod(function(d) { d$dbh[2] <- "4"; d }),
                                pat = "outside taperNOR's range are excluded: 1 with dbh below 5.0 cm"),
  "beyond 80 cm"         = list(d = mod(function(d) { d$dbh[2] <- "85"; d }), pat = "beyond 80 cm dbh"),
  "ignored column"       = list(d = mod(function(d) { d$age <- "60"; d }), pat = "Ignored column(s): age"))
for (nm in names(warn_cases)) {
  p <- prepare_tree_list(warn_cases[[nm]]$d)
  rec(sprintf("Check 05: %s -> accepted with a warning", nm),
      if (isTRUE(p$ok) && any(grepl(warn_cases[[nm]]$pat, p$warnings, fixed = TRUE))) PASS else FAIL,
      paste(c(p$errors, p$warnings), collapse = " / "))
}


# ==============================================================================
section("06. NFI trees as a tree list")
s6 <- list(species_group = "both", hogstkl = c(41L, 42L), abonitet = c(14L, 17L),
           n_plots = 4L, seed = 5L)
r_nfi <- run_sampled_plot_bucking(s6, "default_2021", root_dir)
ts <- r_nfi$tree_summary
up6 <- data.frame(tree_id = ts$treid, species = ts$species_group, dbh = as.character(ts$dbh_cm),
                  height = as.character(ts$height_m), plot_id = ts$plot_id_github,
                  stringsAsFactors = FALSE)
p6 <- prepare_tree_list(up6)
r_tl <- run_tree_list_bucking(p6, root = root_dir)
key_n <- function(l) paste(l$plot_id_github, l$treid, l$StartPos, l$StopPos, l$ProductKey)
key_t <- function(l) paste(l$plot_id_github, sub("^[^/]*/", "", l$treid), l$StartPos, l$StopPos, l$ProductKey)
a <- r_nfi$selected_logs_all; b <- r_tl$selected_logs_all
m6 <- match(key_n(a), key_t(b))
ok6 <- isTRUE(p6$ok) && all(p6$trees$height_source == "measured") &&
  nrow(a) == nrow(b) && !anyNA(m6) &&
  isTRUE(all.equal(a$value_nok, b$value_nok[m6], tolerance = 1e-12)) &&
  isTRUE(all.equal(a$commercial_volume_m3, b$commercial_volume_m3[m6], tolerance = 1e-12)) &&
  isTRUE(all.equal(r_nfi$per_m3_summary$nok_per_m3, r_tl$per_m3_summary$nok_per_m3, tolerance = 1e-12))
rec("Check 06: NFI trees uploaded as a tree list (with their heights) give the same logs, values and NOK/m3",
    if (ok6) PASS else FAIL,
    sprintf("%d trees | logs %d vs %d, unmatched %d | NOK/m3 %.4f vs %.4f", nrow(ts), nrow(a), nrow(b),
            sum(is.na(m6)), r_nfi$per_m3_summary$nok_per_m3[1], r_tl$per_m3_summary$nok_per_m3[1]))


# ==============================================================================
section("07. End to end, mixed list, with a scenario comparison")
mix <- rbind(as_upload(rbind(mk("spruce", 2, 12, 0.5, 71), mk("pine", 2, 4, 0, 72))),
             data.frame(tree_id = "99", species = "spruce", dbh = "4.2", height = NA,
                        plot_id = "P1", stringsAsFactors = FALSE))
mix$tree_id <- make.unique(paste0(mix$plot_id, "-", mix$tree_id))
p7 <- prepare_tree_list(mix)
r7a <- run_tree_list_bucking(p7, root = root_dir)
pm0 <- pm_minimal(load_price_matrix_long(root_dir))
pm7 <- pm0; saw <- grepl("sawlog", pm7$ProductKey); pm7$PriceNOKperM3[saw] <- pm7$PriceNOKperM3[saw] * 1.3
f7 <- tempfile(fileext = ".csv"); pm_export(pm7, f7)
r7b <- run_tree_list_bucking(p7, price_matrix_path = f7, root = root_dir)
cmp7 <- compare_cutting_patterns(r7a, r7b, pm0, pm7)
ok7 <- isTRUE(p7$ok) && nrow(r7a$excluded_trees) == 1L &&
  grepl("dbh below 5.0 cm", r7a$excluded_trees$excluded_reason) &&
  !anyNA(r7a$tree_summary$height_source) &&
  all(c("measured", "fieldPlot (plot)", "FinnHoyde (uncalibrated)") %in% r7a$tree_summary$height_source) &&
  nrow(cmp7) == nrow(r7a$tree_summary) && identical(r7a$tree_source, "tree_list") &&
  r7b$per_m3_summary$nok_per_m3[1] > r7a$per_m3_summary$nok_per_m3[1]
rec("Check 07: Mixed list: run at default and scenario prices, comparison, height source per tree, excluded tree listed",
    if (ok7) PASS else FAIL,
    sprintf("%d trees bucked, %d excluded (%s) | sources: %s | changed trees %d | NOK/m3 %.1f -> %.1f",
            r7a$n_trees_total, nrow(r7a$excluded_trees), paste(r7a$excluded_trees$excluded_reason, collapse = ","),
            paste(names(table(r7a$tree_summary$height_source)), table(r7a$tree_summary$height_source), collapse = ", "),
            sum(cmp7$changed), r7a$per_m3_summary$nok_per_m3[1], r7b$per_m3_summary$nok_per_m3[1]))


# ==============================================================================
section("08. Template")
f8 <- tempfile(fileext = ".csv"); tree_list_template(f8)
p8 <- prepare_tree_list(read_tree_list(f8)$data)
ok8 <- isTRUE(p8$ok) && length(p8$warnings) == 1L &&
  grepl("Heights for 3 pine trees", p8$warnings) &&
  any(grepl("FinnHoyde (calibrated, n = 4)", p8$trees$height_source, fixed = TRUE))
rec("Check 08: Template validates; only warning: uncalibrated pine heights; spruce calibrated (n = 4)",
    if (ok8) PASS else FAIL, paste(c(p8$errors, p8$warnings), collapse = " / "))


# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 21 complete.\n")
cat("Results written to:", out_dir, "\n")
