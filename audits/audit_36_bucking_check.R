# ==============================================================================
# audits/audit_36_bucking_check.R
#
# Purpose : The bucking check (tab 5, roadmap 8): the recorded logs of an .hpr file against the
#           best possible bucking (R/helpers_hpr_logs.R, R/helpers_check.R). Runs on the invented
#           fixture inputs/test_fixtures/hpr_check_synthetic.hpr (scripts/make_hpr_check_fixture.R:
#           8 stems with recorded logs; stems 1 and 6 are recorded as the best bucking, 2 is worse,
#           3 has no recorded StartPos, 4 has a log in a "manual only" cell and a log without a
#           price, 5 ends thick, 7 is multi-tree, 8 is pine). The owner's real files are not used.
#
# Checks  :
#  1.  Reader: 15 logs of 8 stems; the unclassified product is waste; manual = not Automatic or
#      Spinning; a missing StartPos is the sum of the previous lengths; the multi-tree stem is read
#      and left out of the check
#  2.  The unclassified product is found by its definition, not its number (2147483647 works)
#  3.  A recorded bucking equal to the best one has a yield index of exactly 1 (stems 1, 5, 6), and
#      the recorded log is valued exactly like the same log as an optimiser candidate (1e-12)
#  4.  A worse recorded bucking (stem 2): value = price volume (hand-summed 10 cm cylinders) x the
#      file's own price cell
#  5.  A recorded log in a "manual only" cell is valued with the file's price (300), the optimiser
#      never chooses such a cell; a log without a price is valued 0, counted, shown per product
#  6.  Waste zone: the recorded waste log is a defect zone [ceil(S/10)*10, +floor(L/10)*10); with the
#      rule the best bucking of stem 6 is the recorded one, without it the best is higher
#  7.  A possible broken top is flagged and kept (stem 5), never excluded; the key-figure row
#      "without_broken" is the yield index of the other stems
#  8.  Manual-cut shares (7 of 14 logs; 1 of 4 sawlogs) and the yield index with and without manual cuts
#  9.  Best possible bucking restricted to the products the harvester used: never higher than with all
#      products, and equal to the all-products run when every product is used
# 10.  A file without stem profiles: no best bucking, a message, the recorded tables only
# 11.  The three stages (prepare, buck, finish: the app's background job) give the same result as one call
# 12.  Results do not depend on the number of workers
# 13.  Privacy: the fixture holds no coordinate or contact element
# 14.  NFI stem profiles on demand (step 6): no cache in an empty folder; building three plots in chunks
#      gives exactly the profiles of the existing cache
#
# Verdict : PASS / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/check/audit_36_bucking_check.csv
#   outputs/check/audit_36_bucking_check.txt
#
# Usage   : Rscript audits/audit_36_bucking_check.R
# ==============================================================================

root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  env <- Sys.getenv("APTER_ROOT", unset = "")
  if (nzchar(env)) {
    d <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (!is_root(d)) stop("Project root '", d, "' (from APTER_ROOT) does not contain R/helpers_paths.R.", call. = FALSE)
  } else {
    d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    while (!is_root(d) && dirname(d) != d) d <- dirname(d)
    if (!is_root(d)) stop("Cannot locate the project root: set APTER_ROOT, or run from inside the project directory.", call. = FALSE)
  }
  d
})
Sys.setenv(APTER_ROOT = root_dir)

suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)
source(file.path(root_dir, "audits", "helpers_audit.R"))


out_dir <- path_output("check", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_36_bucking_check.txt")
audit_csv <- file.path(out_dir, "audit_36_bucking_check.csv")
init_audit_log(audit_txt)

E <- new.env(parent = emptyenv())
rec <- function(check, result, detail = "") record_check(E, check, result, detail, log_path = audit_txt)
PASS <- "PASS"; FAIL <- "FAIL"
ok <- function(x) if (isTRUE(x)) PASS else FAIL
section <- function(title) {
  cat(sprintf("\n--- %s ---\n", title), file = audit_txt, append = TRUE)
  cat(sprintf("\n--- %s ---\n", title))
}
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)
cat("  Audit: bucking check (roadmap 8, tab 5)\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

FIXTURE <- file.path(root_dir, "inst", "extdata", "inputs", "test_fixtures", "hpr_check_synthetic.hpr")
options(apter.workers = 2L)
x <- bucking_check(FIXTURE)
st <- x$stems; rec_l <- x$logs$recorded; opt_l <- x$logs$optimal
s_of <- function(k) st[st$StemKey == as.character(k), , drop = FALSE]

# ---- 1 ---------------------------------------------------------------------------------------------
section("01. Reader")
h <- read_hpr(FIXTURE, "x"); rl <- read_hpr_logs(FIXTURE, "x", defs = h)$logs
n_manual <- sum(rl$manual); cum3 <- rl[rl$stem_key == "3", ]
ok1 <- nrow(rl) == 16L && length(unique(rl$stem_key)) == 9L && sum(rl$waste) == 5L && n_manual == 8L &&
  all(!rl$manual[rl$CuttingReason %in% c("Automatic", "Spinning")]) &&
  identical(cum3$StartPos, c(0, 490)) && !any(cum3$start_recorded) && all(rl$start_recorded[rl$stem_key == "1"]) &&
  sum(rl$kind == "multi") == 1L && !"7" %in% st$StemKey && all(c("7", "9") %in% x$excluded$treid)
rec("Check 01: 16 recorded logs of 9 stems; 5 waste logs (the unclassified product); manual = not Automatic/Spinning (8); StartPos = cumulative lengths when not recorded; the multi-tree stem is read and left out",
    ok(ok1), sprintf("logs %d, stems %d, waste %d, manual %d, stem 3 starts %s, excluded %s", nrow(rl), length(unique(rl$stem_key)),
                     sum(rl$waste), n_manual, paste(cum3$StartPos, collapse = "/"), paste(x$excluded$treid, collapse = ",")))

# ---- 2 ---------------------------------------------------------------------------------------------
section("02. Unclassified product by definition")
tmp2 <- tempfile(fileext = ".hpr")
txt <- paste(readLines(FIXTURE, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
writeLines(gsub("999999", "2147483647", txt, fixed = TRUE), tmp2, useBytes = TRUE)
x2 <- bucking_check(tmp2)
ok2 <- isTRUE(x2$ok) && sum(x2$logs$recorded$waste) == 5L &&
  isTRUE(all.equal(x2$key_figures$value_rec, x$key_figures$value_rec)) && isTRUE(all.equal(x2$key_figures$value_opt, x$key_figures$value_opt))
rec("Check 02: with the unclassified product numbered 2147483647 the waste logs and every figure are the same",
    ok(ok2), sprintf("waste logs %d; recorded %.4f vs %.4f", sum(x2$logs$recorded$waste), x2$key_figures$value_rec[1L], x$key_figures$value_rec[1L]))
unlink(tmp2)

# ---- 3 ---------------------------------------------------------------------------------------------
section("03. Recorded = best possible")
y <- c(s_of(1)$yield, s_of(5)$yield, s_of(6)$yield)
l1 <- rec_l[rec_l$stem_key == "1" & !rec_l$waste, ]; o1 <- opt_l[as.character(opt_l$treid) == "1", ]
same_cand <- nrow(l1) == 1L && nrow(o1) == 1L && o1$StartPos == l1$StartPos && o1$StopPos == l1$StopPos &&
  abs(o1$value_nok - l1$value_nok) < 1e-12 && abs(o1$price_volume_m3 - l1$price_volume_m3) < 1e-12 &&
  o1$PriceDiameterClass_cm_used == l1$PriceDiameterClass_cm && as.character(o1$ProductKey) == l1$akey
rec("Check 03: stems 1, 5 and 6 are recorded as the best bucking: yield index exactly 1; the recorded log of stem 1 has the same price volume, price class, price and value as the optimiser's candidate (1e-12)",
    ok(all(abs(y - 1) < 1e-12) && same_cand), sprintf("yield %s | recorded value %.10f, optimiser %.10f", paste(format(y, digits = 12), collapse = " "), l1$value_nok, o1$value_nok))

# ---- 4 ---------------------------------------------------------------------------------------------
section("04. Hand value of a worse recorded bucking")
pv2 <- x$job$pvs[["2"]]; pos2 <- as.numeric(pv2$diameterPosition); dub2 <- as.numeric(pv2$DiameterValueUB)
sec_top <- seq(10, 520, by = 10)
vol2 <- sum(pi / 4 * (approx(pos2, dub2, xout = sec_top)$y / 1000)^2 * 0.1)
cells <- h$products[["8015"]]$cells; top2 <- approx(pos2, dub2, xout = 520)$y
dcls <- max(cells$d_mm[cells$d_mm <= floor(top2 / 10) * 10]); price2 <- cells$price[cells$d_mm == dcls & cells$l_cm == 520]
r2 <- rec_l[rec_l$stem_key == "2" & !rec_l$waste, ]
rec("Check 04: stem 2 (pulpwood 520 instead of a sawlog): the recorded value = hand-summed solid volume x the file's own price cell; yield index = that value / the best value",
    ok(abs(r2$value_nok - vol2 * price2) < 1e-9 && abs(s_of(2)$yield - r2$value_nok / s_of(2)$value_opt) < 1e-12 && s_of(2)$yield < 1),
    sprintf("hand %.8f (volume %.8f x %g) vs %.8f; yield %.6f", vol2 * price2, vol2, price2, r2$value_nok, s_of(2)$yield))

# ---- 5 ---------------------------------------------------------------------------------------------
section("05. Manual-only cell and unpriced log")
r4 <- rec_l[rec_l$stem_key == "4", ]; o4 <- opt_l[as.character(opt_l$treid) == "4", ]
ct <- x$control; ub <- ct$unpriced_by_product
ok5 <- r4$PriceNOKperM3[1L] == 300 && r4$value_nok[1L] > 0 && !r4$outside[1L] &&
  is.na(r4$PriceNOKperM3[2L]) && r4$value_nok[2L] == 0 && r4$outside[2L] &&
  !any(o4$LogLength == 370 & o4$ProductKey == "spruce_140_sca_sagt_mmer") &&
  ct$n_unpriced == 1L && nrow(ub) == 1L && ub$key == "spruce_197_massevirke" && ub$logs == 1L && ub$share_pct > 0
rec("Check 05: the log in a 'manual only' cell is valued with the file's price (300 NOK/m3) while the best bucking never cuts that cell; the 130 cm log has no price: 0, flagged outside, counted and listed per product with its share of the product's recorded volume",
    ok(ok5), sprintf("prices %s | values %s | unpriced %d | best lengths %s | share %.2f %%", paste(r4$PriceNOKperM3, collapse = "/"),
                     paste(round(r4$value_nok, 4), collapse = "/"), ct$n_unpriced, paste(o4$LogLength, collapse = "/"), ub$share_pct[1L]))

# ---- 6 ---------------------------------------------------------------------------------------------
section("06. Waste zone")
cells6 <- hpr_reject_cells(seq(0, 890, by = 10), 430, 100)
exp6 <- seq(0, 890, by = 10) >= 430 & seq(0, 890, by = 10) < 530
x6 <- bucking_check(FIXTURE, reject_zones = FALSE)
ok6 <- identical(cells6, exp6) && abs(s_of(6)$yield - 1) < 1e-12 &&
  x6$stems$value_opt[x6$stems$StemKey == "6"] > s_of(6)$value_opt + 1e-9 && x6$stems$yield[x6$stems$StemKey == "6"] < 1
rec("Check 06: the recorded waste log [430, 530) is a defect zone (cells 430..520); with the rule the best bucking of stem 6 is the recorded one (yield 1), without it the best is strictly higher",
    ok(ok6), sprintf("zone cells %s | best with %.4f, without %.4f", paste(which(cells6) * 10 - 10, collapse = ","), s_of(6)$value_opt, x6$stems$value_opt[x6$stems$StemKey == "6"]))

# ---- 7 ---------------------------------------------------------------------------------------------
section("07. Possible broken top")
s5 <- s_of(5); kf <- x$key_figures; kb <- kf[kf$scope == "without_broken", ]
u <- st$use & st$value_opt > 0 & !st$possible_broken_top
ok7 <- s5$possible_broken_top && s5$use && is.na(s5$reason) && !isTRUE(x$broken$excluded) && "5" %in% x$flagged$broken_top &&
  nrow(kb) == 1L && abs(kb$yield - sum(st$value_rec[u]) / sum(st$value_opt[u])) < 1e-12 && kb$stems == sum(u)
rec("Check 07: stem 5 (profile ends at 340 mm) is flagged as a possible broken top and kept, never excluded; the key-figure row 'without_broken' is the yield index of the other stems",
    ok(ok7), sprintf("flagged %s | kept %s | without_broken %.6f over %d stems", paste(x$flagged$broken_top, collapse = ","), s5$use, kb$yield, kb$stems))

# ---- 8 ---------------------------------------------------------------------------------------------
section("08. Manual cuts")
mn <- x$manual
ok8 <- mn$n_all == 14L && abs(mn$all - 0.5) < 1e-12 && mn$n_sawlogs == 4L && abs(mn$sawlogs - 0.25) < 1e-12 &&
  abs(mn$butt_sawlogs - 0.25) < 1e-12 &&
  abs(mn$yield_with - sum(st$value_rec[st$use & st$has_manual]) / sum(st$value_opt[st$use & st$has_manual])) < 1e-12 &&
  abs(mn$yield_without - sum(st$value_rec[st$use & !st$has_manual]) / sum(st$value_opt[st$use & !st$has_manual])) < 1e-12
rec("Check 08: manual cuts 7 of 14 logs (the multi-tree log is not counted), 1 of 4 sawlogs, 1 of 4 butt sawlogs; the yield index with and without manual cuts follows the stems",
    ok(ok8), sprintf("all %.3f sawlogs %.3f butt %.3f | with %.4f without %.4f", mn$all, mn$sawlogs, mn$butt_sawlogs, mn$yield_with, mn$yield_without))

# ---- 9 ---------------------------------------------------------------------------------------------
section("09. Only the products the harvester used")
xu <- x                                            # the default: only the products the harvester used (no box any more)
xall <- bucking_check(FIXTURE, used_only = FALSE)
used_keys <- unique(rl$ProductKey[!rl$waste])
ok9 <- isTRUE(xu$ok) && xu$n_products == length(used_keys) && xall$n_products > xu$n_products &&
  all(xu$stems$value_opt <= xall$stems$value_opt[match(xu$stems$StemKey, xall$stems$StemKey)] + 1e-9) &&
  xu$key_figures$value_opt[1L] < xall$key_figures$value_opt[1L] && xu$key_figures$value_rec[1L] == xall$key_figures$value_rec[1L] &&
  !any(grepl("kubb", as.character(xu$logs$optimal$ProductKey)))
rec("Check 09: by default (no box) the best possible bucking uses only the products the harvester cut a log of; against all products of the file its value is lower (stem 3 no longer uses Kubb), the recorded value is the same, no unused product is in the best bucking",
    ok(ok9), sprintf("products %d of %d | best %.4f vs %.4f | yield %.4f vs %.4f", xu$n_products, xall$n_products, xu$key_figures$value_opt[1L], xall$key_figures$value_opt[1L],
                     xu$key_figures$yield[1L], xall$key_figures$yield[1L]))

# ---- 10 --------------------------------------------------------------------------------------------
section("10. File without stem profiles")
tmp10 <- tempfile(fileext = ".hpr")
writeLines(gsub("(?s)<StemDiameters.*?</StemDiameters>", "", txt, perl = TRUE), tmp10, useBytes = TRUE)
x10 <- bucking_check(tmp10)
ok10 <- isTRUE(x10$ok) && isTRUE(x10$no_profile) && is.null(x10$stems) && nrow(x10$assortments$recorded) >= 3L && !is.na(x10$manual$all) && is.null(x10$assortments$optimal)
rec("Check 10: a file with logs but no StemDiameters gives no best bucking (no_profile), only the recorded tables and manual-cut shares",
    ok(ok10), sprintf("no_profile %s | recorded products %d | manual share %.3f", x10$no_profile, nrow(x10$assortments$recorded), x10$manual$all))
unlink(tmp10)

# ---- 11 --------------------------------------------------------------------------------------------
section("11. Three stages")
p <- bucking_check(FIXTURE, stage = "prepare", root = root_dir)
xs <- bucking_check(root = root_dir, stage = "finish", prep = p, b = buck_job(p$job))
same_tab <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-12, check.attributes = FALSE))
ok11 <- inherits(p, "check_prep") && same_tab(x$key_figures, xs$key_figures) && same_tab(x$stems[, c("StemKey", "value_rec", "value_opt")], xs$stems[, c("StemKey", "value_rec", "value_opt")]) &&
  same_tab(x$control$unpriced_by_product, xs$control$unpriced_by_product)
rec("Check 11: prepare, buck_job() and finish (the stages of the app's background job) give the same tables as one call",
    ok(ok11), sprintf("prep class %s | yield %.12f vs %.12f", class(p)[1L], x$key_figures$yield[1L], xs$key_figures$yield[1L]))

# ---- 12 --------------------------------------------------------------------------------------------
section("12. Workers")
options(apter.workers = 1L); x1 <- bucking_check(FIXTURE)
options(apter.workers = 4L); x4 <- bucking_check(FIXTURE)
rec("Check 12: results are identical with 1 and 4 workers",
    ok(identical(x1$key_figures, x4$key_figures) && identical(x1$stems$value_opt, x4$stems$value_opt) && identical(x1$stems$value_opt, x$stems$value_opt)),
    sprintf("yield %.12f / %.12f", x1$key_figures$yield[1L], x4$key_figures$yield[1L]))

# ---- 13 --------------------------------------------------------------------------------------------
section("13. Privacy")
rec("Check 13: the fixture has no coordinate or contact element and no invented-looking name",
    ok(!grepl("Latitude|Longitude|StemCoordinates|ContactInformation|OperatorName|ObjectName|HarvestDate", txt)), "")

# ---- 15 --------------------------------------------------------------------------------------------
section("15. Manual-only cells for the best possible bucking")
blk <- regmatches(txt, regexpr("(?s)<ProductKey>8019</ProductKey>.*?</ProductDefinition>", txt, perl = TRUE))
txt15 <- sub(blk, gsub("<BuckingCriteria>No limit</BuckingCriteria>", "<BuckingCriteria>Only logs bucked manually allowed</BuckingCriteria>", blk, fixed = TRUE), txt, fixed = TRUE)
tmp15 <- tempfile(fileext = ".hpr"); writeLines(txt15, tmp15, useBytes = TRUE)
x15y <- bucking_check(tmp15); x15n <- bucking_check(tmp15, manual_ok = FALSE)
v15 <- function(z, k) z$stems$value_opt[z$stems$StemKey == k]
ok15 <- isTRUE(x15y$ok) && abs(v15(x15y, "1") - 60.827) < 1e-9 && isTRUE(x15n$ok) && v15(x15n, "1") < 30 &&
  all(x$stems$value_opt >= -1e-9) && isTRUE(all.equal(x15y$logs$recorded$value_nok, x$logs$recorded$value_nok, tolerance = 1e-12))
rec("Check 15: when every sawlog price cell is 'Only logs bucked manually allowed' the best possible bucking in the analysis may still use them with the file's price (stem 1: 60.827 as with free cells), the simulator's rule (manual_ok = FALSE) cannot (best value falls below 30); the recorded valuation is the same",
    ok(ok15), sprintf("stem 1 best: manual cells allowed %.4f, not allowed %.4f", v15(x15y, "1"), v15(x15n, "1")))
unlink(tmp15)

# ---- 16 --------------------------------------------------------------------------------------------
section("16. Incomplete stems")
s9 <- x$excluded[x$excluded$treid == "9", ]
ok16 <- nrow(s9) == 1L && grepl("cover less than half", s9$reason) && x$n_incomplete == 1L && x$n_left_out == 1L && !"9" %in% st$StemKey[st$use] &&
  identical(tr_msg(s9$reason, "no"), "registrerte stokker dekker mindre enn halvparten av den målte stammen")
rec("Check 16: stem 9 (one 100 cm waste log on 800 cm of profile) is left out as incomplete (n_incomplete 1, shown as 'left out'); the multi-tree stem is not counted as left out because of deviating data; the reason translates",
    ok(ok16), sprintf("reason '%s' | incomplete %d | left out %d", paste(s9$reason, collapse = ""), x$n_incomplete, x$n_left_out))

# ---- 17 --------------------------------------------------------------------------------------------
section("17. The PDF report")
if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("pdftools", quietly = TRUE)) {
  rec("Check 17: The PDF report is created in Norsk and English and contains the file name, the title and the translated sections", "REVIEW", "gridExtra or pdftools not installed")
} else {
  ROOT <- root_dir; APP_DIR <- file.path(root_dir, "inst", "app"); suppressPackageStartupMessages(source(file.path(root_dir, "inst", "app", "theme.R"))); source(file.path(root_dir, "inst", "app", "check_report.R"))
  pdf_no <- tempfile(fileext = ".pdf"); pdf_en <- tempfile(fileext = ".pdf")
  r17 <- x; r17$file_name <- "hpr_check_synthetic.hpr"
  chk_report_pdf(r17, "no", pdf_no, r17$file_name, as.Date("2026-11-05")); chk_report_pdf(r17, "en", pdf_en, r17$file_name, as.Date("2026-11-05"))
  t_no <- paste(pdftools::pdf_text(pdf_no), collapse = "
"); t_en <- paste(pdftools::pdf_text(pdf_en), collapse = "
")
  ok17 <- file.size(pdf_no) > 5000 && file.size(pdf_en) > 5000 && grepl("hpr_check_synthetic.hpr", t_no, fixed = TRUE) && grepl("hpr_check_synthetic.hpr", t_en, fixed = TRUE) &&
    grepl("Apteringsanalyse", t_no, fixed = TRUE) && grepl("Bucking analysis", t_en, fixed = TRUE) && grepl("05.11.2026", t_no, fixed = TRUE) &&
    grepl("Kvalitetskoder per sortiment", t_no, fixed = TRUE) && grepl("Quality codes per product", t_en, fixed = TRUE) &&
    grepl("Apteringsgrad", t_no, fixed = TRUE) && grepl("utelatt på grunn av ufullstendige", t_no, fixed = TRUE) &&
    grepl("left out because of incomplete", t_en, fixed = TRUE) && length(pdftools::pdf_text(pdf_no)) >= 1L
  rec("Check 17: The PDF report (A4, cairo) is created in Norsk and English and contains the file name, the title, the date, the translated sections (æøå intact) and the 'left out' line",
      ok(ok17), sprintf("pages %d/%d, bytes %d/%d", length(pdftools::pdf_text(pdf_no)), length(pdftools::pdf_text(pdf_en)), file.size(pdf_no), file.size(pdf_en)))
  unlink(c(pdf_no, pdf_en))
}

# ---- 14 --------------------------------------------------------------------------------------------
section("14. NFI stem profiles on demand")
td_all <- nfi_cache_tree_data(root_dir); pl3 <- sort(unique(td_all$plot_id))[1:3]
tmp_cache <- tempfile("nfi_cache_"); dir.create(tmp_cache)
not_ready <- !nfi_profiles_ready(tmp_cache)
prog_log <- integer(0)
build_nfi_cache(root_dir, out_dir = tmp_cache, plots = pl3, progress = function(d, t) prog_log <<- c(prog_log, d), chunk = 100L)
got <- as.data.frame(dplyr::collect(arrow::open_dataset(tmp_cache)))
ref <- as.data.frame(dplyr::collect(dplyr::filter(arrow::open_dataset(nfi_cache_dir(root_dir)), plot_id_github %in% pl3)))
srt <- function(d) { d <- d[order(d$plot_id_github, d$treid, d$diameterPosition), ]; rownames(d) <- NULL; d }
same <- isTRUE(all.equal(srt(got[, names(ref)]), srt(ref), tolerance = 0, check.attributes = FALSE))
rec("Check 14: no cache in an empty folder; building three plots in chunks (progress called per chunk, ending at the total) gives exactly the profiles of the existing cache (every diameter equal)",
    ok(not_ready && same && length(prog_log) >= 2L && tail(prog_log, 1L) == nrow(nfi_cache_tree_data(root_dir, pl3)) && nrow(got) == nrow(ref) && nrow(ref) > 0L),
    sprintf("rows %d vs %d | progress %s", nrow(got), nrow(ref), paste(prog_log, collapse = "/")))
unlink(tmp_cache, recursive = TRUE)

write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 36 complete.\n")
