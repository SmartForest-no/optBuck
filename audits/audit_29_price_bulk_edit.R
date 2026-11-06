# ==============================================================================
# audits/audit_29_price_bulk_edit.R
#
# Purpose : Phase 4: bulk editing of price matrices (R/helpers_price_matrix.R:
#           pm_bulk_adjust(), pm_bulk_fill(); the controls under each
#           scenario's grid in app/server_scenarios.R). Prices of a selected
#           range change by +/- NOK/m3 or +/- %, or a matrix is filled from a
#           price per diameter class plus a correction per length class.
#           Results are rounded to 2 decimals; forbidden (empty) cells stay
#           empty.
#
# Checks  :
#  1.  Range + amount: cells in the range are old + amount, the rest and the
#      empty cells unchanged
#  2.  Range %: cells in the range are old * (1 + p/100) rounded to 2
#      decimals, the rest and the empty cells unchanged
#  3.  Row price + length correction: every priced cell is row price +
#      correction of its length; empty cells and an empty row stay empty
#  4.  Forbidden cells stay empty in all three, also after the long-format
#      round trip, and the result passes check_price_set(); prices never
#      go below 0
#  5.  App (shinytest2): in Scenario A select a range, +10 NOK/m3 changes
#      exactly those cells in A (not outside the range) and leaves B
#      unchanged; the fill dialog applies a row price and a length
#      correction to one cell; the "Change prices" field and mode keep their
#      values after Apply and after a row/column button (screenshots:
#      outputs/app_smoke/review/audit29/ (gitignored, cleared by each run))
#
# Verdict : PASS / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/assortments/audit_29_price_bulk_edit.csv
#   outputs/assortments/audit_29_price_bulk_edit.txt
#
# Usage   : Rscript audits/audit_29_price_bulk_edit.R
# ==============================================================================

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


out_dir <- path_output("assortments", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_29_price_bulk_edit.txt")
audit_csv <- file.path(out_dir, "audit_29_price_bulk_edit.csv")
init_audit_log(audit_txt)

E <- new.env(parent = emptyenv())
rec <- function(check, result, detail = "")
  record_check(E, check, result, detail, log_path = audit_txt)
PASS <- "PASS"; FAIL <- "FAIL"
ok <- function(x) if (isTRUE(x)) PASS else FAIL

section <- function(title) {
  cat(sprintf("\n--- %s ---\n", title), file = audit_txt, append = TRUE)
  cat(sprintf("\n--- %s ---\n", title))
}

cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)
cat("  Audit: bulk editing of price matrices\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

# ---- the matrix: default spruce sawlog with a forbidden cell and an empty row -------
pm0 <- pm_minimal(load_price_matrix_long(root_dir))
ID  <- "spruce_sawlog"
g0  <- pm_to_grid(pm0, ID)
stopifnot(nrow(g0) >= 8, ncol(g0) >= 4)
g0[2, 1] <- NA                   # a forbidden cell
g0[5, ]  <- NA                   # a forbidden class (empty row)
pmx <- pm_from_grid(g0, ID, pm0)
g   <- pm_to_grid(pmx, ID)       # the starting grid, as the editor shows it
M   <- as.matrix(g)
rows <- 2:4; cols <- 1:2         # the range (contains the forbidden cell 2,1)
in_rng <- matrix(FALSE, nrow(M), ncol(M)); in_rng[rows, cols] <- TRUE
same_na <- function(a, b) identical(is.na(a), is.na(b))
GRIDS <- list()

# ==============================================================================
section("01. Range + amount")
r1 <- as.matrix(pm_to_grid(pm_bulk_adjust(pmx, ID, rows, cols, "amount", 12.5), ID))
exp1 <- M; exp1[in_rng & !is.na(M)] <- round(M[in_rng & !is.na(M)] + 12.5, 2)
GRIDS$amount <- r1
rec("Check 01: Range + amount: cells in the range changed, others and empty cells unchanged",
    ok(isTRUE(all.equal(r1, exp1)) && same_na(r1, M) &&
       isTRUE(all.equal(r1[!in_rng], M[!in_rng]))),
    sprintf("%d cells changed by +12.50, %d empty cells kept",
            sum(in_rng & !is.na(M)), sum(is.na(r1))))

# ==============================================================================
section("02. Range %")
r2 <- as.matrix(pm_to_grid(pm_bulk_adjust(pmx, ID, rows, cols, "percent", -7.5), ID))
exp2 <- M; exp2[in_rng & !is.na(M)] <- round(M[in_rng & !is.na(M)] * 0.925, 2)
GRIDS$percent <- r2
rec("Check 02: Range %: old * (1 + p/100) rounded to 2 decimals, others unchanged",
    ok(isTRUE(all.equal(r2, exp2)) && same_na(r2, M) &&
       isTRUE(all.equal(r2[!in_rng], M[!in_rng])) && all(r2 == round(r2, 2), na.rm = TRUE)),
    sprintf("%d cells changed by -7.5 %%", sum(in_rng & !is.na(M))))

# ==============================================================================
section("03. Row price + length correction")
rp <- setNames(seq(300, by = 25, length.out = nrow(M)), rownames(M))
lc <- setNames(seq(0, by = -3.5, length.out = ncol(M)), colnames(M))
r3 <- as.matrix(pm_to_grid(pm_bulk_fill(pmx, ID, rp, lc), ID))
exp3 <- M; exp3[] <- ifelse(is.na(M), NA_real_, round(outer(rp, lc, "+"), 2))
GRIDS$fill <- r3
rec("Check 03: Row price + length correction in every priced cell; empty cells stay empty",
    ok(isTRUE(all.equal(r3, exp3, check.attributes = FALSE)) && same_na(r3, M)),
    sprintf("%d x %d grid, %d priced cells filled", nrow(M), ncol(M), sum(!is.na(M))))

# ==============================================================================
section("04. Forbidden cells stay empty; valid result")
d4 <- defs_from_products(load_products(root_dir))
chk <- vapply(c("amount", "percent", "fill"), function(k) {
  pm <- switch(k,
    amount  = pm_bulk_adjust(pmx, ID, rows, cols, "amount", 12.5),
    percent = pm_bulk_adjust(pmx, ID, rows, cols, "percent", -7.5),
    fill    = pm_bulk_fill(pmx, ID, rp, lc))
  gr <- as.matrix(pm_to_grid(pm, ID))
  isTRUE(same_na(gr, M)) && isTRUE(check_price_set(d4, pm)$ok)
}, logical(1))
neg <- as.matrix(pm_to_grid(pm_bulk_adjust(pmx, ID, seq_len(nrow(M)), seq_len(ncol(M)), "amount", -1e6), ID))
rec("Check 04: Forbidden cells empty after the round trip, scenario valid, prices never negative",
    ok(all(chk) && all(neg == 0, na.rm = TRUE) && same_na(neg, M)),
    sprintf("empty cells kept and scenario valid: %s | minimum after -1e6: %s",
            paste(names(chk), chk, sep = "=", collapse = ", "), min(neg, na.rm = TRUE)))


# ==============================================================================
section("05. App: range edit and fill dialog")
shot_dir <- file.path(root_dir, "outputs", "app_smoke", "review", "audit29")
unlink(shot_dir, recursive = TRUE); ensure_dir(shot_dir)     # cleared by each run, as audit 17 does
browser_ok <- requireNamespace("shinytest2", quietly = TRUE) && requireNamespace("chromote", quietly = TRUE)
if (!browser_ok) {
  rec("Check 05: App: range +10 NOK in A (B unchanged); fill dialog", FAIL,
      "shinytest2 / chromote not available")
} else {
  Sys.setenv(NOT_CRAN = "true", OPTBUCK_N_PLOTS = "5", OPTBUCK_LANG = "en")
  options(chromote.timeout = 30)
  # Own Chrome profile and Chrome process (helpers_audit.R): stopped below, and at
  # R exit if the script halts first.
  audit_browser_start()
  app <- audit_app_driver(file.path(root_dir, "inst", "app"), name = "audit29",
                          load_timeout = 60 * 1000, timeout = 120 * 1000,
                          width = 1400, height = 1100)
  if (!inherits(app, "AppDriver")) {
    rec("Check 05: App: range +10 NOK in A (B unchanged); fill dialog", FAIL, paste("start error:", app))
    audit_browser_stop()
  } else {
    js <- function(code) tryCatch(app$get_js(code), error = function(e) NA)
    scen <- function(side) {
      f <- app$get_download(paste0("dl_scen_", side))
      pm_minimal(scenario_import(f)$pm)
    }
    gridA <- function(pm) pm_to_grid(pm, ID)
    shot <- function(name) try(app$get_screenshot(file.path(shot_dir, paste0(name, ".png"))), silent = TRUE)
    res <- tryCatch({
      app$set_inputs(main_nav = "prices", wait_ = FALSE)   # the app opens on Stems (restructure R1)
      app$wait_for_js("document.fonts.status === 'loaded'", timeout = 30 * 1000)
      app$wait_for_js("!!document.querySelector('#pm_a .ht_master td')", timeout = 30 * 1000)
      app$wait_for_idle()
      a0 <- scen("a"); b0 <- scen("b")
      ga0 <- as.matrix(gridA(a0))
      shot("01_before_bulk_edit")
      # select rows 2-4 x columns 1-2 (0-based in Handsontable) as a drag would
      app$run_js("HTMLWidgets.find('#pm_a').hot.selectCells([[1, 0, 3, 1]]);")
      app$wait_for_idle()
      app$set_inputs(pm_a_bulk_value = 10, wait_ = FALSE)
      app$wait_for_idle()
      app$click("pm_a_bulk_apply"); app$wait_for_idle()
      app$wait_for_js(sprintf("HTMLWidgets.find('#pm_a').hot.getDataAtCell(1, 0) === %s", format(round(ga0[2, 1] + 10, 2), nsmall = 0)), timeout = 30 * 1000)
      app$wait_for_idle(); Sys.sleep(1)
      shot("02_after_bulk_edit")
      a1 <- scen("a"); b1 <- scen("b")
      # the field keeps its value after Apply and after a row button (mode too)
      kept_apply <- identical(as.numeric(app$get_value(input = "pm_a_bulk_value")), 10)
      app$set_inputs(pm_a_bulk_mode = "percent", wait_ = FALSE); app$wait_for_idle()
      app$click("pm_a_cls_add_lo"); app$wait_for_idle(); Sys.sleep(1)
      kept_row <- identical(as.numeric(app$get_value(input = "pm_a_bulk_value")), 10) &&
        identical(app$get_value(input = "pm_a_bulk_mode"), "percent")
      app$click("pm_a_cls_rm_lo"); app$wait_for_idle(); Sys.sleep(1)
      kept_row <- kept_row && identical(as.numeric(app$get_value(input = "pm_a_bulk_value")), 10)
      app$set_inputs(pm_a_bulk_mode = "amount", wait_ = FALSE); app$wait_for_idle()
      ga1 <- as.matrix(gridA(a1))
      inr <- matrix(FALSE, nrow(ga0), ncol(ga0)); inr[2:4, 1:2] <- TRUE
      chg_ok <- isTRUE(all.equal(ga1[inr], round(ga0[inr] + 10, 2))) &&
        isTRUE(all.equal(ga1[!inr], ga0[!inr])) && same_na(ga1, ga0)
      b_ok <- identical(b1, b0)
      # the fill dialog: class 20 price 777, length correction +5 on the second length
      app$click("pm_a_fill_open"); app$wait_for_idle()
      app$wait_for_js("!!document.querySelector('.modal-content')", timeout = 30 * 1000)
      cls <- rownames(ga1)[1]; len <- colnames(ga1)[2]
      shot("03_fill_dialog")
      pa <- list(777); names(pa) <- paste0("pm_a_fill_row_", cls)
      pb <- list(5);   names(pb) <- paste0("pm_a_fill_len_", len)
      do.call(app$set_inputs, c(pa, list(wait_ = FALSE))); do.call(app$set_inputs, c(pb, list(wait_ = FALSE)))
      app$wait_for_idle()
      app$click("pm_a_fill_apply"); app$wait_for_idle()
      a2 <- scen("a"); ga2 <- as.matrix(gridA(a2))
      fill_ok <- isTRUE(all.equal(unname(ga2[1, 2]), 782)) &&
        (is.na(ga2[1, 1]) || isTRUE(all.equal(unname(ga2[1, 1]), 777))) && same_na(ga2, ga0) &&
        identical(scen("b"), b0)
      shot("04_after_fill")
      list(chg = chg_ok, b = b_ok, fill = fill_ok, kept = kept_apply && kept_row,
           n = sum(inr & !is.na(ga0)), diff = paste(which(!is.na(ga1 - ga0) & (ga1 - ga0) != 0), collapse = ","), d = paste(unique(round((ga1 - ga0)[!is.na(ga1 - ga0) & ga1 != ga0], 2)), collapse = ","))
    }, error = function(e) list(err = conditionMessage(e)))
    rec("Check 05: App: range +10 NOK in A (B unchanged); fill dialog",
        ok(is.null(res$err) && isTRUE(res$chg) && isTRUE(res$b) && isTRUE(res$fill) && isTRUE(res$kept)),
        if (!is.null(res$err)) paste("error:", res$err)
        else sprintf("changed idx %s by %s; A range +10 correct (%d cells): %s | B unchanged: %s | fill cell 777 + 5 = 782 and empty cells kept: %s | field value and mode kept after Apply and a row button: %s",
                     res$diff, res$d, res$n, res$chg, res$b, res$fill, res$kept))
    audit_browser_stop(app)
  }
}

# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 29 complete.\n")