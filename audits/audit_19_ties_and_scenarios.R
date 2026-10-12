# ==============================================================================
# audits/audit_19_ties_and_scenarios.R
#
# Purpose : Audit of Phase 4a: deterministic tie-breaking in both DP
#           optimizers, the price-matrix scenario helpers (grids, diff, reset,
#           upload in long and wide layouts, templates, validation) and the
#           default-vs-scenario cutting-pattern comparison.
#
# Checks  :
#  1.  Synthetic ties, both optimizers give the expected pattern:
#        a. equal-value splits -> fewer logs
#        b. same log count -> higher-priority product (sawlog over pulpwood)
#        c. same product and count -> longer log
#        d. 1e-7 NOK apart is a tie (fewer logs wins); 1e-3 NOK apart is not
#        e. unallocated beats an extra zero-value waste log
#  2.  Order independence: 10 random row permutations of real candidate sets
#      (30 trees) never change the pattern, in either optimizer
#  3.  Matrix helpers: grid <-> long round trip exact for all matrices; diff
#      mask flags exactly the edited cells; reset restores exactly
#  4.  Templates and export round-trip through upload: wide xlsx, long CSV,
#      exported scenario
#  5.  Long-layout upload errors: missing column, negative, non-numeric,
#      unknown product, unknown length, duplicate key -> rejected with a
#      specific message; out-of-range diameter -> accepted with a warning
#  6.  Wide-layout uploads: valid wide CSV (one grid) replaces only its own
#      matrix; wrong headers (non-numeric length, length not in
#      LengthClasses, unknown matrix name), a non-numeric cell and an
#      unrecognised layout are rejected
#  7.  compare_cutting_patterns(), synthetic: a tie-only difference is not
#      counted, a value change is, a volume-only change is
#  8.  Backend scenario run (spruce sawlog x1.2): price_logs() re-pricing
#      reproduces the backend's values exactly; every changed tree has a
#      value or volume reason; a run compared with itself has 0 changes
#
# Outputs :
#   outputs/scenario_comparison/audit_19_ties_and_scenarios.csv
#   outputs/scenario_comparison/audit_19_ties_and_scenarios.txt
#
# Usage   : Rscript audits/audit_19_ties_and_scenarios.R
# ==============================================================================

suppressPackageStartupMessages(library(readr))

root_dir <- local({
  # APTER_ROOT -> walk up from getwd() -> stop()
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  env <- Sys.getenv("APTER_ROOT", unset = "")
  if (nzchar(env)) {
    d <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (!is_root(d))
      stop("Project root '", d, "' (from APTER_ROOT) does not contain ",
           "R/helpers_paths.R.", call. = FALSE)
  } else {
    d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    while (!is_root(d) && dirname(d) != d) d <- dirname(d)
    if (!is_root(d))
      stop("Cannot locate the project root: set APTER_ROOT, or run from ",
           "inside the project directory.", call. = FALSE)
  }
  d
})

source(file.path(root_dir, "R", "helpers_paths.R"))
source(file.path(root_dir, "R", "helpers_audit.R"))
source(file.path(root_dir, "scripts", "app_backend",
                 "01_run_sampled_plot_bucking.R"), local = FALSE)

out_dir <- path_output("scenario_comparison", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_19_ties_and_scenarios.txt")
audit_csv <- file.path(out_dir, "audit_19_ties_and_scenarios.csv")
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
cat("  Audit: tie-breaking and price-matrix scenarios\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

pd  <- load_products(root_dir); lc <- load_length_classes(root_dir)
def <- pm_minimal(load_price_matrix_long(root_dir))
pattern <- function(res) {
  sl <- res$selected_logs
  if (!is.data.frame(sl) || nrow(sl) == 0L) return("")
  paste(sort(paste(sl$StartPos, sl$StopPos, sl$ProductKey)), collapse = "; ")
}


# ==============================================================================
# Check 01: synthetic ties
# ==============================================================================
section("01. Synthetic ties")

cand <- function(...) {
  x <- data.frame(..., stringsAsFactors = FALSE)
  x$price_volume_m3 <- 0.1
  x$ProductPriority <- ifelse(grepl("sawlog", x$ProductKey), 1,
                       ifelse(grepl("pulpwood", x$ProductKey), 2, 99))
  x
}
cases <- list(
  a = list(lbl = "equal-value splits -> fewer logs",
           c = cand(StartPos = c(10, 10, 310), StopPos = c(610, 310, 610),
                    value_nok = c(60, 30, 30), ProductKey = "spruce_pulpwood"),
           expect = "10 610 spruce_pulpwood"),
  b = list(lbl = "same log count -> sawlog over pulpwood",
           c = cand(StartPos = c(10, 10), StopPos = c(410, 410), value_nok = c(50, 50),
                    ProductKey = c("spruce_pulpwood", "spruce_sawlog")),
           expect = "10 410 spruce_sawlog"),
  c = list(lbl = "same product and count -> longer first log",
           c = cand(StartPos = c(10, 310, 10, 410), StopPos = c(310, 710, 410, 710),
                    value_nok = c(35, 25, 40, 20),
                    ProductKey = c("spruce_sawlog", "spruce_pulpwood",
                                   "spruce_sawlog", "spruce_pulpwood")),
           expect = "10 410 spruce_sawlog; 410 710 spruce_pulpwood"),
  d1 = list(lbl = "1e-7 NOK apart is a tie -> fewer logs",
            c = cand(StartPos = c(10, 10, 310), StopPos = c(610, 310, 610),
                     value_nok = c(60 - 1e-7, 30, 30), ProductKey = "spruce_pulpwood"),
            expect = "10 610 spruce_pulpwood"),
  d2 = list(lbl = "1e-3 NOK apart is not a tie -> higher value",
            c = cand(StartPos = c(10, 10, 310), StopPos = c(610, 310, 610),
                     value_nok = c(60 - 1e-3, 30, 30), ProductKey = "spruce_pulpwood"),
            expect = "10 310 spruce_pulpwood; 310 610 spruce_pulpwood"),
  e = list(lbl = "unallocated beats an extra zero-value waste log",
           c = cand(StartPos = c(10, 410), StopPos = c(410, 460), value_nok = c(40, 0),
                    ProductKey = c("spruce_sawlog", "spruce_waste")),
           expect = "10 410 spruce_sawlog")
)
for (nm in names(cases)) {
  cs <- cases[[nm]]
  pf <- pattern(.optimize_dp_fast(cs$c)); pr <- pattern(optimize_one_tree_dp(cs$c))
  rec(sprintf("Check 01%s: %s", nm, cs$lbl),
      if (identical(pf, cs$expect) && identical(pr, cs$expect)) PASS else FAIL,
      sprintf("fast: '%s' | reference: '%s' | expected: '%s'", pf, pr, cs$expect))
}


# ==============================================================================
# Check 02: order independence on real candidate sets
# ==============================================================================
section("02. Order independence")

E_cap <- new.env(); E_cap$sets <- list()
.fast_orig <- .optimize_dp_fast
.optimize_dp_fast <- function(valued_candidates) {
  if (length(E_cap$sets) < 30L) E_cap$sets[[length(E_cap$sets) + 1L]] <- valued_candidates
  .fast_orig(valued_candidates)
}
invisible(run_sampled_plot_bucking(
  list(species_group = "both", hogstkl = c(41L, 42L), abonitet = c(14L, 17L),
       n_plots = 5L, seed = 11L), root = root_dir))
.optimize_dp_fast <- .fast_orig; rm(.fast_orig)

set.seed(19)
n_bad <- 0L; n_runs <- 0L
for (x in E_cap$sets) {
  base_f <- pattern(.optimize_dp_fast(x)); base_r <- pattern(optimize_one_tree_dp(x))
  if (!identical(base_f, base_r)) n_bad <- n_bad + 1L
  for (k in 1:10) {
    xs <- x[sample(nrow(x)), , drop = FALSE]
    if (!identical(pattern(.optimize_dp_fast(xs)), base_f) ||
        !identical(pattern(optimize_one_tree_dp(xs)), base_f)) n_bad <- n_bad + 1L
    n_runs <- n_runs + 2L
  }
}
rec("Check 02: Row permutations never change the pattern (both optimizers)",
    if (length(E_cap$sets) >= 10L && n_bad == 0L) PASS else FAIL,
    sprintf("%d trees x 10 permutations x 2 optimizers = %d runs | differences: %d",
            length(E_cap$sets), n_runs, n_bad))


# ==============================================================================
# Check 03: matrix helpers
# ==============================================================================
section("03. Matrix helpers")

ids <- pm_matrix_ids(def)
rt  <- vapply(ids, function(id) identical(pm_from_grid(pm_to_grid(def, id), id, def), def),
              logical(1))
g   <- pm_to_grid(def, "pine_sawlog"); g["20", "40"] <- g["20", "40"] + 7; g["45", "31"] <- 0
sc  <- pm_from_grid(g, "pine_sawlog", def)
mask <- pm_diff_mask(def, sc, "pine_sawlog")
mask_ok <- sum(mask) == 2L && mask["20", "40"] && mask["45", "31"] &&
  pm_n_diff(def, sc) == 2L && sum(pm_diff_mask(def, sc, "spruce_sawlog")) == 0L
reset_ok <- identical(pm_reset(sc, def, "pine_sawlog"), def)
fg <- pm_to_grid(def, PM_FLAT_ID); fg$PriceNOKperM3[1] <- 999
sc_f <- pm_from_grid(fg, PM_FLAT_ID, def)
flat_ok <- pm_n_diff(def, sc_f) == 1L && identical(pm_reset(sc_f, def, PM_FLAT_ID), def)
rec("Check 03: Grid round trip, diff mask and reset are exact",
    if (all(rt) && mask_ok && reset_ok && flat_ok) PASS else FAIL,
    sprintf("round trip exact: %s | diff mask (2 edits): %s | reset exact: %s | flat table edit/reset: %s",
            paste(names(rt), rt, sep = "=", collapse = ", "), mask_ok, reset_ok, flat_ok))


# ==============================================================================
# Check 04: templates and export round trip
# ==============================================================================
section("04. Templates and export")

f_w <- tempfile(fileext = ".xlsx"); pm_template_wide(def, f_w)
f_l <- tempfile(fileext = ".csv");  pm_template_long(def, f_l)
f_e <- tempfile(fileext = ".csv");  pm_export(sc, f_e)
u_w <- apply_price_upload(f_w, f_w, def, def, pd, lc)
u_l <- apply_price_upload(f_l, f_l, def, def, pd, lc)
u_e <- apply_price_upload(f_e, f_e, def, def, pd, lc)
ok4 <- isTRUE(u_w$ok) && identical(u_w$layout, "wide") && identical(u_w$pm, def) &&
  setequal(u_w$ids, ids) &&
  isTRUE(u_l$ok) && identical(u_l$layout, "long") && identical(u_l$pm, def) &&
  isTRUE(u_e$ok) && identical(u_e$pm, sc)
rec("Check 04: Wide xlsx and long CSV templates, and scenario export, round-trip",
    if (ok4) PASS else FAIL,
    sprintf("wide xlsx: %s/%s sheets %s | long csv: %s/%s | export: %s",
            isTRUE(u_w$ok), u_w$layout, paste(u_w$ids, collapse = ","),
            isTRUE(u_l$ok), u_l$layout, isTRUE(u_e$ok) && identical(u_e$pm, sc)))


# ==============================================================================
# Check 05: long-layout upload errors
# ==============================================================================
section("05. Long-layout upload validation")

try_long <- function(mod) {
  f <- tempfile(fileext = ".csv"); d <- mod(def); readr::write_csv(d, f, na = "")
  apply_price_upload(f, f, def, def, pd, lc)
}
long_cases <- list(
  "missing column"   = list(mod = function(d) d[, setdiff(names(d), "PriceNOKperM3")],
                            pat = "Missing column"),
  "negative price"   = list(mod = function(d) { d$PriceNOKperM3[5] <- -1; d },
                            pat = "Negative price"),
  "non-numeric"      = list(mod = function(d) { d$PriceNOKperM3 <- as.character(d$PriceNOKperM3)
                              d$PriceNOKperM3[7] <- "abc"; d }, pat = "Non-numeric PriceNOKperM3"),
  "unknown product"  = list(mod = function(d) { d$ProductKey[1] <- "birch_sawlog"; d },
                            pat = "Unknown ProductKey"),
  "unknown length"   = list(mod = function(d) { d$LengthClass_dm[d$ProductKey == "pine_sawlog"][1] <- 33L; d },
                            pat = "not in LengthClasses"),
  "duplicate key"    = list(mod = function(d) rbind(d, d[3, ]), pat = "Duplicated price key"))
for (nm in names(long_cases)) {
  u <- try_long(long_cases[[nm]]$mod)
  hit <- any(grepl(long_cases[[nm]]$pat, u$errors, fixed = TRUE))
  rec(sprintf("Check 05: Long upload, %s -> rejected", nm),
      if (!isTRUE(u$ok) && hit) PASS else FAIL,
      sprintf("ok=%s | errors: %s", isTRUE(u$ok), paste(u$errors, collapse = " / ")))
}
u_w60 <- try_long(function(d) rbind(d, transform(
  d[d$ProductKey == "spruce_sawlog" & d$PriceDiameterClass_cm == 50 & d$LengthClass_dm == 55, ],
  PriceDiameterClass_cm = 60L)))
rec("Check 05: Long upload, diameter class 60 cm -> accepted with a warning",
    if (isTRUE(u_w60$ok) && any(grepl("outside 12-50", u_w60$warnings, fixed = TRUE))) PASS else FAIL,
    sprintf("ok=%s | warnings: %s", isTRUE(u_w60$ok), paste(u_w60$warnings, collapse = " / ")))


# ==============================================================================
# Check 06: wide-layout uploads
# ==============================================================================
section("06. Wide-layout upload validation")

wide_csv <- function(grid, corner = "spruce_sawlog", hdr = NULL) {
  f <- tempfile(fileext = ".csv")
  x <- cbind(corner = rownames(grid), grid)
  names(x) <- c(corner, if (is.null(hdr)) colnames(grid) else hdr)
  utils::write.csv(x, f, row.names = FALSE)
  f
}
gw <- pm_to_grid(def, "spruce_sawlog"); gw2 <- gw * 1.1
f6 <- wide_csv(gw2)
u6 <- apply_price_upload(f6, f6, def, def, pd, lc)
only_own <- isTRUE(u6$ok) && identical(u6$layout, "wide") &&
  sum(pm_diff_mask(def, u6$pm, "spruce_sawlog")) == sum(!is.na(as.matrix(gw))) &&
  sum(pm_diff_mask(def, u6$pm, "pine_sawlog")) == 0L &&
  sum(pm_diff_mask(def, u6$pm, PM_FLAT_ID)) == 0L
rec("Check 06a: Valid wide CSV replaces only its own matrix",
    if (only_own) PASS else FAIL,
    sprintf("ok=%s layout=%s ids=%s | spruce cells changed %d, pine %d, flat %d",
            isTRUE(u6$ok), u6$layout, paste(u6$ids, collapse = ","),
            if (isTRUE(u6$ok)) sum(pm_diff_mask(def, u6$pm, "spruce_sawlog")) else NA,
            if (isTRUE(u6$ok)) sum(pm_diff_mask(def, u6$pm, "pine_sawlog")) else NA,
            if (isTRUE(u6$ok)) sum(pm_diff_mask(def, u6$pm, PM_FLAT_ID)) else NA))

wide_cases <- list(
  "non-numeric length header" = list(f = wide_csv(gw, hdr = c("31", "34", "x37", colnames(gw)[-(1:3)])),
                                     pat = "header must be numeric"),
  "length not in LengthClasses" = list(f = wide_csv(gw, hdr = c("31", "34", "36", colnames(gw)[-(1:3)])),
                                       pat = "not in LengthClasses"),
  "unknown matrix name" = list(f = wide_csv(gw, corner = "birch_sawlog"),
                               pat = "Unrecognised layout"),
  "non-numeric cell" = list(f = local({ gx <- gw; gx[] <- lapply(gx, as.character)
                                        gx["20", "43"] <- "4o5"; wide_csv(gx) }),
                            pat = "non-numeric price"),
  "unrecognised layout" = list(f = local({ f <- tempfile(fileext = ".csv")
                                           writeLines(c("a,b,c", "1,2,3"), f); f }),
                               pat = "Unrecognised layout"))
for (nm in names(wide_cases)) {
  u <- apply_price_upload(wide_cases[[nm]]$f, wide_cases[[nm]]$f, def, def, pd, lc)
  hit <- any(grepl(wide_cases[[nm]]$pat, u$errors, fixed = TRUE))
  rec(sprintf("Check 06: Wide upload, %s -> rejected", nm),
      if (!isTRUE(u$ok) && hit) PASS else FAIL,
      sprintf("ok=%s | errors: %s", isTRUE(u$ok), paste(u$errors, collapse = " / ")))
}
# Unknown sheet name in a wide xlsx
f_bad <- tempfile(fileext = ".xlsx")
writexl::write_xlsx(list(birch_sawlog = data.frame(birch_sawlog = 12, `31` = 400,
                                                   check.names = FALSE)), f_bad)
u_bad <- apply_price_upload(f_bad, f_bad, def, def, pd, lc)
rec("Check 06: Wide xlsx with an unknown sheet name -> rejected",
    if (!isTRUE(u_bad$ok) && any(grepl("Unknown matrix", u_bad$errors, fixed = TRUE))) PASS else FAIL,
    sprintf("ok=%s | errors: %s", isTRUE(u_bad$ok), paste(u_bad$errors, collapse = " / ")))


# ==============================================================================
# Check 07: compare_cutting_patterns(), synthetic
# ==============================================================================
section("07. Pattern comparison, synthetic")

pm_s <- data.frame(ProductKey = c("spruce_pulpwood", "spruce_waste"), LengthClass_dm = NA,
                   PriceDiameterClass_cm = NA, PriceNOKperM3 = c(100, 0))
mk_logs <- function(tr, sp, ep, pv, cv) data.frame(
  plot_id_github = "P1", treid = tr, species_group = "spruce",
  StartPos = sp, StopPos = ep, ProductKey = "spruce_pulpwood", ProductType = "pulpwood",
  LengthClass_dm = NA_integer_, PriceDiameterClass_cm_used = NA_integer_,
  price_volume_m3 = pv, commercial_volume_m3 = cv, stringsAsFactors = FALSE)
trees <- data.frame(plot_id_github = "P1", treid = c("T1", "T2", "T3"),
                    species_group = "spruce", stringsAsFactors = FALSE)
r_a <- list(tree_summary = trees, selected_logs_all = rbind(
  mk_logs("T1", c(10, 310), c(310, 610), c(.15, .10), c(.15, .10)),
  mk_logs("T2", 10, 310, .2, .2),
  mk_logs("T3", 10, 310, .3, .30)))
r_b <- list(tree_summary = trees, selected_logs_all = rbind(
  mk_logs("T1", c(10, 410), c(410, 610), c(.20, .05), c(.20, .05)),   # same value and volume
  mk_logs("T2", 10, 410, .3, .3),                                       # higher value
  mk_logs("T3", 20, 320, .3, .31)))                                     # same value, more volume
cs <- compare_cutting_patterns(r_a, r_b, pm_s, pm_s)
ok7 <- identical(cs$tie_only, c(TRUE, FALSE, FALSE)) &&
  identical(cs$changed, c(FALSE, TRUE, TRUE))
rec("Check 07: Tie-only difference not counted; value and volume changes counted",
    if (ok7) PASS else FAIL,
    sprintf("T1 changed=%s tie=%s | T2 changed=%s (gain %.2f) | T3 changed=%s (volume diff %.3f)",
            cs$changed[1], cs$tie_only[1], cs$changed[2], cs$gain_at_scenario[2],
            cs$changed[3], cs$max_volume_diff_m3[3]))


# ==============================================================================
# Check 08: backend scenario run
# ==============================================================================
section("08. Backend scenario run")

p8  <- list(species_group = "spruce", hogstkl = c(41L, 42L), abonitet = c(14L, 17L),
            n_plots = 10L, seed = 42L)
pm2 <- def; saw <- grepl("sawlog", pm2$ProductKey) & pm2$ProductKey == "spruce_sawlog"
pm2$PriceNOKperM3[saw] <- pm2$PriceNOKperM3[saw] * 1.2
f8 <- tempfile(fileext = ".csv"); pm_export(pm2, f8)
r1 <- run_sampled_plot_bucking(p8, root = root_dir)
r2 <- run_sampled_plot_bucking(p8, price_matrix_path = f8, root = root_dir)
rp <- price_logs(r2$selected_logs_all, pm2)
dev_rp <- max(abs(rp$value_nok - r2$selected_logs_all$value_nok))
cmp <- compare_cutting_patterns(r1, r2, def, pm2)
ch  <- cmp[cmp$changed, ]
reasons_ok <- all(ch$gain_at_scenario > DP_TIE_TOL_NOK | ch$gain_at_default > DP_TIE_TOL_NOK |
                  ch$max_volume_diff_m3 > 1e-9)
unchanged_ok <- all(!cmp$pattern_differs[!cmp$changed] | cmp$tie_only[!cmp$changed])
self <- compare_cutting_patterns(r1, r1, def, def)
rec("Check 08: Scenario run re-prices exactly; changes have reasons; self-compare is 0",
    if (dev_rp == 0 && reasons_ok && unchanged_ok && sum(self$changed) == 0L &&
        sum(self$tie_only) == 0L && sum(cmp$changed) > 0L) PASS else FAIL,
    sprintf("%d trees | changed %d, tie-only %d | re-price max |diff| %.1e NOK | every change has a reason: %s | self-compare changed %d",
            nrow(cmp), sum(cmp$changed), sum(cmp$tie_only), dev_rp, reasons_ok,
            sum(self$changed)))


# ==============================================================================
# Verdict and output
# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 19 complete.\n")
cat("Results written to:", out_dir, "\n")
