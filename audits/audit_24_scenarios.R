# ==============================================================================
# audits/audit_24_scenarios.R
#
# Purpose : Phase 4d (2026-10-15): Scenarios A and B each have their own
#           assortments and prices. The scenarios are independent: B starts
#           as a copy of A, nothing is linked, "Make assortment equal to
#           A/B" and "Make matrix equal to A/B" copy one assortment or one
#           matrix (R/helpers_price_matrix.R), and a whole scenario is copied
#           by downloading one side and uploading it to the other. Checks the scenario helpers,
#           the derived assortment fields (type, length step, diameter
#           rounding), runs with B's own rules and price types, and the demo
#           files in demo/.
#
# Checks  :
#  1.  Derived fields: rounding fixed per volume method (Top cylinder, Top,
#      Norwegian mid down to cm; solid volume none); flat lengths every
#      10 cm; the hidden type of a user-added assortment is never "waste"
#  2.  Assortment only in B: assortment_diff() marks it "only" in B and as
#      missing in A; the scenarios differ
#  3.  Changed rule in B with the same prices: the scenarios differ (a
#      comparison runs on an assortment change alone); only that field is
#      marked, on both sides
#  4.  Assortment removed from B only: gone from B's table, still in A; the
#      scenarios differ
#  5.  Make equal: copy_assortment() brings B's changed row back to A's (in
#      place) and re-adds the removed one; with B's own assortment removed,
#      the scenarios are equal again; Scenario A written to a scenario
#      file and read back as B (download, upload) does not differ
#  6.  Backend comparison (100-tree list): B with spruce sawlog min top 14 cm,
#      a flat-priced spruce poles only in B, no pine pulpwood, and spruce
#      pulpwood matrix-priced in B (flat in A). A equals the single run; B has
#      no spruce sawlog under 14 cm top (under bark), has poles and no pine
#      pulpwood, and prices spruce pulpwood from its matrix
#  7.  Renaming the base Waste assortments (type stays waste) leaves every
#      tree's value, volumes and NOK/m3 unchanged
#  8.  Scenario file uploaded to B: scenario_import() gives exactly the
#      file's assortments and prices; it differs from A
#  9.  Demo scenario and price files: both scenario files import, both price
#      files upload into their scenario (upload_price_set()); B differs
# 10.  Demo tree list: reads and prepares without errors (2 plots, spruce and
#      pine, heights imputed with fieldPlot for spruce and the ratio method,
#      "FinnHoyde (calibrated)", for pine), and a comparison with the demo
#      scenarios runs and gives different values
# 11.  Price type per assortment and scenario: spruce pulpwood matrix-priced
#      and pine pulpwood flat within one scenario is valid; copy_prices()
#      copies only assortments priced the same way in both scenarios and
#      reports the others; copying the assortment copies its price type
#
# Verdict : PASS / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/assortments/audit_24_scenarios.csv
#   outputs/assortments/audit_24_scenarios.txt
#
# Usage   : Rscript audits/audit_24_scenarios.R
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

source(file.path(root_dir, "scripts", "performance_tree_lists.R"))
source(file.path(root_dir, "inst", "app", "helpers_upload.R"))     # upload_price_set()

out_dir <- path_output("assortments", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_24_scenarios.txt")
audit_csv <- file.path(out_dir, "audit_24_scenarios.csv")
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
cat("  Audit: scenarios A and B (assortments and prices)\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

pd0  <- load_products(root_dir)
pm0  <- pm_minimal(load_price_matrix_long(root_dir))
defs <- defs_from_products(pd0)
keys <- assortment_table_keys(defs)
kb <- function(d) assortment_table_keys(d)
pm_a0 <- pm0

# A user-added B-only assortment (flat price), as the app's form makes it.
poles <- .type_assortment_table(data.frame(
  Name = "Poles", Type = custom_type("Poles"), Species = "spruce",
  MinTopDiameter_cm = 15, MaxTopDiameter_cm = Inf, MinButtDiameter_cm = 0, MaxButtDiameter_cm = Inf,
  MinLength_cm = 800, MaxLength_cm = 1200, DiameterTopPosition_cm = 0,
  VolumeDiameterCategory = "Top cylinder", PriceType = "flat", stringsAsFactors = FALSE))
pm_poles <- pm_start_prices("spruce_poles", "flat", 900)
# Spruce sawlog with min top 14 cm (B's own rule).
saw14 <- defs[keys == "spruce_sawlog", , drop = FALSE]; saw14$MinTopDiameter_cm <- 14


# ==============================================================================
section("01. Derived fields")
p1 <- products_from_defs(rbind(defs, poles), rbind(pm0, pm_poles))$pd
rnd <- volume_rounding(VOLUME_METHODS)
ok1 <- identical(unname(rnd), c(VOLUME_ROUND_DOWN_CM, VOLUME_ROUND_DOWN_CM, "None", VOLUME_ROUND_DOWN_CM,
                                     VOLUME_ROUND_DOWN_CM)) &&   # ... Norwegian mid, Mid
  identical(p1$VolumeDiameterAdjustment, unname(volume_rounding(p1$VolumeDiameterCategory))) &&
  identical(assortment_lengths(poles, pm_poles)$spruce_poles, seq(800, 1200, by = 10)) &&
  identical(custom_type(c("Waste", "Poles")), c("waste_commercial", "poles")) &&
  !"LengthStep_cm" %in% ASSORTMENT_TABLE_COLS && "VolumeDiameterAdjustment" %in% ASSORTMENT_TABLE_COLS   # hidden field since 2026-11-11 (the file's own rounding)
rec("Check 01: Rounding per volume method, flat lengths every 10 cm, user types never waste",
    ok(ok1), sprintf("rounding: %s | poles lengths: %d (800-1200) | custom types: %s",
                     paste(sprintf("%s = %s", VOLUME_METHODS, rnd), collapse = "; "),
                     length(assortment_lengths(poles, pm_poles)$spruce_poles),
                     paste(custom_type(c("Waste", "Poles")), collapse = ", ")))


# ==============================================================================
section("02. Assortment only in B")
defs2 <- rbind(defs, poles); pm2 <- rbind(pm0, pm_poles)
d2 <- scenarios_differ(defs, pm0, defs2, pm2)
rb <- assortment_diff(defs2, defs); ra <- assortment_diff(defs, defs2)
ok2 <- isTRUE(d2) && identical(rb$status, c(rep("same", length(keys)), "only")) &&
  identical(ra$other_only, "spruce_poles") && all(ra$status == "same") &&
  isTRUE(check_price_set(defs2, pm2)$ok)
rec("Check 02: Assortment only in B marked on both sides; the scenarios differ",
    ok(ok2), sprintf("B statuses: %s | missing in A: %s | differ: %s",
                     paste(rb$status, collapse = ","), paste(ra$other_only, collapse = ","), d2))


# ==============================================================================
section("03. Changed rule in B, same prices")
defs3 <- defs; defs3[keys == "spruce_sawlog", ] <- saw14
d3 <- scenarios_differ(defs, pm0, defs3, pm0)
r3b <- assortment_diff(defs3, defs); r3a <- assortment_diff(defs, defs3)
cols_b <- colnames(r3b$diff)[r3b$diff[match("spruce_sawlog", keys), ]]
cols_a <- colnames(r3a$diff)[r3a$diff[match("spruce_sawlog", keys), ]]
ok3 <- isTRUE(d3) && identical(cols_b, "MinTopDiameter_cm") && identical(cols_a, "MinTopDiameter_cm") &&
  !isTRUE(scenarios_differ(defs, pm0, defs, pm0)) && same_prices(pm0, pm0, "spruce_sawlog")
rec("Check 03: A rule change alone makes the scenarios differ; only that field marked on both sides",
    ok(ok3), sprintf("differ: %s (identical scenarios: %s) | changed fields B: %s, A: %s",
                     d3, scenarios_differ(defs, pm0, defs, pm0),
                     paste(cols_b, collapse = ","), paste(cols_a, collapse = ",")))


# ==============================================================================
section("04. Removed from B only")
defs4 <- defs[keys != "pine_pulpwood", , drop = FALSE]; rownames(defs4) <- NULL
pm4 <- pm0[pm0$ProductKey != "pine_pulpwood", , drop = FALSE]
d4 <- scenarios_differ(defs, pm0, defs4, pm4)
ok4 <- !"pine_pulpwood" %in% kb(defs4) && "pine_pulpwood" %in% keys && isTRUE(d4) &&
  identical(assortment_diff(defs4, defs)$other_only, "pine_pulpwood") &&
  isTRUE(check_price_set(defs4, pm4)$ok)
rec("Check 04: Assortment removed from B only: not in B, still in A; the scenarios differ",
    ok(ok4), sprintf("B keys: %s | missing in B: %s | differ: %s", paste(kb(defs4), collapse = ", "),
                     paste(assortment_diff(defs4, defs)$other_only, collapse = ", "), d4))


# ==============================================================================
section("05. Make equal")
# B: changed sawlog rule, pine pulpwood removed, poles added.
db <- rbind(defs3[kb(defs3) != "pine_pulpwood", , drop = FALSE], poles); rownames(db) <- NULL
pb <- rbind(pm4, pm_poles)
c1 <- copy_assortment(defs, pm0, db, pb, "spruce_sawlog")
c2 <- copy_assortment(defs, pm0, c1$defs, c1$pm, "pine_pulpwood")
eq_pos <- identical(match("spruce_sawlog", kb(c1$defs)), match("spruce_sawlog", kb(db)))
d5a <- scenarios_differ(defs, pm0, c2$defs, c2$pm)                   # poles still only in B
back <- c2$defs[kb(c2$defs) != "spruce_poles", , drop = FALSE]
d5b <- scenarios_differ(defs, pm0, back, c2$pm[c2$pm$ProductKey != "spruce_poles", ])
f5 <- tempfile(fileext = ".xlsx"); scenario_export(defs, pm0, f5)    # A downloaded ...
s5 <- scenario_import(f5); unlink(f5)                                  # ... uploaded to B
d5c <- !isTRUE(s5$ok) || scenarios_differ(defs, pm0, s5$defs, s5$pm)
ok5 <- eq_pos && isTRUE(d5a) && !isTRUE(d5b) && !isTRUE(d5c) &&
  assortment_diff(c2$defs, defs)$status[match("spruce_sawlog", kb(c2$defs))] == "same"
rec("Check 05: Make equal: one assortment copied in place, a removed one re-added, whole scenario",
    ok(ok5), sprintf("copied in place: %s | differ with poles only in B: %s | without poles: %s | A downloaded and uploaded to B differs: %s",
                     eq_pos, d5a, d5b, d5c))


# ==============================================================================
section("06. Backend comparison with B's own assortments and price types")
tl <- perf_tree_list(100L)
TABLES <- c("selected_logs_all", "tree_summary", "per_m3_summary", "per_m3_by_assortment",
            "product_summary")
same_tables <- function(x, y) all(vapply(TABLES, function(k) identical(x[[k]], y[[k]]), logical(1)))
pa <- products_from_defs(defs, pm0)
f_a <- tempfile(fileext = ".csv"); pm_export(pm0, f_a)
r_single <- run_tree_list_bucking(tl, f_a, root_dir, products = pa$pd, length_classes = pa$lc)
# B: sawlog min top 14, poles only in B, no pine pulpwood, spruce pulpwood by a matrix.
pulp_m <- pm_start_prices("spruce_pulpwood", "matrix", 300, 5, 70, seq(300, 540, by = 30))
db6 <- db; db6$PriceType[kb(db6) == "spruce_pulpwood"] <- "matrix"
pb6 <- pm_minimal(rbind(pb[pb$ProductKey != "spruce_pulpwood", ], pulp_m))
v6 <- check_price_set(db6, pb6)
pbp <- products_from_defs(db6, pb6)
f_b <- tempfile(fileext = ".csv"); pm_export(pb6, f_b)
r6 <- run_tree_list_bucking(tl, f_a, root_dir, products = pa$pd, length_classes = pa$lc,
                            scenario = list(price_matrix_path = f_b, products = pbp$pd,
                                            length_classes = pbp$lc))
unlink(c(f_a, f_b))
lb <- r6$scenario$selected_logs_all; la <- r6$selected_logs_all
small_saw <- function(l) sum(l$ProductKey == "spruce_sawlog" & l$TopDiameter_ub < 140)
pulp_b <- lb[lb$ProductKey == "spruce_pulpwood", ]
ok6 <- isTRUE(v6$ok) && same_tables(r_single, r6) && small_saw(lb) == 0L && small_saw(la) > 0L &&
  sum(lb$ProductKey == "spruce_poles") > 0L && sum(lb$ProductKey == "pine_pulpwood") == 0L &&
  sum(la$ProductKey == "pine_pulpwood") > 0L && nrow(pulp_b) > 0L &&
  all(pulp_b$PriceNOKperM3 == 300) && all(pulp_b$LogLength %in% seq(300, 540, by = 30))
rec("Check 06: Comparison runs B's own rules and price types (pulpwood by a matrix in B)",
    ok(ok6), sprintf("B valid: %s | A = single run: %s | spruce sawlogs < 14 cm top u.b.: A %d, B %d | poles in B: %d | pine pulpwood: A %d, B %d | B spruce pulpwood logs %d, matrix prices %s, matrix lengths %s",
                     isTRUE(v6$ok), same_tables(r_single, r6), small_saw(la), small_saw(lb),
                     sum(lb$ProductKey == "spruce_poles"), sum(la$ProductKey == "pine_pulpwood"),
                     sum(lb$ProductKey == "pine_pulpwood"), nrow(pulp_b),
                     all(pulp_b$PriceNOKperM3 == 300), all(pulp_b$LogLength %in% seq(300, 540, by = 30))))


# ==============================================================================
section("07. Renaming Waste")
defs7 <- defs; defs7$Name[defs7$Type == "waste"] <- "Residue"
pm7 <- pm0
for (sp in PRODUCT_SPECIES) pm7 <- pm_rename_key(pm7, paste0(sp, "_waste"), paste0(sp, "_residue"))
p7 <- products_from_defs(defs7, pm7)
f7 <- tempfile(fileext = ".csv"); pm_export(pm7, f7)
r7 <- run_tree_list_bucking(tl, f7, root_dir, products = p7$pd, length_classes = p7$lc)
unlink(f7)
cols7 <- c("plot_id_github", "treid", "total_value_nok", "commercial_volume_m3", "nok_per_m3",
           "n_logs_selected")
ts_a <- r_single$tree_summary[, intersect(cols7, names(r_single$tree_summary))]
ts_b <- r7$tree_summary[, intersect(cols7, names(r7$tree_summary))]
ord <- function(x) { x <- x[order(x$treid), ]; rownames(x) <- NULL; x }
pv <- function(r) sum(r$selected_logs_all$price_volume_m3)
ok7 <- identical(ord(ts_a), ord(ts_b)) && identical(r_single$per_m3_summary, r7$per_m3_summary) &&
  isTRUE(all.equal(pv(r_single), pv(r7))) &&
  all(p7$pd$Priority[grepl("residue", p7$pd$ProductKey)] == 99) &&
  all(p7$pd$ProductType[grepl("residue", p7$pd$ProductKey)] == "waste")
rec("Check 07: Waste renamed 'Residue' (type stays waste): per-tree results unchanged",
    ok(ok7), sprintf("trees identical: %s | per-m3 summary identical: %s | price volume equal: %s | residue priority 99: %s",
                     identical(ord(ts_a), ord(ts_b)), identical(r_single$per_m3_summary, r7$per_m3_summary),
                     isTRUE(all.equal(pv(r_single), pv(r7))),
                     all(p7$pd$Priority[grepl("residue", p7$pd$ProductKey)] == 99)))


# ==============================================================================
section("08. Scenario file uploaded to B")
demo <- file.path(root_dir, "inst", "extdata", "demo")
sa <- scenario_import(file.path(demo, "scenario_A.xlsx"))
sb <- scenario_import(file.path(demo, "scenario_B.xlsx"))
f8 <- tempfile(fileext = ".xlsx"); scenario_export(sb$defs, sb$pm, f8)
s8 <- scenario_import(f8); unlink(f8)
exp_saw <- sb$defs$MinTopDiameter_cm[kb(sb$defs) == "spruce_sawlog"]
ok8 <- isTRUE(sb$ok) && identical(s8$defs, sb$defs) && identical(pm_minimal(s8$pm), pm_minimal(sb$pm)) &&
  identical(exp_saw, 14) && isTRUE(scenarios_differ(sa$defs, sa$pm, sb$defs, sb$pm))
rec("Check 08: A scenario file loads into B exactly (round trip); demo B differs from demo A",
    ok(ok8), sprintf("ok %s | round trip table %s, prices %s | spruce sawlog min top %s | differs from A %s",
                     isTRUE(sb$ok), identical(s8$defs, sb$defs), identical(pm_minimal(s8$pm), pm_minimal(sb$pm)),
                     exp_saw, scenarios_differ(sa$defs, sa$pm, sb$defs, sb$pm)))


# ==============================================================================
section("09. Demo scenario and price files")
ua <- upload_price_set(file.path(demo, "prices_A.xlsx"), "prices_A.xlsx", sa$defs, sa$pm)
ub <- upload_price_set(file.path(demo, "prices_B.xlsx"), "prices_B.xlsx", sa$defs, sa$pm)
d9 <- isTRUE(ub$ok) && scenarios_differ(sa$defs, sa$pm, sa$defs, ub$pm)
ok9 <- isTRUE(sa$ok) && isTRUE(sb$ok) && isTRUE(ua$ok) && isTRUE(ub$ok) &&
  identical(pm_minimal(ua$pm), pm0) && d9 && identical(sa$defs, defs)
rec("Check 09: Demo scenario files import; demo price files upload; B differs",
    ok(ok9), sprintf("scenario A %s, B %s | prices A %s (equal to defaults %s), B %s | B differs: %s | errors: %s",
                     isTRUE(sa$ok), isTRUE(sb$ok), isTRUE(ua$ok), identical(pm_minimal(ua$pm), pm0),
                     isTRUE(ub$ok), d9, paste(c(sa$errors, sb$errors, ua$errors, ub$errors), collapse = "; ")))


# ==============================================================================
section("10. Demo tree list")
rd <- read_tree_list(file.path(demo, "tree_list_example.csv"), "tree_list_example.csv")
pt <- if (length(rd$errors) == 0L) prepare_tree_list(rd$data) else list(ok = FALSE, errors = rd$errors)
hm <- if (!is.null(pt$heights)) setNames(pt$heights$method, pt$heights$species) else character(0)
r10 <- tryCatch({
  pa2 <- products_from_defs(sa$defs, sa$pm); pb2 <- products_from_defs(sb$defs, sb$pm)
  fa <- tempfile(fileext = ".csv"); fb <- tempfile(fileext = ".csv")
  pm_export(sa$pm, fa); pm_export(sb$pm, fb)
  on.exit(unlink(c(fa, fb)))
  run_tree_list_bucking(pt, fa, root_dir, products = pa2$pd, length_classes = pa2$lc,
                        scenario = list(price_matrix_path = fb, products = pb2$pd, length_classes = pb2$lc))
}, error = function(e) conditionMessage(e))
ran <- is.list(r10) && !is.null(r10$scenario)
va <- if (ran) sum(r10$selected_logs_all$value_nok) else NA
vb <- if (ran) sum(r10$scenario$selected_logs_all$value_nok) else NA
ok10 <- isTRUE(pt$ok) && length(unique(pt$trees$plot_id)) == 2L &&
  setequal(pt$trees$species_group, c("spruce", "pine")) &&
  identical(unname(hm["spruce"]), "fieldPlot") && grepl("calibrated", hm["pine"], fixed = TRUE) &&
  ran && va != vb
rec("Check 10: Demo tree list prepares (2 plots, spruce and pine, heights imputed) and a comparison runs",
    ok(ok10), sprintf("ok %s | trees %s | heights: %s | comparison ran: %s | value A %.0f, B %.0f NOK%s",
                      isTRUE(pt$ok), if (!is.null(pt$trees)) nrow(pt$trees) else NA,
                      paste(sprintf("%s %s", names(hm), hm), collapse = ", "), ran, va, vb,
                      if (!ran) paste(" | error:", r10) else ""))


# ==============================================================================
section("11. Price type per assortment and scenario")
v11 <- check_price_set(db6, pb6)                     # spruce pulpwood matrix, others flat
kinds <- setNames(db6$PriceType, kb(db6))
cp <- copy_prices(defs, pm0, db6, pb6, kb(db6))      # B's prices from A where priced alike
c11 <- copy_assortment(db6, pb6, defs, pm0, "spruce_pulpwood")   # A takes B's matrix pulpwood
ok11 <- isTRUE(v11$ok) && identical(unname(kinds[c("spruce_pulpwood", "spruce_waste")]), c("matrix", "flat")) &&
  "spruce_pulpwood" %in% cp$skipped && "spruce_poles" %in% cp$skipped &&
  "spruce_sawlog" %in% cp$copied && "spruce_waste" %in% cp$copied &&
  same_prices(cp$pm, pm0, "spruce_sawlog") && same_prices(cp$pm, pb6, "spruce_pulpwood") &&
  identical(c11$defs$PriceType[kb(c11$defs) == "spruce_pulpwood"], "matrix") &&
  isTRUE(check_price_set(c11$defs, c11$pm)$ok)
rec("Check 11: Price type per assortment and scenario; copy_prices() copies only alike-priced ones",
    ok(ok11), sprintf("matrix and flat in one scenario valid: %s | copied: %s | skipped: %s | A after copying B's pulpwood: %s, valid %s",
                      isTRUE(v11$ok), paste(cp$copied, collapse = ","), paste(cp$skipped, collapse = ","),
                      c11$defs$PriceType[kb(c11$defs) == "spruce_pulpwood"],
                      isTRUE(check_price_set(c11$defs, c11$pm)$ok)))


# ==============================================================================
section("12. No scenario names (R4 D3)")
# Names were removed: the scenario xlsx carries no 'scenario' sheet, and an older
# file that still has one (with a Name) imports exactly the same, ignoring it.
tmp12 <- tempfile(fileext = ".xlsx")
scenario_export(defs, pm0, tmp12)
r12 <- scenario_import(tmp12)
sheets12 <- readxl::excel_sheets(tmp12)
old12 <- tempfile(fileext = ".xlsx")
sh <- lapply(setNames(sheets12, sheets12), function(n) as.data.frame(readxl::read_excel(tmp12, sheet = n, col_types = "text")))
writexl::write_xlsx(c(sh, list(scenario = data.frame(Name = "Høst 2026"))), old12)
r12b <- scenario_import(old12)
ok12 <- isTRUE(r12$ok) && !"scenario" %in% sheets12 && is.null(r12$name) && isTRUE(r12b$ok) && is.null(r12b$name) &&
  identical(r12$defs, r12b$defs) && identical(r12$pm, r12b$pm) &&
  !exists("scenario_label", mode = "function") && !exists("scenario_file_tag", mode = "function")
rec("Check 12: the scenario xlsx has no name sheet; an older xlsx with a name sheet imports the same and the name is ignored; scenario_label()/scenario_file_tag() are gone",
    ok(ok12), sprintf("no scenario sheet %s | old file ok %s | defs %s | pm %s", !"scenario" %in% sheets12, isTRUE(r12b$ok), identical(r12$defs, r12b$defs), identical(r12$pm, r12b$pm)))


# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 24 complete.\n")