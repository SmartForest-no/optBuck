# ==============================================================================
# audits/audit_24_scenarios.R
#
# Purpose : Phase 4d (2026-10-15): Scenarios A and B each have their own
#           assortments and prices. B is linked to A per assortment
#           (b_asm: same / own / none; R/helpers_price_matrix.R). Checks the
#           scenario helpers, the derived assortment fields (type, length
#           step, diameter rounding), runs with B's own rules, and the demo
#           files in demo/.
#
# Checks  :
#  1.  Derived fields: rounding fixed per volume method (Top cylinder, Top,
#      Norwegian mid down to cm; solid volume none); flat lengths every
#      10 cm; the hidden type of a user-added assortment is never "waste"
#  2.  B-only assortment: in B's table (after A's rows), not in A's; B differs
#  3.  Changed rules in B with the same prices: B differs (a comparison runs
#      on an assortment change alone); b_row_diff() marks only the changed
#      field
#  4.  Assortment removed from B only: gone from B's table and prices, still
#      in A; B differs
#  5.  "Make equal to A" (all "same"): B's table and prices equal A's; B does
#      not differ
#  6.  Backend comparison (100-tree list): B with spruce sawlog min top 14 cm,
#      a B-only flat-priced assortment (spruce poles) and pine pulpwood not in
#      B. A equals the single run; B has no spruce sawlog under 14 cm top
#      (under bark), has poles and no pine pulpwood
#  7.  Renaming the base Waste assortments (type stays waste) leaves every
#      tree's value, volumes and NOK/m3 unchanged
#  8.  b_states_from() (scenario upload to B): the demo B gives spruce sawlog
#      "own" (rules and prices), the rest "same"; b_set() gives back B's file
#  9.  Demo scenario and price files: both scenario files import, both price
#      files upload into their scenario (upload_price_set()), B differs
# 10.  Demo tree list: reads and prepares without errors (2 plots, spruce and
#      pine, heights imputed with fieldPlot for spruce and the ratio method,
#      "FinnHoyde (calibrated)", for pine), and a comparison with the demo
#      scenarios runs and gives different values
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
Sys.setenv(APTER_ROOT = root_dir)

source(file.path(root_dir, "R", "helpers_paths.R"))
source(file.path(root_dir, "R", "helpers_audit.R"))
source(file.path(root_dir, "scripts", "app_backend",
                 "01_run_sampled_plot_bucking.R"), local = FALSE)
source(file.path(root_dir, "scripts", "performance_tree_lists.R"))
source(file.path(root_dir, "app", "helpers_upload.R"))     # upload_price_set()

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
same <- setNames(rep("same", length(keys)), keys)
no_own <- defs[0, , drop = FALSE]
kb <- function(b) assortment_table_keys(b$defs)

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
ok1 <- identical(unname(rnd), c(VOLUME_ROUND_DOWN_CM, VOLUME_ROUND_DOWN_CM, "None", VOLUME_ROUND_DOWN_CM)) &&
  identical(p1$VolumeDiameterAdjustment, unname(volume_rounding(p1$VolumeDiameterCategory))) &&
  identical(assortment_lengths(poles, pm_poles)$spruce_poles, seq(800, 1200, by = 10)) &&
  identical(custom_type(c("Waste", "Poles")), c("waste_commercial", "poles")) &&
  !any(c("LengthStep_cm", "VolumeDiameterAdjustment") %in% ASSORTMENT_TABLE_COLS)
rec("Check 01: Rounding per volume method, flat lengths every 10 cm, user types never waste",
    ok(ok1), sprintf("rounding: %s | poles lengths: %d (800-1200) | custom types: %s",
                     paste(sprintf("%s = %s", VOLUME_METHODS, rnd), collapse = "; "),
                     length(assortment_lengths(poles, pm_poles)$spruce_poles),
                     paste(custom_type(c("Waste", "Poles")), collapse = ", ")))


# ==============================================================================
section("02. B-only assortment")
asm2 <- c(same, spruce_poles = "own"); st2 <- asm2
b2 <- b_set(defs, pm0, rbind(pm0, pm_poles), st2, poles, asm2)
d2 <- b_differs(pm0, rbind(pm0, pm_poles), st2, defs, b2$defs)
ok2 <- identical(kb(b2), c(keys, "spruce_poles")) && "spruce_poles" %in% b2$pm$ProductKey &&
  !"spruce_poles" %in% keys && isTRUE(d2) && isTRUE(check_price_set(b2$defs, b2$pm)$ok) &&
  identical(b_row_diff(defs, b2$defs)$status, c(rep("same", length(keys)), "b_only"))
rec("Check 02: B-only assortment in B's table and prices, not in A; B differs",
    ok(ok2), sprintf("B keys: %s | B differs: %s", paste(kb(b2), collapse = ", "), d2))


# ==============================================================================
section("03. Changed rules in B, same prices")
asm3 <- same; asm3["spruce_sawlog"] <- "own"
b3 <- b_set(defs, pm0, pm0, same, saw14, asm3)
d3 <- b_differs(pm0, pm0, same, defs, b3$defs)
rd3 <- b_row_diff(defs, b3$defs)
diff_cols <- colnames(rd3$diff)[rd3$diff[match("spruce_sawlog", kb(b3)), ]]
ok3 <- isTRUE(d3) && identical(diff_cols, "MinTopDiameter_cm") &&
  identical(pm_minimal(b3$pm), pm0) && !isTRUE(b_differs(pm0, pm0, same))
rec("Check 03: A rule change alone makes B differ (comparison); only that field marked",
    ok(ok3), sprintf("B differs: %s (prices alone: %s) | changed fields: %s | prices equal A: %s",
                     d3, b_differs(pm0, pm0, same), paste(diff_cols, collapse = ", "),
                     identical(pm_minimal(b3$pm), pm0)))


# ==============================================================================
section("04. Removed from B only")
asm4 <- same; asm4["pine_pulpwood"] <- "none"; st4 <- asm4
b4 <- b_set(defs, pm0, pm0, st4, no_own, asm4)
d4 <- b_differs(pm0, pm0, st4, defs, b4$defs)
ok4 <- !"pine_pulpwood" %in% c(kb(b4), b4$pm$ProductKey) && "pine_pulpwood" %in% keys &&
  isTRUE(d4) && identical(b_row_diff(defs, b4$defs)$a_only, "pine_pulpwood")
rec("Check 04: Assortment removed from B only: not in B, still in A; B differs",
    ok(ok4), sprintf("B keys: %s | A only: %s | B differs: %s", paste(kb(b4), collapse = ", "),
                     paste(b_row_diff(defs, b4$defs)$a_only, collapse = ", "), d4))


# ==============================================================================
section("05. Make equal to A")
b5 <- b_set(defs, pm0, rbind(pm0, pm_poles), same, rbind(saw14, poles), same)
d5 <- b_differs(pm0, rbind(pm0, pm_poles), same, defs, b5$defs)
ok5 <- identical(b5$defs, defs) && identical(pm_minimal(b5$pm), pm0) && !isTRUE(d5)
rec("Check 05: With every assortment 'same', B equals A (own rows and prices ignored)",
    ok(ok5), sprintf("table equal: %s | prices equal: %s | B differs: %s",
                     identical(b5$defs, defs), identical(pm_minimal(b5$pm), pm0), d5))


# ==============================================================================
section("06. Backend comparison with B's own assortments")
tl <- perf_tree_list(100L)
TABLES <- c("selected_logs_all", "tree_summary", "per_m3_summary", "per_m3_by_assortment",
            "product_summary")
same_tables <- function(x, y) all(vapply(TABLES, function(k) identical(x[[k]], y[[k]]), logical(1)))
pa <- products_from_defs(defs, pm0)
f_a <- tempfile(fileext = ".csv"); pm_export(pm0, f_a)
r_single <- run_tree_list_bucking(tl, f_a, root_dir, products = pa$pd, length_classes = pa$lc)
asm6 <- c(same, spruce_poles = "own"); asm6["spruce_sawlog"] <- "own"; asm6["pine_pulpwood"] <- "none"
st6 <- asm6; st6["spruce_sawlog"] <- "same"
b6 <- b_set(defs, pm0, rbind(pm0, pm_poles), st6, rbind(saw14, poles), asm6)
pb <- products_from_defs(b6$defs, b6$pm)
f_b <- tempfile(fileext = ".csv"); pm_export(b6$pm, f_b)
r6 <- run_tree_list_bucking(tl, f_a, root_dir, products = pa$pd, length_classes = pa$lc,
                            scenario = list(price_matrix_path = f_b, products = pb$pd,
                                            length_classes = pb$lc))
unlink(c(f_a, f_b))
lb <- r6$scenario$selected_logs_all; la <- r6$selected_logs_all
small_saw <- function(l) sum(l$ProductKey == "spruce_sawlog" & l$TopDiameter_ub < 140)
ok6 <- same_tables(r_single, r6) && small_saw(lb) == 0L && small_saw(la) > 0L &&
  sum(lb$ProductKey == "spruce_poles") > 0L && sum(lb$ProductKey == "pine_pulpwood") == 0L &&
  sum(la$ProductKey == "pine_pulpwood") > 0L
rec("Check 06: Comparison runs B's own rules: min top 14 cm, B-only poles, no pine pulpwood",
    ok(ok6), sprintf("A = single run: %s | spruce sawlogs < 14 cm top u.b.: A %d, B %d | poles in B: %d | pine pulpwood: A %d, B %d",
                     same_tables(r_single, r6), small_saw(la), small_saw(lb),
                     sum(lb$ProductKey == "spruce_poles"), sum(la$ProductKey == "pine_pulpwood"),
                     sum(lb$ProductKey == "pine_pulpwood")))


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
section("08. Scenario upload to B (b_states_from)")
demo <- file.path(root_dir, "demo")
sa <- scenario_import(file.path(demo, "scenario_A.xlsx"))
sb <- scenario_import(file.path(demo, "scenario_B.xlsx"))
s8 <- b_states_from(sa$defs, sa$pm, sb$defs, sb$pm)
b8 <- b_set(sa$defs, sa$pm, s8$pm_b, s8$state, s8$defs_b_own, s8$b_asm)
exp_asm <- same; exp_asm["spruce_sawlog"] <- "own"
ok8 <- identical(s8$b_asm, exp_asm) && identical(s8$state, exp_asm) &&
  identical(b8$defs, sb$defs) && identical(pm_minimal(b8$pm), pm_minimal(sb$pm))
rec("Check 08: Demo B uploaded to B: spruce sawlog own (rules and prices), rest same",
    ok(ok8), sprintf("assortments: %s | prices: %s | B in effect equals the file: %s",
                     paste(sprintf("%s %s", names(s8$b_asm), s8$b_asm), collapse = ", "),
                     paste(sprintf("%s %s", names(s8$state), s8$state), collapse = ", "),
                     identical(b8$defs, sb$defs) && identical(pm_minimal(b8$pm), pm_minimal(sb$pm))))


# ==============================================================================
section("09. Demo scenario and price files")
ua <- upload_price_set(file.path(demo, "prices_A.xlsx"), "prices_A.xlsx", "A", sa$defs, sa$pm, sa$pm, same)
ub <- upload_price_set(file.path(demo, "prices_B.xlsx"), "prices_B.xlsx", "B", sa$defs, sa$pm, sa$pm, same)
d9 <- isTRUE(ub$ok) && b_differs(sa$pm, ub$pm, ub$state, sa$defs, sa$defs)
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
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 24 complete.\n")
