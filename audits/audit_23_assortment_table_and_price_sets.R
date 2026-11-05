# ==============================================================================
# audits/audit_23_assortment_table_and_price_sets.R
#
# Purpose : Phase 4c (2026-10-13): one assortment table for base and custom
#           products, price sets A and B with a state per product in B, and
#           matrices whose rows and columns can be edited. Checks the helpers
#           in R/helpers_products.R and R/helpers_price_matrix.R and that the
#           backend runs B with its own lengths.
#
# Checks  :
#  1.  The base ProductData as an assortment table and back
#      (defs_from_products() -> products_from_defs()) is identical to the
#      ProductData and LengthClasses files, row order included
#  2.  Default limits under bark (optBuck's example.hpr): spruce sawlog max
#      top 60 / butt 65 cm, pine sawlog 50 / 60, pulpwood 70 / 70, waste no
#      limit; minimum top diameters unchanged (12, 5, 0 cm);
#      pre_phase4c_limits() sets the base maxima back to Inf only
#  3.  AllowAsFirstLog / AllowAsTopLog are gone from the ProductData file and
#      the live R code
#  4.  Priority is derived: matrix-priced 1, flat-priced commercial 2,
#      waste 99, also for a custom flat assortment
#  5.  Lengths: a matrix-priced product's are its matrix columns (a column
#      removed is a length not allowed); a flat-priced product's are min..max
#      by 10 cm (Phase 4d: no step setting)
#  6.  Rows and columns: classes added and removed at the ends with the
#      neighbour's prices; lengths added (nearest column's prices) and
#      removed; the last class or length cannot be removed; a length that is
#      already there, not whole dm or not positive is refused; a class
#      above the max top diameter is refused (2026-10-22)
#  7.  Validation: a gap in the diameter classes is an error; classes that
#      start above the minimum top diameter give a warning
#  8.  Prices of two scenarios (Phase 4d: independent scenarios replace the
#      Phase 4c B states): scenarios_differ() is FALSE for a copy of A, TRUE
#      for a changed price, a removed length column or a missing product
#  9.  Scenario xlsx (Phase 4d: scenario_export() -> scenario_import(), one
#      scenario per file): table and prices come back identical; a file
#      without the assortments sheet is refused
# 10.  Backend: A with the explicit table, products and price file gives
#      results identical to the default run; a comparison where B drops
#      31 dm from both sawlog matrices has no 31 dm logs in B and the same
#      A as the single run (100-tree list)
#
# Verdict : PASS / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/assortments/audit_23_assortment_table_and_price_sets.csv
#   outputs/assortments/audit_23_assortment_table_and_price_sets.txt
#
# Usage   : Rscript audits/audit_23_assortment_table_and_price_sets.R
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

suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)
source(file.path(root_dir, "audits", "helpers_audit.R"))

source(file.path(root_dir, "scripts", "performance_tree_lists.R"))

out_dir <- path_output("assortments", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_23_assortment_table_and_price_sets.txt")
audit_csv <- file.path(out_dir, "audit_23_assortment_table_and_price_sets.csv")
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
cat("  Audit: assortment table and price sets A and B\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

pd0  <- load_products(root_dir); lc0 <- load_length_classes(root_dir)
pm0  <- pm_minimal(load_price_matrix_long(root_dir))
defs <- defs_from_products(pd0)
keys <- assortment_table_keys(defs)
errmsg <- function(expr) tryCatch({ expr; "" }, error = function(e) conditionMessage(e))


# ==============================================================================
section("01. Round trip of the base products")
p1 <- products_from_defs(defs, pm0)
rec("Check 01: Base ProductData and LengthClasses through the assortment table",
    ok(identical(p1$pd, pd0) && identical(p1$lc, lc0)),
    sprintf("ProductData identical: %s | LengthClasses identical: %s | keys: %s",
            identical(p1$pd, pd0), identical(p1$lc, lc0), paste(keys, collapse = ", ")))


# ==============================================================================
section("02. Default diameter limits")
lim <- function(k, col) pd0[[col]][pd0$ProductKey == k] / 10
exp2 <- list(spruce_sawlog = c(12, 60, 65), pine_sawlog = c(12, 50, 60),
             spruce_pulpwood = c(5, 70, 70), pine_pulpwood = c(5, 70, 70),
             spruce_waste = c(0, Inf, Inf), pine_waste = c(0, Inf, Inf))
got2 <- lapply(names(exp2), function(k)
  c(lim(k, "MinTopDiameter"), lim(k, "MaxTopDiameter"), lim(k, "MaxButtDiameter")))
names(got2) <- names(exp2)
pre <- pre_phase4c_limits(pd0)
pre_ok <- all(is.infinite(pre$MaxTopDiameter)) && all(is.infinite(pre$MaxButtDiameter)) &&
  identical(pre[, setdiff(names(pre), c("MaxTopDiameter", "MaxButtDiameter"))],
            pd0[, setdiff(names(pd0), c("MaxTopDiameter", "MaxButtDiameter"))])
rec("Check 02: Default limits (min top / max top / max butt, cm) and pre_phase4c_limits()",
    ok(identical(got2, exp2) && pre_ok),
    paste(c(sprintf("%s %s", names(got2), vapply(got2, paste, "", collapse = "/")),
            sprintf("pre-Phase-4c maxima all Inf, rest unchanged: %s", pre_ok)), collapse = " | "))


# ==============================================================================
section("03. AllowAsFirstLog / AllowAsTopLog removed")
csv_cols <- names(read.csv(file.path(root_dir, "inst", "extdata", "inputs", "ProductData_spruce_pine_default.csv"),
                           nrows = 1L, check.names = FALSE))
live <- c(list.files(file.path(root_dir, "R"), "\\.R$", full.names = TRUE),
          list.files(file.path(root_dir, "inst", "app"), "\\.R$", full.names = TRUE),
          list.files(file.path(root_dir, "scripts", "app_backend"), "\\.R$", full.names = TRUE))
hits <- live[vapply(live, function(f) any(grepl("AllowAs(First|Top)Log", readLines(f, warn = FALSE))),
                    logical(1))]
rec("Check 03: AllowAs columns gone from the ProductData file and the live code",
    ok(!any(grepl("^AllowAs", csv_cols)) && length(hits) == 0L),
    sprintf("in the CSV: %s | live files mentioning them: %s",
            any(grepl("^AllowAs", csv_cols)), if (length(hits)) paste(basename(hits), collapse = ", ") else "none"))


# ==============================================================================
section("04. Derived priority")
custom <- .type_assortment_table(data.frame(
  Name = c("Poles", "Chips"), Type = c("poles", "chips"), Species = c("spruce", "pine"),
  MinTopDiameter_cm = c(12, 0), MaxTopDiameter_cm = c(30, Inf),
  MinButtDiameter_cm = 0, MaxButtDiameter_cm = Inf,
  MinLength_cm = c(600, 200), MaxLength_cm = c(1000, 400),
  DiameterTopPosition_cm = 0, VolumeDiameterCategory = "All diameters (solid volume)",
  PriceType = c("matrix", "flat"), stringsAsFactors = FALSE))
defs4 <- rbind(defs, custom)
pm4 <- pm_minimal(rbind(pm0, pm_start_prices("spruce_poles", "matrix", 900, 12, 30, c(600, 800, 1000)),
                        pm_start_prices("pine_chips", "flat", 300)))
p4 <- products_from_defs(defs4, pm4)$pd
pri <- setNames(p4$Priority, p4$ProductKey)
exp4 <- c(spruce_sawlog = 1, spruce_pulpwood = 2, pine_sawlog = 1, pine_pulpwood = 2,
          spruce_waste = 99, pine_waste = 99, spruce_poles = 1, pine_chips = 2)
rec("Check 04: Priority matrix 1, flat 2, waste 99 (custom included)",
    ok(identical(pri, exp4)),
    paste(sprintf("%s %g", names(pri), pri), collapse = ", "))


# ==============================================================================
section("05. Lengths")
pm5 <- pm_remove_length(pm4, "spruce_poles", 80)
l5 <- assortment_lengths(defs4, pm5)
ok5 <- identical(l5$spruce_poles, c(600, 1000)) && identical(l5$pine_chips, seq(200, 400, by = 10)) &&
  identical(l5$spruce_sawlog, seq(310, 550, by = 30)) && identical(l5$spruce_pulpwood, seq(300, 550, by = 10))
v5 <- check_price_set(defs4, pm5)
rec("Check 05: Matrix lengths from the columns, flat lengths from min/max by 10 cm",
    ok(ok5 && isTRUE(v5$ok)),
    sprintf("poles after removing 80 dm: %s | chips: %s-%s by 10 | spruce sawlog: %d lengths | price set ok: %s",
            paste(l5$spruce_poles, collapse = ","), min(l5$pine_chips), max(l5$pine_chips),
            length(l5$spruce_sawlog), isTRUE(v5$ok)))


# ==============================================================================
section("06. Rows and columns")
cls <- function(pm, id) sort(unique(pm$PriceDiameterClass_cm[pm$ProductKey == id & !is.na(pm$LengthClass_dm)]))
lns <- function(pm, id) sort(unique(pm$LengthClass_dm[pm$ProductKey == id & !is.na(pm$LengthClass_dm)]))
price <- function(pm, id, d, l) pm$PriceNOKperM3[pm$ProductKey == id & pm$PriceDiameterClass_cm %in% d &
                                                   pm$LengthClass_dm %in% l]
a <- pm_add_class(pm0, "pine_sawlog", "high"); b <- pm_add_class(a, "pine_sawlog", "low")
c6 <- pm_add_length(b, "pine_sawlog", 58); d6 <- pm_add_length(c6, "pine_sawlog", 32)
ok6a <- identical(cls(b, "pine_sawlog"), 11:51) &&
  identical(price(b, "pine_sawlog", 51, 55), price(pm0, "pine_sawlog", 50, 55)) &&
  identical(price(b, "pine_sawlog", 11, 31), price(pm0, "pine_sawlog", 12, 31)) &&
  identical(price(c6, "pine_sawlog", 30, 58), price(pm0, "pine_sawlog", 30, 55)) &&
  identical(price(d6, "pine_sawlog", 30, 32), price(pm0, "pine_sawlog", 30, 31))
back <- pm_remove_length(pm_remove_length(pm_remove_class(pm_remove_class(d6, "pine_sawlog", "high"),
                                                          "pine_sawlog", "low"), "pine_sawlog", 58),
                         "pine_sawlog", 32)
one <- pm0[!(pm0$ProductKey == "pine_sawlog" & (pm0$PriceDiameterClass_cm > 12 | pm0$LengthClass_dm > 31)), ]
e6 <- c(last_class = errmsg(pm_remove_class(one, "pine_sawlog", "low")),
        last_length = errmsg(pm_remove_length(one, "pine_sawlog", 31)),
        existing = errmsg(pm_add_length(pm0, "pine_sawlog", 34)),
        fraction = errmsg(pm_add_length(pm0, "pine_sawlog", 33.5)),
        zero = errmsg(pm_add_length(pm0, "pine_sawlog", 0)),
        # 2026-10-22: no class above the max top diameter (50.9 -> 50 is the largest)
        above_max_top = errmsg(pm_add_class(pm0, "pine_sawlog", "high", max_top_cm = 50.9)))
max_ok <- identical(max(cls(pm_add_class(pm0, "pine_sawlog", "high", max_top_cm = 51), "pine_sawlog")), 51L)
rec("Check 06: Classes at the ends, lengths anywhere, neighbour prices, refusals",
    ok(ok6a && identical(back, pm0) && all(nzchar(e6)) && max_ok),
    sprintf("classes after adding: %d-%d | copies right: %s | back to the start: %s | refused: %s",
            min(cls(b, "pine_sawlog")), max(cls(b, "pine_sawlog")), ok6a, identical(back, pm0),
            paste(sprintf("%s ('%s')", names(e6), e6), collapse = "; ")))


# ==============================================================================
section("07. Validation of classes")
gap <- pm0[!(pm0$ProductKey == "spruce_sawlog" & pm0$PriceDiameterClass_cm %in% 30), ]
v7a <- check_price_set(defs, gap)
v7b <- check_price_set(defs, pm_remove_class(pm0, "spruce_sawlog", "low"))
rec("Check 07: Class gap is an error; classes above the minimum top diameter warn",
    ok(!isTRUE(v7a$ok) && any(grepl("consecutive whole cm", v7a$errors, fixed = TRUE)) &&
         isTRUE(v7b$ok) && any(grepl("logs below 13 cm are unpriced", v7b$warnings, fixed = TRUE))),
    sprintf("gap: ok %s, '%s' | smallest class 13: ok %s, '%s'", isTRUE(v7a$ok),
            paste(v7a$errors, collapse = "; "), isTRUE(v7b$ok), paste(v7b$warnings, collapse = "; ")))


# ==============================================================================
section("08. Prices of two scenarios")
pm_px <- pm0; i8 <- which(pm_px$ProductKey == "spruce_sawlog")[1]
pm_px$PriceNOKperM3[i8] <- pm_px$PriceNOKperM3[i8] + 1
pm_ax <- pm_remove_length(pm0, "spruce_sawlog", 31)
defs_np <- defs[keys != "pine_pulpwood", , drop = FALSE]
d8 <- c(copy = scenarios_differ(defs, pm0, defs, pm_minimal(pm0)),
        price = scenarios_differ(defs, pm0, defs, pm_px),
        axes = scenarios_differ(defs, pm0, defs, pm_ax),
        missing = scenarios_differ(defs, pm0, defs_np, pm0[pm0$ProductKey != "pine_pulpwood", ]))
ok8 <- identical(d8, c(copy = FALSE, price = TRUE, axes = TRUE, missing = TRUE)) &&
  !31L %in% lns(pm_ax, "spruce_sawlog")
rec("Check 08: scenarios_differ() on prices, columns and products",
    ok(ok8), paste(sprintf("%s %s", names(d8), d8), collapse = ", "))


# ==============================================================================
section("09. Scenario xlsx round trip")
f9 <- tempfile(fileext = ".xlsx")
scenario_export(defs4, pm4, f9)
im <- scenario_import(f9)
bad <- tempfile(fileext = ".xlsx")
writexl::write_xlsx(list(prices = pm4), bad)
im_bad <- scenario_import(bad)
ok9 <- isTRUE(im$ok) && identical(im$defs, defs4) && identical(pm_minimal(im$pm), pm_minimal(pm4)) &&
  !isTRUE(im_bad$ok)
rec("Check 09: Scenario xlsx export -> import",
    ok(ok9), sprintf("ok %s | table %s | prices %s | file without assortments refused: %s ('%s')",
                     isTRUE(im$ok), identical(im$defs, defs4),
                     identical(pm_minimal(im$pm), pm_minimal(pm4)),
                     !isTRUE(im_bad$ok), paste(im_bad$errors, collapse = "; ")))


# ==============================================================================
section("10. Backend with explicit products and B's own lengths")
tl <- perf_tree_list(100L)
TABLES <- c("selected_logs_all", "tree_summary", "per_m3_summary", "per_m3_by_assortment",
            "product_summary")
same_tables <- function(x, y) all(vapply(TABLES, function(k) identical(x[[k]], y[[k]]), logical(1)))
r_def <- run_tree_list_bucking(tl, "default_2021", root_dir)
f_a <- tempfile(fileext = ".csv"); pm_export(pm0, f_a)
r_exp <- run_tree_list_bucking(tl, f_a, root_dir, products = p1$pd, length_classes = p1$lc)
pm_b10 <- pm_remove_length(pm_remove_length(pm0, "spruce_sawlog", 31), "pine_sawlog", 31)
pb10 <- products_from_defs(defs, pm_b10)
f_b <- tempfile(fileext = ".csv"); pm_export(pm_b10, f_b)
r_cmp <- run_tree_list_bucking(tl, f_a, root_dir, products = p1$pd, length_classes = p1$lc,
                               scenario = list(price_matrix_path = f_b, products = pb10$pd,
                                               length_classes = pb10$lc))
saw <- function(r) r$selected_logs_all[r$selected_logs_all$ProductKey %in% c("spruce_sawlog", "pine_sawlog"), ]
n31_a <- sum(saw(r_exp)$LogLength == 310); n31_b <- sum(saw(r_cmp$scenario)$LogLength == 310)
unlink(c(f_a, f_b))
rec("Check 10: Explicit A equals the default run; B without 31 dm has no 31 dm logs",
    ok(same_tables(r_def, r_exp) && same_tables(r_exp, r_cmp) && n31_a > 0L && n31_b == 0L),
    sprintf("explicit = default: %s | comparison A = single run: %s | 31 dm sawlogs: A %d, B %d",
            same_tables(r_def, r_exp), same_tables(r_exp, r_cmp), n31_a, n31_b))


# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 23 complete.\n")
