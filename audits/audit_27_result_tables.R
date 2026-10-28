# ==============================================================================
# audits/audit_27_result_tables.R
#
# Purpose : Result tables (2026-10-22, R/helpers_result_tables.R): the log
#           tally per price matrix (stocknota) and value by DBH class, on a
#           100-tree list run (scripts/performance_tree_lists.R) at the
#           default prices (A) and with spruce sawlog capped at 20 cm and x1.3
#           (B), plus small synthetic tallies for B - A.
#
# Checks  :
#  1.  Tally totals: logs per assortment equal product_summary (non-waste),
#      volume per assortment equals per_m3_by_assortment, the grand total
#      equals the Total commercial volume; waste is not tallied
#  2.  Matrix grid: axes equal the price grid's (pm_to_grid()); NA exactly at
#      cells without a price and without logs; row and column totals; the
#      grid's cells add up to the tally
#  3.  Price cap: in B (spruce sawlog matrix up to 20 cm) logs above 20 cm
#      count in the 20 cm row, as priced; no row above 20
#  4.  Flat-priced: one "All diameters" row per length class with the
#      assortment's logs and volume
#  5.  B - A: equals B's grid minus A's on common axes; an assortment only in
#      A is shown against zero; matrix-priced in A and flat in B is compared
#      per length class
#  6.  DBH classes: limits (4.9 -> 0-4.9, 5 -> 5-9.9, 59.9 -> 55-59.9, 60 and
#      75 -> >= 60); per-species totals equal per_m3_summary (trees, volume,
#      value, NOK/m3); classes add up to the totals; NOK/m3 = value / volume
#  7.  Downloads: the xlsx has one sheet per assortment (A, B and B - A
#      blocks on common axes, logs and volume) and a "long" sheet equal to
#      log_tally_long(); its grand totals equal the tally
#  8.  Different assortments (2026-10-22): with each scenario's keys, an
#      assortment only A has gets A's grids only (no B, no B - A); shared
#      ones keep A, B and B - A
#  9.  Type groups: type_group() maps sawlog, pulpwood, custom types and
#      waste to Sawlog, Pulpwood, Other and NA; compare_by_type() per group
#      equals the logs' sums, its groups plus waste add up to the run's
#      value and commercial volume, a custom type counts in Other;
#      compare_per_m3(by = "type") keeps the scope totals and its group
#      contributions add up to each scope's NOK/m3
# 10.  Products table (2026-10-23): assortment_summary() for one run and A/B:
#      logs, price volume and value equal product_summary; NOK/m3
#      contributions add up to the Total NOK/m3 and volume shares to 1; an
#      assortment a scenario does not have is NA, one without logs 0
# 11.  Log length distribution (2026-10-28): log_length_distribution() per
#      length class equals the tally Total row summed over the non-waste keys
#      (A and B); waste is excluded
#
# Verdict : PASS / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/result_tables/audit_27_result_tables.csv
#   outputs/result_tables/audit_27_result_tables.txt
#
# Usage   : Rscript audits/audit_27_result_tables.R
# ==============================================================================

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

out_dir <- path_output("result_tables", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_27_result_tables.txt")
audit_csv <- file.path(out_dir, "audit_27_result_tables.csv")
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
cat("  Audit: result tables (log tally, DBH classes)\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

TOL <- 1e-9
pm_a <- pm_minimal(load_price_matrix_long(root_dir))
# B: spruce sawlog only up to 20 cm (larger logs priced at 20), x 1.3.
pm_b <- pm_a[!(pm_a$ProductKey == "spruce_sawlog" & !is.na(pm_a$PriceDiameterClass_cm) &
               pm_a$PriceDiameterClass_cm > 20), ]
saw <- pm_b$ProductKey == "spruce_sawlog"
pm_b$PriceNOKperM3[saw] <- pm_b$PriceNOKperM3[saw] * 1.3
f_b <- tempfile(fileext = ".csv"); pm_export(pm_b, f_b)
r <- run_tree_list_bucking(perf_tree_list(100L), root = root_dir, workers = 1L,
                           scenario = list(price_matrix_path = f_b))
rb <- r$scenario
ta <- log_tally(r$selected_logs_all, pm_a)
tb <- log_tally(rb$selected_logs_all, pm_b)
sum_by <- function(x, by) tapply(x, by, sum)


# ==============================================================================
section("01. Tally totals")
ps <- r$product_summary; pa <- r$per_m3_by_assortment
ps_nw <- ps[ps$ProductType != "waste", ]
logs_ok <- identical(as.integer(sum_by(ta$logs, ta$ProductKey)[ps_nw$ProductKey]), as.integer(ps_nw$n_logs))
vol_t <- sum_by(ta$volume_m3, ta$ProductKey)
pa_sp <- pa[pa$scope != "Total", ]
vol_ok <- isTRUE(all.equal(as.numeric(vol_t[pa_sp$assortment]), pa_sp$commercial_volume_m3, tolerance = TOL))
tot_ok <- isTRUE(all.equal(sum(ta$volume_m3), r$per_m3_summary$commercial_volume_m3[r$per_m3_summary$scope == "Total"],
                           tolerance = TOL))
waste_in_run <- sum(r$selected_logs_all$ProductType == "waste")
# A waste log (none in this run) is not tallied.
sl_w <- r$selected_logs_all[c(1L, 1L), ]
sl_w$ProductKey[2L] <- "spruce_waste"; sl_w$ProductType[2L] <- "waste"; sl_w$commercial_volume_m3[2L] <- 0
tw <- log_tally(sl_w, pm_a)
waste_ok <- !any(tw$ProductKey == "spruce_waste") && sum(tw$logs) == 1L
ok1 <- logs_ok && vol_ok && tot_ok && !any(ta$ProductType == "waste") && waste_ok &&
  identical(sum(ta$logs), nrow(r$selected_logs_all) - waste_in_run)
rec("Check 01: The tally's logs and volume per assortment and in total equal the run's; waste is not tallied",
    ok(ok1), sprintf("%d tally rows, %d logs (run: %d, of which %d waste) | logs per assortment %s | volume per assortment %s | grand total %.4f m3 = Total %s | a waste log is not tallied %s",
                     nrow(ta), sum(ta$logs), nrow(r$selected_logs_all), waste_in_run, logs_ok, vol_ok,
                     sum(ta$volume_m3), tot_ok, waste_ok))


# ==============================================================================
section("02. Matrix grid layout")
K <- "spruce_sawlog"
g <- log_tally_grid(ta, pm_a, K, "logs"); gv <- log_tally_grid(ta, pm_a, K, "volume")
pg <- pm_to_grid(pm_a, K)
body <- as.matrix(g[-nrow(g), -c(1L, ncol(g))])
t_k <- ta[ta$ProductKey == K, ]
m_logs <- matrix(0, nrow(pg), ncol(pg), dimnames = dimnames(pg))
m_logs[cbind(match(t_k$diameter_class_cm, rownames(pg)), match(t_k$length_class_dm, names(pg)))] <- t_k$logs
na_ok <- identical(unname(is.na(body)), unname(is.na(as.matrix(pg)) & m_logs == 0))
axes_ok <- identical(g[[1L]][-nrow(g)], rownames(pg)) && identical(names(g)[-c(1L, ncol(g))], names(pg))
cells_ok <- isTRUE(all.equal(unname(replace(body, is.na(body), 0)), unname(m_logs)))
totals_ok <- isTRUE(all.equal(g$Total[-nrow(g)], unname(rowSums(m_logs)))) &&
  isTRUE(all.equal(unlist(g[nrow(g), -1L], use.names = FALSE), c(unname(colSums(m_logs)), sum(m_logs)))) &&
  isTRUE(all.equal(gv$Total[nrow(gv)], sum(t_k$volume_m3), tolerance = TOL))
# An unpriced cell (the 50 cm x 31 dm cell removed, no logs there) is NA.
pm_na <- pm_a[!(pm_a$ProductKey == K & pm_a$PriceDiameterClass_cm %in% 50L & pm_a$LengthClass_dm %in% 31L), ]
g_na <- log_tally_grid(ta, pm_na, K, "logs")
unpriced_ok <- is.na(g_na[g_na[[1L]] == "50", "31"]) && sum(is.na(as.matrix(g_na[-nrow(g_na), -c(1L, ncol(g_na))]))) == 1L
ok2 <- axes_ok && na_ok && cells_ok && totals_ok && unpriced_ok
rec("Check 02: A matrix-priced tally is laid out on its price grid; NA only at unpriced cells; totals",
    ok(ok2), sprintf("%s: %d x %d grid (price grid %d x %d) | axes %s | NA cells %d (unpriced, no logs) %s | an unpriced cell is NA %s | cells %s | totals %s | %d logs, %.4f m3",
                     K, nrow(body), ncol(body), nrow(pg), ncol(pg), axes_ok, sum(is.na(body)), na_ok, unpriced_ok,
                     cells_ok, totals_ok, sum(t_k$logs), sum(t_k$volume_m3)))


# ==============================================================================
section("03. Price cap: logs above the largest class")
slb <- rb$selected_logs_all[rb$selected_logs_all$ProductKey == K, ]
above <- sum(slb$PriceDiameterClass_cm_uncapped > 20)
gb <- log_tally_grid(tb, pm_b, K, "logs")
ok3 <- above > 0L && max(as.integer(gb[[1L]][-nrow(gb)])) == 20L &&
  gb$Total[gb[[1L]] == "20"] == sum(slb$PriceDiameterClass_cm_used == 20) &&
  gb$Total[gb[[1L]] == "20"] >= above
rec("Check 03: Logs above the largest price class count in it (as priced)",
    ok(ok3), sprintf("B %s: %d logs above 20 cm uncapped | rows up to %s cm | 20 cm row: %s logs",
                     K, above, max(as.integer(gb[[1L]][-nrow(gb)])), gb$Total[gb[[1L]] == "20"]))


# ==============================================================================
section("04. Flat-priced assortments")
KF <- "spruce_pulpwood"
gf <- log_tally_grid(ta, pm_a, KF, "logs"); gfv <- log_tally_grid(ta, pm_a, KF, "volume")
tf <- ta[ta$ProductKey == KF, ]
lens <- sort(unique(tf$length_class_dm))
ok4 <- identical(gf[[1L]], c("All diameters", "Total")) &&
  identical(names(gf)[-c(1L, ncol(gf))], as.character(lens)) &&
  identical(unname(unlist(gf[1L, as.character(lens)])), as.numeric(sum_by(tf$logs, tf$length_class_dm)[as.character(lens)])) &&
  isTRUE(all.equal(gfv$Total[1L], sum(tf$volume_m3), tolerance = TOL)) && all(is.na(tf$diameter_class_cm))
rec("Check 04: A flat-priced assortment is tallied per length class in one row",
    ok(ok4), sprintf("%s: %d length classes (%s-%s dm), %d logs, %.4f m3", KF, length(lens), min(lens), max(lens),
                     sum(tf$logs), sum(tf$volume_m3)))


# ==============================================================================
section("05. B - A")
# Common axes (both scenarios' 12-19 cm rows): B minus A cell by cell.
d <- log_tally_diff_grid(ta, pm_a, tb, pm_b, K, "logs")
ga_all <- log_tally_grid(ta, pm_a, K, "logs"); gb_all <- log_tally_grid(tb, pm_b, K, "logs")
rows <- intersect(ga_all[[1L]], gb_all[[1L]]); rows <- setdiff(rows, c("Total", "20"))   # 20: B's capped row
cols <- intersect(names(ga_all), names(gb_all))[-1L]
z <- function(x) { x[is.na(x)] <- 0; x }
pick <- function(gr) as.matrix(gr[match(rows, gr[[1L]]), cols])
diff_ok <- isTRUE(all.equal(z(pick(d)), z(pick(gb_all)) - z(pick(ga_all))))
total_ok <- d$Total[nrow(d)] == sum(tb$logs[tb$ProductKey == K]) - sum(ta$logs[ta$ProductKey == K])
# Synthetic: an assortment only in A, and one matrix-priced in A but flat in B.
syn <- function(key, d, l, n) data.frame(ProductKey = key, ProductType = "sawlog", species_group = "spruce",
                                         priced = ifelse(is.na(d), "flat", "matrix"), diameter_class_cm = d,
                                         length_class_dm = l, logs = n, volume_m3 = n / 10, stringsAsFactors = FALSE)
pm_s <- data.frame(ProductKey = "x", LengthClass_dm = c(40L, 40L, 50L, 50L), PriceDiameterClass_cm = c(20L, 21L, 20L, 21L),
                   PriceNOKperM3 = c(500, 510, 520, NA))
sa <- syn("x", c(20L, 21L, 20L), c(40L, 40L, 50L), c(2L, 1L, 3L))
only_a <- log_tally_diff_grid(sa, pm_s, sa[0, ], pm_s[0, ], "x", "logs")
only_ok <- identical(only_a$Total[nrow(only_a)], -6) && identical(only_a[[1L]][-nrow(only_a)], c("20", "21"))
pm_flat <- data.frame(ProductKey = "x", LengthClass_dm = NA_integer_, PriceDiameterClass_cm = NA_integer_, PriceNOKperM3 = 300)
sb <- syn("x", c(NA, NA), c(40L, 50L), c(4L, 1L))
mixed <- log_tally_diff_grid(sa, pm_s, sb, pm_flat, "x", "logs")
mixed_ok <- identical(mixed[[1L]], c("All diameters", "Total")) &&
  identical(unname(unlist(mixed[1L, c("40", "50")])), c(1, -2))
ok5 <- diff_ok && total_ok && only_ok && mixed_ok
rec("Check 05: B - A is B's grid minus A's; one-sided and matrix-vs-flat assortments",
    ok(ok5), sprintf("%s: %d x %d common cells equal B - A %s, total %+d %s | only in A: total %s, rows %s | matrix in A, flat in B: per length %s",
                     K, length(rows), length(cols), diff_ok, as.integer(d$Total[nrow(d)]), total_ok,
                     only_a$Total[nrow(only_a)], paste(only_a[[1L]][-nrow(only_a)], collapse = ","),
                     paste(unlist(mixed[1L, -1L]), collapse = "/")))


# ==============================================================================
section("06. Value by DBH class")
lab <- dbh_class(c(4.9, 5, 9.9, 59.9, 60, 75))$label
lab_ok <- identical(lab, c("0–4.9", "5–9.9", "5–9.9", "55–59.9", "≥ 60", "≥ 60"))
db <- dbh_class_summary(r$tree_summary)
pm3 <- r$per_m3_summary
tot <- db[db$dbh_class == "All", ]
pm3s <- pm3[match(tot$species_group, pm3$scope), ]
tot_ok6 <- identical(as.integer(tot$trees), as.integer(pm3s$n_trees)) &&
  isTRUE(all.equal(tot$commercial_volume_m3, pm3s$commercial_volume_m3, tolerance = TOL)) &&
  isTRUE(all.equal(tot$value_nok, pm3s$value_nok, tolerance = 1e-6)) &&
  isTRUE(all.equal(tot$nok_per_m3, pm3s$nok_per_m3, tolerance = 1e-9))
cls <- db[db$dbh_class != "All", ]
add_ok <- all(vapply(tot$species_group, function(s)
  sum(cls$trees[cls$species_group == s]) == tot$trees[tot$species_group == s] &&
    isTRUE(all.equal(sum(cls$value_nok[cls$species_group == s]), tot$value_nok[tot$species_group == s])), logical(1)))
nok_ok <- isTRUE(all.equal(cls$nok_per_m3, cls$value_nok / cls$commercial_volume_m3))
ok6 <- lab_ok && tot_ok6 && add_ok && nok_ok
rec("Check 06: DBH classes: limits, species totals equal per_m3_summary, classes add up, NOK/m3 = value / volume",
    ok(ok6), sprintf("labels %s | %d classes (%s) | totals = per_m3_summary %s | classes add up %s | NOK/m3 %s",
                     paste(lab, collapse = ", "), nrow(cls), paste(unique(cls$dbh_class), collapse = " "),
                     tot_ok6, add_ok, nok_ok))


# ==============================================================================
section("07. Downloads")
fx <- tempfile(fileext = ".xlsx")
log_tally_xlsx(fx, list(A = ta, B = tb), list(A = pm_a, B = pm_b), label = function(k) gsub("_", " ", k))
sh <- readxl::excel_sheets(fx)
long <- as.data.frame(readxl::read_xlsx(fx, "long"))
want_long <- log_tally_long(list(A = ta, B = tb))
long_ok <- identical(dim(long), dim(want_long)) &&
  isTRUE(all.equal(long$`Volume (m3 u.b.)`, want_long$`Volume (m3 u.b.)`, tolerance = TOL)) &&
  identical(as.integer(long$Logs), want_long$Logs)
s1 <- as.data.frame(readxl::read_xlsx(fx, "spruce sawlog"))
tables <- unique(s1$Table)
gt <- function(tb_name) s1$Total[s1$Table == tb_name & s1$`Diameter (cm)` == "Total"]
sheet_ok <- identical(tables, c("A: Logs", "B: Logs", "A: Volume (m³ u.b.)", "B: Volume (m³ u.b.)",
                                "B - A: Logs", "B - A: Volume (m³ u.b.)")) &&
  gt("A: Logs") == sum(ta$logs[ta$ProductKey == K]) && gt("B: Logs") == sum(tb$logs[tb$ProductKey == K]) &&
  gt("B - A: Logs") == gt("B: Logs") - gt("A: Logs")
ok7 <- setequal(sh, c(gsub("_", " ", log_tally_keys(ta, tb)), "long")) && long_ok && sheet_ok
rec("Check 07: The xlsx has a sheet per assortment (A, B, B - A on common axes) and the long table",
    ok(ok7), sprintf("sheets: %s | long sheet = log_tally_long() %s (%d rows) | spruce sawlog blocks: %s | grand totals A %s, B %s, B - A %s",
                     paste(sh, collapse = ", "), long_ok, nrow(long), paste(tables, collapse = " / "),
                     gt("A: Logs"), gt("B: Logs"), gt("B - A: Logs")))


# ==============================================================================
section("08. Downloads with different assortments")
K2 <- setdiff(log_tally_keys(ta, tb), K)[1L]       # treated as only in A
fx2 <- tempfile(fileext = ".xlsx")
keys_ab <- list(A = log_tally_keys(ta, tb), B = setdiff(log_tally_keys(ta, tb), K2))
log_tally_xlsx(fx2, list(A = ta, B = tb), list(A = pm_a, B = pm_b), label = function(k) gsub("_", " ", k),
               scenario_keys = keys_ab)
t_only <- unique(as.data.frame(readxl::read_xlsx(fx2, gsub("_", " ", K2)))$Table)
t_shared <- unique(as.data.frame(readxl::read_xlsx(fx2, gsub("_", " ", K)))$Table)
ok8 <- identical(t_only, c("A: Logs", "A: Volume (m³ u.b.)")) && identical(t_shared, tables)
rec("Check 08: With different assortments the xlsx has B - A only for shared assortments",
    ok(ok8), sprintf("%s (A only): %s | %s (shared): %s", K2, paste(t_only, collapse = " / "),
                     K, paste(t_shared, collapse = " / ")))
unlink(c(fx, fx2, f_b))


# ==============================================================================
section("09. Type groups")
tg <- type_group(c("sawlog", "pulpwood", "poles", "waste", NA))
tg_ok <- identical(tg, c("Sawlog", "Pulpwood", "Other", NA, NA))
# B with one commercial assortment relabelled as a custom type (Other).
rb2 <- rb; ko <- unique(rb2$selected_logs_all$ProductKey[rb2$selected_logs_all$ProductType == "pulpwood"])[1L]
rb2$selected_logs_all$ProductType[rb2$selected_logs_all$ProductKey == ko] <- "poles"
ct <- compare_by_type(r, rb2)
by_logs <- function(l, grp) { g <- type_group(l$ProductType); k <- g %in% grp
  c(value = sum(l$value_nok[k]), comm = sum(l$commercial_volume_m3[k]), price = sum(l$price_volume_m3[k]), n = sum(k)) }
grp_ok <- all(vapply(seq_len(nrow(ct)), function(i) {
  a <- by_logs(r$selected_logs_all, ct$type[i]); b <- by_logs(rb2$selected_logs_all, ct$type[i])
  isTRUE(all.equal(unname(c(ct$value_a[i], ct$comm_vol_a[i], ct$price_vol_a[i], ct$logs_a[i])), unname(a))) &&
    isTRUE(all.equal(unname(c(ct$value_b[i], ct$comm_vol_b[i], ct$price_vol_b[i], ct$logs_b[i])), unname(b)))
}, logical(1)))
wa <- r$selected_logs_all$ProductType == "waste"
tot9 <- r$per_m3_summary[r$per_m3_summary$scope == "Total", ]
sum_ok <- isTRUE(all.equal(sum(ct$value_a) + sum(r$selected_logs_all$value_nok[wa]), tot9$value_nok)) &&
  isTRUE(all.equal(sum(ct$comm_vol_a) + sum(r$selected_logs_all$commercial_volume_m3[wa]), tot9$commercial_volume_m3)) &&
  isTRUE(all.equal(sum(ct$share_a), 1 - sum(r$selected_logs_all$commercial_volume_m3[wa]) / tot9$commercial_volume_m3))
other_ok <- "Other" %in% ct$type && ct$logs_a[ct$type == "Other"] == 0L && ct$logs_b[ct$type == "Other"] > 0L
pt <- compare_per_m3(r, rb2, by = "type")
pa <- compare_per_m3(r, rb2)
all_ok <- identical(pt[pt$assortment == "All", ], pa[pa$assortment == "All", ], ignore.environment = TRUE) ||
  isTRUE(all.equal(pt[pt$assortment == "All", ], pa[pa$assortment == "All", ], check.attributes = FALSE))
contrib_ok <- all(vapply(unique(pt$scope), function(s) {
  x <- pt[pt$scope == s, ]; all1 <- x[x$assortment == "All", ]; g <- x[x$assortment != "All", ]
  all(g$assortment %in% ASSORTMENT_TYPE_GROUPS) &&
    isTRUE(all.equal(sum(g$nok_per_m3_default), all1$nok_per_m3_default)) &&
    isTRUE(all.equal(sum(g$nok_per_m3_scenario), all1$nok_per_m3_scenario))
}, logical(1)))
ok9 <- tg_ok && grp_ok && sum_ok && other_ok && all_ok && contrib_ok
rec("Check 09: Type groups: mapping, sums per group and in total, Other, NOK/m3 by type",
    ok(ok9), sprintf("type_group %s | groups %s: sums = logs %s | + waste = run totals %s | %s as a custom type in B: Other A %s / B %s logs | per m3 by type: All rows kept %s, contributions add up %s",
                     tg_ok, paste(ct$type, collapse = ","), grp_ok, sum_ok, ko,
                     ct$logs_a[ct$type == "Other"], ct$logs_b[ct$type == "Other"], all_ok, contrib_ok))


# ==============================================================================
section("10. Products table (assortment_summary)")
s1 <- assortment_summary(r)
ps <- r$product_summary[match(s1$key, r$product_summary$ProductKey), ]
tot10 <- r$per_m3_summary[r$per_m3_summary$scope == "Total", ]
one_ok <- identical(s1$key, as.character(r$product_summary$ProductKey)) &&
  identical(as.integer(s1$logs_a), as.integer(ps$n_logs)) &&
  isTRUE(all.equal(s1$price_vol_a, ps$total_volume_m3, tolerance = 1e-4)) &&   # product_summary rounds to 5 decimals
  isTRUE(all.equal(s1$value_a, ps$total_value_nok, tolerance = 1e-6)) &&
  isTRUE(all.equal(sum(s1$contrib_a), tot10$nok_per_m3)) &&
  isTRUE(all.equal(sum(s1$share_a), 1)) && !any(c("logs_b", "value_b") %in% names(s1))
# A/B, with an assortment only A has (K2 dropped from B's keys) and one B
# has without logs (invented key).
keys10 <- list(A = unique(r$selected_logs_all$ProductKey),
               B = c(setdiff(unique(rb$selected_logs_all$ProductKey), K2), "spruce_no_logs"))
s2 <- assortment_summary(r, rb, keys = keys10)
tot10b <- rb$per_m3_summary[rb$per_m3_summary$scope == "Total", ]
k2 <- s2$key == K2
ab_ok <- all(c("logs_a", "logs_b", "price_vol_b", "value_b", "contrib_b", "share_b") %in% names(s2)) &&
  all(is.na(unlist(s2[k2, c("logs_b", "price_vol_b", "value_b", "contrib_b", "share_b")]))) &&
  isTRUE(all.equal(sum(s2$contrib_b, na.rm = TRUE), tot10b$nok_per_m3 -
                     sum(rb$selected_logs_all$value_nok[rb$selected_logs_all$ProductKey == K2]) /
                     tot10b$commercial_volume_m3)) &&
  isTRUE(all.equal(sum(s2$contrib_a), tot10$nok_per_m3))
no_logs <- assortment_summary(r, rb, keys = list(A = c(keys10$A, "spruce_no_logs"), B = keys10$B))
nl_ok <- !"spruce_no_logs" %in% no_logs$key   # keys only appear with logs in a run
ok10 <- one_ok && ab_ok && nl_ok
rec("Check 10: assortment_summary(): sums equal product_summary, contributions add up to NOK/m3, NA when a scenario lacks the assortment",
    ok(ok10), sprintf("one run: %d assortments, sums and shapes %s, contributions %.2f = NOK/m3 %.2f | A/B: %s only in A -> B NA %s | rows only for assortments with logs %s",
                      nrow(s1), one_ok, sum(s1$contrib_a), tot10$nok_per_m3, K2, ab_ok, nl_ok))


# ==============================================================================
section("11. Log length distribution")
# The figure's data: logs per length class and type group. Per scenario the
# counts must equal the tally's Total row (logs per length class) summed over
# the non-waste assortments; waste is not counted.
len_vs_tally <- function(res, tally, pm) {
  d <- log_length_distribution(res$selected_logs_all)
  keys <- unique(tally$ProductKey)
  tot <- NULL
  for (k in keys) {
    g <- log_tally_grid(tally, pm, k, "logs"); tr_ <- g[g[[1L]] == "Total", , drop = FALSE]
    cols <- setdiff(names(tr_), c(names(g)[1L], "Total"))
    v <- setNames(as.numeric(unlist(tr_[1L, cols])), cols)
    tot <- c(tot, v)
  }
  by_len <- tapply(tot, names(tot), sum)
  fig <- tapply(d$logs, as.character(d$length_class_dm), sum)
  by_len <- by_len[by_len > 0]
  nw <- sum(res$product_summary$n_logs[res$product_summary$ProductType != "waste"])
  list(same = identical(sort(names(by_len)), sort(names(fig))) &&
         all(as.numeric(by_len[names(fig)]) == as.numeric(fig)),
       total = sum(d$logs) == nw && sum(by_len) == nw,
       types = all(as.character(d$type) %in% ASSORTMENT_TYPE_GROUPS),
       n = sum(d$logs), waste = sum(res$selected_logs_all$ProductType == "waste"))
}
la <- len_vs_tally(r, ta, pm_a); lb <- len_vs_tally(rb, tb, pm_b)
# Waste is excluded: a waste log added to the run changes nothing.
sl_w <- r$selected_logs_all; sw <- sl_w[1L, ]; sw$ProductType <- "waste"
waste_ok <- identical(log_length_distribution(rbind(sl_w, sw)), log_length_distribution(sl_w))
ok11 <- la$same && la$total && la$types && lb$same && lb$total && lb$types && waste_ok
rec("Check 11: Log length distribution: logs per length class equal the tally's Total row summed over the non-waste assortments (A and B); waste excluded",
    ok(ok11), sprintf("A: per-length equal %s, total %d = non-waste logs %s | B: equal %s, total %d, %s | an added waste log changes nothing %s",
                      la$same, la$n, la$total, lb$same, lb$n, lb$total, waste_ok))


# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 27 complete.\n")
