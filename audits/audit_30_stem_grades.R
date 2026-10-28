# ==============================================================================
# audits/audit_30_stem_grades.R
#
# Purpose : Stem grades and cut at grade changes for .hpr stems (roadmap
#           Phase 6, 2026-10-28). On inputs/test_fixtures/hpr_grades_synthetic.hpr
#           (invented stems, scripts/make_hpr_grades_fixture.R):
#   1  The parser reads StemGrade/GradeValue (start, value) per stem; the grade
#      grid on the 10 cm profile grid (a start of 295 snaps to 300).
#   2  A stem without StemGrade gets the species group's StartGrade (also
#      after changing it, and a group without one: grade-free).
#   3  "Use stem grades": a product that does not permit a grade is never
#      chosen over a section with that grade (and A, without the option,
#      does choose one: the check is not vacuous).
#   4  "Cut at grade changes": no log contains a grade change in its interior
#      (A, without it, has such a log).
#   5  A product without permitted grades permits all grades; default products
#      (no PermittedGrades column) are unaffected by "Use stem grades".
#   6  Both options off: the result equals a run without grades in the
#      profiles; the options are independent of each other.
#   7  Serial = workers with the options on.
#   8  PermittedGrades survives defs <-> products and the scenario xlsx;
#      .prepare_runs() groups runs by options (and by grade sets only when
#      grades are used); scenarios_differ() sees the options.
#   9  Several files: the grades of the second file are its own.
#
# Outputs : outputs/stem_grades/audit_30_stem_grades.{csv,txt}
# Usage   : Rscript audits/audit_30_stem_grades.R
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

out_dir <- path_output("stem_grades", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_30_stem_grades.txt")
audit_csv <- file.path(out_dir, "audit_30_stem_grades.csv")
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
cat("  Audit: stem grades and cut at grade changes (Phase 6)\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

FIXTURE <- file.path(root_dir, "inputs", "test_fixtures", "hpr_grades_synthetic.hpr")
MAP <- c("445" = "pine", "446" = "spruce", "447" = "none", "448" = "none")
OFF <- list(use_grades = FALSE, cut_at_changes = FALSE)
USE <- list(use_grades = TRUE,  cut_at_changes = FALSE)
CUT <- list(use_grades = FALSE, cut_at_changes = TRUE)
BOTH <- list(use_grades = TRUE, cut_at_changes = TRUE)

hp <- read_hpr(FIXTURE)
sc <- hpr_to_scenario(hp, c("445" = "pine", "446" = "spruce", "447" = "not used", "448" = "not used"))
st <- read_hpr_stems(FIXTURE)
pr <- hpr_stem_profiles(st, MAP, grades = TRUE)
pr0 <- hpr_stem_profiles(st, MAP)                       # no grades requested
pth <- tempfile(fileext = ".csv"); pm_export(sc$pm, pth)
prod <- products_from_defs(sc$defs, sc$pm)
keys <- assortment_table_keys(sc$defs)
permitted <- setNames(lapply(sc$defs$PermittedGrades, permitted_grades_set), keys)

# Scenario A (options oa) and B (options ob) on the same products and prices.
run <- function(prep, oa, ob, workers = 1L)
  run_hpr_bucking(prep, pth, products = prod$pd, length_classes = prod$lc,
                  scenario = list(price_matrix_path = pth, products = prod$pd,
                                  length_classes = prod$lc, grade_opts = ob),
                  grade_opts = oa, workers = workers)
logs_of <- function(x) { l <- x$selected_logs_all
  l <- l[order(l$treid, l$StartPos), ]; rownames(l) <- NULL; l }
grade_cells <- function(prep, id, start, stop) {       # grades of the cells [start, stop)
  p <- prep$profiles[prep$profiles$treid == id, ]
  p$grade[p$diameterPosition >= start & p$diameterPosition < stop]
}
runs_of <- function(prep, id) { p <- prep$profiles[prep$profiles$treid == id, ]; r <- rle(p$grade)
  setNames(r$lengths * 10, r$values) }

# ---- 1. the parser and the grade grid ----------------------------------------
section("1. Grades read and put on the grid")
ok1a <- length(st$gval) == 8L && identical(sort(unique(st$gidx)), c(1L, 2L, 4L, 5L)) &&
  identical(as.numeric(st$gpos[st$gidx == 2L]), c(0, 500, 800)) &&
  identical(as.numeric(st$gval[st$gidx == 2L]), c(2, 3, 2))
rec("Check 1a: 8 grades on 4 of the 5 stems, with starts and values (stem 102: 2 from 0, 3 from 500, 2 from 800)",
    ok(ok1a), sprintf("%d grades, stems with grades %s", length(st$gval), paste(sort(unique(st$gidx)), collapse = ",")))
r101 <- runs_of(pr, "101"); r102 <- runs_of(pr, "102"); r104 <- runs_of(pr, "104")
ok1b <- identical(unname(r101), c(300, 1510)) && identical(names(r101), c("7", "2")) &&
  identical(unname(r102), c(500, 300, 1010)) && identical(names(r102), c("2", "3", "2")) &&
  identical(unname(r104), c(300, 1510)) && identical(names(r104), c("2", "7"))
rec("Check 1b: grade runs on the 10 cm grid; a start at 295 cm (stem 104) snaps to 300",
    ok(ok1b), sprintf("101 %s | 102 %s | 104 %s", paste(names(r101), r101, sep = "x", collapse = " "),
                      paste(names(r102), r102, sep = "x", collapse = " "), paste(names(r104), r104, sep = "x", collapse = " ")))
ok1c <- is.null(pr0$profiles$grade) &&
  identical(pr0$profiles[, c("treid", "diameterPosition", "DiameterValue")],
            pr$profiles[, c("treid", "diameterPosition", "DiameterValue")])
rec("Check 1c: profiles carry no grade column unless grades are requested; the diameters are the same either way",
    ok(ok1c), sprintf("grade column without request: %s", !is.null(pr0$profiles$grade)))
g <- hpr_grade_grid(seq(0, 60, 10), c(5, 25), c(4, 9), NA)
rec("Check 1d: hpr_grade_grid(): starts snap half up (5 -> 10, 25 -> 30), before the first start the StartGrade (NA here), later starts win ties",
    ok(identical(g, c(NA, 4, 4, 9, 9, 9, 9)) &&
       identical(hpr_grade_grid(c(0, 10, 20), c(14, 16), c(1, 2), 5), c(5, 1, 2))),
    paste(g, collapse = " "))

# ---- 2. missing StemGrade: the StartGrade --------------------------------------
section("2. Stems without StemGrade")
g103 <- pr$profiles$grade[pr$profiles$treid == "103"]
rec("Check 2a: stem 103 has no StemGrade: grade = the species group's StartGrade (2) along the whole stem",
    ok(length(g103) > 0L && all(g103 == 2)), sprintf("grades %s", paste(unique(g103), collapse = ",")))
txt <- paste(readLines(FIXTURE, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
txt3 <- sub("(?s)(<SpeciesGroupName>GRAN</SpeciesGroupName>.*?<StartGrade>)2(</StartGrade>)", "\\13\\2", txt, perl = TRUE)
f3 <- tempfile(fileext = ".hpr"); writeLines(txt3, f3, useBytes = TRUE)
pr3 <- hpr_stem_profiles(read_hpr_stems(f3), MAP, grades = TRUE)
g103b <- pr3$profiles$grade[pr3$profiles$treid == "103"]
g101b <- pr3$profiles$grade[pr3$profiles$treid == "101"]
rec("Check 2b: with StartGrade 3 in the file stem 103 is grade 3 throughout; stems with grades are unchanged",
    ok(all(g103b == 3) && identical(g101b, pr$profiles$grade[pr$profiles$treid == "101"])),
    sprintf("103: %s", paste(unique(g103b), collapse = ",")))
txtn <- sub("(?s)(<SpeciesGroupName>GRAN</SpeciesGroupName>.*?)<StartGrade>2</StartGrade>", "\\1", txt, perl = TRUE)
fn <- tempfile(fileext = ".hpr"); writeLines(txtn, fn, useBytes = TRUE)
prn <- hpr_stem_profiles(read_hpr_stems(fn), MAP, grades = TRUE)
gn <- prn$profiles$grade[prn$profiles$treid == "103"]
rec("Check 2c: a group without StartGrade and a stem without grades: grade NA (grade-free: every product allowed)",
    ok(length(gn) > 0L && all(is.na(gn))), sprintf("%d positions, all NA: %s", length(gn), all(is.na(gn))))

# ---- 3, 4. the two options -------------------------------------------------------
section("3. Use stem grades; 4. Cut at grade changes")
rU <- run(pr, OFF, USE); rC <- run(pr, OFF, CUT); rB <- run(pr, OFF, BOTH)
lA <- logs_of(rU); lU <- logs_of(rU$scenario); lC <- logs_of(rC$scenario); lB <- logs_of(rB$scenario)
violations <- function(l, prep) sum(vapply(seq_len(nrow(l)), function(i) {
  perm <- permitted[[l$ProductKey[i]]]
  length(perm) > 0L && any(!grade_cells(prep, l$treid[i], l$StartPos[i], l$StopPos[i]) %in% perm)
}, TRUE))
spans <- function(l, prep) sum(vapply(seq_len(nrow(l)), function(i) {
  g <- grade_cells(prep, l$treid[i], l$StartPos[i], l$StopPos[i])
  length(g) > 1L && any(g[-1L] != g[-length(g)])
}, TRUE))
v_off <- violations(lA, pr); v_use <- violations(lU, pr); v_both <- violations(lB, pr)
rec("Check 3a: with Use stem grades no log takes a product that does not permit a grade it covers; without the option some do (not vacuous)",
    ok(v_use == 0L && v_both == 0L && v_off > 0L),
    sprintf("violations: off %d, use %d, use+cut %d", v_off, v_use, v_both))
ok3b <- any(lA$treid == "101" & lA$StartPos < 300 & grepl("sagt|kubb", lA$ProductKey)) &&
  !any(lU$treid == "101" & lU$StartPos < 300 & grepl("sagt|kubb", lU$ProductKey)) &&
  !any(lU$treid == "104" & lU$StartPos >= 300 & grepl("sagt|kubb", lU$ProductKey))
rec("Check 3b: stem 101 (grade 7 below 300 cm): no sawlog or Kubb below 300 cm with the option; stem 104 (grade 7 from 295 -> 300): none above 300",
    ok(ok3b), sprintf("A logs of 101 below 300: %s", paste(lA$ProductKey[lA$treid == "101" & lA$StartPos < 300], collapse = ",")))
key_rows <- function(l, ids) { x <- l[l$treid %in% ids, c("treid", "ProductKey", "StartPos", "StopPos")]
  rownames(x) <- NULL; x }
s_off <- spans(lA, pr); s_cut <- spans(lC, pr); s_both <- spans(lB, pr); s_use <- spans(lU, pr)
rec("Check 4a: with Cut at grade changes no log contains a grade change in its interior; without it some do (not vacuous)",
    ok(s_cut == 0L && s_both == 0L && s_off > 0L),
    sprintf("logs spanning a change: off %d, use only %d, cut only %d, both %d", s_off, s_use, s_cut, s_both))
rec("Check 4b: the options are independent: cut only does not apply permitted grades, use only does not force cuts; a stem without a change (103, 105) is the same in all four runs",
    ok(violations(lC, pr) > 0L && !identical(lU, lB) && !identical(lC, lA) &&
       identical(key_rows(lA, c("103", "105")), key_rows(lB, c("103", "105"))) &&
       identical(key_rows(lA, c("103", "105")), key_rows(lU, c("103", "105"))) &&
       identical(key_rows(lA, c("103", "105")), key_rows(lC, c("103", "105")))),
    sprintf("cut-only violations %d", violations(lC, pr)))

# ---- 5. no permitted set = all grades --------------------------------------------
section("5. Products without permitted grades")
spruce_mass <- "spruce_197_massevirke"
ok5a <- is.na(sc$defs$PermittedGrades[keys == spruce_mass]) && !length(permitted[[spruce_mass]]) &&
  any(lU$treid == "101" & lU$StartPos < 300 & lU$ProductKey == spruce_mass)
rec("Check 5a: '197 Massevirke' has no PermittedGradesDefinition: empty set, and it is chosen inside stem 101's grade-7 butt with the option on",
    ok(ok5a), sprintf("PermittedGrades NA: %s", is.na(sc$defs$PermittedGrades[keys == spruce_mass])))
d_none <- sc$defs; d_none$PermittedGrades <- NA_character_
rec("Check 5b: a table without permitted grades gives ProductData without a PermittedGrades column (as before Phase 6)",
    ok(!"PermittedGrades" %in% names(products_from_defs(d_none, sc$pm)$pd)))
rd <- run_hpr_bucking(pr, workers = 1L, grade_opts = USE)
rd0 <- run_hpr_bucking(pr0, workers = 1L)
rec("Check 5c: default products (no permitted grades) with Use stem grades on give the result of a run without grades",
    ok(identical(logs_of(rd), logs_of(rd0)) && identical(rd$tree_summary[order(rd$tree_summary$treid), ],
                                                          rd0$tree_summary[order(rd0$tree_summary$treid), ])))

# ---- 6. both off = no grades ---------------------------------------------------------
section("6. Both options off")
r_off <- run(pr, OFF, OFF); r_nog <- run(pr0, OFF, OFF)
same_result <- function(a, b) identical(logs_of(a), logs_of(b)) &&
  identical(a$tree_summary[order(a$tree_summary$treid), ], b$tree_summary[order(b$tree_summary$treid), ])
rec("Check 6a: options off with grades in the profiles = a run with profiles without grades (A and B)",
    ok(same_result(r_off, r_nog) && same_result(r_off$scenario, r_nog$scenario)))
rec("Check 6b: grade options without grades in the profiles stop with a message (no silent no-op)",
    ok(inherits(try(run(pr0, USE, OFF), silent = TRUE), "try-error")))

# ---- 7. serial = workers -------------------------------------------------------------
section("7. Serial and parallel")
rW <- run(pr, USE, BOTH, workers = 2L); rS <- run(pr, USE, BOTH, workers = 1L)
rec("Check 7: results with the options on are identical on 1 and 2 workers (A and B)",
    ok(same_result(rW, rS) && same_result(rW$scenario, rS$scenario)))

# ---- 8. plumbing ------------------------------------------------------------------------
section("8. PermittedGrades through the app's tables; run grouping")
back <- defs_from_products(prod$pd)
ok8a <- identical(back$PermittedGrades, sc$defs$PermittedGrades) &&
  identical(prod$pd$PermittedGrades[match("spruce_140_sca_sagt_mmer", prod$pd$ProductKey)], "1;2")
rec("Check 8a: PermittedGrades goes defs -> ProductData -> defs unchanged (e.g. '1;2')", ok(ok8a),
    paste(back$PermittedGrades, collapse = " | "))
xl <- tempfile(fileext = ".xlsx"); scenario_export(sc$defs, sc$pm, xl)
imp <- scenario_import(xl)
rec("Check 8b: the scenario xlsx round trip keeps PermittedGrades; a file without the column reads as all grades",
    ok(isTRUE(imp$ok) && identical(imp$defs$PermittedGrades, sc$defs$PermittedGrades)),
    paste(c(imp$errors, imp$defs$PermittedGrades), collapse = " | "))
mk <- function(pd, o, lc = prod$lc) list(pd = pd, lc = lc, lookup = NULL, opts = o)
pd2 <- prod$pd; pd2$PermittedGrades[pd2$ProductKey == "spruce_154_kubb"] <- "1"
ngr <- function(...) length(.prepare_runs(list(...))$groups)
rec("Check 8c: runs with different options are different definitions (2 groups), equal options one group; different grade sets split the runs only when Use stem grades is on",
    ok(ngr(mk(prod$pd, OFF), mk(prod$pd, OFF)) == 1L && ngr(mk(prod$pd, OFF), mk(prod$pd, CUT)) == 2L &&
       ngr(mk(prod$pd, USE), mk(prod$pd, BOTH)) == 2L && ngr(mk(prod$pd, USE), mk(pd2, USE)) == 2L &&
       ngr(mk(prod$pd, OFF), mk(pd2, OFF)) == 1L && ngr(mk(prod$pd, CUT), mk(pd2, CUT)) == 1L),
    sprintf("%d %d %d %d %d %d", ngr(mk(prod$pd, OFF), mk(prod$pd, OFF)), ngr(mk(prod$pd, OFF), mk(prod$pd, CUT)),
            ngr(mk(prod$pd, USE), mk(prod$pd, BOTH)), ngr(mk(prod$pd, USE), mk(pd2, USE)),
            ngr(mk(prod$pd, OFF), mk(pd2, OFF)), ngr(mk(prod$pd, CUT), mk(pd2, CUT))))
rec("Check 8d: scenarios_differ() is TRUE when only the options differ, FALSE when they are equal",
    ok(scenarios_differ(sc$defs, sc$pm, sc$defs, sc$pm, USE, OFF) &&
       !scenarios_differ(sc$defs, sc$pm, sc$defs, sc$pm, USE, USE) &&
       !scenarios_differ(sc$defs, sc$pm, sc$defs, sc$pm)))

# ---- 9. several files ----------------------------------------------------------------------
section("9. Several files")
txt2 <- gsub("<StemKey>10(\\d)</StemKey>", "<StemKey>20\\1</StemKey>", txt)
f2 <- tempfile(fileext = ".hpr"); writeLines(txt2, f2, useBytes = TRUE)
two <- read_hpr_stem_files(c(FIXTURE, f2), c("a.hpr", "b.hpr"))
map2 <- setNames(c("pine", "spruce", "none", "none", "pine", "spruce", "none", "none"), two$species$key)
pr2 <- hpr_stem_profiles(two, map2, grades = TRUE)
g_of <- function(id) { p <- pr2$profiles[pr2$profiles$treid == id, ]; r <- rle(p$grade); setNames(r$lengths * 10, r$values) }
rec("Check 9: two files: 10 stems with each file's own grades (stems 102 and 202 give the same runs; 104 and 204 snap alike); grade starts of file 2 are not attached to file 1's stems",
    ok(nrow(pr2$tree_data) == 10L && identical(g_of("102"), runs_of(pr, "102")) && identical(g_of("202"), runs_of(pr, "102")) &&
       identical(g_of("204"), runs_of(pr, "104")) && identical(g_of("103"), g_of("203")) && length(two$gval) == 16L),
    sprintf("stems %d, grades %d", nrow(pr2$tree_data), length(two$gval)))
unlink(c(f2, f3, fn))

# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 30 complete.\n")
