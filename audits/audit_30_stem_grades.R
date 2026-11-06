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
#  11  Grade mismatch warning (R4 polish): hpr_grade_lengths() equals the bucking's
#      grade grid; grade_mismatch_check() warns when a sawlog product permits the
#      grades of under half of a species' length; the texts in both languages.
#  12  Same instruction (R5): grades are used only for stems whose file has the
#      scenario's instruction (ProductKey and name); the "not used" note with its
#      share; copies, price edits, .pin round trip, two stem files.
#  13  Assortment type (R5): .hpr_type() prefixes, the user's choice, copy and xlsx.
#
# Outputs : outputs/stem_grades/audit_30_stem_grades.{csv,txt}
# Usage   : Rscript audits/audit_30_stem_grades.R
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

FIXTURE <- file.path(root_dir, "inst", "extdata", "inputs", "test_fixtures", "hpr_grades_synthetic.hpr")
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
# Candidates of stem 101 (grade 7 below 300 cm) for a set of options.
cand_of <- function(prep, id, opts) {
  pv <- profiles_to_profile_vectors(prep$profiles)[[id]]
  geo <- .candidate_columns(pv, .candidate_plan(prod$pd, prod$lc, grade_opts = opts)[[pv$species_group]],
                            0L, 10L, usable_only = TRUE)
  data.frame(key = vapply(geo$products, `[[`, "", "pk")[geo$cols$prod], start = geo$cols$StartPos,
             stop = geo$cols$StopPos, stringsAsFactors = FALSE)
}
c101 <- cand_of(pr, "101", USE)
ok5a <- is.na(sc$defs$PermittedGrades[keys == spruce_mass]) && !length(permitted[[spruce_mass]]) &&
  any(c101$key == spruce_mass & c101$start < 300) && any(c101$key == spruce_mass & c101$start >= 300) &&
  !any(c101$key == "spruce_140_sca_sagt_mmer" & c101$start < 300)
rec("Check 5a: '197 Massevirke' has no PermittedGradesDefinition: empty set, and it has candidates inside stem 101's grade-7 butt with the option on (the sawlog has none there)",
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
rec("Check 8c: runs with different options are different definitions (2 groups), equal options one group; different grade sets split the runs whatever the options (they change the candidates for every stem)",
    ok(ngr(mk(prod$pd, OFF), mk(prod$pd, OFF)) == 1L && ngr(mk(prod$pd, OFF), mk(prod$pd, CUT)) == 2L &&
       ngr(mk(prod$pd, USE), mk(prod$pd, BOTH)) == 2L && ngr(mk(prod$pd, USE), mk(pd2, USE)) == 2L &&
       ngr(mk(prod$pd, OFF), mk(pd2, OFF)) == 2L && ngr(mk(prod$pd, CUT), mk(pd2, CUT)) == 2L &&
       ngr(mk(prod$pd, OFF), mk(prod$pd, OFF)) == 1L),
    sprintf("%d %d %d %d %d %d", ngr(mk(prod$pd, OFF), mk(prod$pd, OFF)), ngr(mk(prod$pd, OFF), mk(prod$pd, CUT)),
            ngr(mk(prod$pd, USE), mk(prod$pd, BOTH)), ngr(mk(prod$pd, USE), mk(pd2, USE)),
            ngr(mk(prod$pd, OFF), mk(pd2, OFF)), ngr(mk(prod$pd, CUT), mk(pd2, CUT))))
rec("Check 8d: scenarios_differ() is TRUE when only the options differ, FALSE when they are equal",
    ok(scenarios_differ(sc$defs, sc$pm, sc$defs, sc$pm, USE, OFF) &&
       !scenarios_differ(sc$defs, sc$pm, sc$defs, sc$pm, USE, USE) &&
       !scenarios_differ(sc$defs, sc$pm, sc$defs, sc$pm)))

# ---- 10. the reference grade (2026-10-28) ---------------------------------------------------
section("10. Reference grade for stems without grade data")
ref <- hpr_reference_grades(hp)
ref_of <- function(sp) sc$defs$ReferenceGrades[sc$defs$Species == sp & sc$defs$Name != "Waste"]
exp_spruce <- norm_permitted_grades(paste(ref[["446"]], collapse = ";"))
exp_pine <- norm_permitted_grades(paste(ref[["445"]], collapse = ";"))
rec("Check 10a: the import stores each product's reference grade(s) (ReferenceGrades) from hpr_reference_grades(); Waste has none; nothing is unticked for grades",
    ok(all(ref_of("spruce") == exp_spruce) && all(ref_of("pine") == exp_pine) &&
       all(is.na(sc$defs$ReferenceGrades[sc$defs$Name == "Waste"])) && all(hpr_product_table(hp, MAP)$default[
         hpr_product_table(hp, MAP)$species != "none"])),
    sprintf("reference grades: spruce %s, pine %s", exp_spruce, exp_pine))
c_off <- cand_of(pr, "101", OFF); c_use <- cand_of(pr, "101", USE); c_nog <- cand_of(pr0, "101", USE)
energy <- "spruce_100_energi"; ref_s <- ref[["446"]]
ok10b <- !any(permitted[[energy]] %in% ref_s) && !any(c_off$key == energy) && !any(c_nog$key == energy) &&
  any(c_use$key == energy) && all(c_use$stop[c_use$key == energy] <= 300)
rec("Check 10b: 100 Energi (grade 7 only) does not permit the reference grade: no candidates with the options off, or on a stem without grade data (tree list) even with Use stem grades; with the option on it is allowed only inside the grade-7 butt of a stem with grades",
    ok(ok10b), sprintf("candidates of 100 Energi on stem 101: off %d, grades %d (all below 300 cm: %s), no grade data %d",
                       sum(c_off$key == energy), sum(c_use$key == energy),
                       all(c_use$stop[c_use$key == energy] <= 300), sum(c_nog$key == energy)))
rec("Check 10c: a product that permits the reference grade keeps its candidates with the options off and on a stem without grade data (REDACTED25)",
    ok(any(c_off$key == "spruce_140_sca_sagt_mmer") && any(c_nog$key == "spruce_140_sca_sagt_mmer")))

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

# ---- 11. grade mismatch warning (R4 polish) -------------------------------------------------
section("11. Grade mismatch warning")
dl <- hpr_grade_lengths(st, MAP)
cells_run <- table(pr$profiles$species_group, pr$profiles$grade, useNA = "ifany")
cells_fun <- tapply(dl$cells, list(dl$species, dl$grade), sum)
ok11a <- sum(dl$cells) == nrow(pr$profiles) &&
  all(vapply(rownames(cells_run), function(sp) all(vapply(colnames(cells_run), function(g)
    (if (is.na(cells_fun[sp, g])) 0L else cells_fun[sp, g]) == cells_run[sp, g], NA)), NA))
rec("Check 11a: hpr_grade_lengths() counts the same 10 cm cells by species and grade as the bucking's grade grid (profiles)",
    ok(ok11a), sprintf("%d cells, species/grade combinations %d", sum(dl$cells), nrow(dl)))
d90 <- data.frame(species = c("spruce", "spruce", "pine", "pine"), grade = c(3, 2, 3, 7), cells = c(900, 100, 970, 30))
sdefs <- sc$defs[match("spruce_140_sca_sagt_mmer", assortment_table_keys(sc$defs)), ]; i_s <- 1L   # the one sawlog product
with_perm <- function(v) { x <- sdefs; x$PermittedGrades[i_s] <- v; x }
w12 <- grade_mismatch_check(d90, with_perm("1;2"), TRUE)
w123 <- grade_mismatch_check(d90, with_perm("1;2;3"), TRUE)
w_off <- grade_mismatch_check(d90, with_perm("1;2"), FALSE)
w_non <- grade_mismatch_check(NULL, with_perm("1;2"), TRUE)
w_all <- grade_mismatch_check(d90, with_perm(NA_character_), TRUE)
w135 <- grade_mismatch_check(data.frame(species = "spruce", grade = c(2, 4), cells = c(800, 200)), with_perm("1;3;5"), TRUE)
ok11b <- nrow(w12) == 1L && w12$key == "spruce_140_sca_sagt_mmer" && w12$grade == 3 && w12$share == 90 && w12$permitted == "1–2" &&
  nrow(w123) == 0L && nrow(w_off) == 0L && nrow(w_non) == 0L && nrow(w_all) == 0L &&
  nrow(w135) == 1L && w135$permitted == "1, 3, 5" && w135$grade == 2
rec("Check 11b: 90 % grade 3 with a sawlog product permitting 1-2 gives exactly one warning (grade 3, share 90, permitted '1-2'); permitting 1-3, option off, no .hpr stems, or no permitted grades give none; non-consecutive grades read '1, 3, 5'",
    ok(ok11b), sprintf("1-2: %d | 1-3: %d | off: %d | no stems: %d | no grades: %d | 1,3,5: '%s'", nrow(w12), nrow(w123), nrow(w_off), nrow(w_non), nrow(w_all), w135$permitted))
txt_en <- grade_warning_text(w12, function(k) "Spruce REDACTED25", "en")
txt_no <- grade_warning_text(w12, function(k) "Gran REDACTED25", "no")
ok11c <- identical(txt_en, paste0("Grade 3 covers 90 % of the spruce stem length, but Spruce REDACTED25 only permits grade 1–2. ",
                                  "With \"Use stem grades\" ticked, little Spruce REDACTED25 will be produced. ",
                                  "The stems and assortments probably come from different jobs with different grade codes.")) &&
  identical(txt_no, paste0("Kvalitet 3 dekker 90 % av lengden på granstammene, men Gran REDACTED25 tillater bare kvalitet 1–2. ",
                           "Med «Bruk stammekvalitet» blir det derfor lite Gran REDACTED25. ",
                           "Stammene og sortimentene kommer trolig fra ulike oppdrag med ulike kvalitetskoder."))
rec("Check 11c: the warning text in English and Norsk (checkbox label, species word, product name, grade, share, permitted grades)",
    ok(ok11c), paste(txt_en, "|", txt_no))

# ---- 12. same instruction (R5, 2026-11-03) -----------------------------------------------------
section("12. Grades only with the same instruction")
MAPN <- c("445" = "pine", "446" = "spruce", "447" = "none", "448" = "none")
same0 <- hpr_same_instruction(st$instruction, MAPN, sc$defs, files = 1L)
ok12a <- all(same0$same) && nrow(same0) == 2L && is.null(grade_files_of(same0, FALSE)) &&
  identical(grade_files_of(same0, TRUE), list(spruce = 1L, pine = 1L))
rec("Check 12a: scenario and stems from the same fixture file: same instruction for spruce and pine; grade files all (1), none when the box is off",
    ok(ok12a), paste(same0$species, same0$same, collapse = ", "))
nt0 <- grade_unused_notes(hpr_grade_lengths(st, MAP), same0)
rec("Check 12b: the same file gives no note", ok(nrow(nt0) == 0L), sprintf("%d notes", nrow(nt0)))

# Different ProductKeys: grades are not used, the note has the share of the stem length off the start grade.
d_other <- sc$defs; d_other$HprProductKey <- ifelse(is.na(d_other$HprProductKey), NA, paste0("x", d_other$HprProductKey))
same1 <- hpr_same_instruction(st$instruction, MAPN, d_other, files = 1L)
dl1 <- hpr_grade_lengths(st, MAP)
sg <- setNames(hp$species$start_grade, hp$species$key)
# By hand from the fixture: spruce stems 101 (7 x 30 cells, rest 2), 102 (3 x 30), 103 (none), 104 (7 x 151); 181 cells each.
exp_sp <- 100 * (30 + 30 + 0 + 151) / (4 * 181)
nt1 <- grade_unused_notes(dl1, same1)
exp_pine <- 0   # stem 105 is pine with grade 1 from 0; the pine StartGrade decides
pine_cells <- dl1[dl1$species == "pine", ]
exp_pine <- 100 * sum(pine_cells$cells[pine_cells$grade != pine_cells$start]) / sum(pine_cells$cells)
fmt_share <- function(p) if (p > 0 && p < 0.5) "<1" else as.character(floor(p + 0.5))
ok12c <- !any(same1$same) && identical(nt1$species, c("spruce", "pine")) &&
  identical(nt1$share[1L], fmt_share(exp_sp)) && identical(nt1$share[2L], fmt_share(exp_pine)) && fmt_share(exp_sp) == "29"
rec("Check 12c: different ProductKeys: no file has the same instruction (the box would be disabled); a note per species with the share of stem length off the start grade (spruce 211 of 724 cells = 29 %, pine by the file's start grade)",
    ok(ok12c), sprintf("spruce %s (by hand %.2f), pine %s (by hand %.2f)", nt1$share[1L], exp_sp, nt1$share[2L], exp_pine))

# Not used = bucked as a stem without grade data (the options off); used = grades decide.
GF_NONE <- list(spruce = integer(0), pine = integer(0))
rUall <- run(pr, USE, OFF)
rUnone <- run(pr, c(USE, list(grade_files = GF_NONE)), OFF)
rBnone <- run(pr, c(BOTH, list(grade_files = GF_NONE)), OFF)
rCnone <- run(pr, c(CUT, list(grade_files = GF_NONE)), OFF)        # cut alone, no file matches
rCall  <- run(pr, CUT, OFF)
rOFF <- run(pr, OFF, OFF)
ok12d <- identical(logs_of(rUnone), logs_of(rOFF)) && identical(logs_of(rBnone), logs_of(rOFF)) &&
  identical(logs_of(rCnone), logs_of(rOFF)) && !identical(logs_of(rCall), logs_of(rOFF)) &&
  !identical(logs_of(rUall), logs_of(rOFF))
rec("Check 12d: with no file's grades used, 'Use stem grades', 'Cut at grade changes' (alone or both) give exactly the result of both options off; with grades used the results differ (not vacuous)",
    ok(ok12d), sprintf("logs: off %d, use-all %d, use-none %d", nrow(logs_of(rOFF)), nrow(logs_of(rUall)), nrow(logs_of(rUnone))))

# Copy A to B with a price edit: still the same instruction.
pm_edit <- sc$pm; i_e <- which(!is.na(pm_edit$PriceNOKperM3))[1L]; pm_edit$PriceNOKperM3[i_e] <- pm_edit$PriceNOKperM3[i_e] + 25
cp <- copy_assortment(sc$defs, pm_edit, sc$defs, sc$pm, assortment_table_keys(sc$defs)[1L])
same_cp <- hpr_same_instruction(st$instruction, MAPN, cp$defs, files = 1L)
rec("Check 12e: a copy of the scenario with an edited price (and an assortment the user added) keeps the same instruction; user-added products are ignored in the test",
    ok(all(same_cp$same) &&
       all(hpr_same_instruction(st$instruction, MAPN,
             rbind(sc$defs, defs_from_products(load_products())[1L, ]), files = 1L)$same)))

# A renamed product no longer matches; a scenario without any file product has no same instruction.
d_ren <- sc$defs; d_ren$Name[match("spruce_140_sca_sagt_mmer", assortment_table_keys(sc$defs))] <- "Something else"
same_ren <- hpr_same_instruction(st$instruction, MAPN, d_ren, files = 1L)
same_base <- hpr_same_instruction(st$instruction, MAPN, defs_from_products(load_products()), files = 1L)
rec("Check 12f: a renamed file product breaks the match for its species only (spruce FALSE, pine TRUE); the base assortments (no file product) match no file",
    ok(identical(same_ren$same, c(FALSE, TRUE)) && !any(same_base$same)),
    sprintf("renamed: %s | base: %s", paste(same_ren$same, collapse = "/"), paste(same_base$same, collapse = "/")))

# An exported .pin read back as a scenario keeps the instruction (names and ProductUserIDs).
pin_path <- tempfile(fileext = ".pin")
pe <- pin_export_scenario(sc$defs, sc$pm, pin_path)
hp_pin <- read_pin(pin_path)
sc_pin <- hpr_to_scenario(hp_pin, setNames(guess_hpr_species(hp_pin$species$name, hp_pin$species$user_id), hp_pin$species$key))
same_pin <- hpr_same_instruction(st$instruction, MAPN, sc_pin$defs, files = 1L)
rec("Check 12g: an exported .pin read back as a scenario has the same instruction as the stem file it came from",
    ok(isTRUE(pe$ok) && isTRUE(sc_pin$ok) && all(same_pin$same)),
    sprintf("export ok %s, import ok %s, same %s", isTRUE(pe$ok), isTRUE(sc_pin$ok), paste(same_pin$same, collapse = "/")))
unlink(pin_path)

# Two stem files, the scenario matches one: grades used for that file's stems only.
txt_n <- gsub("<StemKey>10(\\d)</StemKey>", "<StemKey>20\\1</StemKey>", txt)
txt_n <- gsub("<ProductName>", "<ProductName>Z ", txt_n, fixed = TRUE)
f_n <- tempfile(fileext = ".hpr"); writeLines(txt_n, f_n, useBytes = TRUE)
two_n <- read_hpr_stem_files(c(FIXTURE, f_n), c("a.hpr", "b.hpr"))
map_n <- setNames(c("pine", "spruce", "none", "none", "pine", "spruce", "none", "none"), two_n$species$key)
same_n <- hpr_same_instruction(two_n$instruction, map_n, sc$defs, files = c(1L, 2L))
pr_n <- hpr_stem_profiles(two_n, map_n, grades = TRUE)
gf_n <- grade_files_of(same_n, TRUE)
r_mixed <- run(pr_n, c(USE, list(grade_files = gf_n)), OFF)
r_all   <- run(pr_n, USE, OFF)
r_zero  <- run(pr_n, OFF, OFF)
lg <- function(r, ids) { l <- logs_of(r); l <- l[l$treid %in% ids, ]; rownames(l) <- NULL; l }
f1 <- as.character(101:105); f2s <- as.character(201:205)
ok12h <- identical(same_n$same, c(TRUE, TRUE, FALSE, FALSE)) && identical(gf_n, list(spruce = 1L, pine = 1L)) &&
  identical(lg(r_mixed, f1), lg(r_all, f1)) && identical(lg(r_mixed, f2s), lg(r_zero, f2s)) &&
  !identical(lg(r_all, f2s), lg(r_zero, f2s))
rec("Check 12h: two stem files, the scenario from the first: file 1's stems are bucked with their grades, file 2's like stems without grade data (equal to the run with the options off); the second file's grades would have mattered",
    ok(ok12h), sprintf("same %s | grade files spruce %s", paste(same_n$same, collapse = "/"), paste(gf_n$spruce, collapse = ",")))
nt_n <- grade_unused_notes(hpr_grade_lengths(two_n, map_n), same_n)
rec("Check 12i: the note covers only the stems of the file whose grades are not used (spruce 29 %, as for the single file)",
    ok(identical(nt_n$share[nt_n$species == "spruce"], "29")), paste(nt_n$species, nt_n$share, collapse = ", "))
unlink(f_n)

# Share formatting: whole percent, "<1" above 0 and below 0.5, 0 when none.
shr <- function(off, tot) grade_unused_notes(data.frame(species = "spruce", file = 1L, grade = c(2, 3), start = 2,
                                                         cells = c(tot - off, off)), data.frame(file = 1L, species = "spruce", same = FALSE))$share
ok12j <- identical(shr(10, 100), "10") && identical(shr(1, 1000), "<1") && identical(shr(4, 1000), "<1") &&
  identical(shr(5, 1000), "1") && identical(shr(0, 100), "0") && identical(shr(1, 100), "1") && identical(shr(149, 1000), "15")
rec("Check 12j: share formatting: 10 of 100 -> 10; 1 or 4 of 1000 -> <1; 5 of 1000 (0.5 %) -> 1; none -> 0; 14.9 % -> 15",
    ok(ok12j), paste(shr(10, 100), shr(1, 1000), shr(4, 1000), shr(5, 1000), shr(0, 100), shr(149, 1000)))
nt90 <- grade_unused_notes(data.frame(species = "spruce", file = 1L, grade = c(2, 3), start = 2, cells = c(90, 10)),
                           data.frame(file = 1L, species = "spruce", same = FALSE))
rec("Check 12k: 90 % start grade gives 10 in the texts (English and Norsk, as specified)",
    ok(identical(grade_unused_text(nt90, "A", "en"),
                 paste0("The grade codes in the stem file are not used for spruce because the assortments in scenario A come from a different instruction. ",
                        "10 % of the stem length was marked with a grade other than the start grade; these markings, for example rot, are therefore ignored.")) &&
       identical(grade_unused_text(nt90, "B", "no"),
                 paste0("Kvalitetskodene i stammefila brukes ikke for gran fordi sortimentene i scenario B kommer fra en annen instruks. ",
                        "10 % av stammelengden var merket med en annen kvalitet enn startkvaliteten; disse merkingene, for eksempel råte, blir derfor ikke tatt hensyn til."))))

# The mismatch warning is evaluated over the stems whose grades are used only.
dl_two <- hpr_grade_lengths(two_n, map_n)
w_used <- grade_mismatch_check(grade_dist_by_grade(dl_two, list(spruce = 1L, pine = 1L)), sc$defs, TRUE)
w_none <- grade_mismatch_check(grade_dist_by_grade(dl_two, list(spruce = integer(0), pine = integer(0))), sc$defs, TRUE)
rec("Check 12l: the grade mismatch warning looks only at the stems whose grades are used (no stems used: no warning)",
    ok(nrow(w_none) == 0L && is.data.frame(w_used)))

# ---- 13. assortment type (R5) ----------------------------------------------------------------
section("13. Assortment type")
ty <- function(g, n) .hpr_type(g, n)
ok13a <- identical(ty("FRISK", "MASSE FRISK"), "pulpwood") && identical(ty("0-20%", "RMASSE 0-20%"), "pulpwood") &&
  identical(ty("TØRR", "TØRRGRAN - Fallende"), "pulpwood") && identical(ty("FRISK", "SAG - REDACTED35"), "sawlog") &&
  identical(ty("Tømmer", "REDACTED25"), "sawlog") && identical(ty("Massevirke", "200 Massevirke"), "pulpwood") &&
  identical(type_choice(ty("0-20%", "RMB 0-20%")), "other") && identical(type_choice(ty("MASSE", "BJØRK")), "other") &&
  identical(ty("MASSE", "MASSE - REDACTED54"), "pulpwood")
rec("Check 13a: .hpr_type(): MASSE FRISK, RMASSE 0-20%, TØRRGRAN - Fallende and 200 Massevirke are pulpwood; SAG - REDACTED35 and REDACTED25 sawlog; RMB 0-20% and BJØRK (group MASSE) custom",
    ok(ok13a))
dT <- sc$defs; kT <- "spruce_140_sca_sagt_mmer"
dT2 <- set_assortment_type(dT, kT, "pulpwood")
iT <- match(kT, assortment_table_keys(dT2))
dT3 <- set_assortment_type(dT, kT, "other")
dW <- defs_from_products(load_products()); dW2 <- set_assortment_type(dW, "spruce_waste", "sawlog")
ok13b <- dT2$Type[iT] == "pulpwood" && isTRUE(dT2$TypeUser[iT]) && !any(dT2$TypeUser[-iT]) &&
  type_choice(dT3$Type[iT]) == "other" && isTRUE(dT3$TypeUser[iT]) && !dT3$Type[iT] %in% c("sawlog", "pulpwood", "waste") &&
  identical(dW2$Type, dW$Type) && identical(products_from_defs(dT2, sc$pm)$pd$ProductType[iT], "pulpwood")
rec("Check 13b: set_assortment_type(): the user's choice is stored in Type and flagged (TypeUser), reaches ProductData, 'other' is a custom type apart from sawlog/pulpwood/waste, and a waste row cannot be changed",
    ok(ok13b), paste(dT2$Type[iT], dT3$Type[iT]))
cpT <- copy_assortment(dT2, sc$pm, dT, sc$pm, kT)
xl <- tempfile(fileext = ".xlsx"); scenario_export(dT2, sc$pm, xl); imp <- scenario_import(xl)
ok13c <- cpT$defs$Type[match(kT, assortment_table_keys(cpT$defs))] == "pulpwood" && isTRUE(cpT$defs$TypeUser[match(kT, assortment_table_keys(cpT$defs))]) &&
  isTRUE(imp$ok) && imp$defs$Type[match(kT, assortment_table_keys(imp$defs))] == "pulpwood" &&
  isTRUE(imp$defs$TypeUser[match(kT, assortment_table_keys(imp$defs))]) &&
  !scenarios_differ(dT2, sc$pm, cpT$defs, sc$pm) && scenarios_differ(dT, sc$pm, dT2, sc$pm)
rec("Check 13c: the chosen type survives copy A to B and the scenario xlsx round trip; a different type makes the scenarios differ, a price edit does not touch it",
    ok(ok13c))
ok13d <- identical(type_group(c("sawlog", "pulpwood", "waste", "mymassa", "pulpwood")), c("Sawlog", "Pulpwood", NA, "Other", "Pulpwood")) &&
  identical(type_group(dT2$Type[iT]), "Pulpwood")
rec("Check 13d: the type groups of the result tables and the value-by-type chart follow the field (a sawlog changed to pulpwood is Pulpwood, a custom type Other)",
    ok(ok13d))
unlink(xl)


# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 30 complete.\n")
