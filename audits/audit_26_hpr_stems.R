# ==============================================================================
# audits/audit_26_hpr_stems.R
#
# Purpose : hpr Phase B (2026-10-21): the stems of a StanForD 2010 .hpr file
#           as a tree source (R/helpers_hpr_stems.R, run_hpr_bucking()). Runs
#           on inputs/test_fixtures/hpr_example_synthetic.hpr (13 invented
#           stems; scripts/make_hpr_test_fixture.R) and on variants of it
#           written to tempfiles. The owner's real files are never used here
#           (scripts/hpr_stems_benchmark.R, local only).
#
# Checks  :
#  1.  Reading: 13 stems with kind, diameter category and points as written;
#      stem 1's profile exactly; DBHHeight per species group (GRAN 110, FURU
#      120); the same result with 512-byte and 4 KB chunks, and with the xml2
#      extractor instead of regex
#  2.  Robustness: attribute order, single quotes, line breaks inside tags, a
#      comment hiding a stem start, a commented-out value, CDATA, a character
#      entity, attributes on <Stem>, a self-closing StemDiameters, and
#      "<Stem>" in a biomass function read as intended
#  3.  Heights and DBH: hpr_stump_offset() (130 - DBHHeight, 0-50 cm, 10 cm
#      without DBHHeight) and hpr_height_above_ground(); DBH = the over-bark
#      profile at 1.3 m above ground (GRAN at 110 cm, FURU at 120 cm from the
#      cut), never the file's DBH (999 on stem 2); profiles on the 10 cm grid
#      from 0 to the last measured position rounded down (stem 4: 1012 ->
#      1010, interpolated); the local increase of stem 3 kept
#  4.  Stump 0: the job's stump is 0 even when params ask for 30 cm;
#      candidates start at 0; every bucked fixture stem's first log starts at 0
#  5.  No log above the last measured point: no candidate or selected log
#      ends above the profile end, and the profile end is at or below the last
#      measured position
#  6.  Under bark: barkNOR with the profile DBH along the whole stem
#  7.  Species mapping and dropped stems: the 8 dropped stems, one per reason,
#      with the exact warnings; GRAN set to Not used drops its stems with a
#      per-group warning; everything Not used is an error
#  8.  .hpr.gz and .hpr in a .zip give identical stems; a zip without an .hpr
#      is an error
#  9.  Errors: a file with stems but no profiles (logs only), and a file
#      without stems
# 10.  Privacy: a variant with invented StemCoordinates, dates, an operator
#      and an object name: no coordinate-like column in any output table and
#      none of the invented values anywhere in them
# 11.  End-to-end A/B run: default prices vs sawlog x 1.3; tree_source
#      "hpr", tree IDs = StemKeys, no plots, the dropped stems listed;
#      comparison of all 5 stems, NOK/m3 higher in B
# 12.  Results do not depend on the number of workers (1 vs 2, chunks of 2)
#
# Verdict : PASS / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/hpr_stems/audit_26_hpr_stems.csv
#   outputs/hpr_stems/audit_26_hpr_stems.txt
#
# Usage   : Rscript audits/audit_26_hpr_stems.R
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

out_dir <- path_output("hpr_stems", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_26_hpr_stems.txt")
audit_csv <- file.path(out_dir, "audit_26_hpr_stems.csv")
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
cat("  Audit: .hpr stems (hpr Phase B)\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

FIXTURE <- file.path(root_dir, "inputs", "test_fixtures", "hpr_example_synthetic.hpr")
fx <- readLines(FIXTURE, warn = FALSE, encoding = "UTF-8")
first_stem <- grep("<Stem>", fx, fixed = TRUE)[1L]
machine_end <- grep("</Machine>", fx, fixed = TRUE)
# The fixture's definitions with other stems (text) in place of its own.
with_stems <- function(stems, ext = ".hpr") {
  f <- tempfile(fileext = ext)
  writeLines(c(fx[seq_len(first_stem - 1L)], stems, fx[machine_end:length(fx)]), f, useBytes = TRUE)
  f
}
# One stem record as text.
stem_xml <- function(key, sgk = 446, body = dv_xml(c(0, 10, 20), c(330, 320, 310)), open = "<Stem>",
                     kind = "SingleTreeProcessedStem", cat = 'diameterCategory="Over bark"', extra = "")
  sprintf('%s<StemKey>%s</StemKey><SpeciesGroupKey>%s</SpeciesGroupKey>%s<%s><DBH>300</DBH><StemDiameters %s>%s</StemDiameters><Log><LogKey>1</LogKey><ProductKey>197</ProductKey></Log></%s></Stem>',
          open, key, sgk, extra, kind, cat, body, kind)
dv_xml <- function(p, d) paste(sprintf('<DiameterValue diameterPosition="%s" diameterMeasurementCategory="First">%s</DiameterValue>',
                                      p, d), collapse = "")

st <- read_hpr_stems(FIXTURE)
map <- setNames(guess_hpr_species(st$species$name, st$species$user_id), st$species$key)
p <- hpr_stem_profiles(st, map)
pts <- function(x, key) { i <- match(key, x$stems$stem_key); list(p = x$pos[x$idx == i], d = x$d[x$idx == i]) }
s1 <- seq(0, 1800, by = 10); d1 <- round(330 - s1 * 0.15)


# ==============================================================================
section("01. Reading the fixture's stems")
cmp_read <- function(a, b) identical(a$stems, b$stems) && identical(a$idx, b$idx) &&
  identical(a$pos, b$pos) && identical(a$d, b$d)
st_512 <- read_hpr_stems(FIXTURE, chunk_bytes = 512L)
st_4k  <- read_hpr_stems(FIXTURE, chunk_bytes = 4096L)
xml_parts <- .hpr_stem_chunks(FIXTURE, NULL, .hs_extract_xml2)
reg_parts <- .hpr_stem_chunks(FIXTURE, NULL, .hs_extract_regex)
kinds <- paste(st$stems$kind, collapse = ",")
ok1 <- isTRUE(st$ok) && nrow(st$stems) == 13L &&
  identical(st$stems$stem_key, as.character(c(1:9, 2, 11:13))) &&
  identical(kinds, "single,single,single,single,single,single,multi,single,single,single,single,single,single") &&
  identical(st$stems$category[c(1, 7, 8, 9)], c("Over bark", NA, NA, "Under bark")) &&
  identical(st$stems$n_points[c(7, 8)], c(0L, 0L)) &&
  identical(pts(st, "1")$p, s1) && identical(pts(st, "1")$d, d1) &&
  identical(st$species$dbh_height, c(120, 110, 110, 110)) &&
  identical(st$species$offset_cm, c(10, 20, 20, 20)) &&
  cmp_read(st, st_512) && cmp_read(st, st_4k) && identical(xml_parts, reg_parts)
rec("Check 01: The fixture's 13 stems are read as written, chunk size and extractor do not matter",
    ok(ok1), sprintf("%d stems, keys %s | kinds %s | stem 1: %d points | DBHHeight %s -> offsets %s | 512 B / 4 KB chunks identical %s/%s | xml2 = regex %s",
                     nrow(st$stems), paste(st$stems$stem_key, collapse = ","), kinds, st$stems$n_points[1],
                     paste(st$species$dbh_height, collapse = "/"), paste(st$species$offset_cm, collapse = "/"),
                     cmp_read(st, st_512), cmp_read(st, st_4k), identical(xml_parts, reg_parts)))


# ==============================================================================
section("02. Robustness of the reader")
std <- dv_xml(c(0, 10, 20), c(330, 320, 310))
cases <- list(
  attr_order   = stem_xml(1, body = paste(sprintf('<DiameterValue diameterMeasurementCategory="First" diameterPosition="%d">%d</DiameterValue>',
                                                  c(0, 10, 20), c(330, 320, 310)), collapse = "")),
  single_quote = stem_xml(1, body = gsub('"', "'", std)),
  newlines     = stem_xml(1, body = gsub("<DiameterValue ", "<DiameterValue\n   ", gsub(">([0-9]+)<", ">\n  \\1\n  <", std))),
  comment_val  = stem_xml(1, body = paste0(std, "<!-- ", dv_xml(30, 300), " -->")),
  comment_stem = paste0("<!-- <Stem><StemKey>99</StemKey> -->", stem_xml(1)),
  cdata_key    = stem_xml("<![CDATA[1]]>"),
  entity_value = stem_xml(1, body = sub(">330<", ">&#51;30<", std, fixed = TRUE)),
  stem_attr    = stem_xml(1, open = '<Stem stemType="x">'),
  self_closing = sub("<StemDiameters diameterCategory=\"Over bark\"></StemDiameters>",
                     "<StemDiameters diameterCategory=\"Over bark\"/>", stem_xml(1, body = ""), fixed = TRUE),
  biomass      = paste0("<BioMassFunctions><Detailed><Stem>370</Stem></Detailed></BioMassFunctions>", stem_xml(1)))
res2 <- vapply(names(cases), function(nm) {
  f <- with_stems(c(cases[[nm]], stem_xml(2)))
  x <- read_hpr_stems(f); unlink(f)
  want <- if (nm == "self_closing") 0L else 3L
  isTRUE(x$ok) && nrow(x$stems) == 2L && identical(x$stems$stem_key, c("1", "2")) &&
    x$stems$n_points[1] == want &&
    (want == 0L || (identical(pts(x, "1")$p, c(0, 10, 20)) && identical(pts(x, "1")$d, c(330, 320, 310))))
}, logical(1))
rec("Check 02: Unusual but valid XML is read as intended (regex, xml2 for entities and CDATA)",
    ok(all(res2)), paste(sprintf("%s %s", names(res2), ifelse(res2, "ok", "WRONG")), collapse = " | "))


# ==============================================================================
section("03. Heights above ground, DBH and the profile grid")
td <- p$tree_data; pr <- p$profiles
prof_of <- function(k) pr[pr$treid == k, ]
at <- function(x, h) stats::approx(x$p, x$d, xout = h)$y
x2 <- pts(st, "2"); x4 <- pts(st, "4"); x3 <- pts(st, "3")
dbh_ok <- isTRUE(all.equal(td$dbh_cm[td$treid == "1"], at(pts(st, "1"), 110) / 10)) &&
  isTRUE(all.equal(td$dbh_cm[td$treid == "2"], at(x2, 120) / 10)) &&
  td$dbh_cm[td$treid == "2"] < 99 && isTRUE(all.equal(td$dbh_cm[td$treid == "5"], at(pts(st, "5"), 110) / 10))
g4 <- prof_of("4")
grid_ok <- identical(g4$diameterPosition, seq(0L, 1010L, by = 10L)) &&
  isTRUE(all.equal(g4$DiameterValue, at(x4, seq(0, 1010, by = 10)))) &&
  isTRUE(all.equal(td$height_m[td$treid == "4"], 10.1)) &&
  identical(prof_of("1")$diameterPosition, as.integer(s1)) &&
  identical(prof_of("3")$DiameterValue, x3$d)
fn_ok <- identical(hpr_stump_offset(c(110, 120, NA, 200, 60)), c(20, 10, 10, 0, 50)) &&
  identical(hpr_height_above_ground(c(0, 110), 20), c(20, 130))
ok3 <- dbh_ok && grid_ok && fn_ok && any(diff(prof_of("3")$DiameterValue) > 0) &&
  any(grepl("off the 10 cm grid in 1 stem", p$info))
rec("Check 03: DBH is the over-bark profile at 1.3 m above ground (130 - DBHHeight from the cut), never the file's DBH; 10 cm grid to the last measured point",
    ok(ok3), sprintf("DBH stem 1 (GRAN, at 110 cm) %.2f cm, stem 2 (FURU, at 120 cm; file DBH 999 mm) %.2f cm, stem 5 (no DBH in file) %.2f cm | stem 4: positions every 25 cm to 1012 -> grid 0-%d, interpolated %s | offsets %s | stem 3 increase kept %s",
                     td$dbh_cm[td$treid == "1"], td$dbh_cm[td$treid == "2"], td$dbh_cm[td$treid == "5"],
                     max(g4$diameterPosition), grid_ok, fn_ok, any(diff(prof_of("3")$DiameterValue) > 0)))


# ==============================================================================
section("04. Stump 0")
job <- prepare_hpr_job(p, params = list(stump_height_cm = 30L))
r1 <- run_hpr_bucking(p, workers = 1L, params = list(stump_height_cm = 30L))
sl <- r1$selected_logs_all
plan <- .candidate_species_plan("spruce", job$pps[[1]]$pd, job$pps[[1]]$lc)
cand1 <- .candidate_columns(job$pvs[["1"]], plan, job$stump_cm, job$step_cm, usable_only = TRUE)$cols
first_log <- tapply(sl$StartPos, sl$treid, min)
ok4 <- identical(job$stump_cm, 0L) && identical(r1$diagnostics$stump_height_cm, 0L) &&
  min(cand1$StartPos) == 0L && length(first_log) == 5L && all(first_log == 0L)
rec("Check 04: Bucking starts at the felling cut (stump 0), whatever params say",
    ok(ok4), sprintf("job stump %d cm (params asked 30) | stem 1: %d candidates, first start %d | first log per stem: %s",
                     job$stump_cm, length(cand1$StartPos), as.integer(min(cand1$StartPos)),
                     paste(sprintf("%s:%d", names(first_log), first_log), collapse = " ")))


# ==============================================================================
section("05. Nothing above the last measured point")
last_meas <- vapply(td$treid, function(k) max(pts(st, k)$p), 0)
end_cm <- round(td$height_m * 100)
cand_end <- vapply(td$treid, function(k)
  max(.candidate_columns(job$pvs[[k]], .candidate_species_plan(job$pvs[[k]]$species_group, job$pps[[1]]$pd,
                                                                job$pps[[1]]$lc),
                         0L, 10L, usable_only = FALSE)$cols$StopPos, -Inf), 0)
sel_end <- tapply(sl$StopPos, sl$treid, max)
ok5 <- all(end_cm <= last_meas) && all(cand_end <= end_cm) &&
  all(sel_end <= end_cm[match(names(sel_end), td$treid)])
rec("Check 05: No candidate or selected log ends above the last measured point",
    ok(ok5), paste(sprintf("stem %s: measured to %d, profile end %d, candidates to %d, logs to %d", td$treid,
                           as.integer(last_meas), as.integer(end_cm), as.integer(cand_end),
                           as.integer(sel_end[td$treid])), collapse = " | "))


# ==============================================================================
section("06. Under bark")
ub_ok <- vapply(names(job$pvs), function(k) {
  pv <- job$pvs[[k]]
  isTRUE(all.equal(pv$DiameterValueUB,
                   under_bark_diameter(pv$DiameterValue, td$dbh_cm[td$treid == k] * 10, pv$species_group)))
}, logical(1))
rec("Check 06: Under-bark profiles are barkNOR with the profile DBH along the whole stem",
    ok(all(ub_ok) && length(ub_ok) == 5L),
    sprintf("%d stems | stem 1 at 0 cm: %.1f mm over, %.1f mm under bark", length(ub_ok),
            job$pvs[["1"]]$DiameterValue[1], job$pvs[["1"]]$DiameterValueUB[1]))


# ==============================================================================
section("07. Species mapping and dropped stems")
want_w <- c("1 stem dropped: multi-tree stem (no single-stem profile).",
            "1 stem dropped: no stem profile (StemDiameters).",
            "1 stem dropped: profile recorded 'Under bark', not over bark (not supported).",
            "1 stem dropped: StemKey already used by an earlier stem.",
            "1 stem dropped: species group LAUV (447) is set to Not used.",
            "1 stem dropped: profile does not start at the felling cut (position 0).",
            "1 stem dropped: a diameter above 1,500 mm (likely a unit error).",
            "1 stem dropped: profile ends below 1.3 m above ground (no DBH).")
map_g <- map; map_g["446"] <- "none"
p_g <- hpr_stem_profiles(st, map_g)
p_none <- hpr_stem_profiles(st, setNames(rep("none", 4L), names(map)))
ok7 <- isTRUE(p$ok) && identical(p$n_read, 13L) && identical(p$n_used, 5L) &&
  identical(sort(td$treid), c("1", "2", "3", "4", "5")) &&
  identical(td$species_group[match(c("1", "2"), td$treid)], c("spruce", "pine")) &&
  identical(p$warnings, want_w) &&
  identical(p$excluded$treid, c("6", "7", "8", "9", "2", "11", "12", "13")) &&      # file order
  isTRUE(p_g$ok) && identical(p_g$tree_data$treid, "2") &&
  # stems 1, 3-5 and 11-13: the species check comes before the profile checks
  any(p_g$warnings == "7 stems dropped: species group GRAN (446) is set to Not used.") &&
  !isTRUE(p_none$ok) && grepl("No stems are left", p_none$errors)
rec("Check 07: Species mapping; dropped stems with one warning per reason",
    ok(ok7), sprintf("read %d, used %d (%s) | warnings: %s | GRAN Not used: %s | all Not used: %s",
                     p$n_read, p$n_used, paste(td$treid, td$species_group, collapse = ", "),
                     paste(p$warnings, collapse = " / "), paste(p_g$warnings[grepl("GRAN", p_g$warnings)], collapse = ""),
                     paste(p_none$errors, collapse = "")))


# ==============================================================================
section("08. .hpr.gz and .hpr in a .zip")
gz <- tempfile(fileext = ".hpr.gz"); con <- gzfile(gz, "wb"); writeLines(fx, con, useBytes = TRUE); close(con)
st_gz <- read_hpr_stems(gz, "fixture.hpr.gz")
zd <- tempfile("zipsrc"); dir.create(zd); file.copy(FIXTURE, file.path(zd, "fixture.hpr"))
zf <- tempfile(fileext = ".zip"); zip::zipr(zf, file.path(zd, "fixture.hpr"))
st_zip <- read_hpr_stems(zf, "fixture.zip")
writeLines("no hpr here", file.path(zd, "readme.txt"))
zn <- tempfile(fileext = ".zip"); zip::zipr(zn, file.path(zd, "readme.txt"))
st_nozip <- read_hpr_stems(zn, "other.zip")
ok8 <- isTRUE(st_gz$ok) && cmp_read(st, st_gz) && isTRUE(st_zip$ok) && cmp_read(st, st_zip) &&
  !isTRUE(st_nozip$ok) && any(grepl("contains no .hpr file", st_nozip$errors, fixed = TRUE))
rec("Check 08: .hpr.gz and .hpr in a .zip give the same stems; a zip without an .hpr is an error",
    ok(ok8), sprintf(".gz same %s | .zip same %s | zip without .hpr: '%s'", cmp_read(st, st_gz),
                     cmp_read(st, st_zip), paste(st_nozip$errors, collapse = " ")))
unlink(c(gz, zf, zn, zd), recursive = TRUE)


# ==============================================================================
section("09. Files without stem profiles or without stems")
no_prof <- function(x) sub("<StemDiameters.*?</StemDiameters>", "", x, perl = TRUE)
f_noprof <- with_stems(c(no_prof(stem_xml(1)), no_prof(stem_xml(2))))
f_nostem <- with_stems(character(0))
e_noprof <- read_hpr_stems(f_noprof, "logs_only.hpr")
e_nostem <- read_hpr_stems(f_nostem, "no_stems.hpr")
ok9 <- !isTRUE(e_noprof$ok) && identical(e_noprof$errors,
  "logs_only.hpr has 2 stems but none with a stem profile (StemDiameters): there is nothing to buck.") &&
  !isTRUE(e_nostem$ok) && identical(e_nostem$errors, "no_stems.hpr contains no stems.")
rec("Check 09: Stems without profiles (logs only) and a file without stems are errors",
    ok(ok9), sprintf("'%s' | '%s'", paste(e_noprof$errors, collapse = " "), paste(e_nostem$errors, collapse = " ")))
unlink(c(f_noprof, f_nostem))


# ==============================================================================
section("10. Privacy: coordinates, dates, operators and names are never read")
secret <- c("59.123456", "10.654321", "2026-01-02T03:04:05", "Invented Operator", "Invented Object")
extra <- sprintf('<OperatorKey>7</OperatorKey><HarvestDate>%s</HarvestDate><StemCoordinates receiverPosition="Base"><Latitude latitudeCategory="WGS84">%s</Latitude><Longitude longitudeCategory="WGS84">%s</Longitude><Altitude>123.4</Altitude></StemCoordinates><ObjectName>%s</ObjectName><OperatorName>%s</OperatorName>',
                 secret[3], secret[1], secret[2], secret[5], secret[4])
body10 <- dv_xml(s1, d1)
f10 <- with_stems(c(stem_xml(1, body = body10, extra = extra), stem_xml(2, sgk = 445, body = body10, extra = extra)))
st10 <- read_hpr_stems(f10, "private.hpr")
p10 <- hpr_stem_profiles(st10, map)
r10 <- run_hpr_bucking(p10, workers = 1L)
tabs <- c(Filter(is.data.frame, st10), list(stems = st10$stems, species = st10$species),
          Filter(is.data.frame, p10), Filter(is.data.frame, r10))
cols <- unique(unlist(lapply(tabs, names)))
bad_cols <- grep("(^|_)(lat|lon|latitude|longitude|coord|coordinates|altitude|date|harvestdate|operator|object|objectname)(_|$)",
                 cols, value = TRUE, ignore.case = TRUE)
vals <- unlist(lapply(tabs, function(t) unlist(lapply(t, as.character))), use.names = FALSE)
leaks <- secret[vapply(secret, function(s) any(grepl(s, c(vals, unlist(st10[c("info", "warnings")])), fixed = TRUE)), logical(1))]
ok10 <- isTRUE(st10$ok) && p10$n_used == 2L && length(bad_cols) == 0L && length(leaks) == 0L &&
  identical(names(st10$stems), c("stem_key", "species_key", "kind", "category", "n_points"))
rec("Check 10: No coordinate-like column and none of the invented coordinates, dates, operator or object names in any output",
    ok(ok10), sprintf("%d tables, %d columns | coordinate-like columns: %s | invented values found: %s | stem table columns: %s",
                      length(tabs), length(cols), if (length(bad_cols)) paste(bad_cols, collapse = ",") else "none",
                      if (length(leaks)) paste(leaks, collapse = ",") else "none", paste(names(st10$stems), collapse = ",")))
unlink(f10)


# ==============================================================================
section("11. End-to-end A/B run")
pm0 <- pm_minimal(load_price_matrix_long(root_dir))
pm1 <- pm0; saw <- grepl("sawlog", pm1$ProductKey); pm1$PriceNOKperM3[saw] <- pm1$PriceNOKperM3[saw] * 1.3
f11 <- tempfile(fileext = ".csv"); pm_export(pm1, f11)
r11 <- run_hpr_bucking(p, scenario = list(price_matrix_path = f11), workers = 1L,
                       params = list(file = basename(FIXTURE)))
cmp11 <- compare_cutting_patterns(r11, r11$scenario, pm0, pm1)
ok11 <- identical(r11$tree_source, "hpr") && identical(r11$scenario$tree_source, "hpr") &&
  setequal(r11$tree_summary$treid, td$treid) && all(is.na(r11$tree_summary$plot_id_github)) &&
  r11$n_plots_sampled == 0L && nrow(r11$excluded_trees) == 8L && nrow(cmp11) == 5L &&
  r11$scenario$per_m3_summary$nok_per_m3[1] > r11$per_m3_summary$nok_per_m3[1] &&
  all(r11$tree_summary$height_source == "Measured profile") && grepl(".hpr file only", r11$label, fixed = TRUE)
rec("Check 11: An A/B run on the fixture's stems: stems as trees, no plots, a comparison, higher NOK/m3 at higher sawlog prices",
    ok(ok11), sprintf("source %s | trees %s | plots %d | excluded %d | compared %d, changed %d | NOK/m3 %.1f -> %.1f | %d logs in A",
                      r11$tree_source, paste(sort(r11$tree_summary$treid), collapse = ","), r11$n_plots_sampled,
                      nrow(r11$excluded_trees), nrow(cmp11), sum(cmp11$changed),
                      r11$per_m3_summary$nok_per_m3[1], r11$scenario$per_m3_summary$nok_per_m3[1],
                      nrow(r11$selected_logs_all)))
unlink(f11)


# ==============================================================================
section("12. Results do not depend on the number of workers")
srt <- function(x) { x <- x[do.call(order, x[intersect(c("treid", "StartPos", "ProductKey", "Scope", "scope"), names(x))]), , drop = FALSE]
                     rownames(x) <- NULL; x }
tabs12 <- c("tree_summary", "selected_logs_all", "per_m3_summary", "per_m3_by_assortment")
r_w1 <- run_hpr_bucking(p, scenario = list(price_matrix_path = "default_2021"), workers = 1L)
old <- options(apter.chunk_size = 2L)
r_w2 <- run_hpr_bucking(p, scenario = list(price_matrix_path = "default_2021"), workers = 2L)
options(old)
same12 <- vapply(tabs12, function(t) identical(srt(r_w1[[t]]), srt(r_w2[[t]])) &&
                   identical(srt(r_w1$scenario[[t]]), srt(r_w2$scenario[[t]])), logical(1))
rec("Check 12: The same results on 1 worker and on 2 workers in chunks of 2 stems",
    ok(all(same12)), paste(sprintf("%s %s", tabs12, ifelse(same12, "identical", "DIFFERS")), collapse = " | "))
future::plan(future::sequential)


# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 26 complete.\n")
