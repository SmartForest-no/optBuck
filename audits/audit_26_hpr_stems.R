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
#  7.  Species mapping and dropped stems: the 8 dropped stems, one per reason;
#      warnings only for problems in the file (under bark, duplicate key, not
#      starting at 0, > 1,500 mm), the expected drops (multi-tree, no profile,
#      Not-used species, ending below 1.3 m) counted in info only; GRAN set to
#      Not used drops its stems without a warning; everything Not used is an
#      error
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
# 13.  Several files (Phase 2, 2026-10-28): (a) two copies of the fixture (keys
#      + 1000): unique keys, doubled counts, species merged by name, the same
#      per-stem results as one file, a file column; (b) the same file twice:
#      repeated keys dropped with a warning; (c) a failing file is skipped
#      with its position and reason; (d) all files failing is the error "None
#      of the files can be used."; (e) no uploaded file name in any result
#      table, params, run label, message or CSV download
# 14.  Cleaning (Phase 3, 2026-10-28): hpr_clean_flags() on 12 synthetic stems,
#      one passing and one failing per rule (broken top > 200 mm; minimum
#      fall 1 mm per 30 dm; maximum fall 30 mm per 1 dm and 60 mm per 10 dm),
#      boundaries exactly on the threshold pass, a range under 30 dm and a
#      plateau after 100 mm not checked; per-check counts, one warning line
#      per check; broken-top stems are kept (counted only), too flat and too
#      steep stems dropped once; fixture unchanged; Not-used species not
#      counted; counts summed over two files; Norsk translation
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
# Warnings only for problems in the file; expected drops are counted in info.
want_w <- c("1 stem dropped: profile recorded 'Under bark', not over bark (not supported).",
            "1 stem dropped: StemKey already used by an earlier stem.",
            "1 stem dropped: profile does not start at the felling cut (position 0).",
            "1 stem dropped: a diameter above 1,500 mm (likely a unit error).")
want_i <- c("1 stem dropped: multi-tree stem (no single-stem profile).",
            "1 stem dropped: no stem profile (StemDiameters).",
            "1 stem dropped: species group LAUV (447) is set to Not used.",
            "1 stem dropped: profile ends below 1.3 m above ground (no DBH).")
map_g <- map; map_g["446"] <- "none"
p_g <- hpr_stem_profiles(st, map_g)
p_none <- hpr_stem_profiles(st, setNames(rep("none", 4L), names(map)))
ok7 <- isTRUE(p$ok) && identical(p$n_read, 13L) && identical(p$n_used, 5L) &&
  identical(sort(td$treid), c("1", "2", "3", "4", "5")) &&
  identical(td$species_group[match(c("1", "2"), td$treid)], c("spruce", "pine")) &&
  identical(p$warnings, want_w) && all(want_i %in% p$info) &&
  identical(p$excluded$treid, c("6", "7", "8", "9", "2", "11", "12", "13")) &&      # file order
  isTRUE(p_g$ok) && identical(p_g$tree_data$treid, "2") &&
  # stems 1, 3-5 and 11-13: the species check comes before the profile checks
  any(p_g$info == "7 stems dropped: species group GRAN (446) is set to Not used.") &&
  !any(grepl("Not used", p_g$warnings, fixed = TRUE)) &&
  !isTRUE(p_none$ok) && grepl("No stems are left", p_none$errors)
rec("Check 07: Species mapping; dropped stems: warnings only for problems in the file, expected drops in info",
    ok(ok7), sprintf("read %d, used %d (%s) | warnings: %s | info: %s | GRAN Not used (info): %s | all Not used: %s",
                     p$n_read, p$n_used, paste(td$treid, td$species_group, collapse = ", "),
                     paste(p$warnings, collapse = " / "), paste(p$info[grepl("dropped", p$info)], collapse = " / "),
                     paste(p_g$info[grepl("GRAN", p_g$info)], collapse = ""),
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
section("13. Several files (Phase 2)")
# Two copies of the fixture, the second with every StemKey + 1000: keys are
# unique, counts double, the species groups merge by name, and each stem's
# result equals the single-file run. File names never reach any output.
f2 <- tempfile(fileext = ".hpr"); x2 <- fx; ii <- seq_along(x2) >= first_stem
for (j in which(ii & grepl("<StemKey>[0-9]+</StemKey>", x2))) {
  m <- regmatches(x2[j], gregexpr("<StemKey>[0-9]+</StemKey>", x2[j]))[[1L]]
  for (s in unique(m)) x2[j] <- gsub(s, sprintf("<StemKey>%d</StemKey>", 1000L + as.integer(gsub("[^0-9]", "", s))), x2[j], fixed = TRUE)
}
writeLines(x2, f2, useBytes = TRUE)
nm13 <- c("Landowner_Smith_a.hpr", "Landowner_Smith_b.hpr")
st13 <- read_hpr_stem_files(c(FIXTURE, f2), nm13)
map13 <- setNames(guess_hpr_species(st13$species$name, st13$species$user_id), st13$species$key)
ck13 <- hpr_stem_checks(st13, map13)
p13 <- hpr_stem_profiles(st13, map13)
r13 <- run_hpr_bucking(p13, workers = 1L)
r1 <- run_hpr_bucking(p, workers = 1L)
ts13 <- r13$tree_summary; ts1 <- r1$tree_summary
k2 <- paste0(1000L + as.integer(ts1$treid))
cmp_cols <- c("total_value_nok", "commercial_volume_m3", "n_logs_selected")
same_f1 <- isTRUE(all.equal(ts13[match(ts1$treid, ts13$treid), cmp_cols], ts1[, cmp_cols], check.attributes = FALSE))
same_f2 <- isTRUE(all.equal(ts13[match(k2, ts13$treid), cmp_cols], ts1[, cmp_cols], check.attributes = FALSE))
sl13 <- r13$selected_logs_all; sl1 <- r1$selected_logs_all
logs_eq <- nrow(sl13) == 2L * nrow(sl1) && isTRUE(all.equal(sum(sl13$value_nok), 2 * sum(sl1$value_nok)))
files_ok <- setequal(ts13$file, 1:2) && all(ts13$file[ts13$treid %in% ts1$treid] == 1L) &&
  all(sl13$file == ts13$file[match(sl13$treid, ts13$treid)])
uniq_ok <- !anyDuplicated(ts13$treid) && !anyDuplicated(p13$tree_data$treid)
n_names <- length(unique(tolower(trimws(st$species$name))))
sp_ok <- length(unique(st13$species$name_id)) == n_names && nrow(st13$species) == 2L * nrow(st$species)
cnt_ok <- ck13$n_read == 26L && ck13$n_used == 10L && st13$n_files == 2L && nrow(st13$failed) == 0L
# Privacy: the file names appear nowhere.
tabs13 <- c(Filter(is.data.frame, st13), Filter(is.data.frame, p13), Filter(is.data.frame, r13))
vals13 <- c(unlist(lapply(tabs13, function(t) unlist(lapply(t, as.character))), use.names = FALSE),
            unlist(st13[c("info", "warnings", "errors")]), ck13$info, ck13$warnings,
            unlist(r13$params), r13$run_id, r13$label)
name_ok <- !any(grepl("Landowner|Smith", vals13)) && is.na(st13$file) && is.na(ck13$file)
ok13a <- cnt_ok && uniq_ok && sp_ok && same_f1 && same_f2 && logs_eq && files_ok && name_ok
rec("Check 13a: Two copies of the fixture (keys + 1000): unique keys, doubled counts, species merged by name, the same per-stem results as one file, a file column, no file names",
    ok(ok13a), sprintf("read %d, used %d, files %d | unique keys %s | species groups %d -> %d names %s | stems of file 1 = single run %s, file 2 %s | logs %d = 2 x %d (%s) | file column %s | names absent %s",
                       ck13$n_read, ck13$n_used, st13$n_files, uniq_ok, nrow(st13$species), n_names, sp_ok,
                       same_f1, same_f2, nrow(sl13), nrow(sl1), logs_eq, files_ok, name_ok))

# Identical copies: file 2's stems repeat file 1's StemKeys and are dropped.
st13b <- read_hpr_stem_files(c(FIXTURE, FIXTURE), nm13)
ck13b <- hpr_stem_checks(st13b, setNames(guess_hpr_species(st13b$species$name, st13b$species$user_id), st13b$species$key))
dup_ok <- ck13b$n_read == 26L && ck13b$n_used == 5L &&
  any(grepl("13 stems dropped: StemKey already used by an earlier stem|StemKey already used", ck13b$warnings))
rec("Check 13b: The same file twice: the second file's stems are dropped as already-used StemKeys, with a warning",
    ok(dup_ok), sprintf("read %d, used %d | warnings: %s", ck13b$n_read, ck13b$n_used, paste(ck13b$warnings, collapse = " / ")))

# A file that fails is skipped (named by position); the others are used.
f_np <- with_stems(c(no_prof(stem_xml(1)), no_prof(stem_xml(2))))
st13c <- read_hpr_stem_files(c(FIXTURE, f_np), c(nm13[1L], "Landowner_Smith_c.hpr"))
ck13c <- hpr_stem_checks(st13c, setNames(guess_hpr_species(st13c$species$name, st13c$species$user_id), st13c$species$key))
fail_ok <- isTRUE(st13c$ok) && identical(st13c$failed$file, 2L) && identical(st13c$failed$reason, "no stem profiles") &&
  st13c$n_files == 1L && ck13c$n_read == 13L && ck13c$n_used == 5L && !any(grepl("Smith", unlist(st13c$failed)))
rec("Check 13c: A failing file is skipped with its position and reason, the others are used",
    ok(fail_ok), sprintf("file %s: '%s' | used files %d, stems %d/%d",
                         paste(st13c$failed$file, collapse = ","), paste(st13c$failed$reason, collapse = ","),
                         st13c$n_files, ck13c$n_used, ck13c$n_read))

# (d) All files fail: an error, each file named by position and reason only.
f_nostem2 <- with_stems(character(0))
nm13d <- c("Landowner_Smith_x.hpr", "Landowner_Smith_y.hpr")
st13d <- read_hpr_stem_files(c(f_np, f_nostem2), nm13d)
ck13d <- hpr_stem_checks(st13d, c())
all_fail <- !isTRUE(st13d$ok) && identical(st13d$errors, "None of the files can be used.") &&
  identical(st13d$failed$file, 1:2) && identical(st13d$failed$reason, c("no stem profiles", "no stems")) &&
  !isTRUE(ck13d$ok) && identical(ck13d$errors, "None of the files can be used.") &&
  !any(grepl("Smith", c(unlist(st13d), unlist(ck13d))))
rec("Check 13d: All files failing is an error 'None of the files can be used.', with each file's position and reason and no file name",
    ok(all_fail), sprintf("ok %s | errors '%s' | files %s: %s", st13d$ok, paste(st13d$errors, collapse = " "),
                          paste(st13d$failed$file, collapse = ","), paste(st13d$failed$reason, collapse = ",")))

# (e) No uploaded file name in any result table, params, run label, message
# or download. A and B run on the two named files; every data frame of the
# results (both scenarios, the read and the profiles) is written as CSV, the
# way the app's downloads are, and the text searched.
pm_e <- pm0; saw_e <- grepl("sawlog", pm_e$ProductKey); pm_e$PriceNOKperM3[saw_e] <- pm_e$PriceNOKperM3[saw_e] * 1.3
f_e <- tempfile(fileext = ".csv"); pm_export(pm_e, f_e)
r13e <- run_hpr_bucking(p13, scenario = list(price_matrix_path = f_e), workers = 1L, params = list())
frames <- function(x) Filter(is.data.frame, x)
tabs_e <- c(frames(r13e), frames(r13e$scenario), frames(st13), frames(p13))
dl_text <- unlist(lapply(tabs_e, function(t) {
  f <- tempfile(fileext = ".csv"); write_download_csv(t, f, "en"); on.exit(unlink(f))
  readLines(f, warn = FALSE, encoding = "UTF-8")
}), use.names = FALSE)
meta_e <- c(unlist(r13e$params), r13e$run_id, r13e$label, r13e$scenario$run_id, r13e$scenario$label,
            unlist(r13e$diagnostics[vapply(r13e$diagnostics, is.character, TRUE)]), r13e$height_warnings,
            unlist(st13[c("info", "warnings", "errors")]), ck13$info, ck13$warnings)
leak_e <- grepl("Landowner|Smith|hpr_example|second_upload", c(dl_text, meta_e))
names_e <- "file" %in% names(r13e$tree_summary) && "file" %in% names(r13e$selected_logs_all) &&
  setequal(r13e$tree_summary$file, 1:2) && setequal(r13e$selected_logs_all$file, 1:2)
rec("Check 13e: No uploaded file name in any result table (A and B), params, run label, message or CSV download; the file column is 1/2",
    ok(!any(leak_e) && names_e), sprintf("%d tables, %d CSV lines, %d messages searched | file names found: %d | file column in trees and logs: %s",
                                         length(tabs_e), length(dl_text), length(meta_e), sum(leak_e), names_e))
unlink(c(f2, f_np, f_nostem2, f_e))


# ==============================================================================
section("14. Cleaning (Phase 3): broken top, minimum fall, maximum fall")
kn <- function(x, y) { s <- seq(0, max(x), by = 10); list(p = s, d = stats::approx(x, y, xout = s)$y) }
gr <- function(f, to) { s <- seq(0, to, by = 10); list(p = s, d = f(s)) }
fl <- function(b, f, s) c(b, f, s) == 1                       # expected: broken top, too flat, too steep
s5 <- seq(0, 1800, by = 10)
cases <- list(                                                # profile, expected flags
  base_ok          = list(list(p = s1, d = d1), fl(0, 0, 0)),
  broken_top       = list(gr(function(s) 330 - 0.1 * s, 1000), fl(1, 0, 0)),              # ends at 230 mm
  min_fall_exact   = list(gr(function(s) 205 - s / 300, 1800), fl(0, 0, 0)),               # exactly 1 mm per 30 dm
  min_fall_slow    = list(gr(function(s) 205 - 0.9 * s / 300, 1800), fl(0, 1, 0)),         # 0.9 mm per 30 dm
  plateau          = list(list(p = s5, d = 330 - 0.15 * ifelse(s5 <= 800, s5, ifelse(s5 <= 1300, 800, s5 - 500))),
                          fl(0, 1, 0)),                                                    # 5 m without a fall
  step_31mm        = list(list(p = s1, d = d1 - 31 * (s1 >= 1010)), fl(0, 0, 1)),          # 33 mm over 1 dm
  step_30mm_exact  = list(kn(c(0, 500, 510, 1800), c(205, 200, 170, 160)), fl(0, 0, 0)),   # exactly 30 mm over 1 dm
  fall_60_exact    = list(gr(function(s) 600 - 0.6 * s, 800), fl(0, 0, 0)),                # exactly 60 mm over 10 dm
  fall_61          = list(gr(function(s) 600 - 0.61 * s, 800), fl(0, 0, 1)),               # 61 mm over 10 dm
  short_flat_range = list(gr(function(s) rep(150, length(s)), 400), fl(0, 0, 0)),          # 2 m..4 m: under 30 dm, not checked
  plateau_below_10 = list(kn(c(0, 1500, 1600, 2400), c(330, 105, 99, 98)), fl(0, 0, 0)),   # flat only after 100 mm is reached
  broken_and_steep = list(kn(c(0, 500, 510, 1000), c(500, 450, 410, 380)), fl(1, 0, 1)))   # fails two checks
exp14 <- do.call(rbind, lapply(cases, `[[`, 2))
mk14 <- function(off) unlist(lapply(seq_along(cases), function(i)
  stem_xml(i + off, body = dv_xml(cases[[i]][[1L]]$p, cases[[i]][[1L]]$d))))
f14 <- with_stems(mk14(0L))
st14 <- read_hpr_stems(f14)
map14 <- setNames(guess_hpr_species(st14$species$name, st14$species$user_id), st14$species$key)
fl14 <- as.matrix(hpr_clean_flags(st14$idx, st14$pos, st14$d, nrow(st14$stems)))
ok14a <- isTRUE(st14$ok) && identical(unname(fl14), unname(exp14))
bad14 <- names(cases)[apply(fl14 != exp14, 1L, any)]
rec("Check 14a: hpr_clean_flags(): each synthetic stem (one passing and one failing per rule, boundaries exactly 1 mm / 30 mm / 60 mm pass) is flagged as expected",
    ok(ok14a), sprintf("%d stems | mismatches: %s", length(cases), if (length(bad14)) paste(bad14, collapse = ", ") else "none"))

ck14 <- hpr_stem_checks(st14, map14)
w14 <- c("2 stems with a broken top (last diameter above 200 mm).",
         "2 stems dropped: profile falls less than 1 mm per 30 dm.",
         "3 stems dropped: profile falls more than 30 mm over 1 dm or 60 mm over 10 dm.")
pr14 <- hpr_stem_profiles(st14, map14)
rs14 <- setNames(pr14$excluded$excluded_reason, pr14$excluded$treid)
ok14b <- identical(unname(ck14$clean_counts), c(2L, 2L, 3L)) && ck14$n_read == 12L && ck14$n_used == 7L &&
  setequal(ck14$warnings, w14) && length(ck14$warnings) == 3L && !any(grepl("falls|broken", ck14$info)) &&
  nrow(pr14$tree_data) == 7L && nrow(pr14$excluded) == 5L &&
  all(c("2", "12") %in% pr14$tree_data$treid == c(TRUE, FALSE)) &&   # broken top kept; broken + steep dropped as steep
  !"2" %in% names(rs14) && startsWith(rs14[["12"]], "profile falls more") &&
  startsWith(rs14[["5"]], "profile falls less") && startsWith(rs14[["6"]], "profile falls more")
rec("Check 14b: per-check counts 2 / 2 / 3 (a stem failing two checks counted in both), a broken top only counted (the stem is kept), too flat and too steep dropped once: 7 of 12 stems used, one warning line per check in the existing style",
    ok(ok14b), sprintf("counts %s | used %d/%d | warnings: %s", paste(ck14$clean_counts, collapse = "/"),
                       ck14$n_used, ck14$n_read, paste(ck14$warnings, collapse = " | ")))

# The fixture is untouched; Not-used species are not counted; several files add up.
ckfx <- hpr_stem_checks(st, map)
ck14n <- hpr_stem_checks(st14, setNames(rep("none", length(map14)), names(map14)))
f14b <- with_stems(mk14(100L))
st14ab <- read_hpr_stem_files(c(f14, f14b), c("a.hpr", "b.hpr"))
ck14ab <- hpr_stem_checks(st14ab, setNames(guess_hpr_species(st14ab$species$name, st14ab$species$user_id), st14ab$species$key))
ok14c <- sum(ckfx$clean_counts) == 0L && ckfx$n_used == 5L && sum(ck14n$clean_counts) == 0L && !isTRUE(ck14n$ok) &&
  identical(unname(ck14ab$clean_counts), c(4L, 4L, 6L)) && ck14ab$n_read == 24L && ck14ab$n_used == 14L &&
  any(ck14ab$warnings == "4 stems with a broken top (last diameter above 200 mm).") &&
  any(ck14ab$warnings == "6 stems dropped: profile falls more than 30 mm over 1 dm or 60 mm over 10 dm.")
rec("Check 14c: the 13 fixture stems are not flagged (5 still used); stems of a Not-used species are not counted; two files give summed counts 4 / 4 / 6",
    ok(ok14c), sprintf("fixture flagged %d, used %d | Not used: %d flagged | two files: %s, used %d/%d",
                       sum(ckfx$clean_counts), ckfx$n_used, sum(ck14n$clean_counts),
                       paste(ck14ab$clean_counts, collapse = "/"), ck14ab$n_used, ck14ab$n_read))

if (!exists("tr_msg", mode = "function")) source(file.path(root_dir, "R", "helpers_i18n.R"))
i18n_reset_misses()
no14 <- tr_msg(ck14$warnings, "no")
ok14d <- !length(i18n_misses()) && all(no14 != ck14$warnings) && any(grepl("2 stammer med knekt topp \\(siste diameter over 200 mm\\)", no14)) &&
  any(grepl("faller mindre enn 1 mm per 30 dm", no14)) && any(grepl("faller mer enn 30 mm over 1 dm eller 60 mm over 10 dm", no14))
rec("Check 14d: the three warning lines translate to Norsk without a miss",
    ok(ok14d), paste(no14, collapse = " | "))
unlink(c(f14, f14b))


# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 26 complete.\n")
