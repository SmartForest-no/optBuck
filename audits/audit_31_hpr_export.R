# ==============================================================================
# audits/audit_31_hpr_export.R
#
# Purpose : Phase 5a (2026-10-28): simulation results exported as .hpr, one file
#           per scenario (R/helpers_hpr_export.R). Runs on variants of
#           inputs/test_fixtures/hpr_example_synthetic.hpr written to tempfiles
#           (invented coordinates, operator, contractor and object names, a
#           buyer, stem grades, recorded logs). The owner's real files are
#           never used here.
#
# Checks  :
#  1.  Round trip: the export reads back with read_hpr() and read_hpr_stems();
#      one stem per bucked stem (the dropped ones are left out), one Log per
#      selected log with its length, in position order, and a ProductKey that
#      the file defines
#  2.  Stems are copied unchanged: each exported stem equals its source stem
#      (StemGrade, DBH, StemDiameters, other children) except that the
#      StemCoordinates and the recorded Logs are gone and the simulated logs
#      are in
#  3.  Header, Machine and object data are copied from the source (invented
#      MachineKey, contractor, operator, object name present), coordinates
#      never: none of the invented coordinates and no coordinate element
#  4.  Product keys: a product with a HprProductKey keeps its key and its
#      metadata (the invented buyer); a second assortment from the same
#      product and an assortment without a key get new, unique keys above the
#      file's; the scenario's limits and prices are in the definitions
#  5.  Log content: Top and Mid diameters and the three volumes follow the
#      profile (volumes: m3 (price) = the result's price volume; m3sob >=
#      m3sub), cutting reason Automatic
#  6.  The coordinate check: a coordinate element in a copied block makes the
#      export fail and removes the file
#  7.  Two source files: per-file stem counts equal the result's, keys stay as
#      written, every stem's SpeciesGroupKey is defined in the header
#  8.  Re-bucking the exported stems under the same scenario gives the same
#      cutting patterns (same logs per stem)
#  9.  Messages: the export's error messages translate to Norsk
# 10.  Gaps (2026-10-28): a gap between logs and a missing first log are written as
#      logs of product 999999 so positions stay consecutive; the unclassified
#      ProductDefinition is added when the source lacks it
#
# Verdict : PASS / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/hpr_export/audit_31_hpr_export.csv
#   outputs/hpr_export/audit_31_hpr_export.txt
#
# Usage   : Rscript audits/audit_31_hpr_export.R
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

suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)
source(file.path(root_dir, "audits", "helpers_audit.R"))


out_dir <- path_output("hpr_export", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_31_hpr_export.txt")
audit_csv <- file.path(out_dir, "audit_31_hpr_export.csv")
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
cat("  Audit: .hpr export (Phase 5a)\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

FIXTURE <- file.path(root_dir, "inst", "extdata", "inputs", "test_fixtures", "hpr_example_synthetic.hpr")
txt <- paste(readLines(FIXTURE, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
i1 <- regexpr("<Stem>", txt, fixed = TRUE)[1L]
head_txt <- substr(txt, 1L, i1 - 1L); stems_txt <- substr(txt, i1, nchar(txt))

# The fixture with invented private data, recorded logs and stem grades.
LAT <- "61.123456"; LON <- "11.654321"
head_v <- sub("(<Machine[^>]*>)", paste0("\\1<MachineKey>VIN-INVENTED-1</MachineKey>",
              "<LoggingContractor><BusinessName>Invented Contractor AS</BusinessName></LoggingContractor>",
              "<OperatorDefinition><OperatorKey>1</OperatorKey><ContactInformation><FirstName>Invented",
              "</FirstName></ContactInformation></OperatorDefinition>"), head_txt)
head_v <- sub("</ProductName>", paste0("</ProductName><ProductBuyer><Address><City>Invented Buyer City",
              "</City></Address></ProductBuyer>"), head_v, fixed = TRUE)
obj <- paste0("<ObjectDefinition><ObjectName>Invented Object</ObjectName><Extension><HPRCM><DefaultCoordinate>",
              '<Latitude latitudeCategory="North">', LAT, '</Latitude><Longitude longitudeCategory="East">', LON,
              "</Longitude></DefaultCoordinate></HPRCM></Extension></ObjectDefinition>")
stems_v <- gsub("(<SpeciesGroupKey>[0-9]+</SpeciesGroupKey>)",
                paste0("\\1<StemCoordinates><Latitude latitudeCategory=\"North\">", LAT,
                       "</Latitude><Longitude longitudeCategory=\"East\">", LON, "</Longitude></StemCoordinates>"),
                stems_txt)
stems_v <- gsub("(<DBH>[0-9]+</DBH>)", "\\1<StemGrade><GradeValue gradeStartPosition=\"0\">3</GradeValue></StemGrade>", stems_v)
stems_v <- gsub("</SingleTreeProcessedStem>",
                "<Log><LogKey>1</LogKey><ProductKey>1</ProductKey><LogMeasurement><LogLength>111</LogLength></LogMeasurement></Log></SingleTreeProcessedStem>",
                stems_v, fixed = TRUE)
SRC <- tempfile(fileext = ".hpr")
writeLines(paste0(head_v, obj, stems_v), SRC, useBytes = TRUE)

# Scenario (the file's products) and a run on it.
h <- read_hpr(SRC)
st <- read_hpr_stems(SRC)
map <- setNames(guess_hpr_species(st$species$name, st$species$user_id), st$species$key)
sc <- hpr_to_scenario(h, map)
pm <- pm_minimal(sc$pm); pp <- products_from_defs(sc$defs, pm)
pm_file <- tempfile(fileext = ".rds"); saveRDS(pm, pm_file)
prep <- hpr_stem_profiles(st, map)
run <- function(prepared)
  run_hpr_bucking(prepared, price_matrix_path = pm_file, products = pp$pd, length_classes = pp$lc, workers = 1L)
r <- run(prep)
sbn <- setNames(unname(map), tolower(trimws(st$species$name)))
src_df <- data.frame(path = SRC, name = "source.hpr", stringsAsFactors = FALSE)
OUT <- tempfile(fileext = ".hpr")
x <- hpr_export_result(OUT, src_df, sbn, r, sc$defs, pm)
raw_out <- paste(readLines(OUT, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
doc <- xml2::read_xml(OUT); xml2::xml_ns_strip(doc)
xstems <- xml2::xml_find_all(doc, "/*/Machine/Stem")
xkeys <- xml2::xml_text(xml2::xml_find_first(xstems, "./StemKey"))
xlogs <- function(s) xml2::xml_find_all(s, "./SingleTreeProcessedStem/Log")


# ==============================================================================
section("01. Round trip")
h2 <- read_hpr(OUT); st2 <- read_hpr_stems(OUT)
pkeys_file <- xml2::xml_text(xml2::xml_find_all(doc, "/*/Machine/ProductDefinition/ProductKey"))
log_pk <- xml2::xml_text(xml2::xml_find_all(doc, "//Log/ProductKey"))
len_ok <- all(vapply(seq_along(xstems), function(i) {
  L <- r$selected_logs_all[as.character(r$selected_logs_all$treid) == xkeys[i], , drop = FALSE]
  L <- L[order(L$StartPos), ]
  identical(as.numeric(xml2::xml_text(xml2::xml_find_all(xlogs(xstems[[i]]), "./LogMeasurement/LogLength"))),
            as.numeric(L$StopPos - L$StartPos))
}, NA))
ok1 <- isTRUE(x$ok) && isTRUE(h2$ok) && isTRUE(st2$ok) && length(xstems) == nrow(r$tree_summary) &&
  setequal(xkeys, as.character(r$tree_summary$treid)) && length(log_pk) == nrow(r$selected_logs_all) &&
  all(log_pk %in% pkeys_file) && len_ok && nrow(st$stems) > nrow(r$tree_summary)
rec("Check 01: The export reads back; one stem per bucked stem (dropped ones left out), one Log per selected log in position order, ProductKeys defined",
    ok(ok1), sprintf("%d stems of %d read, %d logs, lengths ok %s, export ok %s", length(xstems), nrow(st$stems),
                     length(log_pk), len_ok, x$ok))


# ==============================================================================
section("02. Stems are copied unchanged")
recs <- regmatches(stems_v, gregexpr("(?s)<Stem>.*?</Stem>", stems_v, perl = TRUE))[[1L]]
src_key <- sub("(?s).*?<StemKey>([^<]*)</StemKey>.*", "\\1", recs, perl = TRUE)
norm <- function(s) gsub("\\s+", "", gsub("(?s)<Log>.*?</Log>", "", gsub("(?s)<StemCoordinates>.*?</StemCoordinates>", "",
                                                                       s, perl = TRUE), perl = TRUE))
same <- vapply(seq_along(xstems), function(i) {
  a <- recs[match(xkeys[i], src_key)]
  b <- regmatches(raw_out, gregexpr("(?s)<Stem>.*?</Stem>", raw_out, perl = TRUE))[[1L]][i]
  identical(norm(a), norm(b))
}, NA)
n_grade_src <- sum(lengths(regmatches(recs[match(xkeys, src_key)], gregexpr("<StemGrade>", recs[match(xkeys, src_key)], fixed = TRUE))))
n_grade_out <- length(xml2::xml_find_all(doc, "//StemGrade"))
ok2 <- all(same) && n_grade_src > 0L && n_grade_out == n_grade_src &&
  length(xml2::xml_find_all(doc, "//StemCoordinates")) == 0L
rec("Check 02: Each exported stem equals its source stem apart from StemCoordinates and Logs; StemGrade kept",
    ok(ok2), sprintf("%d/%d identical, StemGrade %d of %d kept", sum(same), length(same), n_grade_out, n_grade_src))


# ==============================================================================
section("03. Header, Machine and object data copied; coordinates never")
ok3 <- grepl("VIN-INVENTED-1", raw_out, fixed = TRUE) && grepl("Invented Contractor AS", raw_out, fixed = TRUE) &&
  grepl("<FirstName>Invented</FirstName>", raw_out, fixed = TRUE) && grepl("Invented Object", raw_out, fixed = TRUE) &&
  !grepl(LAT, raw_out, fixed = TRUE) && !grepl(LON, raw_out, fixed = TRUE) &&
  !grepl("<Latitude|<Longitude|<Altitude|StemCoordinates|DefaultCoordinate", raw_out) &&
  !.hpr_export_has_coordinates(OUT) && .hpr_export_has_coordinates(SRC)
rec("Check 03: Machine key, contractor, operator and object name are copied; no coordinate element or invented coordinate value in the file",
    ok(ok3), sprintf("coordinate scan: export %s, source %s", .hpr_export_has_coordinates(OUT), .hpr_export_has_coordinates(SRC)))


# ==============================================================================
section("04. Product keys and metadata")
src_keys <- xml2::xml_text(xml2::xml_find_all(xml2::xml_ns_strip(xml2::read_xml(SRC)), "/*/Machine/ProductDefinition/ProductKey"))
d2 <- sc$defs
extra1 <- d2[1L, ]; extra1$Name <- "Second from same product"
extra2 <- d2[d2$Type == "waste", ][1L, ]; extra2$Name <- "Extra assortment"; extra2$HprProductKey <- NA_character_
d2 <- rbind(d2, extra1, extra2); rownames(d2) <- NULL
pm2 <- pm_minimal(pm_bind(pm, pm_start_prices(assortment_table_keys(d2)[nrow(d2) - 1L], "matrix", 100, 10, 12, c(310, 340)),
                        pm_start_prices(assortment_table_keys(d2)[nrow(d2)], "flat", 5)))
pr <- hpr_export_products(d2, pm2, setNames(as.list(rep("", length(src_keys))), src_keys),
                          data.frame(key = h$species$key, species = unname(map[h$species$key])))
kk <- pr$keys
first_hpr <- sc$defs$HprProductKey[1L]
new_keys <- kk[c(length(kk) - 1L, length(kk))]
ok4a <- identical(unname(kk[1L]), first_hpr) && !any(duplicated(kk)) &&
  all(as.numeric(new_keys) > max(setdiff(as.numeric(src_keys), 999999))) && !any(new_keys %in% src_keys) &&
  all(kk[!is.na(d2$HprProductKey)][!duplicated(d2$HprProductKey[!is.na(d2$HprProductKey)])] %in% src_keys)
first_def <- xml2::xml_find_first(doc, sprintf("/*/Machine/ProductDefinition[ProductKey='%s']", first_hpr))
buyer <- xml2::xml_text(xml2::xml_find_first(first_def, "./ClassifiedProductDefinition/ProductBuyer"))
dmin <- as.numeric(xml2::xml_text(xml2::xml_find_first(first_def, ".//DiameterMINTop")))
ok4b <- grepl("Invented Buyer City", buyer) && !is.na(dmin) && dmin == sc$defs$MinTopDiameter_cm[1L] * 10 &&
  length(xml2::xml_find_all(first_def, ".//ProductMatrixItem")) > 0L
rec("Check 04: A product with a HprProductKey keeps its key and metadata; repeated and key-less assortments get new unique keys above the file's; limits and prices come from the scenario",
    ok(ok4a && ok4b), sprintf("keys %s | new %s | buyer '%s' | MINTop %s", paste(kk, collapse = ","),
                              paste(new_keys, collapse = ","), buyer, dmin))


# ==============================================================================
section("05. Log content")
lg1 <- xlogs(xstems[[1L]])[[1L]]
L1 <- r$selected_logs_all[as.character(r$selected_logs_all$treid) == xkeys[1L], ]; L1 <- L1[order(L1$StartPos), ][1L, ]
v <- function(cat) as.numeric(xml2::xml_text(xml2::xml_find_first(lg1, sprintf("./LogVolume[@logVolumeCategory='%s']", cat))))
dg <- function(cat) as.numeric(xml2::xml_text(xml2::xml_find_first(lg1, sprintf(".//LogDiameter[@logDiameterCategory='%s']", cat))))
ok5 <- abs(v("m3 (price)") - L1$price_volume_m3) < 1e-4 && v("m3sob") >= v("m3sub") && v("m3sub") > 0 &&
  dg("Top ob") >= dg("Top ub") && dg("Mid ob") >= dg("Top ob") &&
  identical(xml2::xml_text(xml2::xml_find_first(lg1, ".//CuttingReason")), "Automatic")
rec("Check 05: m3 (price) = the result's price volume, m3sob >= m3sub, Mid >= Top diameters, ob >= ub, cutting reason Automatic",
    ok(ok5), sprintf("price %.5f vs %.5f | sob %.5f sub %.5f | Mid ob %d Top ob %d Top ub %d", v("m3 (price)"),
                     L1$price_volume_m3, v("m3sob"), v("m3sub"), dg("Mid ob"), dg("Top ob"), dg("Top ub")))


# ==============================================================================
section("06. The coordinate check")
bad_src <- tempfile(fileext = ".hpr")
writeLines(sub("<Machine([^>]*)>", paste0("<Machine\\1><Extension><MachinePosition><Latitude>61</Latitude>",
              "</MachinePosition></Extension>"), paste0(head_v, obj, stems_v)), bad_src, useBytes = TRUE)
bad_out <- tempfile(fileext = ".hpr")
xb <- hpr_export_result(bad_out, data.frame(path = bad_src, name = "bad.hpr"), sbn, r, sc$defs, pm)
ok6 <- identical(xb$ok, FALSE) && !file.exists(bad_out) && any(grepl("Coordinates found", xb$errors))
rec("Check 06: A coordinate element in a copied block makes the export fail and removes the file",
    ok(ok6), paste(xb$errors, collapse = " | "))
unlink(bad_src)


# ==============================================================================
section("07. Two source files")
mk2 <- function(off) {
  s2 <- gsub("<StemKey>([0-9]+)</StemKey>", sprintf("<StemKey>\\1%d</StemKey>", off), paste0(head_v, obj, stems_v))
  f <- tempfile(fileext = ".hpr"); writeLines(s2, f, useBytes = TRUE); f
}
SRC2 <- mk2(7)
st12 <- read_hpr_stem_files(c(SRC, SRC2), c("a.hpr", "b.hpr"))
map12 <- setNames(guess_hpr_species(st12$species$name, st12$species$user_id), st12$species$key)
r12 <- run(hpr_stem_profiles(st12, map12))
sbn12 <- setNames(unname(map12), tolower(trimws(st12$species$name)))
OUT2 <- tempfile(fileext = ".hpr")
x12 <- hpr_export_result(OUT2, data.frame(path = c(SRC, SRC2), name = c("a.hpr", "b.hpr")), sbn12, r12, sc$defs, pm)
doc12 <- xml2::read_xml(OUT2); xml2::xml_ns_strip(doc12)
k12 <- xml2::xml_text(xml2::xml_find_all(doc12, "/*/Machine/Stem/StemKey"))
sg12 <- unique(xml2::xml_text(xml2::xml_find_all(doc12, "/*/Machine/Stem/SpeciesGroupKey")))
def12 <- xml2::xml_text(xml2::xml_find_all(doc12, "/*/Machine/SpeciesGroupDefinition/SpeciesGroupKey"))
ok7 <- isTRUE(x12$ok) && identical(as.integer(x12$per_file), as.integer(table(factor(r12$tree_summary$file, 1:2)))) &&
  length(k12) == nrow(r12$tree_summary) && setequal(k12, as.character(r12$tree_summary$treid)) && all(sg12 %in% def12)
rec("Check 07: Two source files: per-file stem counts equal the result's, keys stay as written, every SpeciesGroupKey is defined",
    ok(ok7), sprintf("per file %s vs %s | %d stems | stem groups %s defined %s", paste(x12$per_file, collapse = "/"),
                     paste(table(factor(r12$tree_summary$file, 1:2)), collapse = "/"), length(k12),
                     paste(sg12, collapse = ","), paste(def12, collapse = ",")))
unlink(c(SRC2, OUT2))


# ==============================================================================
section("08. Re-bucking the export gives the same patterns")
st3 <- read_hpr_stems(OUT)
map3 <- setNames(guess_hpr_species(st3$species$name, st3$species$user_id), st3$species$key)
r3 <- run(hpr_stem_profiles(st3, map3))
key_l <- function(res) { l <- res$selected_logs_all
  l <- l[order(as.character(l$treid), l$StartPos), ]
  paste(l$treid, l$StartPos, l$StopPos, l$ProductKey) }
ok8 <- identical(key_l(r), key_l(r3)) && nrow(r3$tree_summary) == nrow(r$tree_summary)
rec("Check 08: Bucking the exported file's stems under the same scenario gives the same logs per stem",
    ok(ok8), sprintf("%d vs %d logs", nrow(r$selected_logs_all), nrow(r3$selected_logs_all)))


# ==============================================================================
section("09. Messages")
i18n_reset_misses()
m <- c("The source .hpr file(s) are no longer available.", "The source file is not UTF-8; it cannot be exported.",
       "The source file has no stems.", "Coordinates found in the export; the file was not written.",
       "The source file has no classified products.")
no <- tr_msg(m, "no")
rec("Check 09: The export's messages translate to Norsk without a miss",
    ok(!length(i18n_misses()) && all(no != m)), paste(no, collapse = " | "))

# ==============================================================================
section("10. Gaps between logs are written as unclassified logs")
# Stem 1 without its middle log, and without the first log of another stem.
lg_all <- r$selected_logs_all
tid <- names(which(table(as.character(lg_all$treid)) >= 3L))[1L]
drop_i <- which(as.character(lg_all$treid) == tid)[2L]
first_i <- which(as.character(lg_all$treid) == setdiff(unique(as.character(lg_all$treid)), tid)[1L])[1L]
r_gap <- r; r_gap$selected_logs_all <- lg_all[-c(drop_i, first_i), ]
src_nounc <- tempfile(fileext = ".hpr")                  # the same source without the unclassified product
writeLines(sub("(?s)<ProductDefinition>\\s*<ProductKey>999999</ProductKey>.*?</ProductDefinition>", "",
               paste0(head_v, obj, stems_v), perl = TRUE), src_nounc, useBytes = TRUE)
OUT_G <- tempfile(fileext = ".hpr")
xg <- hpr_export_result(OUT_G, data.frame(path = src_nounc, name = "nounc.hpr"), sbn, r_gap, sc$defs, pm)
dg_ <- xml2::read_xml(OUT_G); xml2::xml_ns_strip(dg_)
stem_logs <- function(key) { s <- xml2::xml_find_first(dg_, sprintf("/*/Machine/Stem[StemKey='%s']", key)); xlogs(s) }
lens_of <- function(ls) as.numeric(xml2::xml_text(xml2::xml_find_all(ls, "./LogMeasurement/LogLength")))
pk_of <- function(ls) xml2::xml_text(xml2::xml_find_all(ls, "./ProductKey"))
Lt <- lg_all[as.character(lg_all$treid) == tid, ]; Lt <- Lt[order(Lt$StartPos), ]
lt <- stem_logs(tid)
gap_len <- Lt$StopPos[1L] - Lt$StopPos[1L] + (Lt$StartPos[3L] - Lt$StopPos[2L])
Lf <- lg_all[-c(drop_i), ]; Lf <- Lf[as.character(Lf$treid) == tid, ]
mid_gap_len <- Lt$StopPos[2L] - Lt$StartPos[2L]
ok10a <- isTRUE(xg$ok) && sum(lens_of(lt)) == max(Lt$StopPos) - 0 &&
  sum(pk_of(lt) == "999999") >= 1L && mid_gap_len %in% lens_of(lt)[pk_of(lt) == "999999"]
first_key <- setdiff(unique(as.character(lg_all$treid)), tid)[1L]
L1g <- lg_all[as.character(lg_all$treid) == first_key, ]; L1g <- L1g[order(L1g$StartPos), ]
l1 <- stem_logs(first_key)
ok10b <- pk_of(l1)[1L] == "999999" && lens_of(l1)[1L] == L1g$StopPos[1L] - 0 &&
  sum(lens_of(l1)) == max(L1g$StopPos)
unc_def <- xml2::xml_find_all(dg_, "/*/Machine/ProductDefinition[ProductKey='999999']/UnclassifiedProductDefinition")
ok10c <- length(unc_def) == 1L && xg$gap_logs >= 2L && x$gap_logs == 0L
rec("Check 10: A gap between logs and a missing first log become logs of product 999999, so the lengths add up to the last log's stop; the unclassified ProductDefinition is added when the source lacks it; no gap logs in a gap-free export",
    ok(ok10a && ok10b && ok10c), sprintf("middle gap %s cm in stem %s (logs %s, products %s) | leading gap in stem %s | unclassified defs %d | gap logs %s (clean export %s)",
                                         mid_gap_len, tid, paste(lens_of(lt), collapse = "/"), paste(pk_of(lt), collapse = "/"),
                                         first_key, length(unc_def), xg$gap_logs, x$gap_logs))
unlink(c(src_nounc, OUT_G))

unlink(c(SRC, OUT, pm_file))
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 31 complete.\n")
