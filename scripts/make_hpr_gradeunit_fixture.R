# ==============================================================================
# scripts/make_hpr_gradeunit_fixture.R
#
# Purpose : Build three small .hpr files for the grade position unit tests
#           (tests/testthat/test-grade-unit.R), from the committed synthetic
#           fixture inputs/test_fixtures/hpr_example_synthetic.hpr:
#             hpr_gradeunit_dm.hpr          150 invented stems, grade changes at the end of
#                                           the first log, positions written in dm, a made-up
#                                           control-system version (the positions decide)
#             hpr_gradeunit_cm.hpr          the same stems, positions in cm
#             hpr_gradeunit_dm_version.hpr  6 stems, positions in dm, the control-system
#                                           version that writes dm (TimbermaticH 1.27.17:
#                                           the version decides; too few stems for the positions)
# Privacy : nothing is read from a real file; every stem is invented (the smooth
#           spruce profile of the old stem 1, DBH 300). No coordinates or names.
# Usage   : Rscript scripts/make_hpr_gradeunit_fixture.R
# ==============================================================================

root <- local({
  d <- normalizePath(Sys.getenv("OPTBUCK_ROOT", getwd()), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "R", "helpers_paths.R")) && dirname(d) != d) d <- dirname(d)
  d
})
fx <- file.path(root, "inst", "extdata", "inputs", "test_fixtures")

make <- function(out, n, unit_div, version) {
  doc <- xml2::read_xml(file.path(fx, "hpr_example_synthetic.hpr")); xml2::xml_ns_strip(doc)
  m <- xml2::xml_find_first(doc, "./Machine")
  pp <- seq(0, 1500, 100)      # a short smooth profile, over bark (mm)
  dia_txt <- paste0("<StemDiameters diameterCategory=\"Over bark\">", paste(sprintf("<DiameterValue diameterPosition=\"%d\" diameterMeasurementCategory=\"First\">%d</DiameterValue>", pp, round(330 * (1 - pp / 1700)^0.9)), collapse = ""), "</StemDiameters>")
  for (s in xml2::xml_find_all(m, "./Stem")) xml2::xml_remove(s)
  xml2::xml_add_child(m, xml2::read_xml(sprintf("<MachineApplicationVersion>%s</MachineApplicationVersion>", version)), .where = 0L)
  set.seed(7)
  for (i in seq_len(n)) {
    len <- sample(seq(300, 520, 10), 3L, replace = TRUE) + sample(0:9, 3L, replace = TRUE)   # not on a 10 cm grid
    logs <- paste(sprintf("<Log><LogKey>%d</LogKey><ProductKey>8015</ProductKey><LogMeasurement><LogLength>%d</LogLength></LogMeasurement></Log>",
                          1:3, len), collapse = "")
    g <- sprintf("<StemGrade><GradeValue gradeStartPosition=\"0\">5</GradeValue></StemGrade><StemGrade><GradeValue gradeStartPosition=\"%d\">2</GradeValue></StemGrade>",
                 as.integer(round(len[1L] / unit_div)))
    xml2::xml_add_child(m, xml2::read_xml(sprintf(
      "<Stem><StemKey>%d</StemKey><SpeciesGroupKey>446</SpeciesGroupKey><SingleTreeProcessedStem><DBH>300</DBH>%s%s%s</SingleTreeProcessedStem></Stem>",
      1000L + i, g, dia_txt, logs)))
  }
  tmp <- tempfile(fileext = ".hpr"); xml2::write_xml(doc, tmp)
  txt <- paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (grepl("Latitude|Longitude|StemCoordinates|ContactInformation|OperatorName|ObjectName", txt))
    stop("The fixture would contain coordinate or contact fields; not written.")
  file.copy(tmp, file.path(fx, out), overwrite = TRUE)
  cat("Wrote", out, "(", file.size(file.path(fx, out)), "bytes )\n")
}
make("hpr_gradeunit_dm.hpr", 150L, 10, "Invented 1.0")
make("hpr_gradeunit_cm.hpr", 150L, 1, "Invented 1.0")
make("hpr_gradeunit_dm_version.hpr", 6L, 10, "TimbermaticH 1.27.17")
