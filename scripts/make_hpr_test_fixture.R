# ==============================================================================
# scripts/make_hpr_test_fixture.R
#
# Purpose : Build inputs/test_fixtures/hpr_example_synthetic.hpr, the .hpr
#           file audit 25 reads, from optBuck's example.hpr
#           (SmartForest-no/optBuck, inst/extdata/example.hpr).
#
# Privacy : the example has exact stem coordinates and names. Only a
#           whitelist is copied: species groups (key, name, user ID, grades)
#           and products (definitions, price matrices, permitted grades).
#           No machine, operator, object, contact, date or stem data. DBHHeight
#           is copied per species group (hpr Phase B), FURU's set to 120 cm so
#           the groups differ. The stems are invented (hpr Phase B, audit 26):
#           stem 1 is Phase A's smooth spruce profile, unchanged; stems 2-5 are
#           bucked (pine, non-monotonic, off-grid, without DBH); stems 6-13
#           are dropped, one per reason.
#
# Usage   : Rscript scripts/make_hpr_test_fixture.R <path to example.hpr>
# ==============================================================================

args <- commandArgs(TRUE)
if (length(args) < 1L) stop("Usage: Rscript scripts/make_hpr_test_fixture.R <example.hpr>")
root <- local({
  d <- normalizePath(Sys.getenv("APTER_ROOT", getwd()), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "R", "helpers_paths.R")) && dirname(d) != d) d <- dirname(d)
  d
})
src <- xml2::read_xml(args[1L]); xml2::xml_ns_strip(src)
m <- xml2::xml_find_first(src, "./Machine")

out <- xml2::xml_new_root("HarvestedProduction", messageType = "hpr", diameterUnit = "mm",
                          lengthUnit = "cm", volumeUnit = "m3", version = "3.3",
                          xmlns = "urn:skogforsk:stanford2010")
mo <- xml2::xml_add_child(out, "Machine", machineCategory = "Harvester")
copy <- function(to, from, path) {
  for (n in xml2::xml_find_all(from, path)) xml2::xml_add_child(to, n)
}
for (sg in xml2::xml_find_all(m, "./SpeciesGroupDefinition")) {
  s <- xml2::xml_add_child(mo, "SpeciesGroupDefinition")
  for (p in c("./SpeciesGroupUserID", "./SpeciesGroupName", "./Grades", "./DBHHeight", "./SpeciesGroupKey"))
    copy(s, sg, p)
  if (identical(xml2::xml_text(xml2::xml_find_first(s, "./SpeciesGroupName")), "FURU"))
    xml2::xml_set_text(xml2::xml_find_first(s, "./DBHHeight"), "120")
}
for (pd in xml2::xml_find_all(m, "./ProductDefinition")) {
  p <- xml2::xml_add_child(mo, "ProductDefinition")
  copy(p, pd, "./ProductKey")
  cp <- xml2::xml_find_first(pd, "./ClassifiedProductDefinition")
  if (inherits(cp, "xml_missing")) { copy(p, pd, "./UnclassifiedProductDefinition"); next }
  c2 <- xml2::xml_add_child(p, "ClassifiedProductDefinition")
  for (q in c("./ProductName", "./ProductGroupName", "./DiameterDefinition", "./LengthDefinition",
              "./PriceDefinition", "./PermittedGradesDefinition", "./ProductMatrixes",
              "./SpeciesGroupKey"))
    copy(c2, cp, q)
}
# Invented stems: positions in cm from the felling cut, diameters in mm, no
# coordinates, dates or logs.
add_stem <- function(key, sgk, pos = NULL, d = NULL, dbh = 300, kind = "SingleTreeProcessedStem",
                     category = "Over bark") {
  st <- xml2::xml_add_child(mo, "Stem")
  xml2::xml_add_child(st, "StemKey", as.character(key))
  xml2::xml_add_child(st, "SpeciesGroupKey", as.character(sgk))
  sp <- xml2::xml_add_child(st, kind)
  if (!is.null(dbh)) xml2::xml_add_child(sp, "DBH", as.character(dbh))
  if (is.null(pos)) return(invisible())
  sd <- xml2::xml_add_child(sp, "StemDiameters", diameterCategory = category)
  for (i in seq_along(pos))
    xml2::xml_add_child(sd, "DiameterValue", as.character(round(d[i])),
                        diameterPosition = as.character(pos[i]), diameterMeasurementCategory = "First")
}
smooth <- function(top, d0, k) { p <- seq(0, top, by = 10); list(p = p, d = d0 - k * p) }
s1 <- smooth(1800, 330, 0.15)
add_stem(1, 446, s1$p, s1$d)                                  # spruce, Phase A's stem
s2 <- smooth(1500, 300, 0.14)
add_stem(2, 445, s2$p, s2$d, dbh = 999)                       # pine; the file DBH (999) is not used
s3 <- smooth(1200, 280, 0.15); s3$d[s3$p >= 400 & s3$p <= 450] <- s3$d[s3$p >= 400 & s3$p <= 450] + 15
add_stem(3, 446, s3$p, s3$d)                                  # spruce, a local diameter increase
p4 <- c(seq(0, 1000, by = 25), 1012)
add_stem(4, 446, p4, 260 - 0.16 * p4)                         # spruce, off the 10 cm grid
s5 <- smooth(600, 220, 0.2)
add_stem(5, 446, s5$p, s5$d, dbh = NULL)                      # spruce without DBH
add_stem(6, 447, s1$p, s1$d)                                  # LAUV: Not used
add_stem(7, 446, kind = "MultiTreeProcessedStem")             # multi-tree, no profile
add_stem(8, 446)                                              # no StemDiameters
add_stem(9, 446, s1$p, s1$d, category = "Under bark")         # under bark
add_stem(2, 446, s1$p, s1$d)                                  # StemKey 2 again
s11 <- smooth(100, 250, 0.2)
add_stem(11, 446, s11$p, s11$d)                               # ends below 1.3 m above ground
add_stem(12, 446, s1$p[-(1:2)], s1$d[-(1:2)])                 # starts at 20 cm, not at the cut
add_stem(13, 446, s1$p, s1$d * 6)                             # 1,980 mm at the butt: a unit error

dest <- file.path(root, "inputs", "test_fixtures", "hpr_example_synthetic.hpr")
dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
xml2::write_xml(out, dest)
txt <- readLines(dest, warn = FALSE)
stopifnot(!any(grepl("Latitude|Longitude|StemCoordinates|ObjectName|Address|vasket", txt)))
cat("Wrote", dest, "\n")
