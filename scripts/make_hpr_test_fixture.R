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
#           No machine, operator, object, contact, date or stem data; the one
#           stem in the fixture is invented (a smooth spruce profile).
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
  for (p in c("./SpeciesGroupUserID", "./SpeciesGroupName", "./Grades", "./SpeciesGroupKey"))
    copy(s, sg, p)
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
# One invented spruce stem: over bark, every 10 cm from the felling cut, no
# coordinates, dates or logs.
st <- xml2::xml_add_child(mo, "Stem")
xml2::xml_add_child(st, "StemKey", "1")
xml2::xml_add_child(st, "SpeciesGroupKey", "446")
sp <- xml2::xml_add_child(st, "SingleTreeProcessedStem")
xml2::xml_add_child(sp, "DBH", "300")
sd <- xml2::xml_add_child(sp, "StemDiameters", diameterCategory = "Over bark")
for (pos in seq(0, 1800, by = 10))
  xml2::xml_add_child(sd, "DiameterValue", as.character(round(330 - pos * 0.15)),
                      diameterPosition = as.character(pos), diameterMeasurementCategory = "First")

dest <- file.path(root, "inputs", "test_fixtures", "hpr_example_synthetic.hpr")
dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
xml2::write_xml(out, dest)
txt <- readLines(dest, warn = FALSE)
stopifnot(!any(grepl("Latitude|Longitude|StemCoordinates|ObjectName|Address|vasket", txt)))
cat("Wrote", dest, "\n")
