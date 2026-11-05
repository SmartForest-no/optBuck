# ==============================================================================
# scripts/make_hpr_tuning_fixture.R
#
# Purpose : Build inputs/test_fixtures/hpr_tuning_synthetic.hpr, the .hpr file
#           audit 17 uses for the Price tuning tab (roadmap 7, session 3): the
#           same species groups and products as hpr_example_synthetic.hpr
#           (spruce 140 SCA Sagtommer has three length classes, 23 diameter
#           classes and a Distribution), but 80 invented spruce stems, enough
#           logs for the tab to tune (at least 20 logs in a diameter class).
#
# Privacy : nothing is read from a real file. Every stem copies the smooth
#           spruce profile of the old stem 1 with all diameters scaled by an
#           invented factor (0.62-1.10). The script refuses to write a file
#           containing coordinate or contact fields.
#
# Usage   : Rscript scripts/make_hpr_tuning_fixture.R
# ==============================================================================

root <- local({
  d <- normalizePath(Sys.getenv("APTER_ROOT", getwd()), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "R", "helpers_paths.R")) && dirname(d) != d) d <- dirname(d)
  d
})
src_file <- file.path(root, "inst", "extdata", "inputs", "test_fixtures", "hpr_example_synthetic.hpr")
out_file <- file.path(root, "inst", "extdata", "inputs", "test_fixtures", "hpr_tuning_synthetic.hpr")

doc <- xml2::read_xml(src_file); xml2::xml_ns_strip(doc)
m <- xml2::xml_find_first(doc, "./Machine")
old <- xml2::xml_find_all(m, "./Stem")
dia_txt <- as.character(xml2::xml_find_first(old[[1L]], "./SingleTreeProcessedStem/StemDiameters"))
for (s in old) xml2::xml_remove(s)

scale_profile <- function(txt, f) {
  v <- regmatches(txt, gregexpr(">[0-9]+</DiameterValue>", txt))[[1L]]
  for (x in unique(v)) {
    n <- as.integer(gsub("[^0-9]", "", x))
    txt <- gsub(x, sprintf(">%d</DiameterValue>", as.integer(round(n * f))), txt, fixed = TRUE)
  }
  txt
}
# Scaling rounds the diameters to whole mm, so some scaled profiles fail the
# .hpr cleaning (too flat or too steep). Candidates are scaled (0.80-1.20), checked
# with the app's own reader and cleaning, and the first 120 that pass are kept.
Sys.setenv(APTER_ROOT = root)
suppressMessages(pkgload::load_all(root, quiet = TRUE))   # the package (R/, inst/)
cand <- seq(0.80, 1.20, length.out = 400L)
add_stems <- function(factors, keys) for (i in seq_along(factors))
  xml2::xml_add_child(m, xml2::read_xml(sprintf(paste0("<Stem><StemKey>%d</StemKey><SpeciesGroupKey>446</SpeciesGroupKey>",
    "<SingleTreeProcessedStem><DBH>300</DBH>%s</SingleTreeProcessedStem></Stem>"),
    keys[i], scale_profile(dia_txt, factors[i]))))
add_stems(cand, 1000L + seq_along(cand))
tmp0 <- tempfile(fileext = ".hpr"); xml2::write_xml(doc, tmp0)
st <- read_hpr_stems(tmp0); smap0 <- setNames(guess_hpr_species(st$species$name, st$species$user_id), st$species$key)
ck <- hpr_stem_checks(st, smap0)
unlink(tmp0)
ok_idx <- head(ck$use, 120L)
keep <- as.integer(st$stems[[grep("key", setdiff(names(st$stems), "species_key"), value = TRUE)[1L]]][ok_idx])
for (x in xml2::xml_find_all(m, "./Stem")) xml2::xml_remove(x)
add_stems(cand[keep - 1000L], keep)
cat(length(keep), "stems kept
")
tmp <- tempfile(fileext = ".hpr"); xml2::write_xml(doc, tmp)
txt <- paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
if (grepl("Latitude|Longitude|StemCoordinates|ContactInformation|OperatorName|ObjectName", txt))
  stop("The fixture would contain coordinate or contact fields; not written.")
file.copy(tmp, out_file, overwrite = TRUE)
cat("Wrote", out_file, "(", file.size(out_file), "bytes )\n")
