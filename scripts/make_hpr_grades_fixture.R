# ==============================================================================
# scripts/make_hpr_grades_fixture.R
#
# Purpose : Build inputs/test_fixtures/hpr_grades_synthetic.hpr, the .hpr file
#           audit 30 (stem grades, Phase 6) reads, from the committed
#           synthetic fixture inputs/test_fixtures/hpr_example_synthetic.hpr.
#
# Privacy : nothing is read from a real file. The species groups and products
#           of the synthetic fixture are kept (they hold no coordinates or
#           names); every stem is replaced by invented ones that copy the
#           smooth spruce profile of the old stem 1 and carry invented stem
#           grades. The script refuses to write a file containing coordinate
#           or contact fields.
#
# Stems (all spruce, group 446, unless noted):
#   101  grades 7 from 0, 2 from 300          (a rot butt: grade 7 below 3 m)
#   102  grades 2 from 0, 3 from 500, 2 from 800
#   103  no StemGrade                         (StartGrade applies)
#   104  grades 2 from 0, 7 from 295          (start snaps to 300)
#   105  pine (445), grade 1 from 0
# Product changes: "197 Massevirke" (group 446) loses its
# PermittedGradesDefinition (permits all grades).
#
# Usage   : Rscript scripts/make_hpr_grades_fixture.R
# ==============================================================================

root <- local({
  d <- normalizePath(Sys.getenv("APTER_ROOT", getwd()), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "R", "helpers_paths.R")) && dirname(d) != d) d <- dirname(d)
  d
})
src_file <- file.path(root, "inputs", "test_fixtures", "hpr_example_synthetic.hpr")
out_file <- file.path(root, "inputs", "test_fixtures", "hpr_grades_synthetic.hpr")

doc <- xml2::read_xml(src_file); xml2::xml_ns_strip(doc)
m <- xml2::xml_find_first(doc, "./Machine")

# Products: 197 Massevirke (spruce group 446) permits all grades.
for (pd in xml2::xml_find_all(m, "./ProductDefinition[ProductKey='8015']"))
  xml2::xml_remove(xml2::xml_find_first(pd, "./ClassifiedProductDefinition/PermittedGradesDefinition"))

# The profile of the old stem 1 (smooth spruce), then drop all old stems.
old <- xml2::xml_find_all(m, "./Stem")
dia <- xml2::xml_find_first(old[[1L]], "./SingleTreeProcessedStem/StemDiameters")
dia_txt <- as.character(dia)
for (s in old) xml2::xml_remove(s)

add_stem <- function(key, sgk, grades = NULL) {
  g <- if (length(grades))
    paste(sprintf("<StemGrade><GradeValue gradeStartPosition=\"%d\">%d</GradeValue></StemGrade>",
                  as.integer(grades$pos), as.integer(grades$val)), collapse = "")
  else ""
  txt <- sprintf(paste0("<Stem><StemKey>%d</StemKey><SpeciesGroupKey>%s</SpeciesGroupKey>",
                        "<SingleTreeProcessedStem><DBH>300</DBH>%s%s</SingleTreeProcessedStem></Stem>"),
                 key, sgk, g, dia_txt)
  xml2::xml_add_child(m, xml2::read_xml(txt))
}
add_stem(101L, "446", list(pos = c(0, 300), val = c(7, 2)))
add_stem(102L, "446", list(pos = c(0, 500, 800), val = c(2, 3, 2)))
add_stem(103L, "446")
add_stem(104L, "446", list(pos = c(0, 295), val = c(2, 7)))
add_stem(105L, "445", list(pos = 0, val = 1))

tmp <- tempfile(fileext = ".hpr"); xml2::write_xml(doc, tmp)
txt <- paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
if (grepl("Latitude|Longitude|StemCoordinates|ContactInformation|OperatorName|ObjectName", txt))
  stop("The fixture would contain coordinate or contact fields; not written.")
file.copy(tmp, out_file, overwrite = TRUE)
cat("Wrote", out_file, "(", file.size(out_file), "bytes )\n")
