# Builds inst/extdata/inputs/test_fixtures/hpr_logsonly_synthetic.hpr: the invented check fixture
# (hpr_check_synthetic.hpr) with every StemDiameters block removed, so the file has recorded logs
# (Butt/Mid/Top diameters), DBH and DBHHeight but no stem profiles (as the older John Deere files).
# Nothing comes from a real file.
root <- if (file.exists("DESCRIPTION")) "." else stop("run from the repository root")
dir <- file.path(root, "inst", "extdata", "inputs", "test_fixtures")
x <- paste(readLines(file.path(dir, "hpr_check_synthetic.hpr"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
n0 <- lengths(regmatches(x, gregexpr("<StemDiameters", x, fixed = TRUE)))
x <- gsub("(?s)<StemDiameters(?:\\s[^>]*)?>.*?</StemDiameters>", "", x, perl = TRUE)
stopifnot(!grepl("StemDiameters", x, fixed = TRUE), !grepl("StemCoordinates", x, fixed = TRUE))
writeLines(x, file.path(dir, "hpr_logsonly_synthetic.hpr"), useBytes = TRUE)
cat("removed", n0, "StemDiameters blocks\n")
