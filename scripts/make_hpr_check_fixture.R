# ==============================================================================
# scripts/make_hpr_check_fixture.R  (roadmap 8, 2026-11-04)
#
# Purpose : Build inputs/test_fixtures/hpr_check_synthetic.hpr, the .hpr file audit 36 (bucking
#           check, tab 5) reads: invented stems WITH recorded logs, under the species groups and
#           products of the committed synthetic fixture hpr_example_synthetic.hpr.
#
# Privacy : nothing is read from a real file; no coordinates, names or dates are written. The script
#           refuses to write a file containing coordinate or contact fields (as audit 26 check 10).
#
# Stems (spruce group 446 unless noted; linear tapers, mm over bark = d0 - slope * position in cm):
#   1  H 560, d0 300, slope 0.35   8019 SCA sawlog 430 (Automatic) + waste 120 (Other manual)
#                                   = the best bucking: yield index 1
#   2  same profile                 8015 pulpwood 520 + waste 30 (Other manual): worse than stem 1
#   3  H 900, d0 320, slope 0.20   8019 490 (Automatic) + 8015 400 (Spinning); NO StartPos recorded
#   4  H 500, d0 300, slope 0.30   8019 370 in a "manual only" cell (the file's price counts) +
#                                   8015 130 (shorter than any length class: no price, outside limits)
#   5  H 300, d0 400, slope 0.20   8015 300; the profile ends at 340 mm: possible broken top (kept)
#   6  H 900, d0 300, slope 0.25   8019 430 + waste 100 (Other manual, a defect zone) + 8015 370;
#                                   with the zone rule the best bucking equals the recorded one
#   7  multi-tree stem             left out of the check
#   8  pine (445), H 600, d0 280, slope 0.25   7953 pulpwood 400 (Automatic) + waste 200
#
# Usage   : Rscript scripts/make_hpr_check_fixture.R
# ==============================================================================
root <- local({
  d <- normalizePath(Sys.getenv("APTER_ROOT", getwd()), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "R", "helpers_paths.R")) && dirname(d) != d) d <- dirname(d)
  d
})
src_file <- file.path(root, "inputs", "test_fixtures", "hpr_example_synthetic.hpr")
out_file <- file.path(root, "inputs", "test_fixtures", "hpr_check_synthetic.hpr")

doc <- xml2::read_xml(src_file); xml2::xml_ns_strip(doc)
m <- xml2::xml_find_first(doc, "./Machine")
for (s in xml2::xml_find_all(m, "./Stem")) xml2::xml_remove(s)

profile <- function(H, d0, slope) {
  pos <- seq(0, H, by = 10); d <- round(d0 - slope * pos)
  list(pos = pos, d = d, f = function(p) round(d0 - slope * p),
       xml = sprintf("<StemDiameters diameterCategory=\"Over bark\"><DiameterMeasuredStartHeight>0</DiameterMeasuredStartHeight><DiameterMeasuredEndHeight>%d</DiameterMeasuredEndHeight>%s</StemDiameters>",
                     H, paste(sprintf("<DiameterValue diameterPosition=\"%d\" diameterMeasurementCategory=\"First\">%d</DiameterValue>", pos, d), collapse = "")))
}
log_xml <- function(key, pk, len, start, reason, prof, record_start = TRUE) {
  stop_pos <- start + len; ob <- function(p) prof$f(min(p, max(prof$pos))); ub <- function(p) ob(p) - 6
  dtop <- ub(stop_pos); vol_sub <- pi / 4 * (ub(start + len / 2) / 1000)^2 * len / 100
  vol_sob <- pi / 4 * (ob(start + len / 2) / 1000)^2 * len / 100
  sprintf(paste0("<Log><LogKey>%d</LogKey><ProductKey>%s</ProductKey>",
                 "<LogVolume logVolumeCategory=\"m3 (price)\" logMeasurementCategory=\"Machine\">%.4f</LogVolume>",
                 "<LogVolume logVolumeCategory=\"m3sob\" logMeasurementCategory=\"Machine\">%.4f</LogVolume>",
                 "<LogVolume logVolumeCategory=\"m3sub\" logMeasurementCategory=\"Machine\">%.4f</LogVolume>",
                 "<CuttingCategory><CuttingReason>%s</CuttingReason></CuttingCategory>%s",
                 "<LogMeasurement logMeasurementCategory=\"Machine\">",
                 "<LogDiameter logDiameterCategory=\"Butt ob\">%d</LogDiameter><LogDiameter logDiameterCategory=\"Butt ub\">%d</LogDiameter>",
                 "<LogDiameter logDiameterCategory=\"Mid ob\">%d</LogDiameter><LogDiameter logDiameterCategory=\"Mid ub\">%d</LogDiameter>",
                 "<LogDiameter logDiameterCategory=\"Top ob\">%d</LogDiameter><LogDiameter logDiameterCategory=\"Top ub\">%d</LogDiameter>",
                 "<LogLength>%d</LogLength></LogMeasurement></Log>"),
          key, pk, vol_sub, vol_sob, vol_sub, reason,
          if (record_start) sprintf("<Extension><StartPos>%d</StartPos></Extension>", start) else "",
          ob(start), ub(start), ob(start + len / 2), ub(start + len / 2), ob(stop_pos), dtop, len)
}
add_stem <- function(key, sgk, prof, logs, multi = FALSE) {
  tag <- if (multi) "MultiTreeProcessedStem" else "SingleTreeProcessedStem"
  txt <- sprintf("<Stem><StemKey>%d</StemKey><SpeciesGroupKey>%s</SpeciesGroupKey><%s><DBH>300</DBH>%s%s</%s></Stem>",
                 key, sgk, tag, prof$xml, paste(logs, collapse = ""), tag)
  xml2::xml_add_child(m, xml2::read_xml(txt))
}
chain <- function(prof, spec, record_start = TRUE) {            # spec: list(c(pk, len, reason), ...)
  start <- 0; out <- character(0)
  for (i in seq_along(spec)) {
    s <- spec[[i]]; len <- as.integer(s[2L])
    out <- c(out, log_xml(i, s[1L], len, start, s[3L], prof, record_start)); start <- start + len
  }
  out
}
p1 <- profile(560, 300, 0.35)
add_stem(1L, "446", p1, chain(p1, list(c("8019", 430, "Automatic"), c("999999", 120, "Other manual"))))
add_stem(2L, "446", p1, chain(p1, list(c("8015", 520, "Other manual"), c("999999", 30, "Other manual"))))
p3 <- profile(900, 320, 0.20)
add_stem(3L, "446", p3, chain(p3, list(c("8019", 490, "Automatic"), c("8015", 400, "Spinning")), record_start = FALSE))
p4 <- profile(500, 300, 0.30)
add_stem(4L, "446", p4, chain(p4, list(c("8019", 370, "Other manual"), c("8015", 130, "Other manual"))))
p5 <- profile(300, 400, 0.20)
add_stem(5L, "446", p5, chain(p5, list(c("8015", 300, "Automatic"))))
p6 <- profile(900, 300, 0.25)
add_stem(6L, "446", p6, chain(p6, list(c("8019", 430, "Automatic"), c("999999", 100, "Other manual"), c("8015", 370, "Automatic"))))
add_stem(7L, "446", p1, chain(p1, list(c("8019", 430, "Automatic"))), multi = TRUE)
p8 <- profile(600, 280, 0.25)
add_stem(8L, "445", p8, chain(p8, list(c("7953", 400, "Automatic"), c("999999", 200, "Other manual"))))

tmp <- tempfile(fileext = ".hpr"); xml2::write_xml(doc, tmp)
txt <- paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
if (grepl("Latitude|Longitude|StemCoordinates|ContactInformation|OperatorName|ObjectName", txt))
  stop("The fixture would contain coordinate or contact fields; not written.")
file.copy(tmp, out_file, overwrite = TRUE)
cat("Wrote", out_file, "(", file.size(out_file), "bytes )\n")
