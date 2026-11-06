# ==============================================================================
# scripts/export_classes_check.R (R4 polish, task 5, 2026-11-03)
#
# For the owner's private files (file1..file5 = the .hpr files in
# C:/optbuck_private/hpr alphabetically, hand.pin, real.pin): import every product,
# export the scenario as a .pin and compare, per product (by ProductUserID), the
# diameter class lower limits of the source with those written. Result table in
# outputs/polish/export_classes.txt (gitignored); nothing from the files is copied.
#
# Usage: Rscript scripts/export_classes_check.R
# ==============================================================================
root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!is_root(d) && dirname(d) != d) d <- dirname(d)
  if (!is_root(d)) stop("Cannot locate the project root.", call. = FALSE)
  d
})
Sys.setenv(OPTBUCK_ROOT = root_dir)
suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)

out <- file.path(root_dir, "outputs", "polish"); dir.create(out, recursive = TRUE, showWarnings = FALSE)
log <- file.path(out, "export_classes.txt"); unlink(log)
say <- function(...) { x <- sprintf(...); cat(x, "\n"); cat(x, "\n", file = log, append = TRUE) }

files <- c(file1 = "REDACTED1", file2 = "REDACTED40", file3 = "REDACTED33",
           file4 = "REDACTED27", file5 = "REDACTED28")
paths <- c(setNames(file.path("C:/optbuck_private/hpr", files), names(files)),
           real.pin = "C:/optbuck_private/pin/real.pin", hand.pin = "C:/optbuck_private/pin/hand.pin")
say("%-9s %-34s %7s %7s  %s", "file", "product", "source", "written", "equal / missing in export")
tot <- 0L; bad <- 0L
for (nm in names(paths)) {
  f <- paths[[nm]]; if (!file.exists(f)) { say("%s: not available", nm); next }
  h <- read_product_file(f); stopifnot(isTRUE(h$ok))
  map <- setNames(guess_hpr_species(h$species$name, h$species$user_id), h$species$key)
  sc <- hpr_to_scenario(h, map); if (!isTRUE(sc$ok)) { say("%s: import failed: %s", nm, paste(sc$errors, collapse = " ")); next }
  p <- tempfile(fileext = ".pin"); e <- pin_export_scenario(sc$defs, pm_minimal(sc$pm), p); stopifnot(isTRUE(e$ok))
  d <- xml2::read_xml(p); xml2::xml_ns_strip(d)
  pd <- xml2::xml_find_all(d, "//ProductDefinition")
  out_cls <- lapply(pd, function(q) sort(as.numeric(xml2::xml_text(xml2::xml_find_all(q, ".//DiameterClass/DiameterClassLowerLimit")))))
  names(out_cls) <- xml2::xml_text(xml2::xml_find_first(pd, "./ProductUserID"))
  # source: each imported product's own classes, by its ProductUserID (SourceXml)
  for (i in which(!is.na(sc$defs$SourceXml))) {
    uid <- .hx_text(source_xml_part(sc$defs$SourceXml[i], "uid"), "ProductUserID")
    if (is.na(uid)) next
    Encoding(uid) <- "UTF-8"
    srcp <- Filter(function(q) { u <- .hx_text(q$src_uid, "ProductUserID"); Encoding(u) <- "UTF-8"; identical(u, uid) || identical(q$uid, uid) }, h$products)
    if (!length(srcp)) next
    s <- sort(unique(srcp[[1L]]$dia_classes)); w <- out_cls[[uid]]
    if (is.null(w)) next
    tot <- tot + 1L; same <- identical(as.numeric(s), as.numeric(w)); if (!same) bad <- bad + 1L
    say("%-9s %-34s %7d %7d  %s", nm, substr(sc$defs$Name[i], 1, 34), length(s), length(w),
        if (same) "equal" else paste("DIFFERENT, missing:", paste(setdiff(s, w), collapse = ","), "extra:", paste(setdiff(w, s), collapse = ",")))
  }
  unlink(p)
}
say("products compared: %d, different: %d", tot, bad)
