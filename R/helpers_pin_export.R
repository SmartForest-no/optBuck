# ==============================================================================
# R/helpers_pin_export.R
#
# Purpose : Export a scenario's assortments and prices as a StanForD 2010 .pin
#           file (ProductInstruction), one file per scenario (Phase 5b,
#           2026-10-29).
#
# What is written (decided 2026-10-29):
#   - A fixed minimal header: CreationDate, ModificationDate and
#     SenderApplication "optBuck". Nothing is copied from any source file and
#     no coordinates are written; the written file is scanned for coordinate
#     elements (.hpr_export_has_coordinates()) and deleted if one is found.
#   - One ProductDefinition per assortment, waste rows left out, from
#     hpr_export_products(style = "pin"): ProductUserID (the assortment's name,
#     spaces as underscores, made unique with _2, _3, ...), diameter and length
#     classes, price definition, price matrix, PermittedGradesDefinition when
#     the assortment has permitted grades, SpeciesGroupUserID "Spruce" or
#     "Pine". No ProductKey, SpeciesGroupKey or product metadata.
#   - Known, intended differences when the file is read back (read_pin()):
#     "Top cylinder" is written as "Top" (StanForD has no cylinder rule), a
#     flat price becomes a one-class matrix, the Type survives through
#     ProductGroupName (sawlog, pulpwood, else the type's name).
# ==============================================================================

if (!exists("hpr_export_products", mode = "function")) {
  .px_root <- Sys.getenv("APTER_ROOT", unset = "")
  if (nzchar(.px_root)) source(file.path(.px_root, "R", "helpers_hpr_export.R"))
}

#' SenderApplication of an exported .pin.
PIN_SENDER_APPLICATION <- "optBuck"

#' ProductUserIDs of assortment names: spaces become underscores; a repeated
#' ID gets _2, _3, ... (the first keeps its name).
pin_product_ids <- function(names) {
  id <- gsub("[[:space:]]+", "_", trimws(as.character(names)))
  id[!nzchar(id)] <- "product"
  out <- character(length(id))
  for (i in seq_along(id)) {
    cand <- id[i]; n <- 1L
    while (cand %in% out[seq_len(i - 1L)]) { n <- n + 1L; cand <- paste0(id[i], "_", n) }
    out[i] <- cand
  }
  out
}

# ISO 8601 time with the offset as +hh:mm.
.pin_time <- function(t = Sys.time()) {
  z <- format(t, "%z")
  paste0(format(t, "%Y-%m-%dT%H:%M:%S"), substr(z, 1L, 3L), ":", substr(z, 4L, 5L))
}

#' Write one scenario as a .pin file.
#'
#' @param defs,pm The scenario's assortment table and price matrix.
#' @param path    Output file.
#' @return list(ok, errors, path, n_products, ids).
pin_export_scenario <- function(defs, pm, path) {
  fail <- function(msg) { if (file.exists(path)) unlink(path); list(ok = FALSE, errors = msg) }
  defs <- defs[as.character(defs$Type) != "waste", , drop = FALSE]
  if (nrow(defs) == 0L) return(fail("The scenario has no assortments to export (waste is not exported)."))
  rownames(defs) <- NULL
  ids <- pin_product_ids(defs$Name)
  prod <- tryCatch(hpr_export_products(defs, pm, orig = list(), groups = NULL, used_keys = character(0),
                                       style = "pin", ids = ids),
                   error = function(e) conditionMessage(e))
  if (is.character(prod)) return(fail(prod))
  now <- .pin_time()
  txt <- paste0(
    '<?xml version="1.0" encoding="utf-8"?>\n',
    '<ProductInstruction messageType="pin" diameterUnit="mm" lengthUnit="cm" version="3.3" ',
    'versionDate="2017-02-06" xmlns="urn:skogforsk:stanford2010">\n',
    "<ProductInstructionHeader><SenderApplication>", .xesc(PIN_SENDER_APPLICATION), "</SenderApplication>",
    "<CreationDate>", now, "</CreationDate><ModificationDate>", now, "</ModificationDate>",
    "</ProductInstructionHeader>\n",
    paste0(prod$xml, collapse = "\n"), "\n</ProductInstruction>\n")
  con <- file(path, "wb")
  writeBin(charToRaw(enc2utf8(txt)), con)
  close(con)
  if (is.null(tryCatch(xml2::read_xml(path), error = function(e) NULL)))
    return(fail("The .pin file could not be written correctly."))
  if (.hpr_export_has_coordinates(path)) return(fail("Coordinates found in the export; the file was not written."))
  list(ok = TRUE, errors = character(0), path = path, n_products = nrow(defs), ids = ids)
}
