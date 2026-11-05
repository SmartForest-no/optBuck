# ==============================================================================
# R/helpers_hpr_logs.R  (roadmap 8, step 4, 2026-11-04)
#
# Purpose : The recorded logs of a StanForD 2010 .hpr file (Log elements), for the
#           bucking check (tab 5). Replaces getLogs() of the old optBuck package:
#           the same chunked, whitelisted regex reading as read_hpr_stems()
#           (never parsed whole, one pass, vectorised), instead of one XML parse
#           and one data.frame per log.
#
# Reads   : per Log: LogKey, ProductKey, StartPos (Extension, optional), LogLength,
#           CuttingCategory/CuttingReason, LogVolume (m3 (price), m3sob, m3sub and
#           the ...Estimated variants), LogDiameter (Butt/Mid/Top ob/ub). Per stem:
#           StemKey, SpeciesGroupKey, single/multi-tree kind.
# Never   : names, dates, coordinates, operators, objects (same whitelist rule as
#           R/helpers_hpr_stems.R).
# ==============================================================================


# CuttingReason values that are NOT manual cuts (StanForD 2010: "Automatic" and "Spinning").
HPR_AUTOMATIC_REASONS <- c("Automatic", "Spinning")

# One chunk's logs (r: complete stem records as bytes).
.hl_extract <- function(r) {
  key  <- trimws(.hs_re1(r, "<StemKey>([^<]*)</StemKey>"))
  sp   <- trimws(.hs_re1(r, "<SpeciesGroupKey>([^<]*)</SpeciesGroupKey>"))
  kind <- ifelse(grepl("<MultiTreeProcessedStem[\\s>/]", r, perl = TRUE, useBytes = TRUE), "multi", "single")
  lg   <- regmatches(r, gregexpr("(?s)<Log>.*?</Log>", r, perl = TRUE, useBytes = TRUE))
  cnt  <- lengths(lg); lg <- unlist(lg, use.names = FALSE)
  if (is.null(lg)) lg <- character(0)
  i <- rep.int(seq_along(r), cnt)
  vol <- function(cat) .hs_num1(lg, sprintf("logVolumeCategory=\"%s(?:Estimated)?\"[^>]*>\\s*([^<]*?)\\s*</LogVolume>", cat))
  dia <- function(cat) .hs_num1(lg, sprintf("logDiameterCategory=\"%s\"[^>]*>\\s*([^<]*?)\\s*</LogDiameter>", cat))
  data.frame(
    stem_key = key[i], species_key = sp[i], kind = kind[i],
    LogKey = .hs_num1(lg, "<LogKey>\\s*([^<]*?)\\s*</LogKey>"),
    ProductKey = trimws(.hs_re1(lg, "<ProductKey>([^<]*)</ProductKey>")),
    StartPos_recorded = .hs_num1(lg, "<StartPos>\\s*([^<]*?)\\s*</StartPos>"),
    LogLength = .hs_num1(lg, "<LogLength>\\s*([^<]*?)\\s*</LogLength>"),
    CuttingReason = trimws(.hs_re1(lg, "<CuttingReason>([^<]*)</CuttingReason>")),
    m3_price = vol("m3 \\(price\\)"), m3sob = vol("m3sob"), m3sub = vol("m3sub"),
    estimated = grepl("logVolumeCategory=\"m3s(?:o|u)bEstimated\"", lg, perl = TRUE, useBytes = TRUE),
    ButtOb = dia("Butt ob"), ButtUb = dia("Butt ub"), MidOb = dia("Mid ob"), MidUb = dia("Mid ub"),
    TopOb = dia("Top ob"), TopUb = dia("Top ub"),
    stringsAsFactors = FALSE)
}

# Read the recorded logs of a StanForD 2010 .hpr file (also .hpr.gz or an .hpr in a .zip).
#
# @param path     File path.
# @param filename Name used in messages (its extension decides .zip).
# @param defs     read_hpr() result, if already read (gives the unclassified product key).
# @param chunk_bytes Bytes per chunk (results do not depend on it).
# @return list(ok, errors, warnings, info, logs (data.frame, one row per recorded log in file
#   order: stem_key, species_key, kind, LogKey, ProductKey, StartPos (recorded, else the sum of
#   the previous lengths), start_recorded, LogLength, StopPos, CuttingReason, manual (not
#   "Automatic" or "Spinning"), waste (the unclassified product), m3_price, m3sob, m3sub,
#   estimated, ButtOb ... TopUb in mm), unclassified_keys, seconds).
read_hpr_logs <- function(path, filename = basename(path), defs = NULL,
                          chunk_bytes = HPR_STEM_CHUNK_BYTES) {
  t0 <- Sys.time()
  fail <- function(msg) list(ok = FALSE, errors = msg, warnings = character(0), info = character(0))
  if (is.null(defs)) defs <- read_hpr(path, filename)
  if (!isTRUE(defs$ok)) return(fail(defs$errors))
  src <- .hpr_source(path, filename)
  if (length(src$errors)) return(fail(src$errors))
  parts <- tryCatch(.hpr_stem_chunks(src$path, src$entry, .hl_extract, chunk_bytes), error = function(e) e)
  if (inherits(parts, "error"))
    return(fail(sprintf("The logs of %s cannot be read: %s", filename, conditionMessage(parts))))
  lg <- if (length(parts)) do.call(rbind, parts) else .hl_extract(character(0))
  rownames(lg) <- NULL
  # Start positions: recorded when the file has them (Extension/StartPos), else the previous start
  # plus the previous length (file5 has none). Logs are in file order within a stem.
  cum <- ave(lg$LogLength, lg$stem_key, FUN = function(x) c(0, cumsum(x)[-length(x)]))
  lg$start_recorded <- !is.na(lg$StartPos_recorded)
  lg$StartPos <- ifelse(lg$start_recorded, lg$StartPos_recorded, cum)
  lg$StartPos_recorded <- NULL
  lg$StopPos <- lg$StartPos + lg$LogLength
  lg$manual <- ifelse(is.na(lg$CuttingReason), NA, !lg$CuttingReason %in% HPR_AUTOMATIC_REASONS)
  uk <- defs$unclassified_keys
  if (is.null(uk) || length(uk) == 0L) uk <- "999999"
  lg$waste <- lg$ProductKey %in% uk
  warnings <- character(0)
  if (anyNA(lg$LogLength) || anyNA(lg$ProductKey))
    warnings <- c(warnings, "Some recorded logs have no length or product and are ignored.")
  lg <- lg[!is.na(lg$LogLength) & !is.na(lg$ProductKey), , drop = FALSE]
  list(ok = TRUE, errors = character(0), warnings = warnings,
       info = sprintf("Read %s recorded logs of %s stems from %s.", format(nrow(lg), big.mark = ","),
                      format(length(unique(lg$stem_key)), big.mark = ","), filename),
       logs = lg, unclassified_keys = uk, seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

# The permitted stem grades of each product of an .hpr file with the file's grade names
# (the table the old getPermittedGrades() gave, plus names): one row per product x species group
# x permitted grade; a product with no PermittedGradesDefinition has grade NA (= all grades).
# The unclassified product is listed with grade NA as well (it takes any grade).
#
# @param hpr read_hpr() result.
hpr_permitted_grades <- function(hpr) {
  gn <- hpr$grade_names
  one <- function(p) {
    sp <- if (length(p$species_keys)) p$species_keys else NA_character_
    g <- if (is.null(p$grades) || !length(p$grades)) NA_real_ else p$grades
    d <- expand.grid(species_key = sp, grade = g, stringsAsFactors = FALSE)
    data.frame(ProductKey = p$key, ProductName = p$name, species_key = d$species_key, grade = d$grade,
               stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, c(list(data.frame(ProductKey = character(0), ProductName = character(0),
                                          species_key = character(0), grade = numeric(0),
                                          stringsAsFactors = FALSE)), lapply(hpr$products, one)))
  out$grade_name <- if (is.null(gn) || nrow(gn) == 0L) NA_character_ else
    gn$name[match(paste(out$species_key, out$grade), paste(gn$species_key, gn$grade))]
  rownames(out) <- NULL
  out
}
