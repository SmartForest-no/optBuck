# ==============================================================================
# scripts/make_example_hpr.R  (2026-11-09)
#
# Purpose : Build inst/extdata/example.hpr, the anonymised example file of the package, from one of the
#           owner's real StanForD 2010 .hpr files (file5, C:/optbuck_private/hpr/REDACTED28). The real
#           file is never committed and nothing identifying is printed; the script prints counts only.
#
# What it does
#   1. Stems    : a random subset of whole stems (fixed seed) with recorded logs, as many as fit in MAX_BYTES.
#                 Stem and log measurements are copied byte for byte; only StemCoordinates is removed.
#   2. Products : only products with at least one recorded log in the chosen stems are kept (the others are
#                 removed with their matrices); buyer, company and place words are replaced by neutral text
#                 and the type words (Sag, Sagtommer, Massevirke, Stamblokk, ...) stay.
#   3. Prices   : every price of every kept product x 1.15, rounded to the nearest 5 NOK. Distribution,
#                 length and diameter classes and all other settings are unchanged.
#   4. Identity : operator, contact, machine owner, contractor, organisation, buyer, destination, forest
#                 owner, object and sub-object names, machine keys and ids, free text: neutral placeholders.
#                 Dates are shifted by a fixed offset (the year is kept).
#   5. Check    : every identifying string of the original header and object fields is searched for in the
#                 new file (scripted; only the number of strings and hits is printed).
#
# Usage   : Rscript scripts/make_example_hpr.R [restricted]
#           "restricted" writes, instead, the ORIGINAL header and prices with the same stems (for the
#           yield-index comparison) to C:/optbuck_private/example_check/ (never in the repository).
# ==============================================================================

root_dir <- local({
  d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "R", "helpers_paths.R")) && dirname(d) != d) d <- dirname(d)
  if (!file.exists(file.path(d, "R", "helpers_paths.R"))) stop("Run from inside the optBuck repository.", call. = FALSE)
  d
})
suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))
suppressMessages(library(xml2))

args       <- commandArgs(trailingOnly = TRUE)
RESTRICTED <- "restricted" %in% args
SRC        <- Sys.getenv("OPTBUCK_EXAMPLE_SRC", "C:/optbuck_private/hpr/REDACTED28")
OUT        <- if (RESTRICTED) "C:/optbuck_private/example_check/original_same_stems.hpr" else file.path(root_dir, "inst", "extdata", "example.hpr")
MAX_BYTES  <- 5000000
SEED       <- 1L
DATE_SHIFT <- 37L      # days; if the year would change, the date is shifted back instead (the year is kept)
PRICE_MULT <- 1.15
PRICE_STEP <- 5
SAFETY     <- 150000L   # bytes kept free for the header estimate (the written header is checked against MAX_BYTES)

if (!file.exists(SRC)) stop("Source file not found.", call. = FALSE)
dir.create(dirname(OUT), recursive = TRUE, showWarnings = FALSE)
dir.create("C:/optbuck_private/example_check", recursive = TRUE, showWarnings = FALSE)
say <- function(...) cat(sprintf(...), "\n", sep = "")

# ---- 1 read the source, find the stems ---------------------------------------------------------------------------
raw_all <- readBin(SRC, "raw", file.size(SRC))
txt <- rawToChar(raw_all)
starts <- as.integer(gregexpr("<Stem>\\s*<StemKey>", txt, perl = TRUE, useBytes = TRUE)[[1L]])
ends   <- as.integer(gregexpr("</Stem>", txt, fixed = TRUE, useBytes = TRUE)[[1L]])
ends   <- ends[ends > starts[1L]]
stopifnot(length(starts) == length(ends), all(starts < ends), all(ends[-length(ends)] < starts[-1L]))
ends   <- ends + 6L                                   # last byte of "</Stem>"
header_end <- starts[1L] - 1L                         # the bytes before the first stem
trailer <- rawToChar(raw_all[(ends[length(ends)] + 1L):length(raw_all)])
stopifnot(grepl("^\\s*</Machine>\\s*</HarvestedProduction>\\s*$", trailer))
stem_key <- vapply(seq_along(starts), function(i)
  sub("^.*?<StemKey>\\s*([^<]*?)\\s*</StemKey>.*$", "\\1", rawToChar(raw_all[starts[i]:min(starts[i] + 300L, ends[i])]), perl = TRUE), "")
stem_bytes <- ends - starts + 1L
say("source: %d stems, header %d bytes", length(starts), header_end)

h  <- read_hpr(SRC)
stopifnot(isTRUE(h$ok))
rl <- read_hpr_logs(SRC, defs = h)
stopifnot(isTRUE(rl$ok))
lg <- rl$logs
eligible <- which(stem_key %in% unique(lg$stem_key[!lg$waste]))
say("stems with recorded (non-waste) logs: %d", length(eligible))

# ---- 2 the header, parsed ---------------------------------------------------------------------------------------------
header_raw <- raw_all[seq_len(header_end)]
doc <- read_xml(optBuck:::.hpr_close_tags(header_raw))
xp <- function(path) paste0("//", paste0("*[local-name()='", strsplit(path, "/", fixed = TRUE)[[1L]], "']", collapse = "/"))
nodes_of <- function(path) xml_find_all(doc, xp(path))
txt_of   <- function(nodes) trimws(xml_text(nodes))
# A ProductDefinition element holds the ProductKey and one ClassifiedProductDefinition (or the unclassified one).
PROD_XP    <- "//*[local-name()='ProductDefinition'][*[local-name()='ClassifiedProductDefinition']]"
prod_nodes <- xml_find_all(doc, PROD_XP)
prod_key   <- txt_of(xml_find_first(prod_nodes, "./*[local-name()='ProductKey']"))
stopifnot(!anyNA(prod_key), !anyDuplicated(prod_key))
say("products in the header: %d", length(prod_nodes))

# ---- identifying strings of the original (collected before anything is replaced) -------------------------------------------
orig_strings <- character(0)
collect <- function(nodes, attrs = TRUE) {
  for (n in nodes) {
    # (the internal numeric keys, ObjectKey / SubObjectKey / OperatorKey, are kept on purpose: they only link stems to the header)
    v <- trimws(xml_text(xml_find_all(n, ".//*[not(*)][not(contains(local-name(), 'Key'))] | self::*[not(*)][not(contains(local-name(), 'Key'))]")))
    orig_strings <<- c(orig_strings, v[nzchar(v)])
    if (attrs) for (a in xml_find_all(n, ".//@* | @*")) { s <- trimws(xml_text(a)); if (nzchar(s) && xml_name(a) != "agency") orig_strings <<- c(orig_strings, s) }
  }
}
for (p in c("Machine/MachineKey", "Machine/MachineUserID", "Machine/MachineOwnerID", "Machine/MachineOwner",
            "Machine/BaseMachineManufacturerID", "Machine/LoggingContractor", "Machine/OperatorDefinition",
            "Machine/ObjectDefinition/ObjectName", "Machine/ObjectDefinition/ObjectUserID", "Machine/ObjectDefinition/ContractNumber",
            "Machine/ObjectDefinition/ForestOwner", "Machine/ObjectDefinition/LoggingOrganisation", "Machine/ObjectDefinition/SubObject",
            "Machine/ObjectDefinition/TextFromMachine", "Machine/ObjectDefinition/LoggingForm",
            "Machine/SpeciesGroupDefinition/LoggingOrganisation", "ClassifiedProductDefinition/ProductBuyer",
            "ClassifiedProductDefinition/ProductDestination", "ClassifiedProductDefinition/LoggingOrganisation",
            "HPRCMSettings/DefaultCoordinate", "HPRCMSettings/HPRCMSettingsItem/GeographicalRestriction", "HPRCMSettings/CopySettings",
            "HPRCMSettings/Description", "HPRCMSettings/HPRCMSettingsID", "HPRCMSettings/HPRCMSettingsTitle", "HPRCMSettings/HPRCMSettingsItem/Title"))
  collect(nodes_of(p))
# Words of product names and user ids that name a buyer, company or place (replaced below).
# They are listed in a private file (token, name_form, id_form), not in the repository: the words
# themselves name buyers and places.
NAME_MAP_FILE <- "C:/optbuck_private/example_check/name_map.csv"
nm <- utils::read.csv(NAME_MAP_FILE, encoding = "UTF-8", stringsAsFactors = FALSE, na.strings = character(0))
NAME_MAP <- lapply(seq_len(nrow(nm)), function(i) list(nm$token[i], nm$name_form[i], nm$id_form[i]))
orig_strings <- c(orig_strings, nm$token, unlist(strsplit(nm$token, "[ _]")))
orig_strings <- unique(orig_strings[nchar(orig_strings) >= 2L & !grepl("^[0-9]{1,3}$", orig_strings)])   # short numbers occur everywhere as data values

# ---- 3 stem choice ----------------------------------------------------------------------------------------------------------
set.seed(SEED)
order_idx <- eligible[sample.int(length(eligible))]
products_of <- function(keys_chosen) unique(lg$ProductKey[lg$stem_key %in% keys_chosen & !lg$waste])
header_estimate <- header_end
for (iter in 1:6) {
  cum <- cumsum(stem_bytes[order_idx])
  n <- sum(cum <= MAX_BYTES - header_estimate - SAFETY)
  chosen <- order_idx[seq_len(n)]
  keep_keys <- products_of(stem_key[chosen])
  # size of the header with only those products (their share of the product definitions)
  prod_sz <- vapply(prod_nodes, function(x) nchar(as.character(x), type = "bytes"), 0)
  new_est <- header_end - sum(prod_sz[!prod_key %in% c(keep_keys)]) - 0L
  if (abs(new_est - header_estimate) < 2000) break
  header_estimate <- new_est
}
if (RESTRICTED) header_estimate <- header_end
chosen <- sort(chosen)
keep_keys <- products_of(stem_key[chosen])
say("chosen stems: %d of %d eligible; products used by them: %d", length(chosen), length(eligible), length(keep_keys))

# ---- 4 the stems (coordinates removed, nothing else changed) -----------------------------------------------------------------
strip_coords <- function(s) gsub("<StemCoordinates\\b[^>]*?/>|<StemCoordinates\\b.*?</StemCoordinates>", "", s, perl = TRUE)
stem_txt <- vapply(chosen, function(i) {
  s <- rawToChar(raw_all[starts[i]:ends[i]])
  if (RESTRICTED) s else strip_coords(s)
}, "")
if (!RESTRICTED) {
  allowed <- c("Stem", "StemKey", "SpeciesGroupKey", "ObjectKey", "SubObjectKey", "OperatorKey", "StemNumber", "ProcessingCategory",
               "Extension", "CabinBoomDistance", "HPRCMResults", "StemInfo", "NoTop", "StemJoined", "DBHCorrected", "EstimatedTop",
               "HeightToCrown", "TreeHeight", "DominantHeight", "Biomass", "SingleTreeProcessedStem", "DBH", "ReferenceDiameter",
               "StemDiameters", "DiameterMeasuredStartHeight", "DiameterMeasuredEndHeight", "DiameterValue", "StemGrade", "GradeValue",
               "Log", "LogKey", "ProductKey", "LogVolume", "LogMeasurement", "LogDiameter", "LogLength", "CuttingCategory", "CuttingReason")
  tags <- unique(unlist(regmatches(stem_txt, gregexpr("(?<=<)[A-Za-z][A-Za-z0-9_.]*", stem_txt, perl = TRUE))))
  unknown <- setdiff(tags, allowed)
  say("stem element names: %d, not in the reviewed list: %d%s", length(tags), length(unknown),
      if (length(unknown)) paste0(" (", paste(unknown, collapse = ", "), ")") else "")
  if (length(unknown)) stop("Unreviewed element names in the stems: review them before writing the file.", call. = FALSE)
  if (any(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", stem_txt))) stop("A date-like string in a stem.", call. = FALSE)
  if (any(grepl("StemCoordinates|Latitude|Longitude", stem_txt))) stop("Coordinates left in a stem.", call. = FALSE)
}

# ---- 5 the header edits ------------------------------------------------------------------------------------------------------
if (!RESTRICTED) {
  # 5a products with no recorded log in the chosen stems are removed with everything inside them
  drop <- !prod_key %in% keep_keys
  pname_of <- function(n) trimws(xml_text(xml_find_first(n, ".//*[local-name()='ProductName']")))
  removed_names <- vapply(prod_nodes[drop], pname_of, "")
  xml_remove(prod_nodes[drop])
  prod_nodes <- xml_find_all(doc, PROD_XP)
  say("products kept %d, removed %d", sum(!drop), sum(drop))

  # 5b prices x 1.15, rounded to the nearest 5 NOK (kept products only)
  pr <- xml_find_all(doc, "//*[local-name()='ClassifiedProductDefinition']//*[local-name()='Price']")
  v  <- suppressWarnings(as.numeric(trimws(xml_text(pr))))
  nv <- ifelse(is.na(v), NA, round(v * PRICE_MULT / PRICE_STEP) * PRICE_STEP)
  ok <- !is.na(nv)
  xml_set_text(pr[ok], format(nv[ok], scientific = FALSE, trim = TRUE))
  say("prices changed: %d of %d cells (x%.2f, nearest %d)", sum(ok & v != nv, na.rm = TRUE), length(pr), PRICE_MULT, PRICE_STEP)

  # 5c names of products: buyer, company and place words replaced, type words kept
  rename_one <- function(s, form) {
    for (m in NAME_MAP) {
      new <- if (form == "name") m[[2L]] else m[[3L]]
      pat <- paste0("(?<![\\p{L}\\p{N}])", gsub("([.()])", "\\\\\\1", m[[1L]]), "(?![\\p{L}\\p{N}])")
      s <- gsub(pat, new, s, perl = TRUE)
    }
    s <- gsub("\\s+", " ", trimws(s)); gsub("_+", "_", gsub("^_|_$", "", s))
  }
  pn <- nodes_of("ClassifiedProductDefinition/ProductName")
  xml_set_text(pn, vapply(txt_of(pn), rename_one, "", form = "name"))
  pu <- nodes_of("ClassifiedProductDefinition/ProductUserID")
  xml_set_text(pu, vapply(txt_of(pu), rename_one, "", form = "id"))
  nn <- txt_of(nodes_of("ClassifiedProductDefinition/ProductName"))
  say("kept product names (neutral): %s", paste(nn, collapse = " | "))
  say("removed product names: %s", paste(removed_names, collapse = " | "))
  # a word that is removed altogether can leave a dangling dash
  nodes_fix <- nodes_of("ClassifiedProductDefinition/ProductName")
  xml_set_text(nodes_fix, sub("\\s*-\\s*$", "", txt_of(nodes_fix)))

  # 5d placeholders for identifying fields (the same original value always gets the same placeholder)
  counters <- new.env()
  placeholder <- function(label, orig, digits = 0L, numeric_ok = FALSE) {
    key <- paste(label, orig)
    if (is.null(counters[[key]])) {
      n <- sum(startsWith(ls(counters), paste0(label, " "))) + 1L
      counters[[key]] <- if (numeric_ok && grepl("^[0-9]+$", orig)) as.character(1000L + n)
                         else if (digits > 0L) sprintf(paste0("%s %0", digits, "d"), label, n) else sprintf("%s %d", label, n)
    }
    counters[[key]]
  }
  replace_text <- function(path, label, ...) {
    nn <- nodes_of(path)
    for (n in nn[!vapply(nn, function(x) xml_length(x) > 0L, NA)]) {
      o <- trimws(xml_text(n)); if (nzchar(o)) xml_set_text(n, placeholder(label, o, ...))
    }
  }
  replace_text("Machine/MachineKey", "MachineKey", digits = 4L)
  replace_text("Machine/BaseMachineManufacturerID", "BaseMachineID", digits = 4L)
  replace_text("Machine/MachineOwner/BusinessID", "Owner", digits = 4L)
  replace_text("Machine/MachineOwnerID", "Owner ID", digits = 4L)
  replace_text("Machine/MachineUserID", "Machine", digits = 4L)
  replace_text("Machine/LoggingContractor/BusinessName", "Example contractor")
  replace_text("Machine/OperatorDefinition/ContactInformation/FirstName", "Operator")
  replace_text("Machine/OperatorDefinition/OperatorUserID", "Operator")
  replace_text("Machine/ObjectDefinition/ObjectName", "Example object")
  replace_text("Machine/ObjectDefinition/ObjectUserID", "Object", numeric_ok = TRUE)
  replace_text("Machine/ObjectDefinition/ContractNumber", "Contract", numeric_ok = TRUE)
  replace_text("Machine/ObjectDefinition/TextFromMachine", "Example text")
  # Object coordinates of the biomass settings (HPRCM extension): a default position and a geographical restriction.
  # Both are removed (no coordinates stay in the file); the free-text names of the settings are replaced.
  n_coord <- length(xml_find_all(doc, "//*[local-name()='DefaultCoordinate' or local-name()='GeographicalRestriction']"))
  xml_remove(xml_find_all(doc, "//*[local-name()='DefaultCoordinate' or local-name()='GeographicalRestriction']"))
  say("object coordinate blocks removed: %d", n_coord)
  replace_text("HPRCMSettings/CopySettings", "Example settings")
  replace_text("HPRCMSettings/Description", "Example settings")
  replace_text("HPRCMSettings/HPRCMSettingsID", "Example settings")
  replace_text("HPRCMSettings/HPRCMSettingsTitle", "Example settings")
  replace_text("HPRCMSettings/HPRCMSettingsItem/Title", "Example settings")
  replace_text("Machine/ObjectDefinition/SubObject/SubObjectName", "Example sub-object")
  replace_text("Machine/ObjectDefinition/SubObject/SubObjectUserID", "SubObject")
  replace_text("Machine/ObjectDefinition/SubObject/RealEstateIDSubObject", "Property")
  replace_text("Machine/ObjectDefinition/SubObject/LoggingForm/LoggingFormDescription", "Example logging form")
  replace_text("Machine/ObjectDefinition/LoggingForm/LoggingFormDescription", "Example logging form")
  replace_text("Machine/ObjectDefinition/LoggingOrganisation/ContactInformation/BusinessName", "Example organisation")
  replace_text("Machine/ObjectDefinition/LoggingOrganisation/District", "Example district")
  replace_text("Machine/ObjectDefinition/LoggingOrganisation/Team", "Example team")
  replace_text("Machine/SpeciesGroupDefinition/LoggingOrganisation/ContactInformation/BusinessName", "Example organisation")
  replace_text("Machine/SpeciesGroupDefinition/LoggingOrganisation/District", "Example district")
  replace_text("Machine/SpeciesGroupDefinition/LoggingOrganisation/Team", "Example team")
  replace_text("ClassifiedProductDefinition/LoggingOrganisation/ContactInformation/BusinessName", "Example organisation")
  replace_text("ClassifiedProductDefinition/LoggingOrganisation/District", "Example district")
  replace_text("ClassifiedProductDefinition/LoggingOrganisation/Team", "Example team")
  replace_text("ClassifiedProductDefinition/ProductBuyer/BusinessName", "Example buyer")
  replace_text("ClassifiedProductDefinition/ProductDestination/BusinessName", "Example destination")
  # Any other leaf below a person/organisation/address element that still has text (address lines, phone, mail, ...).
  for (grp in c("ContactInformation", "Address", "ForestOwner", "MachineOwner", "LoggingContractor", "ProductBuyer", "ProductDestination")) {
    lv <- xml_find_all(doc, sprintf("//*[local-name()='%s']//*[not(*)]", grp))
    for (n in lv) {
      o <- trimws(xml_text(n))
      if (nzchar(o) && !grepl("^(Example|Operator|Owner|Machine|BaseMachine)", o) && xml_name(n) != "Country")
        xml_set_text(n, placeholder("Example field", o))
    }
  }
  # The agency attributes are schema role codes (Buyer, ForestOwner, Other, ...), not identifying: left as they are.

  # 5e dates: shifted by a fixed offset, the year kept (elements, attributes and the ddmmyyyy / ddmmyy product versions)
  shift_date <- function(d) { d2 <- d + DATE_SHIFT; back <- format(d2, "%Y") != format(d, "%Y"); d2[back] <- d[back] - DATE_SHIFT; d2 }
  shift_iso <- function(s) {
    ok <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}(T.*)?$", s)
    out <- s
    out[ok] <- paste0(format(shift_date(as.Date(substr(s[ok], 1, 10)))), substring(s[ok], 11))
    out
  }
  leaves <- xml_find_all(doc, "//*[not(*)]")
  lv <- trimws(xml_text(leaves))
  di <- which(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}(T.*)?$", lv))
  if (length(di)) xml_set_text(leaves[di], shift_iso(lv[di]))
  n_dates <- length(di)
  # Attribute values that look like dates (the schema version date on the root is not a data date).
  at <- xml_find_all(doc, "//@*[not(parent::*[local-name()='HarvestedProduction'])]")
  if (any(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", xml_text(at)))) stop("A date-like attribute value: handle it explicitly.", call. = FALSE)
  for (n in nodes_of("ClassifiedProductDefinition/ProductVersion")) {
    o <- trimws(xml_text(n))
    if (grepl("^[0-9]{8}$", o)) { d <- as.Date(o, "%d%m%Y"); xml_set_text(n, format(shift_date(d), "%d%m%Y")); n_dates <- n_dates + 1L }
    else if (grepl("^[0-9]{6}$", o)) { d <- as.Date(o, "%d%m%y"); xml_set_text(n, format(shift_date(d), "%d%m%y")); n_dates <- n_dates + 1L }
  }
  say("dates shifted: %d (offset %d days, year kept)", n_dates, DATE_SHIFT)
}

# ---- 6 write ------------------------------------------------------------------------------------------------------------------
if (RESTRICTED) {
  hdr <- rawToChar(header_raw)
} else {
  hdr <- as.character(doc)
  hdr <- sub("\\s*</Machine>\\s*</HarvestedProduction>\\s*$", "", hdr)
  if (!grepl("^<\\?xml", hdr)) stop("Unexpected serialisation.", call. = FALSE)
}
body <- paste0(hdr, paste(stem_txt, collapse = ""), "</Machine></HarvestedProduction>\n")
con <- file(OUT, "wb"); writeBin(charToRaw(enc2utf8(body)), con); close(con)
say("written: %.2f MB, %d stems", file.size(OUT) / 1024^2, length(chosen))
if (!RESTRICTED && file.size(OUT) > MAX_BYTES) stop("The file is larger than the limit.", call. = FALSE)

# ---- 7 identifying strings ---------------------------------------------------------------------------------------------------
if (!RESTRICTED) {
  out_txt <- paste(readLines(OUT, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  hit <- vapply(orig_strings, function(s) {
    pat <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", s)
    pat <- paste0("(?<![\\p{L}\\p{N}])", pat, "(?![\\p{L}\\p{N}])")
    grepl(pat, out_txt, perl = TRUE, ignore.case = nchar(s) >= 4L)
  }, NA)
  say("identifying strings searched: %d, found in the new file: %d", length(orig_strings), sum(hit))
  if (any(hit)) say("  hits (by position in the list, values not printed): %s", paste(which(hit), collapse = ", "))
  saveRDS(list(strings = orig_strings, hit = hit, chosen_stem_keys = stem_key[chosen], keep_keys = keep_keys),
          "C:/optbuck_private/example_check/example_build.rds")
} else {
  saveRDS(list(chosen_stem_keys = stem_key[chosen], keep_keys = keep_keys), "C:/optbuck_private/example_check/restricted_build.rds")
}
