# ==============================================================================
# R/helpers_hpr.R
#
# Purpose : StanForD 2010 .hpr files (harvester production reports), Phase A
#           (2026-10-20): read the species groups, products, price matrices
#           and permitted grades, and turn them into a scenario (assortment
#           table and prices) for "Import from .hpr" on the Assortments and
#           prices tab. Stems are not read here: hpr Phase B reads them in
#           R/helpers_hpr_stems.R (".hpr stems" tree source).
#
# Source  : ported from optBuck (SmartForest-no/optBuck: getSpeciesGroup-
#           Definition(), getProductData(), getPriceMatrices(),
#           getPermittedGrades()), written on xml2. Deliberate departures
#           (CLAUDE.md): min top = DiameterMINTop, max top = DiameterClassMAX,
#           max butt = DiameterMAXButt; length classes round UP to whole dm;
#           "Forbidden to buck the log even manually" and "Only logs bucked
#           manually allowed" cells are left empty (Phase 5b);
#           the price volume of "Length as defined in LengthClasses" products
#           uses the class lower limit (PriceLengths_cm, helpers_products.R).
#
# Privacy : never reads StemCoordinates, dates, operators, ObjectName or any
#           contact fields.
#
# Errors  : read_hpr() and hpr_to_scenario() return errors and warnings as
#           character vectors (shown in the import dialog); they do not stop.
# ==============================================================================


# Token prefixes that identify a species group's species (lower case).
HPR_SPECIES_PATTERNS <- list(
  spruce = c("gran", "spruce", "picea", "kuusi", "fichte"),
  pine   = c("furu", "fura", "tall", "pine", "pinus", "m\u00e4nty", "manty", "kiefer"))

# Bucking criterion of a price cell that may not be cut: left empty.
HPR_FORBIDDEN <- "Forbidden to buck the log even manually"

# Bucking criterion of a price cell for logs bucked by hand only. optBuck
# simulates automatic bucking, so such cells are left empty too (decided
# 2026-10-29, Phase 5b). "The log is excluded from apportionment bucking,
# only bucking according to value allowed" stays priced.
HPR_MANUAL <- "Only logs bucked manually allowed"

# Criteria of cells the optimiser may not use (left empty).
HPR_UNAVAILABLE <- c(HPR_FORBIDDEN, HPR_MANUAL)

# StanForD volume categories optBuck computes (others become solid volume).
HPR_VOLUME_METHODS <- c("Top", "Calculated Norwegian mid", "Mid", "All diameters (solid volume)")

# The price volume method an imported product gets (0.3.1): the file's own method, per product.
# StanForD "Top" is the cylinder at the top diameter (optBuck's "Top cylinder"); the other three
# keep their names. A method that is not listed gets the frustum (optBuck's "Top") and one warning.
HPR_VOLUME_METHOD_MAP <- c("Top" = "Top cylinder", "Calculated Norwegian mid" = "Calculated Norwegian mid",
                           "Mid" = "Mid", "All diameters (solid volume)" = "All diameters (solid volume)")
HPR_VOLUME_FALLBACK <- "Top"

# Volume method of a .pin product without a PriceDefinition: the default of a
# new assortment in the app (Phase 5b). Never read from a file, so it is not
# in HPR_VOLUME_METHODS; hpr_to_scenario() accepts it without a warning.
PIN_DEFAULT_VOLUME_METHOD <- "Top cylinder"

# Product group / name tokens of sawlogs and pulpwood (the hidden Type).
HPR_SAWLOG_TOKENS   <- c("t\u00f8mmer", "tommer", "sag", "timber", "saw", "kubb", "stock")
HPR_PULPWOOD_TOKENS <- c("massevirke", "massa", "pulp", "energi", "energy", "bio")
# Extra pulpwood prefixes read from the product name only (R5): a product group "MASSE" also
# holds a birch product that stays a custom type.
HPR_PULPWOOD_NAME_TOKENS <- c("masse", "rmasse", "t\u00f8rr", "torr")

# Text (trimmed) / number / attribute of the first node matching `xpath`
# under one node, NA if none; all matching texts.
.hnum <- function(n, xpath) suppressWarnings(as.numeric(.hx1(n, xpath)))
.hx1  <- function(n, xpath) {
  r <- xml2::xml_find_first(n, xpath)
  if (inherits(r, "xml_missing")) NA_character_ else trimws(xml2::xml_text(r))
}
.hattr1 <- function(n, xpath, attr) {
  r <- xml2::xml_find_first(n, xpath)
  if (inherits(r, "xml_missing")) NA_character_ else xml2::xml_attr(r, attr)
}
.hall <- function(n, xpath) trimws(xml2::xml_text(xml2::xml_find_all(n, xpath)))

# Lower-case letter tokens of a text ("NO1_FURU" -> "no", "furu").
.hpr_tokens <- function(x) {
  x <- tolower(paste(x[!is.na(x)], collapse = " "))
  t <- strsplit(x, "[^[:alpha:]\u00e6\u00f8\u00e5\u00e4\u00f6\u00fc]+", perl = TRUE)[[1L]]
  t[nzchar(t)]
}

# Guess optBuck's species of a species group from its name and user ID:
# "spruce", "pine", or "none" (no match, or both).
guess_hpr_species <- function(name, user_id = NA) {
  vapply(seq_along(name), function(i) {
    tok <- .hpr_tokens(c(name[i], user_id[i]))
    hit <- vapply(HPR_SPECIES_PATTERNS, function(p)
      any(vapply(p, function(q) any(startsWith(tok, q)), logical(1))), logical(1))
    if (sum(hit) == 1L) names(hit)[hit] else "none"
  }, character(1))
}

# The hidden Type of an imported product: sawlog / pulpwood from the product
# group (or name), else a custom type (never "waste").
.hpr_type <- function(group, name) {
  tg <- .hpr_tokens(group); tn <- .hpr_tokens(name)
  has <- function(tok, p) any(vapply(p, function(q) any(startsWith(tok, q)), logical(1)))
  if (has(tg, HPR_SAWLOG_TOKENS)) return("sawlog")
  if (has(tg, HPR_PULPWOOD_TOKENS)) return("pulpwood")
  if (has(tn, HPR_SAWLOG_TOKENS)) return("sawlog")
  if (has(tn, c(HPR_PULPWOOD_TOKENS, HPR_PULPWOOD_NAME_TOKENS))) return("pulpwood")
  custom_type(name)
}


# One product of a StanForD 2010 product definition (an .hpr Machine/ProductDefinition
# or a .pin ProductDefinition) as a list. `key` is ProductKey (.hpr); the .pin
# reader overwrites it (a .pin has none).
.hpr_product <- function(p) {
  cp <- xml2::xml_find_first(p, "./ClassifiedProductDefinition")
  dd <- "./DiameterDefinition"; dc <- paste0(dd, "/DiameterClasses")
  items <- xml2::xml_find_all(cp, "./ProductMatrixes/ProductMatrixItem")
  grades <- .hall(cp, "./PermittedGradesDefinition/PermittedGradeNumber")
  # Phase 7a: the product's LengthDistributionDefinition (target distribution), kept
  # as it is (NA when the product has none); not used by runs.
  ld <- "./LengthDistributionDefinition"
  has_ld <- !inherits(xml2::xml_find_first(cp, ld), "xml_missing")
  ldl <- function(x) if (is.na(x)) NA else tolower(x) %in% c("true", "1")
  list(
    dist_allowed  = if (has_ld) ldl(.hx1(cp, paste0(ld, "/DistributionAllowed"))) else NA,
    dist_category = if (has_ld) .hx1(cp, paste0(ld, "/DistributionCategory")) else NA_character_,
    dist_within   = if (has_ld) ldl(.hx1(cp, paste0(ld, "/DistributionWithinProduct"))) else NA,
    dist_maxdev   = if (has_ld) .hnum(cp, paste0(ld, "/MAXDeviation")) else NA_real_,
    # R4 D6: the product's own text and identity elements, kept for the export
    # (source_xml_build()). A .pin has the ProductUserID on the ProductDefinition.
    src_cp  = as.character(cp),
    src_uid = {
      u <- xml2::xml_find_first(cp, "./ProductUserID")
      if (inherits(u, "xml_missing")) u <- xml2::xml_find_first(p, "./ProductUserID")
      if (inherits(u, "xml_missing")) NA_character_ else as.character(u)
    },
    src_sg  = {
      g <- xml2::xml_find_first(cp, "./SpeciesGroupUserID")
      if (inherits(g, "xml_missing")) NA_character_ else as.character(g)
    },
    key        = .hx1(p, "./ProductKey"),
    name       = .hx1(cp, "./ProductName"),
    group      = .hx1(cp, "./ProductGroupName"),
    species_keys = unique(.hall(cp, "./SpeciesGroupKey")),
    dia_classes  = sort(suppressWarnings(as.numeric(.hall(cp, paste0(dc, "/DiameterClass/DiameterClassLowerLimit"))))),
    dia_class_max = .hnum(cp, paste0(dc, "/DiameterClassMAX")),
    dia_under_bark = .hx1(cp, paste0(dc, "/DiameterUnderBark")),
    min_top    = .hnum(cp, paste0(dd, "/DiameterMINTop")),
    max_butt   = .hnum(cp, paste0(dd, "/DiameterMAXButt")),
    top_pos    = .hnum(cp, paste0(dd, "/DiameterTopPosition")),
    len_classes = sort(suppressWarnings(as.numeric(.hall(cp, "./LengthDefinition/LengthClass/LengthClassLowerLimit")))),
    len_margins = suppressWarnings(as.numeric(.hall(cp, "./LengthDefinition/LengthClass/LengthClassMargin"))),
    len_class_max = .hnum(cp, "./LengthDefinition/LengthClassMAX"),
    volume_method = .hx1(cp, "./PriceDefinition/VolumeDiameterCategory"),
    volume_top_pos = suppressWarnings(as.numeric(.hattr1(cp, "./PriceDefinition/VolumeDiameterCategory",
                                                           "volumeDiameterTopPosition"))),
    volume_adjustment = .hx1(cp, "./PriceDefinition/VolumeDiameterAdjustment"),
    volume_length = .hx1(cp, "./PriceDefinition/VolumeLengthCategory"),
    volume_under_bark = .hx1(cp, "./PriceDefinition/VolumeUnderBark"),
    condition = .hx1(cp, "./ProductCondition"),   # roadmap 8: "Buck always" | "Buck butt-log only" | "Do not buck butt log"
    grades = if (inherits(xml2::xml_find_first(cp, "./PermittedGradesDefinition"), "xml_missing")) NULL
             else suppressWarnings(as.numeric(grades)),
    cells = data.frame(
      d_mm  = suppressWarnings(as.numeric(xml2::xml_attr(items, "diameterClassLowerLimit"))),
      l_cm  = suppressWarnings(as.numeric(xml2::xml_attr(items, "lengthClassLowerLimit"))),
      # One XPath call per column over all items (not one per item).
      price = suppressWarnings(as.numeric(xml2::xml_text(xml2::xml_find_first(items, "./Price")))),
      criteria = trimws(xml2::xml_text(xml2::xml_find_first(items, "./BuckingCriteria"))),
      dist  = suppressWarnings(as.numeric(xml2::xml_text(xml2::xml_find_first(items, "./Distribution")))),
      limit = suppressWarnings(as.numeric(xml2::xml_text(xml2::xml_find_first(items, "./Limitation")))),
      stringsAsFactors = FALSE))
}

# The hidden SourceCells text (R4 polish): the cells of the source diameter classes
# that have no whole-cm row in optBuck (two classes inside one cm, classes below the
# min or above the max top diameter), so an export can write every class of the
# source. One row per cell "d_mm|l_cm|price|dist|limit|criteria" joined by ";"
# (criteria percent-encoded); a class without cells gets one row with empty fields
# after d_mm. NA when every class has a row.
source_cells_build <- function(p, mapped_mm) {
  cl <- sort(unique(c(p$dia_classes, p$cells$d_mm))); cl <- cl[!is.na(cl)]
  un <- setdiff(cl, mapped_mm)
  if (length(un) == 0L) return(NA_character_)
  num <- function(x) ifelse(is.na(x), "", format(x, digits = 15, trim = TRUE, scientific = FALSE))
  rows <- unlist(lapply(un, function(d) {
    c <- p$cells[!is.na(p$cells$d_mm) & p$cells$d_mm == d & !is.na(p$cells$l_cm), , drop = FALSE]
    if (nrow(c) == 0L) return(paste0(num(d), "|||||"))
    paste(num(c$d_mm), num(c$l_cm), num(c$price), num(c$dist), num(c$limit),
          vapply(ifelse(is.na(c$criteria), "", c$criteria), utils::URLencode, "", reserved = TRUE), sep = "|")
  }))
  paste(rows, collapse = ";")
}

# SourceCells text -> data.frame(d_mm, l_cm, price, dist, limit, criteria) (empty when NA).
source_cells_parse <- function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(x))
    return(data.frame(d_mm = numeric(0), l_cm = numeric(0), price = numeric(0), dist = numeric(0),
                      limit = numeric(0), criteria = character(0), stringsAsFactors = FALSE))
  f <- strsplit(strsplit(x, ";", fixed = TRUE)[[1L]], "|", fixed = TRUE)
  g <- function(i) vapply(f, function(r) if (length(r) >= i) r[i] else "", "")
  n <- function(i) suppressWarnings(as.numeric(g(i)))
  data.frame(d_mm = n(1), l_cm = n(2), price = n(3), dist = n(4), limit = n(5),
             criteria = vapply(g(6), utils::URLdecode, "", USE.NAMES = FALSE), stringsAsFactors = FALSE)
}

# ---- header repair ---------------------------------------------------------------------

# Some files need repair before an XML parser accepts them: (1) a few John
# Deere TimbermaticH files start with the literal ASCII text "<U+FEFF>" (a
# byte order mark written as text) before the XML declaration, and (2) some
# files declare UTF-8 but hold windows-1252 bytes (for example in the header's
# names). The marker is removed; lines that are not valid UTF-8 are converted
# from windows-1252, other lines are left as they are. Valid UTF-8 and real
# byte order marks are not touched (the same bytes come back).
.hpr_clean_bytes <- function(bytes) {
  bytes <- bytes[bytes != as.raw(0L)]
  if (!length(bytes)) return(bytes)
  head <- rawToChar(bytes[seq_len(min(64L, length(bytes)))])
  m <- regexpr("^[[:space:]]*<U[+]FEFF>", head, useBytes = TRUE)
  if (m > 0L) bytes <- bytes[-seq_len(attr(m, "match.length"))]
  if (validUTF8(rawToChar(bytes))) return(bytes)
  nl <- which(bytes == as.raw(10L))
  starts <- c(1L, nl + 1L); ends <- c(nl, length(bytes))
  keep <- starts <= ends
  starts <- starts[keep]; ends <- ends[keep]
  out <- lapply(seq_along(starts), function(i) {
    ln <- bytes[starts[i]:ends[i]]
    if (validUTF8(rawToChar(ln))) ln
    else charToRaw(enc2utf8(iconv(rawToChar(ln), "windows-1252", "UTF-8", sub = "?")))
  })
  unlist(out, use.names = FALSE)
}

# All bytes of a (possibly gzipped) file, repaired by .hpr_clean_bytes().
.hpr_read_clean <- function(path) {
  con <- gzfile(path, "rb"); on.exit(close(con))
  parts <- list()
  repeat {
    b <- readBin(con, "raw", HPR_CHUNK_BYTES)
    if (!length(b)) break
    parts[[length(parts) + 1L]] <- b
  }
  .hpr_clean_bytes(unlist(parts, use.names = FALSE))
}

# ---- reading -------------------------------------------------------------------------

# Bytes read per step when looking for the first stem.
HPR_CHUNK_BYTES <- 4L * 1024L * 1024L

# A stem record: <Stem> directly followed by its StemKey. ("<Stem>" alone
# also occurs inside species groups' biomass functions, and StemTypeDefinition
# and StemCode start with "<Stem".)
HPR_STEM_START <- "<Stem[^>]*>[[:space:]]*<StemKey"

# Byte position of the first stem record in `buf` (NA if none): the first
# "<StemKey>" (a fast fixed search), then its "<Stem" opening tag, checked
# with HPR_STEM_START on that short stretch only.
.hpr_stem_pos <- function(buf) {
  for (k in grepRaw("<StemKey>", buf, fixed = TRUE, all = TRUE)) {
    if (k <= 1L) next
    from <- max(1L, k - 400L)
    win <- buf[from:(k - 1L)]                       # up to the "<StemKey>"
    s <- grepRaw("<Stem", win, fixed = TRUE, all = TRUE)
    if (!length(s)) next
    p <- s[length(s)]                               # the last "<Stem" before it
    if (grepl("^<Stem[^>]*>[[:space:]]*$", rawToChar(win[p:length(win)]), useBytes = TRUE))
      return(from + p - 1L)
  }
  NA_integer_
}

# The bytes of an .hpr (or .gz) file before its first stem record:
# list(bytes (raw), found (a stem was found), total (bytes read)). Only that
# part is read; a file without stems is read whole.
.hpr_prefix <- function(path, entry = NULL) {
  # gzfile() also reads uncompressed files; unz() streams one zip entry.
  con <- if (is.null(entry)) gzfile(path, "rb") else unz(path, entry, open = "rb")
  on.exit(close(con))
  done <- list(); tail <- raw(0); total <- 0
  repeat {
    b <- readBin(con, "raw", HPR_CHUNK_BYTES)
    if (length(b) == 0L)
      return(list(bytes = c(unlist(done, use.names = FALSE), tail), found = FALSE, total = total))
    total <- total + length(b)
    buf <- c(tail, b)
    m <- .hpr_stem_pos(buf)
    if (!is.na(m)) {
      return(list(bytes = c(unlist(done, use.names = FALSE), buf[seq_len(m - 1L)]),
                  found = TRUE, total = total))
    }
    keep <- max(0L, length(buf) - 512L)   # a match may straddle two chunks
    done[[length(done) + 1L]] <- buf[seq_len(keep)]
    tail <- buf[seq_along(buf) > keep]   # not buf[-seq_len(keep)]: keep = 0 would select nothing
  }
}

# Close the elements still open at the end of a truncated XML text (the part
# before the first stem), so it parses as a whole document.
.hpr_close_tags <- function(bytes) {
  txt <- rawToChar(bytes)
  tags <- regmatches(txt, gregexpr("<(/?)([A-Za-z_][A-Za-z0-9_.:-]*)[^<>]*?(/?)>", txt, perl = TRUE,
                                   useBytes = TRUE))[[1L]]
  tags <- tags[!grepl("^<[?!]", tags, useBytes = TRUE) & !grepl("/>$", tags, useBytes = TRUE)]
  name <- sub("^</?([A-Za-z_][A-Za-z0-9_.:-]*).*$", "\\1", tags, perl = TRUE, useBytes = TRUE)
  closing <- startsWith(tags, "</")
  stack <- character(0)
  for (i in seq_along(tags)) {
    if (!closing[i]) stack <- c(stack, name[i])
    else if (length(stack) && stack[length(stack)] == name[i]) stack <- stack[-length(stack)]
  }
  c(bytes, charToRaw(paste0(sprintf("</%s>", rev(stack)), collapse = "")))
}

# The .hpr file to read: the file itself, or for a .zip the first .hpr
# inside it (`entry`: streamed with unz(), extracted only when the whole
# file must be parsed). list(path, entry, errors, warnings).
.hpr_source <- function(path, filename) {
  if (!grepl("\\.zip$", filename, ignore.case = TRUE))
    return(list(path = path, entry = NULL, errors = character(0), warnings = character(0)))
  ls <- tryCatch(utils::unzip(path, list = TRUE), error = function(e) NULL)
  if (is.null(ls)) return(list(errors = sprintf("%s is not a readable zip file.", filename)))
  hp <- ls$Name[grepl("\\.hpr$", ls$Name, ignore.case = TRUE)]
  if (length(hp) == 0L) return(list(errors = sprintf("%s contains no .hpr file.", filename)))
  list(path = path, entry = hp[1L], errors = character(0),
       warnings = if (length(hp) > 1L) sprintf("%s holds %d .hpr files; %s was read.", filename, length(hp), hp[1L])
                  else character(0))
}

# Read the definitions of a StanForD 2010 .hpr file (also .hpr.gz, or an
# .hpr inside a .zip): species groups, products with their price matrices
# and permitted grades. Only the part before the first stem is parsed
# (definitions precede the stems in StanForD 2010 files); if no definitions
# are found there, the whole file is parsed.
#
# @param path     File path.
# @param filename Name shown in messages (its extension decides .zip).
# @return list(ok, errors, warnings, header (file, version, read: "prefix"
#   or "whole", mb_read, mb_file), species (data.frame: key, name, user_id,
#   best_grade, start_grade, dbh_height (cm from the felling cut; NA if
#   absent), n_products), products (list, one per classified product)).
#   Stems are not read here (R/helpers_hpr_stems.R).
read_hpr <- function(path, filename = basename(path)) {
  fail <- function(msg) list(ok = FALSE, errors = msg, warnings = character(0))
  src <- .hpr_source(path, filename)
  if (length(src$errors)) return(fail(src$errors))
  path <- src$path
  pre <- tryCatch(.hpr_prefix(path, src$entry), error = function(e) NULL)
  if (is.null(pre)) return(fail(sprintf("%s cannot be read.", filename)))
  parse <- function(x) tryCatch(xml2::read_xml(if (is.character(x)) .hpr_read_clean(x) else x,
                                               options = c("NOBLANKS", "HUGE")),
                                error = function(e) NULL)
  x <- parse(.hpr_clean_bytes(if (pre$found) .hpr_close_tags(pre$bytes) else pre$bytes))
  how <- if (pre$found) "prefix" else "whole"
  # Both kinds of definitions must be in the part read; otherwise (e.g. a
  # stem before the products) the whole file is parsed.
  has_defs <- function(x) !is.null(x) && {
    xml2::xml_ns_strip(x)
    length(xml2::xml_find_all(x, "/*/Machine/SpeciesGroupDefinition")) > 0L &&
      length(xml2::xml_find_all(x, "/*/Machine/ProductDefinition")) > 0L
  }
  if (pre$found && !has_defs(x)) {                 # definitions after a stem: read all
    whole <- path
    if (!is.null(src$entry)) {                       # a zip: extract that entry first
      dir <- tempfile("hpr_zip_")
      whole <- tryCatch(utils::unzip(path, files = src$entry, exdir = dir, junkpaths = TRUE)[1L],
                        error = function(e) NA_character_)
    }
    x <- if (is.na(whole)) NULL else parse(whole)
    how <- "whole"
    if (!is.null(src$entry)) unlink(dirname(whole), recursive = TRUE)
  }
  if (is.null(x)) return(fail(sprintf("%s is not a readable XML file.", filename)))
  xml2::xml_ns_strip(x)
  if (xml2::xml_name(x) != "HarvestedProduction")
    return(fail(sprintf("%s is not a StanForD 2010 .hpr or .pin file (root element '%s', expected HarvestedProduction or ProductInstruction).",
                        filename, xml2::xml_name(x))))
  att <- xml2::xml_attrs(x)
  mt <- att["messageType"]
  if (!is.na(mt) && mt != "hpr")
    return(fail(sprintf("%s is a StanForD '%s' message, not hpr.", filename, mt)))
  errors <- character(0); warnings <- src$warnings
  du <- unname(att["diameterUnit"]); lu <- unname(att["lengthUnit"])
  if (!is.na(du) && du != "mm") errors <- c(errors, sprintf("Diameter unit is '%s'; optBuck reads mm only (no conversion).", du))
  if (!is.na(lu) && lu != "cm") errors <- c(errors, sprintf("Length unit is '%s'; optBuck reads cm only (no conversion).", lu))
  if (is.na(du) || is.na(lu)) warnings <- c(warnings, "The file does not state its units; read as mm and cm.")
  machine <- xml2::xml_find_first(x, "./Machine")
  if (inherits(machine, "xml_missing")) errors <- c(errors, "The file has no Machine element.")
  if (length(errors) > 0L) return(list(ok = FALSE, errors = errors, warnings = warnings))

  # Species groups
  sgs <- xml2::xml_find_all(machine, "./SpeciesGroupDefinition")
  species <- data.frame(
    key        = vapply(sgs, .hx1, "", "./SpeciesGroupKey"),
    name       = vapply(sgs, .hx1, "", "./SpeciesGroupName"),
    user_id    = vapply(sgs, .hx1, "", "./SpeciesGroupUserID"),
    best_grade = vapply(sgs, function(s) {
      g <- suppressWarnings(as.numeric(.hall(s, "./Grades/Grade/GradeNumber")))
      if (any(!is.na(g))) min(g, na.rm = TRUE) else NA_real_ }, 0),
    start_grade = vapply(sgs, .hnum, 0, "./Grades/StartGrade"),
    dbh_height  = vapply(sgs, .hnum, 0, "./DBHHeight"),    # cm from the felling cut (hpr stems)
    # The group's BarkFunction (2026-11-11): category and, for the supported ones, double bark (mm) = a + b * d_ob.
    bark_category = vapply(sgs, function(s) {
      e <- xml2::xml_find_first(s, "./BarkFunction")
      if (inherits(e, "xml_missing")) NA_character_ else as.character(xml2::xml_attr(e, "barkFunctionCategory")) }, ""),
    bark_a = vapply(sgs, .hnum, 0, "./BarkFunction/SwedishZacco/ConstantA"),
    bark_b = vapply(sgs, .hnum, 0, "./BarkFunction/SwedishZacco/FactorB"),
    stringsAsFactors = FALSE)
  species <- hpr_bark_columns(species)
  if (nrow(species) == 0L) errors <- c(errors, "The file defines no species groups.")
  if (any(duplicated(species$key)))
    errors <- c(errors, sprintf("Species group key(s) defined twice: %s.",
                                paste(unique(species$key[duplicated(species$key)]), collapse = ", ")))

  # Products (classified only; 999999 "Unclassified" has no definition)
  pds <- xml2::xml_find_all(machine, "./ProductDefinition[ClassifiedProductDefinition]")
  products <- lapply(pds, .hpr_product)
  pkeys <- vapply(products, `[[`, "", "key")
  if (length(products) == 0L) errors <- c(errors, "The file defines no products with prices (ClassifiedProductDefinition).")
  if (any(duplicated(pkeys)))
    errors <- c(errors, sprintf("Product key(s) defined twice: %s.",
                                paste(unique(pkeys[duplicated(pkeys)]), collapse = ", ")))
  names(products) <- pkeys
  # Roadmap 8: the unclassified product(s) = waste logs. Found by the element, not by the number
  # (999999 in most files, 2147483647 in one of the owner's).
  unclassified_keys <- unique(.hall(machine, "./ProductDefinition[UnclassifiedProductDefinition]/ProductKey"))
  # Roadmap 8: the file's own grade names per species group (GradeNumber -> GradeName).
  grade_names <- do.call(rbind, c(list(data.frame(species_key = character(0), grade = numeric(0),
                                                  name = character(0), stringsAsFactors = FALSE)),
    lapply(sgs, function(s) {
      g <- xml2::xml_find_all(s, "./Grades/Grade")
      if (length(g) == 0L) return(NULL)
      data.frame(species_key = .hx1(s, "./SpeciesGroupKey"),
                 grade = suppressWarnings(as.numeric(vapply(g, .hx1, "", "./GradeNumber"))),
                 name = vapply(g, .hx1, "", "./GradeName"), stringsAsFactors = FALSE)
    })))

  if (nrow(species) > 0L)
    species$n_products <- vapply(species$key, function(k)
      sum(vapply(products, function(p) k %in% p$species_keys, logical(1))), integer(1))
  unknown <- unlist(lapply(products, function(p)
    if (length(p$species_keys) == 0L || !all(p$species_keys %in% species$key)) p$name))
  if (length(unknown) > 0L)
    warnings <- c(warnings, sprintf("Product(s) with a missing or unknown species group, not imported: %s.",
                                    paste(unknown, collapse = ", ")))

  list(ok = length(errors) == 0L, errors = errors, warnings = warnings, kind = "hpr",
       header = list(file = filename, version = unname(att["version"]),
                     read = how, mb_read = if (how == "prefix") round(length(pre$bytes) / 1048576, 2) else NA,
                    mb_file = round(file.size(path) / 1048576, 2)),
       species = species, products = products, unclassified_keys = unclassified_keys,
       grade_names = grade_names)
}


# ---- .pin files (Phase 5b) ---------------------------------------------------------------

# Read the products of a StanForD 2010 .pin file (ProductInstruction): the
# same ProductDefinition elements as an .hpr, without species group
# definitions or product keys. Nothing of the header is read. The result has
# the shape of read_hpr() plus `kind = "pin"`: the product key is its
# ProductUserID (else its ProductName; a repeated one gets _2, _3, ...), and
# the species table is made of the distinct SpeciesGroupUserIDs (key = name =
# user ID), so the species map and the import dialog work as for an .hpr.
# A missing PriceDefinition means the volume method of a new assortment
# ("Top cylinder"); missing optional elements are not reported.
read_pin <- function(path, filename = basename(path)) {
  fail <- function(msg) list(ok = FALSE, errors = msg, warnings = character(0))
  x <- tryCatch(xml2::read_xml(.hpr_read_clean(path), options = c("NOBLANKS", "HUGE")),
                error = function(e) NULL)
  if (is.null(x)) return(fail(sprintf("%s is not a readable XML file.", filename)))
  xml2::xml_ns_strip(x)
  if (xml2::xml_name(x) != "ProductInstruction")
    return(fail(sprintf("%s is not a StanForD 2010 .hpr or .pin file (root element '%s', expected HarvestedProduction or ProductInstruction).",
                        filename, xml2::xml_name(x))))
  att <- xml2::xml_attrs(x)
  mt <- att["messageType"]
  if (!is.na(mt) && mt != "pin")
    return(fail(sprintf("%s is a StanForD '%s' message, not pin.", filename, mt)))
  errors <- character(0); warnings <- character(0)
  du <- unname(att["diameterUnit"]); lu <- unname(att["lengthUnit"])
  if (!is.na(du) && du != "mm") errors <- c(errors, sprintf("Diameter unit is '%s'; optBuck reads mm only (no conversion).", du))
  if (!is.na(lu) && lu != "cm") errors <- c(errors, sprintf("Length unit is '%s'; optBuck reads cm only (no conversion).", lu))
  if (length(errors) > 0L) return(list(ok = FALSE, errors = errors, warnings = warnings))

  pds <- xml2::xml_find_all(x, "./ProductDefinition[ClassifiedProductDefinition]")
  products <- lapply(pds, function(p) {
    q <- .hpr_product(p)
    cp <- xml2::xml_find_first(p, "./ClassifiedProductDefinition")
    q$uid <- .hx1(p, "./ProductUserID")
    q$species_keys <- unique(.hall(cp, "./SpeciesGroupUserID"))
    q$species_keys <- q$species_keys[nzchar(q$species_keys)]
    if (inherits(xml2::xml_find_first(cp, "./PriceDefinition"), "xml_missing")) {
      q$volume_method <- PIN_DEFAULT_VOLUME_METHOD
      q$volume_adjustment <- NA_character_
    }
    q
  })
  if (length(products) == 0L) errors <- c(errors, "The file defines no products with prices (ClassifiedProductDefinition).")

  # Keys: ProductUserID, else ProductName (a repeated or missing user ID), else _2, _3, ...
  uid <- vapply(products, function(q) if (is.na(q$uid)) "" else q$uid, "")
  dup <- uid != "" & (duplicated(uid) | duplicated(uid, fromLast = TRUE))
  keys <- character(length(products))
  taken <- uid[uid != "" & !dup]
  for (i in seq_along(products)) {
    k <- if (uid[i] != "" && !dup[i]) uid[i] else {
      nm <- if (is.na(products[[i]]$name) || !nzchar(products[[i]]$name)) "product" else products[[i]]$name
      cand <- nm; n <- 1L
      while (cand %in% taken) { n <- n + 1L; cand <- paste0(nm, "_", n) }
      taken <- c(taken, cand)
      cand
    }
    keys[i] <- k
    products[[i]]$key <- k
  }
  names(products) <- keys

  ids <- as.character(unique(unlist(lapply(products, `[[`, "species_keys"))))
  species <- data.frame(key = ids, name = ids, user_id = ids,
                        best_grade = rep(NA_real_, length(ids)), start_grade = rep(NA_real_, length(ids)),
                        dbh_height = rep(NA_real_, length(ids)),
                        bark_category = rep(NA_character_, length(ids)), bark_a = rep(NA_real_, length(ids)),
                        bark_b = rep(NA_real_, length(ids)), stringsAsFactors = FALSE)
  if (nrow(species) > 0L)
    species$n_products <- vapply(species$key, function(k)
      sum(vapply(products, function(p) k %in% p$species_keys, logical(1))), integer(1))
  unknown <- unlist(lapply(products, function(p) if (length(p$species_keys) == 0L) p$name))
  if (length(unknown) > 0L)
    warnings <- c(warnings, sprintf("Product(s) with a missing or unknown species group, not imported: %s.",
                                    paste(unknown, collapse = ", ")))
  if (length(products) > 0L && nrow(species) == 0L)
    errors <- c(errors, "The file defines no species groups.")

  list(ok = length(errors) == 0L, errors = errors, warnings = warnings, kind = "pin",
       header = list(file = filename, version = unname(att["version"]), read = "whole",
                     mb_read = NA, mb_file = round(file.size(path) / 1048576, 2)),
       species = species, products = products)
}

# Read a product file by its root element: a .pin (ProductInstruction) with
# read_pin(), anything else with read_hpr() (which also takes .gz and .zip).
# Only the first bytes are looked at to decide, as .hpr files can be 150 MB.
read_product_file <- function(path, filename = basename(path)) {
  head <- tryCatch(readBin(path, "raw", 4096L), error = function(e) raw(0))
  txt <- rawToChar(head[head != as.raw(0L)])
  if (grepl("<ProductInstruction[[:space:]>]", txt, useBytes = TRUE)) read_pin(path, filename)
  else read_hpr(path, filename)
}


# ---- products and the grade filter ----------------------------------------------------

# Reference grades per species group (decided 2026-10-22): the grade(s)
# permitted by the most products of the group, each product counted once
# (ties keep all tied grades); if no product of the group lists permitted
# grades, its StartGrade, else its lowest GradeNumber. Grade names and
# product names are not used.
#
# @return Named list (species group key -> numeric grades; numeric(0) when
#   the group defines no grades).
hpr_reference_grades <- function(hpr) {
  sp <- hpr$species
  out <- lapply(seq_len(NROW(sp)), function(i) {
    g <- unlist(lapply(hpr$products, function(p)
      if (sp$key[i] %in% p$species_keys) unique(p$grades[!is.na(p$grades)])))
    if (length(g)) { n <- table(g); return(sort(as.numeric(names(n)[n == max(n)]))) }
    if (!is.na(sp$start_grade[i])) return(sp$start_grade[i])
    if (!is.na(sp$best_grade[i])) return(sp$best_grade[i])
    numeric(0)
  })
  setNames(out, sp$key)
}

# Whether a product permits a reference grade of any of its species groups
# (hpr_reference_grades()): TRUE / FALSE, NA when the product has no
# permitted grades or its groups no reference grade.
hpr_grade_ok <- function(p, ref) {
  if (is.null(p$grades) || length(p$grades) == 0L) return(NA)
  r <- unlist(ref[intersect(p$species_keys, names(ref))])
  if (length(r) == 0L) return(NA)
  any(p$grades %in% r)
}

# "130–360 mm (14 classes)", "305 cm (1 class)".
.hpr_range <- function(x, unit) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return("\u2013")
  n <- sprintf("(%d class%s)", length(x), if (length(x) == 1L) "" else "es")
  if (length(x) == 1L) sprintf("%g %s %s", x, unit, n)
  else sprintf("%g\u2013%g %s %s", min(x), max(x), unit, n)
}

# One row per product for the import dialog: its species (after the
# species map), forbidden cells, and whether it is imported by default
# (not if its species is not used or it has no priced cells), with the
# reason. Grades no longer untick products (2026-10-28): a product that
# permits none of its group's reference grades (hpr_reference_grades()) is
# kept, and the run leaves it out for stems without grade data
# (`ReferenceGrades`, .candidate_species_plan()).
#
# @param species_map Named character: species group key -> "spruce",
#   "pine" or "none".
hpr_product_table <- function(hpr, species_map) {
  if (length(hpr$products) == 0L) return(data.frame())
  ref <- hpr_reference_grades(hpr)
  rows <- lapply(hpr$products, function(p) {
    sp <- unique(unname(species_map[p$species_keys])); sp <- sp[!is.na(sp) & sp != "none"]
    grade <- hpr_grade_ok(p, ref)
    priced <- sum(!is.na(p$cells$price) & !p$cells$criteria %in% HPR_UNAVAILABLE)
    reason <- if (length(sp) == 0L) "Species group not used"
              else if (priced == 0L) "No priced cells"
              else ""
    data.frame(key = p$key, name = p$name,
               group = paste(hpr$species$name[match(p$species_keys, hpr$species$key)], collapse = ", "),
               species = if (length(sp)) paste(sp, collapse = ", ") else "none",
               type = .hpr_type(p$group, p$name),
               diameters = .hpr_range(p$dia_classes, "mm"),
               lengths = .hpr_range(p$len_classes, "cm"),
               forbidden = sum(p$cells$criteria %in% HPR_UNAVAILABLE),
               default = !nzchar(reason), reason = reason,
               grade_ok = grade, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}


# ---- one product -> assortment row and prices ------------------------------------------

# The price grid of one product in optBuck's layout: whole-cm diameter rows x
# whole-dm length columns. Returns list(pm (long), limits (kept class lower
# limits, cm), lengths (cm), errors, warnings (something lost), info (not
# shown: what follows the documented rules, e.g. rounding)).
.hpr_grid <- function(p, key, manual_ok = FALSE) {
  w <- character(0); info <- character(0)
  cells <- p$cells[!is.na(p$cells$d_mm) & !is.na(p$cells$l_cm), , drop = FALSE]
  forbidden <- cells$criteria %in% (if (manual_ok) HPR_FORBIDDEN else HPR_UNAVAILABLE)   # manual_ok: the bucking check lets the best bucking use manual-only cells
  n_forb <- sum(forbidden)
  ok_cells <- cells[!is.na(cells$price) & !forbidden, , drop = FALSE]
  if (manual_ok)   # a manual-only class no longer than the top-diameter position (or 10 cm) is no log: left out
    ok_cells <- ok_cells[!(ok_cells$criteria %in% HPR_MANUAL & ok_cells$l_cm <= max(c(10, p$top_pos), na.rm = TRUE)), , drop = FALSE]
  if (n_forb > 0L) info <- c(info, sprintf("%d forbidden cell(s) left empty (never chosen).", n_forb))
  if (nrow(ok_cells) == 0L) return(list(errors = "no priced cells"))

  # Lengths: each class lower limit rounds up to whole dm. The cut must stay
  # below the next class's limit (the last class: at most LengthClassMAX);
  # a class without such a whole-dm length is dropped.
  L <- sort(unique(ok_cells$l_cm))
  allL <- sort(unique(c(p$len_classes, cells$l_cm)))
  nxt <- vapply(L, function(l) { u <- allL[allL > l]; if (length(u)) min(u) else NA_real_ }, 0)
  cut <- ceiling(L / 10) * 10
  keep_l <- ifelse(!is.na(nxt), cut < nxt, is.na(p$len_class_max) | cut <= p$len_class_max)
  if (any(!keep_l))
    w <- c(w, sprintf("Length class(es) %s cm dropped: no whole-dm length inside the class.",
                      paste(L[!keep_l], collapse = ", ")))
  if (any(keep_l & L %% 10 != 0))
    info <- c(info, sprintf("Length classes rounded up to whole dm: %s.",
                      paste(sprintf("%g\u2192%g", L[keep_l & L %% 10 != 0], cut[keep_l & L %% 10 != 0]),
                            collapse = ", ")))
  L <- L[keep_l]; cut <- cut[keep_l]
  ok_cells <- ok_cells[ok_cells$l_cm %in% L, , drop = FALSE]
  if (nrow(ok_cells) == 0L) return(list(errors = "no length class with a whole-dm length"))

  # Diameter rows: whole cm from the first to the last class with a price;
  # row c takes the price of the largest class <= c * 10 mm among ALL the
  # file's classes (a forbidden class leaves its rows empty; the first priced
  # class for rows below it). Fully forbidden classes at either end narrow
  # the top-diameter limits instead (the run caps above the last row).
  D <- sort(unique(ok_cells$d_mm)); D_all <- sort(unique(cells$d_mm))
  if (any(D_all %% 10 != 0))
    info <- c(info, sprintf("Diameter class limits not in whole cm (%s mm): prices change at whole cm.",
                      paste(D_all[D_all %% 10 != 0], collapse = ", ")))
  rows_cm <- seq(floor(min(D) / 10), floor(max(D) / 10))
  cls <- vapply(rows_cm, function(cc) { u <- D_all[D_all <= cc * 10 & D_all >= min(D)]
                                        if (length(u)) max(u) else min(D) }, 0)
  above <- D_all[D_all > max(D)]
  max_top_mm <- if (length(above)) min(above) - 1 else NA_real_
  min_top_mm <- if (any(D_all < min(D))) min(D) else NA_real_
  if (length(above))
    info <- c(info, sprintf("Diameter class(es) from %g mm fully forbidden: max top diameter set to %g mm.",
                      min(above), max_top_mm))
  g <- expand.grid(i = seq_along(rows_cm), j = seq_along(L), KEEP.OUT.ATTRS = FALSE)
  price <- ok_cells$price[match(paste(cls[g$i], L[g$j]), paste(ok_cells$d_mm, ok_cells$l_cm))]
  long <- data.frame(ProductKey = key, LengthClass_dm = as.integer(cut[g$j] / 10),
                     PriceDiameterClass_cm = as.integer(rows_cm[g$i]),
                     PriceNOKperM3 = price, stringsAsFactors = FALSE)
  # Phase 7a: the cell's target Distribution (%) goes to every whole-cm row of its file
  # class, the same way as the price; the column exists only if the product has any.
  if (any(!is.na(ok_cells$dist)))
    long[[PM_DISTRIBUTION_COL]] <- ok_cells$dist[match(paste(cls[g$i], L[g$j]), paste(ok_cells$d_mm, ok_cells$l_cm))]
  # The lower limit (mm) of the file class the row was spread from, for price tuning.
  long[[PM_FILE_CLASS_COL]] <- cls[g$i]
  # Columns without any priced cell are dropped (that length is not cut);
  # a whole-cm row without a priced cell stays as an empty row (a fully
  # forbidden inner class: never chosen), as pm_drop_empty_cells() keeps it.
  priced_cols <- unique(long$LengthClass_dm[!is.na(long$PriceNOKperM3)])
  long <- pm_drop_empty_cells(long[long$LengthClass_dm %in% priced_cols, , drop = FALSE])
  empty <- setdiff(rows_cm, unique(long$PriceDiameterClass_cm[!is.na(long$PriceNOKperM3)]))
  if (length(empty) > 0L)
    info <- c(info, sprintf("Diameter class(es) %s cm fully forbidden: empty rows (never chosen).",
                      paste(empty, collapse = ", ")))
  kept <- (cut / 10) %in% priced_cols             # columns with a priced cell
  list(pm = long, limits = L[kept], lengths = cut[kept], min_top_mm = min_top_mm,
       max_top_mm = max_top_mm, warnings = w, info = info, errors = character(0))
}

# Turn a read_hpr() result into a scenario: the assortment table and prices
# of the chosen products (one row per product and species), plus a 0-priced
# Waste row per species, checked with check_price_set().
#
# @param hpr          read_hpr() result.
# @param species_map  Named character: species group key -> "spruce",
#   "pine" or "none".
# @param keep         Product keys to import (default: those
#   hpr_product_table() imports by default).
# @param base_defs    The base assortment table (its Waste rows are
#   added); default from load_products().
# @return list(ok, defs, pm, errors, warnings (shown: something lost or
#   replaced, e.g. a product skipped or a volume method replaced), info (not
#   shown: what follows the documented rules, e.g. rounding, forbidden cells)).
hpr_to_scenario <- function(hpr, species_map, keep = NULL, base_defs = NULL, manual_ok = FALSE) {
  fail <- function(msg, w = character(0)) list(ok = FALSE, errors = msg, warnings = w)
  if (!isTRUE(hpr$ok)) return(fail(hpr$errors))
  tab <- hpr_product_table(hpr, species_map)
  if (is.null(keep)) keep <- tab$key[tab$default]
  if (!any(species_map %in% PRODUCT_SPECIES))
    return(fail("Map at least one species group to Spruce or Pine."))
  w <- character(0); info <- character(0)

  # Several groups as one species: their products all go to that species.
  for (sp in PRODUCT_SPECIES) {
    g <- names(species_map)[species_map == sp]
    if (length(g) > 1L)
      info <- c(info, sprintf("Species groups %s are all %s: a %s stem can be bucked into any of their products.",
                        paste(hpr$species$name[match(g, hpr$species$key)], collapse = " and "), sp, sp))
  }
  base <- if (is.null(base_defs)) defs_from_products(load_products()) else base_defs   # Waste rows
  ref <- hpr_reference_grades(hpr)      # per group: stored with each product (ReferenceGrades)
  rows <- list(); pms <- list(); used_keys <- character(0); warned_methods <- character(0)
  for (k in keep) {
    p <- hpr$products[[k]]
    if (is.null(p)) next
    sps <- unique(unname(species_map[p$species_keys])); sps <- sps[!is.na(sps) & sps %in% PRODUCT_SPECIES]
    for (sp in sps) {
      name <- trimws(p$name)
      ak <- paste0(sp, "_", assortment_slug(name))
      if (ak %in% used_keys) {
        grp <- hpr$species$name[match(p$species_keys[species_map[p$species_keys] == sp][1L], hpr$species$key)]
        name <- sprintf("%s (%s)", name, grp); ak <- paste0(sp, "_", assortment_slug(name))
        if (ak %in% used_keys) { name <- sprintf("%s %s", name, p$key); ak <- paste0(sp, "_", assortment_slug(name)) }
        info <- c(info, sprintf("Renamed a second '%s' for %s to '%s'.", trimws(p$name), sp, name))
      }
      grid <- .hpr_grid(p, ak, manual_ok = manual_ok)
      if (length(grid$errors) > 0L) {
        w <- c(w, sprintf("%s not imported: %s.", p$name, grid$errors)); next
      }
      pw <- grid$warnings; pinfo <- grid$info
      method <- p$volume_method
      if (!is.na(method) && method %in% names(HPR_VOLUME_METHOD_MAP)) {
        method <- unname(HPR_VOLUME_METHOD_MAP[method])
      } else if (!identical(method, PIN_DEFAULT_VOLUME_METHOD)) {
        shown <- if (is.na(method)) "(none)" else method
        if (!shown %in% warned_methods) {                      # once per method, not per product
          w <- c(w, sprintf("Volume method '%s' is not available; the frustum (Top, 1 cm/m taper) is used.", shown))
          warned_methods <- c(warned_methods, shown)
        }
        method <- HPR_VOLUME_FALLBACK
      }
      len_rule <- or_default(p$volume_length, "Physical length cm")
      class_len <- identical(len_rule, "Length as defined in LengthClasses") &&
        method != "All diameters (solid volume)"
      if (!len_rule %in% c("Length as defined in LengthClasses", "Physical length cm",
                           "Rounded downwards to nearest dm-module"))
        pw <- c(pw, sprintf("Volume length '%s' is not available; the cut length is used.", len_rule))
      if (!identical(tolower(or_default(p$dia_under_bark, "true")), "true"))
        pw <- c(pw, "Diameters over bark in the file; optBuck reads all limits under bark.")
      if (!identical(tolower(or_default(p$volume_under_bark, "true")), "true"))
        pw <- c(pw, "Price volume over bark in the file; optBuck prices volume under bark.")
      if (!is.na(p$volume_top_pos) && !is.na(p$top_pos) && p$volume_top_pos != p$top_pos)
        pw <- c(pw, sprintf("Volume top position %g cm differs from the diameter top position %g cm; %g cm used.",
                            p$volume_top_pos, p$top_pos, p$top_pos))
      if (any(p$len_margins > 0, na.rm = TRUE))
        pw <- c(pw, "Length class margins ignored (logs are cut at the class length).")
      if (length(pw)) w <- c(w, paste0(name, ": ", pw))
      if (length(pinfo)) info <- c(info, paste0(name, ": ", pinfo))

      min_top <- if (!is.na(p$min_top)) p$min_top else if (length(p$dia_classes)) min(p$dia_classes) else 0
      min_top <- max(min_top, grid$min_top_mm, na.rm = TRUE)
      max_top <- min(if (is.na(p$dia_class_max)) Inf else p$dia_class_max, grid$max_top_mm, na.rm = TRUE)
      rows[[length(rows) + 1L]] <- data.frame(
        Name = name, Type = .hpr_type(p$group, p$name), Species = sp,
        MinTopDiameter_cm = min_top / 10,
        MaxTopDiameter_cm = max_top / 10,
        MinButtDiameter_cm = 0,
        MaxButtDiameter_cm = if (is.na(p$max_butt)) Inf else p$max_butt / 10,
        MinLength_cm = min(grid$lengths), MaxLength_cm = max(grid$lengths),
        DiameterTopPosition_cm = or_default(p$top_pos, 0),
        VolumeDiameterCategory = method, PriceType = "matrix",
        PriceLengths_cm = if (class_len && any(grid$limits %% 10 != 0))
          paste(grid$limits, collapse = ",") else NA_character_,
        # Phase 6: the product's permitted stem grades (NA = the file lists none: all grades)
        PermittedGrades = if (length(p$grades)) norm_permitted_grades(paste(p$grades, collapse = ";"))
                          else NA_character_,
        # The reference grade(s) of the product's own species group(s) of this species: the grade
        # of every cell of a stem without grade data (decided 2026-10-28).
        # (.pin files have no species group definitions: no reference grade, no .hpr key.)
        ReferenceGrades = {
          r <- if (identical(hpr$kind, "pin")) NULL
               else unlist(ref[intersect(p$species_keys[species_map[p$species_keys] == sp], names(ref))])
          if (length(r)) norm_permitted_grades(paste(r, collapse = ";")) else NA_character_
        },
        # The file's own diameter rounding for the price volume (2026-11-11); NA = the method's default.
        VolumeDiameterAdjustment = if (method == "All diameters (solid volume)" || is.na(p$volume_adjustment)) NA_character_
                                   else if (identical(p$volume_adjustment, VOLUME_ROUND_DOWN_CM)) VOLUME_ROUND_DOWN_CM
                                   else if (identical(p$volume_adjustment, "Measured diameter in mm")) "None" else NA_character_,
        HprProductKey = if (identical(hpr$kind, "pin")) NA_character_ else p$key,
        # Roadmap 8: where on the stem the product may be cut (NA = anywhere)
        ButtCondition = if (isTRUE(p$condition %in% BUTT_CONDITIONS)) p$condition else NA_character_,
        # Phase 7a: the product's LengthDistributionDefinition (NA = the file has none)
        DistributionAllowed = p$dist_allowed, DistributionCategory = p$dist_category,
        DistributionWithinProduct = p$dist_within, MAXDeviation = p$dist_maxdev,
        # R4 D6: the source product's own elements (ProductUserID, SpeciesGroupUserID,
        # ModificationDate, ...); the fingerprint of its prices and targets is set below.
        SourceCells = source_cells_build(p, unique(grid$pm[[PM_FILE_CLASS_COL]])),
        SourceXml = source_xml_build(p$src_cp, p$src_uid, {
          if (identical(hpr$kind, "pin")) p$src_sg else {
            gk <- p$species_keys[species_map[p$species_keys] == sp][1L]
            ug <- hpr$species$user_id[match(gk, hpr$species$key)]
            if (is.na(ug) || !nzchar(ug)) NA_character_
            else sprintf('<SpeciesGroupUserID agency="Other">%s</SpeciesGroupUserID>', .xesc(ug))
          }
        }),
        stringsAsFactors = FALSE)
      pms[[length(pms) + 1L]] <- grid$pm
      used_keys <- c(used_keys, ak)
    }
  }
  if (length(rows) == 0L) return(fail("No product could be imported.", w))
  defs <- .type_assortment_table(do.call(rbind, rows))
  # A 0-priced Waste row per species, as in the base table.
  for (sp in unique(defs$Species)) {
    wr <- base[base$Species == sp & base$Type == "waste", , drop = FALSE]
    if (nrow(wr) && !paste0(sp, "_", assortment_slug(wr$Name)) %in% used_keys) {
      defs <- rbind(defs, wr)
      pms[[length(pms) + 1L]] <- pm_start_prices(paste0(sp, "_", assortment_slug(wr$Name)), "flat", 0)
    }
  }
  rownames(defs) <- NULL
  pm <- pm_minimal(do.call(pm_bind, lapply(pms, pm_minimal)))
  v <- check_price_set(defs, pm)
  if (!v$ok) return(fail(v$errors, w))
  # The fingerprint of each imported product's prices and targets (R4 D6).
  fpm <- pm_minimal(v$pm)
  defs$SourceFingerprint <- ifelse(is.na(defs$SourceXml), NA_character_,
    vapply(assortment_table_keys(defs), function(k) product_fingerprint(fpm, k), "", USE.NAMES = FALSE))
  list(ok = TRUE, defs = defs, pm = v$pm, errors = character(0),
       warnings = w, info = c(info, v$warnings))   # validation notes: info
}
