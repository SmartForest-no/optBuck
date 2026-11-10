# ==============================================================================
# R/helpers_hpr_export.R
#
# Purpose : Export a simulated run of .hpr stems as a StanForD 2010 .hpr file,
#           one file per scenario (roadmap Phase 5a, 2026-10-28).
#
# What is written (decided 2026-10-28):
#   - Everything before the first stem (header, Machine, ObjectDefinition,
#     SpeciesGroupDefinitions, ...) is copied from the first source file,
#     except the coordinates, and except the classified ProductDefinitions,
#     which are the scenario's own (hpr_export_products()).
#   - Each stem that was bucked is copied from its source file unchanged
#     (StemGrade, DBH, StemDiameters, ...), minus StemCoordinates, with its
#     recorded Log elements replaced by the simulated logs.
#   - Stems the run dropped or cleaned out are left out. Keys stay as written.
#   - Streamed: the source is read in chunks of complete stem records (as
#     read_hpr_stems() does), so memory stays low on large files.
#   - Coordinates never reach the output: the writer removes them, and a check
#     on the written file deletes it if any coordinate element is left.
#   Product definitions keep the metadata (buyer, organisation, ...) of the
#   original product with the same ProductKey (the assortment's hidden
#   HprProductKey); other products get new keys above the existing ones.
# ==============================================================================


# Elements removed from everything copied from a source file.
HPR_EXPORT_COORD_TAGS <- c("StemCoordinates", "DefaultCoordinate",
                           "GeographicalRestriction", "RestrictionPoint")

# Any of these left in the written file makes the export fail.
HPR_EXPORT_COORD_CHECK <- paste0("<(?:Latitude|Longitude|Altitude|StemCoordinates|",
                                 "DefaultCoordinate|RestrictionPoint|GeographicalRestriction)[\\s>/]")

# ProductKey of StanForD's unclassified product (never a new key's base).
HPR_UNCLASSIFIED_KEY <- 999999

# CuttingReason of the simulated logs.
HPR_EXPORT_CUTTING_REASON <- "Automatic"

# Metadata copied from the original ClassifiedProductDefinition of a product
# with the same key, in file order (before the generated definitions).
HPR_EXPORT_META_BEFORE <- c("ProductInfo", "ProductVersion", "ProductBuyer", "ProductClass",
                            "LoggingOrganisation", "ProductDestination", "ProductGroupName",
                            "StemTypeCode", "ProductPresentationOrder", "ProductDescription")


# ---- small helpers --------------------------------------------------------------------

.xesc <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# Number as text: at most `digits` decimals, no trailing zeros.
.fnum <- function(x, digits = 5L) {
  s <- formatC(round(as.numeric(x), digits), format = "f", digits = digits)
  s <- sub("\\.?0+$", "", s)
  s[s %in% c("", "-0")] <- "0"
  s
}

.hx_bytes <- function(x) { x <- enc2utf8(x); Encoding(x) <- "bytes"; x }

# First element `tag` of `x` (with its content), or NA.
.hx_elem <- function(x, tag) {
  m <- regexpr(sprintf("(?s)<%1$s(?:\\s[^>]*)?(?:/>|>.*?</%1$s>)", tag), x, perl = TRUE, useBytes = TRUE)
  if (m[1L] == -1L) NA_character_ else regmatches(x, m)
}

# `x` without the elements `tags` (all occurrences).
.hx_strip <- function(x, tags) {
  for (t in tags)
    x <- gsub(sprintf("(?s)<%1$s(?:\\s[^>]*)?(?:/>|>.*?</%1$s>)", t), "", x, perl = TRUE, useBytes = TRUE)
  x
}

# Text of the first <tag>...</tag> of `x`.
.hx_text <- function(x, tag) {
  m <- regexec(sprintf("(?s)<%1$s(?:\\s[^>]*)?>(.*?)</%1$s>", tag), x, perl = TRUE, useBytes = TRUE)
  v <- regmatches(x, m)[[1L]]
  if (length(v) >= 2L) trimws(v[2L]) else NA_character_
}


# ---- product definitions ----------------------------------------------------------------

# The LengthDistributionDefinition of an assortment (Phase 7a): its stored
# DistributionAllowed, DistributionCategory, MAXDeviation and
# DistributionWithinProduct, in the element order of the source files; NULL
# when none is stored (then nothing is written, as before).
.hpr_export_length_distribution <- function(d) {
  g <- function(n) if (n %in% names(d)) d[[n]][1L] else NA
  al <- g("DistributionAllowed"); ct <- g("DistributionCategory")
  dv <- g("MAXDeviation"); wp <- g("DistributionWithinProduct")
  if (all(is.na(c(al, ct, dv, wp)))) return(NULL)
  tf <- function(x) if (isTRUE(x)) "true" else "false"
  paste0('<LengthDistributionDefinition modificationRestricted="false">',
         if (!is.na(al)) paste0("<DistributionAllowed>", tf(al), "</DistributionAllowed>"),
         if (!is.na(ct)) paste0("<DistributionCategory>", .xesc(ct), "</DistributionCategory>"),
         if (!is.na(dv)) paste0("<MAXDeviation>", .fnum(as.numeric(dv), 4L), "</MAXDeviation>"),
         if (!is.na(wp)) paste0("<DistributionWithinProduct>", tf(wp), "</DistributionWithinProduct>"),
         "</LengthDistributionDefinition>")
}

# ---- what an imported product keeps (R4 D6) ---------------------------------------------

# ASCII text for an ID: Norwegian letters transliterated (ae, o, aa), every
# other character outside [A-Za-z0-9_-] a "_" (runs squeezed, ends trimmed).
ascii_id <- function(x) {
  x <- as.character(x)
  x <- gsub("\u00e6", "ae", x, fixed = TRUE); x <- gsub("\u00c6", "Ae", x, fixed = TRUE)
  x <- gsub("\u00f8", "o", x, fixed = TRUE);  x <- gsub("\u00d8", "O", x, fixed = TRUE)
  x <- gsub("\u00e5", "aa", x, fixed = TRUE); x <- gsub("\u00c5", "Aa", x, fixed = TRUE)
  x <- iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = "_")
  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  gsub("^_+|_+$", "", x)
}

# Made-unique IDs: the first of a repeated ID keeps it, the others get _2,
# _3, ... (never an ID that exists in the list).
unique_ids <- function(id) {
  id <- as.character(id); out <- id
  taken <- id[!duplicated(id)]
  for (i in which(duplicated(id))) {
    n <- 2L; cand <- paste0(id[i], "_", n)
    while (cand %in% taken) { n <- n + 1L; cand <- paste0(id[i], "_", n) }
    out[i] <- cand; taken <- c(taken, cand)
  }
  out
}

# Hash of an assortment's imported prices and targets: its matrix cells
# (diameter class, length class, price, Distribution) in a fixed order. A
# product whose current fingerprint differs from the stored one has been
# edited, and gets a new ModificationDate on export.
# @param pm  A price matrix (pm_minimal()).
# @param key The assortment's ProductKey.
product_fingerprint <- function(pm, key) {
  r <- pm[as.character(pm$ProductKey) == key, , drop = FALSE]
  if (nrow(r) == 0L) return(NA_character_)
  r <- r[order(r$PriceDiameterClass_cm, r$LengthClass_dm), , drop = FALSE]
  d <- if (PM_DISTRIBUTION_COL %in% names(r)) r[[PM_DISTRIBUTION_COL]] else rep(NA_real_, nrow(r))
  num <- function(x) ifelse(is.na(x), "NA", formatC(as.numeric(x), format = "f", digits = 4))
  s <- paste(sprintf("%s|%s|%s|%s", r$PriceDiameterClass_cm, r$LengthClass_dm, num(r$PriceNOKperM3), num(d)),
             collapse = ";")
  f <- tempfile(); on.exit(unlink(f))
  writeBin(charToRaw(s), f)
  unname(tools::md5sum(f))
}

# The elements of a source product kept as they are: one <src> text with the
# children md (ModificationDate), pre (the elements between ProductName and
# DiameterDefinition), mid (ManualFreeBuck .. CuttingWindowDefinition), grades
# (PermittedGradesDefinition), limit (LimitationDefinition), post
# (LongLogButtHeight), uid (the ProductUserID element) and sguid (the
# SpeciesGroupUserID element). `cp` is the ClassifiedProductDefinition as text.
source_xml_build <- function(cp, uid = NA_character_, sguid = NA_character_) {
  cp <- gsub(">[[:space:]]+<", "><", as.character(cp))
  pre_txt <- sub("(?s)^.*?</ProductName>(.*?)<DiameterDefinition(?:\\s[^>]*)?>.*$", "\\1", cp, perl = TRUE)
  if (identical(pre_txt, cp)) pre_txt <- ""
  md <- .hx_text(pre_txt, "ModificationDate")
  pre <- .hx_strip(pre_txt, "ModificationDate")
  mid <- paste(stats::na.omit(vapply(c("ManualFreeBuck", "TopLogFreeBuck", "ProductCondition", "CuttingWindowDefinition"),
                                     function(t) .hx_elem(cp, t), "")), collapse = "")
  one <- function(tag, v) if (is.na(v) || !nzchar(v)) "" else sprintf("<%1$s>%2$s</%1$s>", tag, v)
  if (is.na(uid)) uid <- .hx_elem(cp, "ProductUserID")
  if (is.na(sguid)) sguid <- .hx_elem(cp, "SpeciesGroupUserID")
  out <- paste0("<src>", one("md", md), one("pre", pre), one("mid", mid),
                one("grades", .hx_elem(cp, "PermittedGradesDefinition")),
                one("limit", .hx_elem(cp, "LimitationDefinition")),
                one("post", .hx_elem(cp, "LongLogButtHeight")),
                one("uid", gsub(">[[:space:]]+<", "><", uid)), one("sguid", gsub(">[[:space:]]+<", "><", sguid)), "</src>")
  Encoding(out) <- "UTF-8"
  out
}

# One part of source_xml_build(): "" when absent or the source is NA.
source_xml_part <- function(src, tag) {
  if (is.na(src)) return("")
  v <- .hx_text(src, tag)
  if (is.na(v)) return("")
  Encoding(v) <- "UTF-8"; v
}

# TRUE for a valid xs:dateTime ("2026-11-03T10:15:00", optional fraction and zone).
.valid_datetime <- function(x) {
  x <- as.character(x)
  !is.na(x) & grepl("^-?[0-9]{4,}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?(Z|[+-][0-9]{2}:[0-9]{2})?$", trimws(x))
}

# Every CreationDate / ModificationDate element of `txt` whose text is not a valid
# xs:dateTime (anonymised files hold e.g. "vasket") gets `now`.
.fix_datetimes <- function(txt, now) {
  re <- "<(CreationDate|ModificationDate)>([^<]*)</\\1>"
  m <- gregexpr(re, txt, perl = TRUE, useBytes = TRUE)
  if (m[[1L]][1L] < 0L) return(txt)
  hit <- regmatches(txt, m)[[1L]]
  inner <- sub(re, "\\2", hit, perl = TRUE, useBytes = TRUE)
  bad <- !.valid_datetime(inner)
  if (!any(bad)) return(txt)
  tag <- sub(re, "\\1", hit, perl = TRUE, useBytes = TRUE)
  hit[bad] <- sprintf("<%1$s>%2$s</%1$s>", tag[bad], now)
  regmatches(txt, m) <- list(hit)
  txt
}

# Neutral values for what a product created in optBuck has no source for (R4 D6).
EXPORT_NEUTRAL_MID <- paste0("<ManualFreeBuck>false</ManualFreeBuck><TopLogFreeBuck>false</TopLogFreeBuck>",
                             "<ProductCondition>Buck always</ProductCondition>")
EXPORT_NEUTRAL_LIMIT <- paste0('<LimitationDefinition modificationRestricted="false"><LimitationCategory>',
                               "No limitation</LimitationCategory><LimitationResult>No action</LimitationResult>",
                               "</LimitationDefinition>")
EXPORT_NEUTRAL_LENGTH_DIST <- paste0('<LengthDistributionDefinition modificationRestricted="false">',
  "<DistributionAllowed>false</DistributionAllowed><DistributionCategory>Number of logs</DistributionCategory>",
  "<MAXDeviation>0</MAXDeviation><DistributionWithinProduct>true</DistributionWithinProduct>",
  "</LengthDistributionDefinition>")
EXPORT_MAX_TOP_NONE_MM <- 2000     # DiameterClassMAX is required: no max top diameter becomes 200 cm

# Whole numbers per row that keep a row of shares at 100 (largest remainder);
# a row that does not add up to 100 is rounded as it is.
.round_shares <- function(x) {
  out <- round(x)
  ok <- !is.na(x)
  if (any(ok) && abs(sum(x[ok]) - 100) < 1e-6) {
    f <- floor(x[ok]); rem <- round(100 - sum(f))
    if (rem > 0) { up <- order(-(x[ok] - f), seq_along(f))[seq_len(rem)]; f[up] <- f[up] + 1 }
    out[ok] <- f
  }
  out
}

# The ProductDefinition elements of a scenario.
#
# @param defs   Assortment table (ASSORTMENT_TABLE_COLS, with HprProductKey).
# @param pm     The scenario's price matrix (pm_minimal()).
# @param orig   Named list of the source file's classified ProductDefinition
#   texts (names = ProductKey): the fallback for an assortment that has no
#   stored SourceXml.
# @param groups data.frame(key, species): the header's species groups.
# @param used_keys All ProductKeys of the source file (numeric-looking ones
#   set the start of the new keys).
# @param style  "hpr" (default) or "pin" (Phase 5b): a .pin ProductDefinition
#   has the ProductUserID first and a SpeciesGroupUserID last, no ProductKey
#   and no SpeciesGroupKey.
# @param now    The export time (ModificationDate of edited and new products).
# @param ids    Optional ProductUserIDs, one per row of `defs`, instead of the source's
#   or the name's (made unique).
# @details An imported product keeps its own ModificationDate, ProductUserID,
#   SpeciesGroupUserID, metadata, ManualFreeBuck, TopLogFreeBuck,
#   ProductCondition, CuttingWindowDefinition, PermittedGradesDefinition and
#   LimitationDefinition (hidden SourceXml); its ModificationDate is the
#   export time when its prices or targets differ from the imported ones
#   (SourceFingerprint). Products without a source get neutral values
#   (EXPORT_NEUTRAL_*), a ProductUserID made from the name (ASCII) and the
#   SpeciesGroupUserID of an imported product of the same species, else
#   "Spruce" / "Pine". ProductUserIDs are unique in the file (_2, _3, ...).
#   Prices and shares are whole numbers (the schema's type).
# @return list(xml (one text per assortment), keys (named by
#   assortment_table_keys(defs): the exported ProductKey), ids (the exported
#   ProductUserIDs)).
hpr_export_products <- function(defs, pm, orig, groups, used_keys = names(orig),
                                style = c("hpr", "pin"), now = .pin_time(), ids = NULL) {
  style <- match.arg(style)
  pin <- identical(style, "pin")
  akeys <- assortment_table_keys(defs)
  lens  <- assortment_lengths(defs, pm)
  hk <- if ("HprProductKey" %in% names(defs)) as.character(defs$HprProductKey) else rep(NA_character_, nrow(defs))
  hk[!is.na(hk) & !hk %in% names(orig)] <- NA_character_      # not a product of this file
  if (pin) hk[] <- NA_character_                              # a .pin copies nothing from a source file
  hk[duplicated(hk) & !is.na(hk)] <- NA_character_            # the first row keeps the key
  uk <- suppressWarnings(as.numeric(used_keys))
  top <- max(c(0, uk[!uk %in% HPR_UNCLASSIFIED_KEY]), na.rm = TRUE)    # 999999 is the unclassified product
  newk <- seq_len(sum(is.na(hk))) + top
  keys <- hk; keys[is.na(hk)] <- format(newk, scientific = FALSE, trim = TRUE)
  pm <- pm_minimal(pm)            # keeps the hidden Distribution and file class columns
  orig <- lapply(orig, function(s) { Encoding(s) <- "UTF-8"; s })   # files are UTF-8 (the caller checks)

  # What each assortment keeps of its source: stored SourceXml, else (an .hpr
  # export of an assortment of this file) the file's own product text.
  has_src <- "SourceXml" %in% names(defs)
  src <- vapply(seq_len(nrow(defs)), function(i) {
    s <- if (has_src) defs$SourceXml[i] else NA_character_
    if (is.na(s) && !is.na(hk[i])) s <- source_xml_build(orig[[hk[i]]])
    s
  }, "")
  src[is.na(src)] <- NA_character_
  fp_stored <- if ("SourceFingerprint" %in% names(defs)) as.character(defs$SourceFingerprint) else rep(NA_character_, nrow(defs))
  fp_now <- vapply(akeys, function(k) product_fingerprint(pm, k), "")
  has_md <- vapply(src, function(s) nzchar(source_xml_part(s, "md")), NA, USE.NAMES = FALSE)
  unchanged <- !is.na(src) & has_md & !is.na(fp_stored) & fp_stored == fp_now

  # ProductUserID (unique in the file) and SpeciesGroupUserID.
  uid_el <- vapply(src, function(s) source_xml_part(s, "uid"), "", USE.NAMES = FALSE)
  uid_txt <- vapply(uid_el, function(e) if (nzchar(e)) .hx_text(e, "ProductUserID") else NA_character_, "", USE.NAMES = FALSE)
  uid_agency <- vapply(uid_el, function(e) {
    m <- regmatches(e, regexec('agency="([^"]*)"', e, perl = TRUE))[[1L]]
    if (length(m) > 1L) m[2L] else "Other"
  }, "", USE.NAMES = FALSE)
  nm_id <- ascii_id(defs$Name); nm_id[!nzchar(nm_id)] <- "product"
  ids <- unique_ids(ifelse(is.na(uid_txt) | !nzchar(uid_txt), nm_id, vapply(uid_txt, function(x) { Encoding(x) <- "UTF-8"; x }, "")))
  if (!is.null(ids)) ids <- unique_ids(ids)
  sg_el <- vapply(src, function(s) source_xml_part(s, "sguid"), "", USE.NAMES = FALSE)
  sg_by_species <- function(sp) { i <- which(nzchar(sg_el) & as.character(defs$Species) == sp); if (length(i)) sg_el[i[1L]] else NA_character_ }

  xml <- vapply(seq_len(nrow(defs)), function(i) {
    d <- defs[i, , drop = FALSE]
    s <- src[i]
    part <- function(tag) source_xml_part(s, tag)
    meta <- if (nzchar(part("pre"))) part("pre") else {
      grp <- switch(as.character(d$Type), sawlog = "T\u00f8mmer", pulpwood = "Massevirke", as.character(d$Type))
      sprintf("<ProductGroupName>%s</ProductGroupName>", .xesc(grp))
    }
    md <- if (unchanged[i] && .valid_datetime(part("md"))) part("md") else now
    rows <- pm[pm$ProductKey == akeys[i], , drop = FALSE]
    rows_dist <- if (PM_DISTRIBUTION_COL %in% names(rows)) rows[[PM_DISTRIBUTION_COL]] else rep(NA_real_, nrow(rows))
    matrix_priced <- identical(as.character(d$PriceType), "matrix")
    len_cm <- as.numeric(lens[[akeys[i]]])
    min_top <- as.numeric(d$MinTopDiameter_cm) * 10; max_top <- as.numeric(d$MaxTopDiameter_cm) * 10
    if (matrix_priced) {
      # The file's own diameter classes where the rows have one (FileDiameterClass_mm),
      # else one class per whole cm.
      fc <- if (PM_FILE_CLASS_COL %in% names(rows)) rows[[PM_FILE_CLASS_COL]] else rep(NA_real_, nrow(rows))
      cls_mm <- ifelse(is.na(fc), rows$PriceDiameterClass_cm * 10, fc)
      dcl <- sort(unique(cls_mm[!is.na(cls_mm)]))
      lim <- price_lengths(d$PriceLengths_cm, len_cm)           # class lower limits (cm)
      cells <- expand.grid(dc = dcl, li = seq_along(len_cm), KEEP.OUT.ATTRS = FALSE)
      hit <- match(paste(cells$dc, len_cm[cells$li] / 10), paste(cls_mm, rows$LengthClass_dm))
      pr <- rows$PriceNOKperM3[hit]
      dist <- rows_dist[hit]        # the first row of a class: the class's stored value
      for (dc in dcl) { j <- cells$dc == dc; dist[j] <- .round_shares(dist[j]) }
      # Source classes without a whole-cm row (SourceCells) are written as they were in the file.
      sc <- source_cells_parse(if ("SourceCells" %in% names(d)) d$SourceCells else NA_character_)
      un_cl <- setdiff(unique(sc$d_mm), cls_mm)
      lm_v <- rep(0, nrow(cells)); cr_v <- ifelse(is.na(pr), HPR_FORBIDDEN, "No limit")
      if (length(un_cl)) {
        ex <- expand.grid(dc = un_cl, li = seq_along(len_cm), KEEP.OUT.ATTRS = FALSE)
        # a source length class is the assortment's cut length rounded up to whole dm (as in the import)
        m <- match(paste(ex$dc, len_cm[ex$li]), paste(sc$d_mm, ceiling(sc$l_cm / 10) * 10))
        e_pr <- sc$price[m]; e_cr <- sc$criteria[m]
        e_cr[is.na(e_cr) | !nzchar(e_cr)] <- ifelse(is.na(e_pr[is.na(e_cr) | !nzchar(e_cr)]), HPR_FORBIDDEN, "No limit")
        cells <- rbind(cells, ex); pr <- c(pr, e_pr); dist <- c(dist, sc$dist[m])
        lm_v <- c(lm_v, ifelse(is.na(sc$limit[m]), 0, sc$limit[m])); cr_v <- c(cr_v, e_cr)
        dcl <- sort(unique(c(dcl, un_cl)))
        o <- order(cells$li, cells$dc)             # the order of the matrix items: length, then diameter
        cells <- cells[o, , drop = FALSE]; pr <- pr[o]; dist <- dist[o]; lm_v <- lm_v[o]; cr_v <- cr_v[o]
      }
    } else {
      dcl <- max(1, floor(min_top))
      lim <- len_cm
      cells <- expand.grid(dc = dcl, li = seq_along(len_cm), KEEP.OUT.ATTRS = FALSE)
      pr <- rep(rows$PriceNOKperM3[is.na(rows$LengthClass_dm)][1L], nrow(cells))
      dist <- rep(NA_real_, nrow(cells))
      lm_v <- rep(0, nrow(cells)); cr_v <- ifelse(is.na(pr), HPR_FORBIDDEN, "No limit")
    }
    dcl <- pmax(1, dcl)
    top_pos <- or_default(as.numeric(d$DiameterTopPosition_cm), 0)
    method <- as.character(d$VolumeDiameterCategory)
    if (identical(method, "Top cylinder")) method <- "Top"       # StanForD has no cylinder rule
    round_down <- !identical(volume_rounding(d$VolumeDiameterCategory), "None")
    int <- function(x) format(round(as.numeric(x)), scientific = FALSE, trim = TRUE)
    items <- sprintf(
      paste0('<ProductMatrixItem diameterClassLowerLimit="%s" lengthClassLowerLimit="%s"><Price>%s</Price>',
             '<Distribution>%s</Distribution><Limitation>%s</Limitation><BuckingCriteria>%s</BuckingCriteria>',
             '</ProductMatrixItem>'),
      int(pmax(1, cells$dc)), int(lim[cells$li]), ifelse(is.na(pr), "0", int(pr)),
      ifelse(is.na(dist), "0", int(dist)),     # stored target distribution (Phase 7a), 0 when none
      int(lm_v), .xesc(cr_v))
    gk <- groups$key[groups$species == as.character(d$Species)]
    grades <- if (nzchar(part("grades"))) part("grades") else {
      g <- if ("PermittedGrades" %in% names(d)) permitted_grades_set(d$PermittedGrades) else numeric(0)
      paste0('<PermittedGradesDefinition modificationRestricted="false">',
             if (length(g)) paste0("<PermittedGradeNumber>", format(g, scientific = FALSE, trim = TRUE), "</PermittedGradeNumber>", collapse = ""),
             "</PermittedGradesDefinition>")
    }
    uid <- sprintf('<ProductUserID agency="%s">%s</ProductUserID>', uid_agency[i], .xesc(ids[i]))
    sg <- if (nzchar(sg_el[i])) sg_el[i] else {
      o <- sg_by_species(as.character(d$Species))
      if (!is.na(o)) o else paste0('<SpeciesGroupUserID agency="Other">',
                                   if (identical(as.character(d$Species), "pine")) "Pine" else "Spruce", "</SpeciesGroupUserID>")
    }
    paste0(
      "<ProductDefinition>",
      if (pin) paste0(uid, "<ClassifiedProductDefinition>") else paste0("<ProductKey>", keys[i], "</ProductKey><ClassifiedProductDefinition>"),
      "<ProductName>", .xesc(d$Name), "</ProductName>",
      "<ModificationDate>", md, "</ModificationDate>", meta,
      '<DiameterDefinition modificationRestricted="false"><DiameterClasses diameterClassCategory="Top">',
      paste0("<DiameterClass><DiameterClassLowerLimit>", int(dcl),
             "</DiameterClassLowerLimit><DiameterClassName/></DiameterClass>", collapse = ""),
      "<DiameterClassMAX>", int(if (is.finite(max_top)) max_top else EXPORT_MAX_TOP_NONE_MM), "</DiameterClassMAX>",
      "<DiameterClassAdjustment>DiameterClassAdjustment1</DiameterClassAdjustment>",
      "<DiameterUnderBark>true</DiameterUnderBark></DiameterClasses>",
      "<DiameterMINTop>", int(min_top), "</DiameterMINTop>",
      if (is.finite(as.numeric(d$MaxButtDiameter_cm)))
        paste0("<DiameterMAXButt>", int(as.numeric(d$MaxButtDiameter_cm) * 10), "</DiameterMAXButt>"),
      "<DiameterTopPosition>", int(top_pos), "</DiameterTopPosition></DiameterDefinition>",
      '<LengthDefinition modificationRestricted="false"><LengthClassAdjustment>LengthClassAdjustment1</LengthClassAdjustment>',
      paste0("<LengthClass><LengthClassLowerLimit>", int(lim),
             "</LengthClassLowerLimit><LengthClassMargin>0</LengthClassMargin></LengthClass>", collapse = ""),
      "<LengthClassMAX>", int(max(len_cm)), "</LengthClassMAX></LengthDefinition>",
      '<PriceDefinition modificationRestricted="false"><VolumeDiameterAdjustment>',
      if (round_down) VOLUME_ROUND_DOWN_CM else "Measured diameter in mm", "</VolumeDiameterAdjustment>",
      sprintf('<VolumeDiameterCategory volumeDiameterTopPosition="%s">%s</VolumeDiameterCategory>',
              int(top_pos), .xesc(method)),
      "<VolumeLengthCategory>",
      if (matrix_priced && !identical(method, "All diameters (solid volume)"))
        "Length as defined in LengthClasses" else "Physical length cm",
      "</VolumeLengthCategory><VolumeUnderBark>true</VolumeUnderBark></PriceDefinition>",
      if (nzchar(part("mid"))) part("mid") else EXPORT_NEUTRAL_MID,
      grades,
      # LengthDistributionDefinition (Phase 7a): the stored fields, else a neutral one (the element is required).
      or_default(.hpr_export_length_distribution(d), EXPORT_NEUTRAL_LENGTH_DIST),
      if (nzchar(part("limit"))) part("limit") else EXPORT_NEUTRAL_LIMIT,
      '<ProductMatrixes modificationRestrictedPrice="false" modificationRestrictedDistribution="false" ',
      'modificationRestrictedLimitation="false" modificationRestrictedBuckingCriteria="false" ',
      'modificationRestrictedLogColorMarking="false">', paste(items, collapse = ""), "</ProductMatrixes>",
      part("post"),
      if (pin) sg else paste0(uid, if (length(gk)) paste0("<SpeciesGroupKey>", gk, "</SpeciesGroupKey>", collapse = "")),
      "</ClassifiedProductDefinition></ProductDefinition>")
  }, "")
  list(xml = xml, keys = setNames(keys, akeys), ids = ids)
}


# ---- logs -----------------------------------------------------------------------------

# The Log elements of one stem. `L`: the stem's selected logs (any order);
# `pv`: its profile vector with DiameterValueUB; `pkey`: assortment key ->
# exported ProductKey. A file lists logs one after the other, so every gap
# before or between the logs (from position 0) is written as a log of the
# unclassified product (HPR_UNCLASSIFIED_KEY): positions stay consecutive.
# Returns list(xml, n_gap_logs).
.hpr_export_logs <- function(L, pv, pkey) {
  L <- L[order(L$StartPos), , drop = FALSE]
  pos <- pv$diameterPosition
  at <- function(x, d) round(stats::approx(pos, d, xout = x, rule = 2)$y)
  vol <- function(d, a, b) tryCatch(section_volume_fun(pos, d)(a, b), error = function(e) rep(NA_real_, length(a)))
  prev_end <- c(0, L$StopPos[-nrow(L)])
  gap <- L$StartPos > prev_end
  seg <- data.frame(a = c(L$StartPos, prev_end[gap]), b = c(L$StopPos, L$StartPos[gap]),
                    pk = c(unname(pkey[as.character(L$ProductKey)]), rep(as.character(HPR_UNCLASSIFIED_KEY), sum(gap))),
                    pv = c(L$price_volume_m3, rep(0, sum(gap))), stringsAsFactors = FALSE)
  seg <- seg[order(seg$a), , drop = FALSE]
  a <- seg$a; b <- seg$b
  mid <- (a + b) / 2
  v_ob <- vol(pv$DiameterValue, a, b); v_ub <- vol(pv$DiameterValueUB, a, b)
  num <- function(x) ifelse(is.na(x), "0", .fnum(x, 5L))
  xml <- sprintf(
    paste0('<Log><LogKey>%d</LogKey><ProductKey>%s</ProductKey>',
           '<LogVolume logVolumeCategory="m3 (price)" logMeasurementCategory="Machine">%s</LogVolume>',
           '<LogVolume logVolumeCategory="m3sob" logMeasurementCategory="Machine">%s</LogVolume>',
           '<LogVolume logVolumeCategory="m3sub" logMeasurementCategory="Machine">%s</LogVolume>',
           '<CuttingCategory><CuttingReason>%s</CuttingReason></CuttingCategory>',
           '<LogMeasurement logMeasurementCategory="Machine">',
           '<LogDiameter logDiameterCategory="Mid ob">%d</LogDiameter>',
           '<LogDiameter logDiameterCategory="Mid ub">%d</LogDiameter>',
           '<LogDiameter logDiameterCategory="Top ob">%d</LogDiameter>',
           '<LogDiameter logDiameterCategory="Top ub">%d</LogDiameter>',
           '<LogLength>%s</LogLength></LogMeasurement></Log>'),
    seq_along(a), seg$pk,
    num(seg$pv), num(v_ob), num(v_ub), HPR_EXPORT_CUTTING_REASON,
    as.integer(at(mid, pv$DiameterValue)), as.integer(at(mid, pv$DiameterValueUB)),
    as.integer(at(b, pv$DiameterValue)), as.integer(at(b, pv$DiameterValueUB)),
    .fnum(b - a, 2L))
  list(xml = paste(xml, collapse = ""), n_gap_logs = sum(gap))
}

# ---- the export -----------------------------------------------------------------------

# Species group definitions of a file's prefix text: data.frame(key, name_id,
# block).
.hpr_export_groups <- function(txt) {
  blk <- regmatches(txt, gregexpr("(?s)<SpeciesGroupDefinition(?:\\s[^>]*)?>.*?</SpeciesGroupDefinition>",
                                  txt, perl = TRUE, useBytes = TRUE))[[1L]]
  data.frame(key = vapply(blk, .hx_text, "", "SpeciesGroupKey", USE.NAMES = FALSE),
             name_id = tolower(vapply(blk, .hx_text, "", "SpeciesGroupName", USE.NAMES = FALSE)),
             block = blk, stringsAsFactors = FALSE)
}

# Write one scenario's result as an .hpr file.
#
# @param path     Output file.
# @param sources  data.frame(path, name): the source file(s) in upload order
#   (a stem's `file` is its row).
# @param species_by_name Named character: lower-case trimmed species group
#   name -> "spruce", "pine" or "none".
# @param result   The scenario's backend result (tree_summary,
#   selected_logs_all).
# @param defs,pm  The scenario's assortment table and price matrix.
# @return list(ok, errors, path, n_stems, n_logs, per_file (stems per source
#   file), no_profile (stems written without logs), gap_logs (logs of the unclassified product that fill
#   gaps between the logs)).
hpr_export_result <- function(path, sources, species_by_name, result, defs, pm) {
  fail <- function(msg) { if (file.exists(path)) unlink(path); list(ok = FALSE, errors = msg) }
  ts <- result$tree_summary; lg <- result$selected_logs_all
  fidx <- function(x) if ("file" %in% names(x)) as.integer(x$file) else rep(1L, nrow(x))
  kept <- paste(fidx(ts), as.character(ts$treid), sep = "\t")
  by_stem <- split(seq_len(nrow(lg)), paste(fidx(lg), as.character(lg$treid), sep = "\t"))
  miss <- !file.exists(sources$path)
  if (any(miss)) return(fail("The source .hpr file(s) are no longer available."))

  # Prefix of each file, its species groups and the header's.
  pre <- vector("list", nrow(sources)); defs_f <- vector("list", nrow(sources))
  for (i in seq_len(nrow(sources))) {
    src <- .hpr_source(sources$path[i], sources$name[i])
    if (length(src$errors)) return(fail(src$errors))
    p <- .hpr_prefix(src$path, src$entry)
    if (!p$found) return(fail("The source file has no stems."))
    p$bytes <- .hpr_clean_bytes(p$bytes)        # marker text / windows-1252 repaired in the copy
    txt <- .as_bytes(rawToChar(p$bytes))
    decl <- regmatches(txt, regexpr("^[^<]*<\\?xml[^>]*\\?>", txt, perl = TRUE, useBytes = TRUE))
    if (length(decl) && grepl("encoding\\s*=\\s*[\"'](?!utf-8)", decl, perl = TRUE, ignore.case = TRUE))
      return(fail("The source file is not UTF-8; it cannot be exported."))
    d <- read_hpr(sources$path[i], sources$name[i])
    if (!isTRUE(d$ok)) return(fail(d$errors))
    sp <- d$species; sp$offset_cm <- hpr_stump_offset(sp$dbh_height)
    pre[[i]] <- list(txt = txt, entry = src$entry, path = src$path, close = .hpr_close_tags(p$bytes)[-seq_along(p$bytes)])
    defs_f[[i]] <- list(species = sp, groups = .hpr_export_groups(txt))
  }
  grp <- defs_f[[1L]]$groups                       # header groups (grown below)
  key_map <- list(setNames(grp$key, grp$key))
  extra <- character(0)
  all_keys <- grp$key
  for (i in seq_along(pre)[-1L]) {
    g <- defs_f[[i]]$groups; km <- setNames(g$key, g$key)
    for (j in seq_len(nrow(g))) {
      m <- match(g$name_id[j], grp$name_id)
      if (!is.na(m)) { km[g$key[j]] <- grp$key[m]; next }
      nk <- g$key[j]
      if (nk %in% all_keys) nk <- as.character(max(suppressWarnings(as.numeric(all_keys)), 0, na.rm = TRUE) + 1)
      blk <- sub("(<SpeciesGroupKey>)\\s*[^<]*(</SpeciesGroupKey>)", paste0("\\1", nk, "\\2"),
                 g$block[j], perl = TRUE, useBytes = TRUE)
      extra <- c(extra, blk); all_keys <- c(all_keys, nk); km[g$key[j]] <- nk
      grp <- rbind(grp, data.frame(key = nk, name_id = g$name_id[j], block = blk, stringsAsFactors = FALSE))
    }
    key_map[[i]] <- km
  }
  grp$species <- unname(species_by_name[grp$name_id]); grp$species[is.na(grp$species)] <- "none"

  # Header: file 1's prefix without coordinates, the scenario's products.
  txt <- pre[[1L]]$txt
  blocks <- regmatches(txt, gregexpr("(?s)<ProductDefinition(?:\\s[^>]*)?>.*?</ProductDefinition>",
                                     txt, perl = TRUE, useBytes = TRUE))[[1L]]
  cls <- blocks[grepl("<ClassifiedProductDefinition", blocks, fixed = TRUE, useBytes = TRUE)]
  okeys <- vapply(blocks, .hx_text, "", "ProductKey", USE.NAMES = FALSE)
  orig <- setNames(as.list(cls), vapply(cls, .hx_text, "", "ProductKey", USE.NAMES = FALSE))
  prod <- hpr_export_products(defs, pm, orig, grp, used_keys = okeys)
  mark <- "@@OPTBUCK_PRODUCTS@@"
  for (k in seq_along(cls))
    txt <- sub(cls[k], if (k == 1L) mark else "", txt, fixed = TRUE, useBytes = TRUE)
  if (length(cls) == 0L) return(fail("The source file has no classified products."))
  unclassified <- if (HPR_UNCLASSIFIED_KEY %in% suppressWarnings(as.numeric(okeys))) "" else   # the gap logs' product
    sprintf(paste0("<ProductDefinition><ProductKey>%d</ProductKey><UnclassifiedProductDefinition>",
                   "<ProductName>Unclassified</ProductName></UnclassifiedProductDefinition></ProductDefinition>"),
            HPR_UNCLASSIFIED_KEY)
  txt <- sub(mark, paste(.hx_bytes(c(prod$xml, unclassified)), collapse = ""), txt, fixed = TRUE, useBytes = TRUE)
  if (length(extra)) {                             # groups of other files, after the last one
    last <- gregexpr("</SpeciesGroupDefinition>", txt, fixed = TRUE, useBytes = TRUE)[[1L]]
    last <- last[length(last)] + nchar("</SpeciesGroupDefinition>", type = "bytes") - 1L
    txt <- paste0(substring(txt, 1L, last), paste(extra, collapse = ""),
                  substring(txt, last + 1L, nchar(txt, type = "bytes")))
  }
  txt <- .hx_strip(txt, HPR_EXPORT_COORD_TAGS)
  txt <- .fix_datetimes(txt, .pin_time())          # an invalid stored date (e.g. "vasket") becomes the export time

  con <- file(path, "wb"); open_con <- TRUE
  on.exit(if (open_con) close(con), add = TRUE)
  writeBin(charToRaw(txt), con)

  seen <- new.env(); n_logs <- 0L; no_profile <- 0L; gap_logs <- 0L
  per_file <- integer(nrow(sources))
  prod_keys <- prod$keys
  for (i in seq_len(nrow(sources))) {
    sp_i <- defs_f[[i]]$species
    sp_map <- setNames(species_by_name[tolower(trimws(sp_i$name))], sp_i$key)
    sp_map[is.na(sp_map)] <- "none"
    km <- key_map[[i]]
    write_chunk <- function(records) {
      part <- .hs_extract(records)
      fk <- paste(i, part$stems$stem_key, sep = "\t")
      keep <- fk %in% kept & !duplicated(fk) & !vapply(fk, exists, NA, envir = seen, inherits = FALSE)
      for (k in fk[keep]) assign(k, TRUE, envir = seen)
      pvs <- list()
      if (any(keep)) {
        rows <- which(keep); ni <- match(part$idx, rows); ok <- !is.na(ni)
        st <- list(ok = TRUE, stems = part$stems[rows, , drop = FALSE], idx = ni[ok], pos = part$pos[ok],
                   d = part$d[ok], species = sp_i, warnings = character(0), info = character(0), file = "")
        pr <- hpr_stem_profiles(st, sp_map)
        if (!is.null(pr$profiles) && nrow(pr$profiles) > 0L)
          pvs <- lapply(profiles_to_profile_vectors(pr$profiles), add_under_bark_profile)
      }
      out <- character(0)
      for (r in which(keep)) {
        rec <- sub("(?s)^(.*</Stem>).*$", "\\1", records[r], perl = TRUE, useBytes = TRUE)
        rec <- .hx_strip(rec, c(HPR_EXPORT_COORD_TAGS))
        rec <- gsub("(?s)<Log(?:\\s[^>]*)?>.*?</Log>", "", rec, perl = TRUE, useBytes = TRUE)
        sk <- part$stems$stem_key[r]
        lx <- ""
        ii <- by_stem[[paste(i, sk, sep = "\t")]]
        if (!is.null(ii) && !is.null(pvs[[sk]])) {
          lo <- .hpr_export_logs(lg[ii, , drop = FALSE], pvs[[sk]], prod_keys)
          lx <- lo$xml; n_logs <<- n_logs + length(ii); gap_logs <<- gap_logs + lo$n_gap_logs
        } else if (!is.null(ii)) no_profile <<- no_profile + 1L
        rec <- sub("</SingleTreeProcessedStem>", paste0(.hx_bytes(lx), "</SingleTreeProcessedStem>"), rec,
                   fixed = TRUE, useBytes = TRUE)
        old <- part$stems$species_key[r]
        if (!is.na(old) && !is.na(km[old]) && km[old] != old)
          rec <- sub("(<SpeciesGroupKey>)\\s*[^<]*(</SpeciesGroupKey>)", paste0("\\1", km[old], "\\2"), rec,
                     perl = TRUE, useBytes = TRUE)
        out <- c(out, rec)
        per_file[i] <<- per_file[i] + 1L
      }
      if (length(out)) writeBin(charToRaw(paste0(out, collapse = "")), con)
      NULL
    }
    .hpr_stem_chunks(pre[[i]]$path, pre[[i]]$entry, write_chunk)
  }
  writeBin(pre[[1L]]$close, con)
  close(con); open_con <- FALSE

  if (.hpr_export_has_coordinates(path)) return(fail("Coordinates found in the export; the file was not written."))
  list(ok = TRUE, errors = character(0), path = path, n_stems = sum(per_file), n_logs = n_logs,
       per_file = per_file, no_profile = no_profile, gap_logs = gap_logs)
}

# TRUE if the file holds a coordinate element (scanned in chunks).
.hpr_export_has_coordinates <- function(path, chunk_bytes = 8L * 1024L * 1024L) {
  con <- file(path, "rb"); on.exit(close(con))
  tail <- ""
  repeat {
    b <- readBin(con, "raw", chunk_bytes)
    if (length(b) == 0L) return(FALSE)
    txt <- paste0(tail, .as_bytes(rawToChar(b[b != as.raw(0L)])))
    if (grepl(HPR_EXPORT_COORD_CHECK, txt, perl = TRUE, useBytes = TRUE)) return(TRUE)
    n <- nchar(txt, type = "bytes")
    tail <- .as_bytes(substring(txt, max(1L, n - 63L), n))
  }
}
