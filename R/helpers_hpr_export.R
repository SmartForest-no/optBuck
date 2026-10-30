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

if (!exists("or_default", mode = "function")) {
  .hx_root <- Sys.getenv("APTER_ROOT", unset = "")
  if (nzchar(.hx_root)) source(file.path(.hx_root, "R", "helpers_utils.R"))
}

#' Elements removed from everything copied from a source file.
HPR_EXPORT_COORD_TAGS <- c("StemCoordinates", "DefaultCoordinate",
                           "GeographicalRestriction", "RestrictionPoint")

#' Any of these left in the written file makes the export fail.
HPR_EXPORT_COORD_CHECK <- paste0("<(?:Latitude|Longitude|Altitude|StemCoordinates|",
                                 "DefaultCoordinate|RestrictionPoint|GeographicalRestriction)[\\s>/]")

#' ProductKey of StanForD's unclassified product (never a new key's base).
HPR_UNCLASSIFIED_KEY <- 999999

#' CuttingReason of the simulated logs.
HPR_EXPORT_CUTTING_REASON <- "Automatic"

#' Metadata copied from the original ClassifiedProductDefinition of a product
#' with the same key, in file order (before the generated definitions).
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

#' The LengthDistributionDefinition of an assortment (Phase 7a): its stored
#' DistributionAllowed, DistributionCategory, MAXDeviation and
#' DistributionWithinProduct, in the element order of the source files; NULL
#' when none is stored (then nothing is written, as before).
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

#' The ProductDefinition elements of a scenario.
#'
#' @param defs   Assortment table (ASSORTMENT_TABLE_COLS, with HprProductKey).
#' @param pm     The scenario's price matrix (pm_minimal()).
#' @param orig   Named list of the source file's classified ProductDefinition
#'   texts (names = ProductKey).
#' @param groups data.frame(key, species): the header's species groups.
#' @param used_keys All ProductKeys of the source file (numeric-looking ones
#'   set the start of the new keys).
#' @param style  "hpr" (default) or "pin" (Phase 5b): a .pin ProductDefinition has
#'   a ProductUserID (`ids`, one per row of `defs`) instead of a ProductKey, no
#'   metadata copied from a source product, no SpeciesGroupKey, a
#'   SpeciesGroupUserID ("Spruce" / "Pine") and the permitted grades of the
#'   assortment (only when it has some).
#' @param ids    The ProductUserIDs of a .pin (style "pin").
#' @return list(xml (one text per assortment), keys (named by
#'   assortment_table_keys(defs): the exported ProductKey)).
hpr_export_products <- function(defs, pm, orig, groups, used_keys = names(orig),
                                style = c("hpr", "pin"), ids = NULL) {
  style <- match.arg(style)
  pin <- identical(style, "pin")
  akeys <- assortment_table_keys(defs)
  lens  <- assortment_lengths(defs, pm)
  hk <- if ("HprProductKey" %in% names(defs)) as.character(defs$HprProductKey) else rep(NA_character_, nrow(defs))
  hk[!is.na(hk) & !hk %in% names(orig)] <- NA_character_      # not a product of this file
  if (pin) hk[] <- NA_character_                              # a .pin copies nothing from a source
  hk[duplicated(hk) & !is.na(hk)] <- NA_character_            # the first row keeps the key
  uk <- suppressWarnings(as.numeric(used_keys))
  top <- max(c(0, uk[!uk %in% HPR_UNCLASSIFIED_KEY]), na.rm = TRUE)    # 999999 is the unclassified product
  newk <- seq_len(sum(is.na(hk))) + top
  keys <- hk; keys[is.na(hk)] <- format(newk, scientific = FALSE, trim = TRUE)
  pm <- pm_minimal(pm)            # keeps the hidden Distribution column
  orig <- lapply(orig, function(s) { Encoding(s) <- "UTF-8"; s })   # files are UTF-8 (the caller checks)
  xml <- vapply(seq_len(nrow(defs)), function(i) {
    d <- defs[i, , drop = FALSE]
    o <- if (!is.na(hk[i])) orig[[hk[i]]] else NA_character_
    meta <- if (is.na(o)) {
      grp <- switch(as.character(d$Type), sawlog = "Tømmer", pulpwood = "Massevirke", as.character(d$Type))
      sprintf("<ProductGroupName>%s</ProductGroupName>", .xesc(grp))
    } else paste(stats::na.omit(vapply(HPR_EXPORT_META_BEFORE, function(t) .hx_elem(o, t), "")), collapse = "")
    rows <- pm[pm$ProductKey == akeys[i], , drop = FALSE]
    rows_dist <- if (PM_DISTRIBUTION_COL %in% names(rows)) rows[[PM_DISTRIBUTION_COL]] else rep(NA_real_, nrow(rows))
    matrix_priced <- identical(as.character(d$PriceType), "matrix")
    len_cm <- as.numeric(lens[[akeys[i]]])
    min_top <- as.numeric(d$MinTopDiameter_cm) * 10; max_top <- as.numeric(d$MaxTopDiameter_cm) * 10
    if (matrix_priced) {
      dcl <- sort(unique(rows$PriceDiameterClass_cm[!is.na(rows$PriceDiameterClass_cm)])) * 10
      lim <- price_lengths(d$PriceLengths_cm, len_cm)           # class lower limits (cm)
      cells <- expand.grid(dc = dcl, li = seq_along(len_cm), KEEP.OUT.ATTRS = FALSE)
      hit <- match(paste(cells$dc / 10, len_cm[cells$li] / 10),
                   paste(rows$PriceDiameterClass_cm, rows$LengthClass_dm))
      pr <- rows$PriceNOKperM3[hit]
      dist <- rows_dist[hit]        # one class per whole-cm row: each row's own stored value
    } else {
      dcl <- max(0, floor(min_top))
      lim <- len_cm
      cells <- expand.grid(dc = dcl, li = seq_along(len_cm), KEEP.OUT.ATTRS = FALSE)
      pr <- rep(rows$PriceNOKperM3[is.na(rows$LengthClass_dm)][1L], nrow(cells))
      dist <- rep(NA_real_, nrow(cells))
    }
    top_pos <- or_default(as.numeric(d$DiameterTopPosition_cm), 0)
    method <- as.character(d$VolumeDiameterCategory)
    if (identical(method, "Top cylinder")) method <- "Top"       # StanForD has no cylinder rule
    round_down <- !identical(volume_rounding(d$VolumeDiameterCategory), "None")
    items <- sprintf(
      paste0('<ProductMatrixItem diameterClassLowerLimit="%s" lengthClassLowerLimit="%s"><Price>%s</Price>',
             '<Distribution>%s</Distribution><Limitation>0</Limitation><BuckingCriteria>%s</BuckingCriteria>',
             '</ProductMatrixItem>'),
      .fnum(cells$dc, 2L), .fnum(lim[cells$li], 2L), ifelse(is.na(pr), "0", .fnum(pr, 2L)),
      ifelse(is.na(dist), "0", .fnum(dist, 4L)),     # stored target distribution (Phase 7a), 0 when none
      ifelse(is.na(pr), HPR_FORBIDDEN, "No limit"))
    gk <- groups$key[groups$species == as.character(d$Species)]
    paste0(
      if (pin) paste0("<ProductDefinition><ProductUserID>", .xesc(ids[i]), "</ProductUserID><ClassifiedProductDefinition>")
      else paste0("<ProductDefinition><ProductKey>", keys[i], "</ProductKey><ClassifiedProductDefinition>"),
      "<ProductName>", .xesc(d$Name), "</ProductName>", meta,
      '<DiameterDefinition modificationRestricted="false"><DiameterClasses diameterClassCategory="Top">',
      paste0("<DiameterClass><DiameterClassLowerLimit>", .fnum(dcl, 2L),
             "</DiameterClassLowerLimit><DiameterClassName/></DiameterClass>", collapse = ""),
      if (is.finite(max_top)) paste0("<DiameterClassMAX>", .fnum(max_top, 2L), "</DiameterClassMAX>"),
      "<DiameterClassAdjustment>DiameterClassAdjustment1</DiameterClassAdjustment>",
      "<DiameterUnderBark>true</DiameterUnderBark></DiameterClasses>",
      "<DiameterMINTop>", .fnum(min_top, 2L), "</DiameterMINTop>",
      if (is.finite(as.numeric(d$MaxButtDiameter_cm)))
        paste0("<DiameterMAXButt>", .fnum(as.numeric(d$MaxButtDiameter_cm) * 10, 2L), "</DiameterMAXButt>"),
      "<DiameterTopPosition>", .fnum(top_pos, 2L), "</DiameterTopPosition></DiameterDefinition>",
      '<LengthDefinition modificationRestricted="false"><LengthClassAdjustment>LengthClassAdjustment1</LengthClassAdjustment>',
      paste0("<LengthClass><LengthClassLowerLimit>", .fnum(lim, 2L),
             "</LengthClassLowerLimit><LengthClassMargin>0</LengthClassMargin></LengthClass>", collapse = ""),
      "<LengthClassMAX>", .fnum(max(len_cm), 2L), "</LengthClassMAX></LengthDefinition>",
      '<PriceDefinition modificationRestricted="false"><VolumeDiameterAdjustment>',
      if (round_down) VOLUME_ROUND_DOWN_CM else "Measured diameter in mm", "</VolumeDiameterAdjustment>",
      sprintf('<VolumeDiameterCategory volumeDiameterTopPosition="%s">%s</VolumeDiameterCategory>',
              .fnum(top_pos, 2L), .xesc(method)),
      "<VolumeLengthCategory>",
      if (matrix_priced && !identical(method, "All diameters (solid volume)"))
        "Length as defined in LengthClasses" else "Physical length cm",
      "</VolumeLengthCategory><VolumeUnderBark>true</VolumeUnderBark></PriceDefinition>",
      if (pin) {
        g <- if ("PermittedGrades" %in% names(d)) permitted_grades_set(d$PermittedGrades) else numeric(0)
        if (length(g)) paste0('<PermittedGradesDefinition modificationRestricted="false">',
                              paste0("<PermittedGradeNumber>", format(g, scientific = FALSE, trim = TRUE), "</PermittedGradeNumber>", collapse = ""),
                              "</PermittedGradesDefinition>")
      } else if (!is.na(o)) { g <- .hx_elem(o, "PermittedGradesDefinition"); if (!is.na(g)) g },
      # LengthDistributionDefinition (Phase 7a): where the source files have it, after
      # PermittedGradesDefinition and before the matrices; only when the assortment has stored values.
      .hpr_export_length_distribution(d),
      "<ProductMatrixes>", paste(items, collapse = ""), "</ProductMatrixes>",
      if (!pin && !is.na(o)) { u <- .hx_elem(o, "ProductUserID"); if (!is.na(u)) u },
      if (pin) paste0('<SpeciesGroupUserID agency="Other">', if (identical(as.character(d$Species), "pine")) "Pine" else "Spruce",
                      "</SpeciesGroupUserID>")
      else if (length(gk)) paste0("<SpeciesGroupKey>", gk, "</SpeciesGroupKey>", collapse = ""),
      "</ClassifiedProductDefinition></ProductDefinition>")
  }, "")
  list(xml = xml, keys = setNames(keys, akeys))
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

#' Write one scenario's result as an .hpr file.
#'
#' @param path     Output file.
#' @param sources  data.frame(path, name): the source file(s) in upload order
#'   (a stem's `file` is its row).
#' @param species_by_name Named character: lower-case trimmed species group
#'   name -> "spruce", "pine" or "none".
#' @param result   The scenario's backend result (tree_summary,
#'   selected_logs_all).
#' @param defs,pm  The scenario's assortment table and price matrix.
#' @return list(ok, errors, path, n_stems, n_logs, per_file (stems per source
#'   file), no_profile (stems written without logs), gap_logs (logs of the unclassified product that fill
#'   gaps between the logs)).
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
  mark <- "@@APTER_PRODUCTS@@"
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
