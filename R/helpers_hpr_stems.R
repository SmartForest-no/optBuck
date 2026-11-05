# ==============================================================================
# R/helpers_hpr_stems.R
#
# Purpose : hpr Phase B (2026-10-21): the stems of a StanForD 2010 .hpr file
#           as a tree source (".hpr stems"). Each stem's measured over-bark
#           diameter profile is bucked as it is: positions are measured from
#           the felling cut (position 0 = the cut, so the stump is 0), and only
#           the measured part is used (nothing above the last DiameterValue).
#
# Reading : read_hpr_stems() streams the file in chunks (HPR_STEM_CHUNK_BYTES)
#           and never parses it whole. A chunk's complete stem records (from
#           one "<Stem><StemKey>" to the next) are reduced to a whitelist with
#           regular expressions; a chunk with entities or CDATA, which only an
#           XML parser decodes, goes through xml2 instead (same whitelist,
#           identical results on the owner's files; 2026-10-21). Text is
#           handled as bytes: R's char/byte offset conversion is quadratic on
#           long non-ASCII strings.
#
# Privacy : only StemKey, SpeciesGroupKey, the processed-stem kind, the
#           StemDiameters diameterCategory and the DiameterValue positions and
#           values become R values. StemCoordinates, dates, operators, names,
#           recorded logs and HPRCMResults are never extracted (they exist only
#           as unparsed bytes of the current chunk). The file's DBH is not read.
#
# Heights : hpr_height_above_ground() is the only conversion from felling-cut
#           positions to heights above ground: + the stump offset, 130 cm minus
#           the species group's DBHHeight (harvesters measure DBH at DBHHeight
#           from the cut; on the owner's files the file DBH equals the profile
#           at DBHHeight in 99.6-100 % of stems). DBH is the over-bark profile
#           at 1.3 m above ground. Bucking stays in felling-cut positions.
#
# Errors  : as in R/helpers_hpr.R, errors, warnings and info come back as
#           character vectors; warnings only for stems dropped because of a
#           problem in the file (hpr_stem_checks()); expected drops go to info.
# ==============================================================================


# Bytes read per chunk of stems (2 MB: as fast as 8 MB, less than half the
# memory on the owner's largest files).
HPR_STEM_CHUNK_BYTES <- 2L * 1024L * 1024L

# A stem record starts with <Stem> (attributes allowed) directly followed by
# its StemKey (PCRE; "<Stem>" alone also occurs in biomass functions).
HPR_STEM_RECORD_RE <- "<Stem(?:\\s[^>]*)?>\\s*<StemKey>"

# DBH height above ground (cm).
HPR_DBH_ABOVE_GROUND_CM <- 130

# Stump offset (cm) when a species group has no DBHHeight: Apter's stump.
HPR_DEFAULT_STUMP_CM <- 10

# Largest accepted stump offset (cm).
HPR_MAX_STUMP_CM <- 50

# Diameters above this (mm) suggest a unit error: the stem is dropped.
HPR_MAX_DIAMETER_MM <- 1500

# Profile grid (cm), as for taperNOR profiles.
HPR_PROFILE_STEP_CM <- 10L

# Cleaning (Phase 3, Stambas defaults): checked from HPR_CLEAN_FROM_CM from the
# felling cut up to the first measured diameter <= HPR_CLEAN_END_MM.
HPR_CLEAN_FROM_CM         <- 200
HPR_CLEAN_END_MM          <- 100
HPR_CLEAN_BROKEN_TOP_MM   <- 200    # last measured diameter above this: broken top
HPR_CLEAN_MIN_FALL_MM     <- 1      # a fall of at least this ...
HPR_CLEAN_MIN_FALL_DM     <- 30     # ... over every window of this length
HPR_CLEAN_MAX_FALL_1DM_MM <- 30     # at most this over 1 dm
HPR_CLEAN_MAX_FALL_10DM_MM <- 60    # at most this over 10 dm


# ---- heights -------------------------------------------------------------------------

# Stump offset (cm) of a species group: 130 - DBHHeight, within 0-50 cm;
# HPR_DEFAULT_STUMP_CM when DBHHeight is missing.
hpr_stump_offset <- function(dbh_height_cm) {
  off <- HPR_DBH_ABOVE_GROUND_CM - as.numeric(dbh_height_cm)
  off[is.na(off)] <- HPR_DEFAULT_STUMP_CM
  pmin(pmax(off, 0), HPR_MAX_STUMP_CM)
}

# Height above ground (cm) of hpr positions (cm from the felling cut).
hpr_height_above_ground <- function(pos_cm, offset_cm) pos_cm + offset_cm


# ---- reading -------------------------------------------------------------------------

.as_bytes <- function(x) { Encoding(x) <- "bytes"; x }

# Stream the complete stem records of a file and call FUN(records) per chunk
# (records: character, bytes). Returns the list of FUN's results.
.hpr_stem_chunks <- function(path, entry = NULL, FUN, chunk_bytes = HPR_STEM_CHUNK_BYTES) {
  con <- if (is.null(entry)) gzfile(path, "rb") else unz(path, entry, open = "rb")
  on.exit(close(con))
  out <- list(); tail <- .as_bytes("")
  repeat {
    b <- readBin(con, "raw", chunk_bytes)
    eof <- length(b) == 0L
    txt <- if (eof) tail else .as_bytes(paste0(tail, .as_bytes(rawToChar(b[b != as.raw(0L)]))))
    if (grepl("<!--", txt, fixed = TRUE, useBytes = TRUE))     # a comment may hide a stem start
      txt <- .as_bytes(gsub("(?s)<!--.*?-->", "", txt, perl = TRUE, useBytes = TRUE))
    n <- nchar(txt, type = "bytes")
    st <- gregexpr(HPR_STEM_RECORD_RE, txt, perl = TRUE, useBytes = TRUE)[[1L]]
    if (st[1L] == -1L) {                                        # before the first stem
      if (eof) break
      tail <- if (n > 1024L) .as_bytes(substring(txt, n - 1023L, n)) else txt
      next
    }
    starts <- if (eof) st else st[-length(st)]                  # the last record may be incomplete
    if (length(starts)) {
      ends <- c(st[-1L] - 1L, n)[seq_along(starts)]
      out[[length(out) + 1L]] <- FUN(.as_bytes(substring(txt, starts, ends)))
    }
    if (eof) break
    tail <- .as_bytes(substring(txt, st[length(st)], n))         # substring's default last is 1e6
  }
  out
}

# Group 1 of `re` in each element (NA where it does not match), as text.
.hs_re1 <- function(x, re) {
  m <- regexec(re, x, perl = TRUE, useBytes = TRUE)
  vapply(regmatches(x, m), function(v) if (length(v) >= 2L) v[2L] else NA_character_, "")
}

# Group 1 of `re` as a number; sub() is ~10x faster than regexec() on the
# many short DiameterValue strings.
.hs_num1 <- function(x, re) {
  hit <- grepl(re, x, perl = TRUE, useBytes = TRUE)
  out <- rep(NA_real_, length(x))
  out[hit] <- suppressWarnings(as.numeric(sub(paste0("(?s)^.*?", re, ".*$"), "\\1", x[hit],
                                              perl = TRUE, useBytes = TRUE)))
  out
}

# One chunk's whitelist: stem table plus flat profile vectors (idx = row of
# the stem in the chunk) and the stem grades (Phase 6: gidx = stem row, gpos =
# gradeStartPosition in cm, gval = GradeValue; flat, like idx/pos/d).
.hs_chunk <- function(key, sp, kind, cat, cnt, pos, d, gcnt = integer(length(key)),
                      gpos = numeric(0), gval = numeric(0))
  list(stems = data.frame(stem_key = key, species_key = sp, kind = kind, category = cat,
                          n_points = as.integer(cnt), stringsAsFactors = FALSE),
       idx = rep.int(seq_along(key), cnt), pos = pos, d = d,
       gidx = rep.int(seq_along(key), gcnt), gpos = gpos, gval = gval)

# The stem grades of each record: StemGrade/GradeValue, start position (cm)
# from the felling cut and grade number.
.hs_grades_regex <- function(r) {
  gv <- regmatches(r, gregexpr("<StemGrade(?:\\s[^>]*)?>\\s*<GradeValue(?:\\s[^>]*)?>[^<]*</GradeValue>",
                               r, perl = TRUE, useBytes = TRUE))
  cnt <- lengths(gv); gv <- unlist(gv, use.names = FALSE)
  list(cnt = cnt,
       pos = .hs_num1(gv, "gradeStartPosition\\s*=\\s*[\"']\\s*([^\"']*?)\\s*[\"']"),
       val = .hs_num1(gv, ">\\s*([^<]*?)\\s*</GradeValue>"))
}

.hs_extract_regex <- function(r) {
  key  <- trimws(.hs_re1(r, "<StemKey>([^<]*)</StemKey>"))
  sp   <- trimws(.hs_re1(r, "<SpeciesGroupKey>([^<]*)</SpeciesGroupKey>"))
  kind <- ifelse(grepl("<MultiTreeProcessedStem[\\s>/]", r, perl = TRUE, useBytes = TRUE), "multi",
          ifelse(grepl("<SingleTreeProcessedStem[\\s>/]", r, perl = TRUE, useBytes = TRUE), "single",
                 "other"))
  blk  <- .hs_re1(r, "(?s)(<StemDiameters(?:\\s[^>]*)?>.*?</StemDiameters>)")
  cat  <- .hs_re1(.hs_re1(blk, "^(<StemDiameters[^>]*>)"), "diameterCategory\\s*=\\s*[\"']([^\"']*)[\"']")
  blk[is.na(blk)] <- ""
  dv <- regmatches(blk, gregexpr("<DiameterValue(?:\\s[^>]*)?>[^<]*</DiameterValue>", blk,
                                 perl = TRUE, useBytes = TRUE))
  cnt <- lengths(dv); dv <- unlist(dv, use.names = FALSE)
  g <- .hs_grades_regex(r)
  .hs_chunk(key, sp, kind, cat, cnt,
            .hs_num1(dv, "diameterPosition\\s*=\\s*[\"']\\s*([^\"']*?)\\s*[\"']"),
            .hs_num1(dv, ">\\s*([^<]*?)\\s*</DiameterValue>"),
            g$cnt, g$pos, g$val)
}

.hs_extract_xml2 <- function(r) {
  r <- sub("(?s)^(.*</Stem>).*$", "\\1", r, perl = TRUE, useBytes = TRUE)   # drop what follows
  doc <- xml2::read_xml(.as_bytes(paste0("<r>", paste(r, collapse = ""), "</r>")),
                        encoding = "UTF-8", options = c("NOBLANKS", "HUGE"))
  st <- xml2::xml_find_all(doc, "/r/Stem")
  txt1 <- function(xp) { v <- trimws(xml2::xml_text(xml2::xml_find_first(st, xp))); v[!nzchar(v)] <- NA; v }
  kind <- ifelse(xml2::xml_find_lgl(st, "boolean(./MultiTreeProcessedStem)"), "multi",
          ifelse(xml2::xml_find_lgl(st, "boolean(./SingleTreeProcessedStem)"), "single", "other"))
  dv <- xml2::xml_find_all(st, "./*/StemDiameters/DiameterValue")
  gv <- xml2::xml_find_all(st, "./*/StemGrade/GradeValue")
  .hs_chunk(txt1("./StemKey"), txt1("./SpeciesGroupKey"), kind,
            xml2::xml_attr(xml2::xml_find_first(st, "./*/StemDiameters"), "diameterCategory"),
            xml2::xml_find_num(st, "count(./*/StemDiameters/DiameterValue)"),
            suppressWarnings(as.numeric(xml2::xml_attr(dv, "diameterPosition"))),
            suppressWarnings(as.numeric(trimws(xml2::xml_text(dv)))),
            xml2::xml_find_num(st, "count(./*/StemGrade/GradeValue)"),
            suppressWarnings(as.numeric(xml2::xml_attr(gv, "gradeStartPosition"))),
            suppressWarnings(as.numeric(trimws(xml2::xml_text(gv)))))
}

# Regex, or xml2 for a chunk with entities or CDATA (none in the owner's files).
.hs_extract <- function(r) {
  if (any(grepl("&", r, fixed = TRUE, useBytes = TRUE)) ||
      any(grepl("<![CDATA[", r, fixed = TRUE, useBytes = TRUE))) .hs_extract_xml2(r)
  else .hs_extract_regex(r)
}

# Read the stems of a StanForD 2010 .hpr file (also .hpr.gz, or an .hpr in
# a .zip), streaming. The definitions (species groups, units, DBHHeight)
# come from read_hpr(), which parses only the part before the first stem.
#
# @param path        File path.
# @param filename    Name shown in messages (its extension decides .zip).
# @param defs        read_hpr() result, if already read.
# @param chunk_bytes Bytes per chunk (results do not depend on it).
# @return list(ok, errors, warnings, info, file, species (read_hpr()'s, plus
#   offset_cm, n_stems, n_profiles), stems (data.frame: stem_key,
#   species_key, kind "single"/"multi"/"other", category, n_points),
#   idx/pos/d (profile points: stem row, cm from the cut, mm), seconds).
read_hpr_stems <- function(path, filename = basename(path), defs = NULL,
                           chunk_bytes = HPR_STEM_CHUNK_BYTES) {
  t0 <- Sys.time()
  fail <- function(msg, w = character(0)) list(ok = FALSE, errors = msg, warnings = w, info = character(0),
                                                file = filename)
  if (is.null(defs)) defs <- read_hpr(path, filename)
  if (!isTRUE(defs$ok)) return(fail(defs$errors, defs$warnings))
  src <- .hpr_source(path, filename)
  if (length(src$errors)) return(fail(src$errors))
  parts <- tryCatch(.hpr_stem_chunks(src$path, src$entry, .hs_extract, chunk_bytes),
                    error = function(e) e)
  if (inherits(parts, "error"))
    return(fail(sprintf("The stems of %s cannot be read: %s", filename, conditionMessage(parts))))
  n_of <- vapply(parts, function(p) nrow(p$stems), 0L)
  off <- cumsum(c(0L, n_of))[seq_along(parts)]
  stems <- if (length(parts)) do.call(rbind, lapply(parts, `[[`, "stems")) else
    .hs_chunk(character(0), character(0), character(0), character(0), integer(0), numeric(0), numeric(0))$stems
  x <- list(idx = unlist(Map(function(p, o) p$idx + o, parts, off), use.names = FALSE),
            pos = unlist(lapply(parts, `[[`, "pos"), use.names = FALSE),
            d   = unlist(lapply(parts, `[[`, "d"), use.names = FALSE),
            gidx = unlist(Map(function(p, o) p$gidx + o, parts, off), use.names = FALSE),
            gpos = unlist(lapply(parts, `[[`, "gpos"), use.names = FALSE),
            gval = unlist(lapply(parts, `[[`, "gval"), use.names = FALSE))
  errors <- character(0)
  if (nrow(stems) == 0L) errors <- sprintf("%s contains no stems.", filename)
  else if (all(stems$n_points == 0L))
    errors <- sprintf("%s has %s stems but none with a stem profile (StemDiameters): there is nothing to buck.",
                      filename, format(nrow(stems), big.mark = ","))
  sp <- defs$species
  sp$offset_cm  <- hpr_stump_offset(sp$dbh_height)
  sp$n_stems    <- vapply(sp$key, function(k) sum(stems$species_key == k, na.rm = TRUE), 0L)
  sp$n_profiles <- vapply(sp$key, function(k) sum(stems$species_key == k & stems$n_points > 0L, na.rm = TRUE), 0L)
  info <- c(sprintf("Read %s stems from %s.", format(nrow(stems), big.mark = ","), filename),
            "Recorded logs are not read; the stems are bucked from their profiles.")
  list(ok = length(errors) == 0L, errors = errors, warnings = c(defs$warnings, src$warnings),
       info = info, file = filename, species = sp, stems = stems,
       instruction = hpr_instruction_table(defs),
       idx = x$idx, pos = x$pos, d = x$d, gidx = x$gidx, gpos = x$gpos, gval = x$gval,
       seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}


# ---- profiles ------------------------------------------------------------------------

# Cleaning checks on the stems' own measured points (over bark, cm from the
# felling cut, mm). Range: from HPR_CLEAN_FROM_CM (interpolated if no point
# lies there) to the first measured point at or above it with a diameter <=
# HPR_CLEAN_END_MM, else the last point. Windows start at every measured
# point of the range, end by interpolation and lie inside the range; only
# decreases count (local increases are never a max-fall failure). Thresholds
# are strict (exactly 30 mm, 60 mm or 1 mm passes). Pure: no state.
#
# @param idx,pos,d Profile points (stem row, cm, mm), as read_hpr_stems().
# @param n         Number of stems.
# @param check      Stem rows to check (the others stay FALSE).
# @return data.frame(broken_top, too_flat, too_steep), n rows.
hpr_clean_flags <- function(idx, pos, d, n, check = seq_len(n)) {
  broken <- flat <- steep <- rep(FALSE, n)
  if (length(pos) == 0L || length(check) == 0L)
    return(data.frame(broken_top = broken, too_flat = flat, too_steep = steep))
  rows <- split(seq_along(pos), factor(idx, levels = seq_len(n)))
  eps <- 1e-9
  fall <- function(p, x, w) {                       # falls over windows of w cm that fit in p
    s <- which(p + w <= p[length(p)] + eps)
    if (length(s) == 0L) return(numeric(0))
    x[s] - stats::approx(p, x, xout = p[s] + w, ties = mean)$y
  }
  for (i in check) {
    ii <- rows[[i]]
    if (length(ii) == 0L) next
    o <- order(pos[ii], na.last = NA); p <- pos[ii][o]; x <- d[ii][o]
    if (length(p) == 0L) next
    broken[i] <- x[length(x)] > HPR_CLEAN_BROKEN_TOP_MM
    if (anyDuplicated(p)) { x <- as.numeric(tapply(x, p, mean)); p <- sort(unique(p)) }
    if (length(p) < 2L || p[length(p)] < HPR_CLEAN_FROM_CM) next
    if (p[1L] < HPR_CLEAN_FROM_CM) {                # start the range at the 2 m point
      x0 <- stats::approx(p, x, xout = HPR_CLEAN_FROM_CM)$y
      keep <- p > HPR_CLEAN_FROM_CM
      p <- c(HPR_CLEAN_FROM_CM, p[keep]); x <- c(x0, x[keep])
    }
    end <- which(x <= HPR_CLEAN_END_MM)[1L]
    if (!is.na(end)) { p <- p[seq_len(end)]; x <- x[seq_len(end)] }
    if (length(p) < 2L) next
    f1 <- fall(p, x, 10); f10 <- fall(p, x, 100)
    steep[i] <- any(f1 > HPR_CLEAN_MAX_FALL_1DM_MM + eps) || any(f10 > HPR_CLEAN_MAX_FALL_10DM_MM + eps)
    flat[i]  <- any(fall(p, x, HPR_CLEAN_MIN_FALL_DM * 10) < HPR_CLEAN_MIN_FALL_MM - eps)
  }
  data.frame(broken_top = broken, too_flat = flat, too_steep = steep)
}

# Which read stems Apter bucks, with the species map (species group key ->
# "spruce" / "pine" / "none"); cheap (no profiles built), for the app's
# message as the map changes.
#
# Dropped, in this order: multi-tree stems, stems without diameters,
# profiles not over bark, duplicate StemKeys, species groups set to Not used
# (per group), unreadable values, profiles not starting at the felling cut,
# diameters above HPR_MAX_DIAMETER_MM, and profiles ending below 1.3 m above
# ground (no DBH). Only real problems in the file are warnings (profiles not
# over bark, duplicate keys, unreadable values, not starting at 0, too large
# diameters; one per reason, with counts). Expected drops (multi-tree stems,
# stems without a profile, Not-used species, profiles ending below 1.3 m)
# are counted in info only: the summary line (stems read, used) covers them
# (decided 2026-10-21).
#
# Cleaning (Phase 3, hpr_clean_flags()) then checks the stems left: too flat and
# too steep profiles are dropped (once, under the first failing check); a broken
# top is only counted, the stem is kept (2026-10-28). One warning line per check,
# counted independently (`clean_counts` has the counts).
#
# @return list(ok, errors, warnings, info, file, n_read, n_used, use (row
#   indices of the stems bucked), species (per stem), offset, last (cm),
#   group (per stem), reason (NA if bucked), species_used (per stem bucked)).
hpr_stem_checks <- function(st, species_map) {
  if (!isTRUE(st$ok)) return(list(ok = FALSE, errors = st$errors, warnings = st$warnings,
                                  info = character(0), file = st$file))
  s <- st$stems; n <- nrow(s); sp <- st$species
  gi <- match(s$species_key, sp$key)
  group <- ifelse(is.na(gi), s$species_key,
                  if (is.null(sp$name_id)) sprintf("%s (%s)", sp$name[gi], sp$key[gi]) else sp$name[gi])   # several files: keys repeat
  species <- unname(species_map[s$species_key]); species[is.na(species)] <- "none"
  offset <- ifelse(is.na(gi), HPR_DEFAULT_STUMP_CM, sp$offset_cm[gi])
  f <- factor(st$idx, levels = seq_len(n))
  first <- rep(NA_real_, n); last <- first; dmax <- first; unreadable <- rep(FALSE, n)
  if (length(st$pos)) {
    first <- as.numeric(tapply(st$pos, f, min)); last <- as.numeric(tapply(st$pos, f, max))
    dmax  <- as.numeric(tapply(st$d, f, max))
    unreadable <- as.logical(tapply(is.na(st$pos) | is.na(st$d), f, any)) %in% TRUE
  }
  dbh_at <- HPR_DBH_ABOVE_GROUND_CM - offset                 # cm from the cut

  reason <- rep(NA_character_, n); seen <- character(0)    # reasons in the order checked
  warned <- character(0)                                   # reasons that are real problems
  drop <- function(cond, why, warn) { m <- is.na(reason) & (cond %in% TRUE)
    reason[m] <<- if (length(why) == 1L) why else why[m]
    seen <<- c(seen, unique(reason[m]))
    if (warn) warned <<- c(warned, unique(reason[m])) }
  drop(s$kind == "multi", "multi-tree stem (no single-stem profile)", FALSE)
  drop(s$n_points == 0L | s$kind != "single", "no stem profile (StemDiameters)", FALSE)
  drop(is.na(s$category) | s$category != "Over bark",
       ifelse(is.na(s$category), "profile without diameterCategory (over or under bark unknown)",
              sprintf("profile recorded '%s', not over bark (not supported)", s$category)), TRUE)
  drop(duplicated(s$stem_key), "StemKey already used by an earlier stem", TRUE)
  drop(!species %in% c("spruce", "pine"), sprintf("species group %s is set to Not used", group), FALSE)
  drop(unreadable, "unreadable diameter values", TRUE)
  drop(first != 0, "profile does not start at the felling cut (position 0)", TRUE)
  drop(dmax > HPR_MAX_DIAMETER_MM, sprintf("a diameter above %s mm (likely a unit error)",
                                           format(HPR_MAX_DIAMETER_MM, big.mark = ",")), TRUE)
  drop(last < dbh_at, "profile ends below 1.3 m above ground (no DBH)", FALSE)

  # Cleaning (Phase 3): per-check counts are independent; a broken top is only counted (the stem is kept),
  # a stem failing a fall check is dropped once, under its first failing check.
  fl <- hpr_clean_flags(st$idx, st$pos, st$d, n, which(is.na(reason)))
  clean_why <- c(too_flat = sprintf("profile falls less than %s mm per %s dm", HPR_CLEAN_MIN_FALL_MM, HPR_CLEAN_MIN_FALL_DM),
                 too_steep = sprintf("profile falls more than %s mm over 1 dm or %s mm over 10 dm",
                                     HPR_CLEAN_MAX_FALL_1DM_MM, HPR_CLEAN_MAX_FALL_10DM_MM))
  for (k in names(clean_why)) reason[is.na(reason) & fl[[k]]] <- clean_why[[k]]
  clean_counts <- vapply(fl, sum, 0L)

  use <- which(is.na(reason)); dropped <- which(!is.na(reason))
  fmt <- function(k) prettyNum(k, big.mark = ",")
  tab <- table(factor(reason[dropped], levels = seen))
  lines <- if (length(tab))
    sprintf("%s %s dropped: %s.", fmt(as.integer(tab)), ifelse(as.integer(tab) == 1L, "stem", "stems"), names(tab))
  real <- names(tab) %in% warned
  noun <- function(k) ifelse(k == 1L, "stem", "stems")
  hit <- clean_counts[names(clean_why)] > 0L
  clean_lines <- c(
    if (clean_counts[["broken_top"]] > 0L)
      sprintf("%s %s with a broken top (last diameter above %s mm).", fmt(clean_counts[["broken_top"]]),
              noun(clean_counts[["broken_top"]]), HPR_CLEAN_BROKEN_TOP_MM),
    sprintf("%s %s dropped: %s.", fmt(as.integer(clean_counts[names(clean_why)][hit])),
            noun(clean_counts[names(clean_why)][hit]), clean_why[hit]))
  warnings <- c(st$warnings, lines[real], clean_lines)
  used_groups <- unique(gi[use]); used_groups <- used_groups[!is.na(used_groups)]
  info <- c(st$info, lines[!real],
            if (length(used_groups))
              sprintf("Height above ground = position + %g cm (130 cm - DBHHeight %s) for %s.",
                      sp$offset_cm[used_groups],
                      ifelse(is.na(sp$dbh_height[used_groups]), "not given; Apter's stump",
                             sprintf("%g cm", sp$dbh_height[used_groups])),
                      sp$name[used_groups]))
  errors <- if (length(use) == 0L)
    "No stems are left to buck: no stem has a usable profile in a species group set to spruce or pine."
  else character(0)
  list(ok = length(errors) == 0L, errors = errors, warnings = warnings, info = info,
       file = st$file, n_read = n, n_used = length(use), use = use, species = species,
       offset = offset, last = last, group = group, reason = reason, species_used = species[use],
       clean_counts = clean_counts)
}

# The stem grade at each 10 cm grid point (Phase 6). A grade runs from its
# gradeStartPosition (snapped to the nearest grid point, halves up) to the
# next change; the grade at grid point g is the grade of the cell [g, g+10).
# Before the first start, and for a stem without grades, the species group's
# StartGrade applies (NA when the group has none: the stem is then treated as
# grade-free). Where two starts snap to the same point the later one wins.
#
# @param g           Grid positions (cm, ascending).
# @param gpos,gval   The stem's own grade starts (cm) and numbers.
# @param start_grade Species group StartGrade (NA if none).
# @return Numeric vector, length(g).
hpr_grade_grid <- function(g, gpos, gval, start_grade, step = HPR_PROFILE_STEP_CM) {
  ok <- !is.na(gpos) & !is.na(gval)
  out <- rep(as.numeric(start_grade), length(g))
  if (!any(ok)) return(out)
  gpos <- gpos[ok]; gval <- gval[ok]
  o <- order(gpos, seq_along(gpos)); gpos <- gpos[o]; gval <- gval[o]
  at <- findInterval(g, floor(gpos / step + 0.5) * step)
  out[at > 0L] <- gval[at[at > 0L]]
  out
}

# Turn read stems into the stems Apter bucks (hpr_stem_checks(), then the
# profiles). Kept stems get a 10 cm profile from 0 to the last measured
# position rounded down to 10 cm (interpolated where positions are off the
# grid; local diameter increases are kept), and DBH = the over-bark profile
# at 1.3 m above ground.
#
# @return hpr_stem_checks()'s list plus tree_data (plot_id NA, treid =
#   StemKey, species_group, dbh_cm, height_m = measured length,
#   height_source), profiles (long: treid, plot_id, species_group,
#   diameterPosition, DiameterValue, DBH, height_m; for
#   profiles_to_profile_vectors()) and excluded (treid, species_group_name,
#   excluded_reason).
# Cells (10 cm, [g, g+10)) inside recorded waste logs: the whole cells of [ceiling(start/10)*10,
# + floor(length/10)*10). This is the zone the old optBuck set to grade -1 (roadmap 8).
# @param g Grid positions (cm); start,len the waste logs of one stem (cm).
hpr_reject_cells <- function(g, start, len, step = HPR_PROFILE_STEP_CM) {
  out <- rep(FALSE, length(g))
  if (!length(start)) return(out)
  s0 <- ceiling(start / step) * step; e0 <- s0 + floor(len / step) * step
  for (j in seq_along(s0)) out <- out | (g >= s0[j] & g + step <= e0[j])
  out
}

hpr_stem_profiles <- function(st, species_map, grades = FALSE, reject = NULL) {
  ck <- hpr_stem_checks(st, species_map)
  if (is.null(ck$use)) return(ck)
  s <- st$stems; use <- ck$use
  f <- factor(st$idx, levels = seq_len(nrow(s)))
  rows <- split(seq_along(st$pos), f)
  if (grades) {                                   # Phase 6: the stem grades on the grid
    grows <- split(seq_along(st$gpos), factor(st$gidx, levels = seq_len(nrow(s))))
    sgrade <- st$species$start_grade[match(s$species_key, st$species$key)]
  }
  step <- HPR_PROFILE_STEP_CM
  off_grid <- 0L
  rjl <- if (!is.null(reject)) split(reject, as.character(reject$stem_key))   # recorded waste logs per stem
  prof <- lapply(use, function(i) {
    ii <- rows[[i]]; p <- st$pos[ii]; d <- st$d[ii]
    o <- order(p); p <- p[o]; d <- d[o]
    g <- seq(0, floor(ck$last[i] / step) * step, by = step)
    dg <- if (length(p) == length(g) && all(p == g)) d else {
      off_grid <<- off_grid + 1L
      stats::approx(p, d, xout = g, ties = mean)$y
    }
    list(g = g, d = dg,
         dbh = dbh_from_profile(hpr_height_above_ground(p, ck$offset[i]), d),   # at 1.3 m above ground
         grade = if (grades) hpr_grade_grid(g, st$gpos[grows[[i]]], st$gval[grows[[i]]], sgrade[i]),
         rej = if (!is.null(rjl)) { w <- rjl[[as.character(s$stem_key[i])]]; hpr_reject_cells(g, w$start, w$length) })
  })
  ng <- vapply(prof, function(q) length(q$g), 0L)
  key <- s$stem_key[use]
  dbh_mm <- vapply(prof, `[[`, 0, "dbh")
  height_m <- vapply(prof, function(q) max(q$g), 0) / 100
  ck$profiles <- data.frame(
    treid = rep(key, ng), plot_id = rep(NA_character_, sum(ng)), species_group = rep(ck$species[use], ng),
    diameterPosition = as.integer(unlist(lapply(prof, `[[`, "g"), use.names = FALSE)),
    DiameterValue = unlist(lapply(prof, `[[`, "d"), use.names = FALSE),
    DBH = rep(dbh_mm, ng), height_m = rep(height_m, ng), stringsAsFactors = FALSE)
  ck$tree_data <- data.frame(plot_id = rep(NA_character_, length(use)), treid = key,
                             sesong = rep(NA, length(use)), species_group = ck$species[use],
                             dbh_cm = dbh_mm / 10, height_m = height_m,
                             height_source = rep("Measured profile", length(use)), stringsAsFactors = FALSE)
  if (!is.null(rjl)) ck$profiles$reject <- unlist(lapply(prof, `[[`, "rej"), use.names = FALSE)
  if (grades) {
    ck$profiles$grade <- unlist(lapply(prof, `[[`, "grade"), use.names = FALSE)
    # The stem's file (R5): which files' grades a scenario uses is decided per file (grade_files).
    ck$profiles$file <- rep(if (is.null(s$file)) rep(1L, nrow(s))[use] else s$file[use], ng)
  }
  if (!is.null(s$file)) ck$tree_data$file <- s$file[use]     # the file's position in the upload
  dropped <- which(!is.na(ck$reason))
  ck$excluded <- data.frame(treid = s$stem_key[dropped], species_group_name = ck$group[dropped],
                            excluded_reason = ck$reason[dropped], stringsAsFactors = FALSE)
  if (!is.null(s$file)) ck$excluded$file <- s$file[dropped]
  if (off_grid > 0L)
    ck$info <- c(ck$info, sprintf("Positions off the 10 cm grid in %s stem profile(s): interpolated.",
                                  prettyNum(off_grid, big.mark = ",")))
  ck
}


# ---- several files -------------------------------------------------------------------
# Phase 2 (2026-10-28): the stems of several .hpr files as one tree source.
# Files are read one at a time (read_hpr_stems()), so memory stays as for a
# single file. StemKeys are kept as written and must be unique across files
# (a repeated one is dropped by hpr_stem_checks(), like a duplicate within a
# file). File names never reach results: stems carry `file`, the file's
# position in the upload, and messages name files by position.

# Why a file could not be used, without the file name (a short phrase the
# app shows as "File 2 not used: <reason>.").
hpr_file_failure_reason <- function(errors, filename) {
  e <- paste(errors, collapse = " ")
  if (grepl("contains no stems", e, fixed = TRUE)) return("no stems")
  if (grepl("none with a stem profile", e, fixed = TRUE)) return("no stem profiles")
  if (grepl("is not a readable XML file|is not a StanForD 2010 .hpr file|is a StanForD '", e)) return("not a readable StanForD 2010 .hpr file")
  if (grepl("The stems of .* cannot be read|cannot be read", e)) return("the stems cannot be read")
  gsub(filename, "the file", errors[1L], fixed = TRUE)
}

# Combine read_hpr_stems() results (one per file, in upload order) into one
# read_hpr_stems()-shaped list for hpr_stem_checks() and hpr_stem_profiles().
#
# Species group keys become "<n>:<key>" (n = the file's position), because
# keys such as 1, 2, 3 repeat across files; `name_id` (lower case, trimmed
# group name) is what the app maps species by. Each stem gets `file` = n.
#
# @param reads     List of read_hpr_stems() results.
# @param filenames Their names (only used to keep them out of messages).
# @return list(ok, errors, warnings, info, file = NA, n_files (used), failed
#   (data.frame: file, reason), species, stems (with `file`), idx, pos, d).
combine_hpr_stems <- function(reads, filenames = vapply(reads, function(r) or_default(r$file, ""), "")) {
  good <- which(vapply(reads, function(r) isTRUE(r$ok), TRUE))
  bad  <- setdiff(seq_along(reads), good)
  failed <- data.frame(file = as.integer(bad),
                       reason = vapply(bad, function(i) hpr_file_failure_reason(reads[[i]]$errors, filenames[i]), ""),
                       stringsAsFactors = FALSE)
  unnamed <- function(x, i) gsub(filenames[i], sprintf("file %d", i), x, fixed = TRUE)
  warnings <- unique(unlist(lapply(good, function(i) unnamed(reads[[i]]$warnings, i))))
  if (length(good) == 0L)
    return(list(ok = FALSE, errors = "None of the files can be used.", warnings = warnings,
                info = character(0), file = NA_character_, n_files = 0L, failed = failed))
  n_of <- vapply(good, function(i) nrow(reads[[i]]$stems), 0L)
  off  <- cumsum(c(0L, n_of))[seq_along(good)]
  stems <- do.call(rbind, lapply(seq_along(good), function(j) {
    s <- reads[[good[j]]]$stems
    s$species_key <- paste0(good[j], ":", s$species_key)
    s$file <- rep(good[j], nrow(s)); s
  }))
  sp <- do.call(rbind, lapply(good, function(i) {
    s <- reads[[i]]$species
    s$key <- paste0(i, ":", s$key); s$file <- rep(i, nrow(s)); s
  }))
  sp$name_id <- tolower(trimws(sp$name))
  # The files' product definitions (R5): group keys get the same "<n>:" prefix as the species.
  instr <- do.call(rbind, lapply(good, function(i) {
    x <- reads[[i]]$instruction
    if (is.null(x) || nrow(x) == 0L) return(NULL)
    x$groups <- vapply(strsplit(x$groups, "|", fixed = TRUE), function(g) paste0(i, ":", g, collapse = "|"), "")
    x$file <- rep(i, nrow(x)); x
  }))
  list(ok = TRUE, errors = character(0), warnings = warnings, instruction = instr,
       info = "Recorded logs are not read; the stems are bucked from their profiles.",
       file = NA_character_, n_files = length(good), failed = failed,
       species = sp, stems = stems,
       idx = unlist(Map(function(i, o) reads[[i]]$idx + o, good, off), use.names = FALSE),
       pos = unlist(lapply(good, function(i) reads[[i]]$pos), use.names = FALSE),
       d   = unlist(lapply(good, function(i) reads[[i]]$d), use.names = FALSE),
       gidx = unlist(Map(function(i, o) reads[[i]]$gidx + o, good, off), use.names = FALSE),
       gpos = unlist(lapply(good, function(i) reads[[i]]$gpos), use.names = FALSE),
       gval = unlist(lapply(good, function(i) reads[[i]]$gval), use.names = FALSE))
}

# Read several .hpr files one after the other and combine them
# (combine_hpr_stems()). The app runs this on a worker.
#
# @param paths     File paths, in upload order.
# @param filenames Names shown in the reader's own messages (extension
#   decides .zip / .gz); they are kept out of the combined messages.
read_hpr_stem_files <- function(paths, filenames = basename(paths)) {
  reads <- vector("list", length(paths))
  for (i in seq_along(paths))
    reads[[i]] <- tryCatch(read_hpr_stems(paths[i], filenames[i]),
                           error = function(e) list(ok = FALSE, file = filenames[i],
                             errors = sprintf("The stems of %s cannot be read: %s", filenames[i], conditionMessage(e)),
                             warnings = character(0)))
  combine_hpr_stems(reads, filenames)
}


# ---- grade mismatch warning (R4 polish, 2026-11-03) -----------------------------------------
# Stem grades and product grades are compared by number only, and the same number can mean
# another quality in another job's file. With "Use stem grades" on, a sawlog product that permits
# the grades of less than half of a species' stem length gets a warning.

# Length of the stems by species and grade on the grade grid the bucking uses: one 10 cm cell
# per grid point of each stem still bucked (hpr_stem_checks()), the grade of a cell as in
# hpr_stem_profiles(grades = TRUE) (a stem without grades has its group's StartGrade; NA =
# grade-free).
#
# @param st,species_map As hpr_stem_checks().
# @param ck  hpr_stem_checks(st, species_map) (default: computed).
# @return data.frame(species, grade, cells) (cells = number of 10 cm cells); NULL without stems.
hpr_grade_lengths <- function(st, species_map, ck = hpr_stem_checks(st, species_map)) {
  if (!isTRUE(ck$ok) || is.null(st$gpos) || length(ck$use) == 0L) return(NULL)
  s <- st$stems; sp <- st$species; step <- HPR_PROFILE_STEP_CM
  grows <- split(seq_along(st$gpos), factor(st$gidx, levels = seq_len(nrow(s))))
  sgrade <- sp$start_grade[match(s$species_key, sp$key)]
  gr <- lapply(ck$use, function(i)
    hpr_grade_grid(seq(0, floor(ck$last[i] / step) * step, by = step),
                   st$gpos[grows[[i]]], st$gval[grows[[i]]], sgrade[i]))
  file <- if (is.null(s$file)) rep(1L, nrow(s)) else s$file
  d <- data.frame(species = rep(ck$species[ck$use], lengths(gr)), file = rep(file[ck$use], lengths(gr)),
                  grade = unlist(gr, use.names = FALSE),
                  start = rep(sgrade[ck$use], lengths(gr)), stringsAsFactors = FALSE)
  d <- d[d$species %in% c("spruce", "pine"), , drop = FALSE]
  if (nrow(d) == 0L) return(NULL)
  tab <- as.data.frame(table(species = d$species, file = d$file, grade = d$grade, start = d$start, useNA = "ifany"),
                       stringsAsFactors = FALSE)
  tab <- tab[tab$Freq > 0L, , drop = FALSE]
  # (species, file, grade, start): the stem length per grade and file; `start` = the group's StartGrade
  data.frame(species = tab$species, file = suppressWarnings(as.integer(tab$file)),
             grade = suppressWarnings(as.numeric(tab$grade)), start = suppressWarnings(as.numeric(tab$start)),
             cells = tab$Freq, stringsAsFactors = FALSE)
}

# Grade numbers as text: consecutive numbers "1–2", otherwise "1, 3, 5".
format_grades <- function(g) {
  g <- sort(unique(g[!is.na(g)]))
  if (length(g) >= 2L && all(diff(g) == 1)) return(sprintf("%g\u2013%g", min(g), max(g)))
  paste(format(g, trim = TRUE, scientific = FALSE), collapse = ", ")
}

# The grade mismatch check: with "Use stem grades" on, every sawlog product whose permitted
# grades cover less than half of its species' stem length (cells without a grade count as
# permitted).
#
# @param dist       hpr_grade_lengths() result (NULL: no .hpr stems).
# @param defs       The scenario's assortment table.
# @param use_grades The scenario's "Use stem grades" option.
# @param threshold  Warn below this permitted share (0.5).
# @return data.frame(key, species, grade (the species' dominant grade), share (whole percent of
#   the species' length), permitted (text)); no rows when nothing applies.
grade_mismatch_check <- function(dist, defs, use_grades, threshold = 0.5) {
  none <- data.frame(key = character(0), species = character(0), grade = numeric(0),
                     share = numeric(0), permitted = character(0), stringsAsFactors = FALSE)
  if (!isTRUE(use_grades) || is.null(dist) || NROW(dist) == 0L || NROW(defs) == 0L) return(none)
  if ("file" %in% names(dist)) dist <- grade_dist_by_grade(dist)    # per species and grade
  if (is.null(dist)) return(none)
  keys <- assortment_table_keys(defs)
  rows <- lapply(which(defs$Type == "sawlog" & defs$Species %in% dist$species), function(i) {
    perm <- permitted_grades_set(defs$PermittedGrades[i])
    if (length(perm) == 0L) return(NULL)                      # no grade restriction
    d <- dist[dist$species == defs$Species[i], , drop = FALSE]
    tot <- sum(d$cells)
    if (sum(d$cells[is.na(d$grade) | d$grade %in% perm]) / tot >= threshold) return(NULL)
    g <- d[!is.na(d$grade), , drop = FALSE]; top <- g[which.max(g$cells), ]
    data.frame(key = keys[i], species = defs$Species[i], grade = top$grade,
               share = round(100 * top$cells / tot), permitted = format_grades(perm), stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, NA)]
  if (length(rows) == 0L) none else do.call(rbind, rows)
}

# The warning texts (one per row of grade_mismatch_check()) in `lang`.
# @param label Function: assortment key -> display name.
grade_warning_text <- function(w, label, lang = "en") {
  if (NROW(w) == 0L) return(character(0))
  vapply(seq_len(nrow(w)), function(i)
    tr("grade_mismatch", format(w$grade[i], trim = TRUE), format(w$share[i], trim = TRUE),
       if (identical(w$species[i], "pine")) tr("gm_species_pine", lang = lang) else tr("gm_species_spruce", lang = lang),
       label(w$key[i]), w$permitted[i], tr("lbl_use_grades", lang = lang), lang = lang), "")
}


# ---- same instruction (R5, 2026-11-03) ------------------------------------------------------
# Grade numbers mean different things in different buyers' instructions, so a stem file's grades
# are used for a scenario only when the scenario's assortments come from the same instruction as
# the file: every scenario product of the species that came from a file matches a product of that
# species in the stem file by ProductKey (or, for a .pin product, ProductUserID) AND product name.

# The product definitions of a read_hpr() result for the comparison with a scenario.
# @return data.frame(key, name, uid, groups): ProductKey, ProductName, ProductUserID (NA if none) and
#   the product's species group keys joined by "|".
hpr_instruction_table <- function(h) {
  p <- h$products
  if (length(p) == 0L) return(data.frame(key = character(0), name = character(0), uid = character(0),
                                         groups = character(0), stringsAsFactors = FALSE))
  one <- function(f) vapply(p, f, "", USE.NAMES = FALSE)
  data.frame(key = one(function(x) as.character(or_default(x$key, NA_character_))),
             name = one(function(x) trimws(as.character(or_default(x$name, "")))),
             uid = one(function(x) as.character(or_default(x$src_uid, NA_character_))),
             groups = one(function(x) paste(x$species_keys, collapse = "|")),
             stringsAsFactors = FALSE)
}

# The ProductUserID in SourceXml strings (NA if none).
.source_uid <- function(xml) {
  m <- regmatches(xml, regexec("<ProductUserID[^>]*>([^<]*)</ProductUserID>", xml))
  vapply(m, function(x) if (length(x) == 2L && nzchar(x[2L])) x[2L] else NA_character_, "")
}

# TRUE when scenario name `nm` is the file product name `n`, or its renamed duplicate
# ("Name (GROUP)" or "Name <ProductKey>", hpr_to_scenario()).
.same_product_name <- function(nm, n, key) {
  nm <- trimws(nm)
  identical(nm, n) || startsWith(nm, paste0(n, " (")) && endsWith(nm, ")") || nm == paste(n, key)
}

# Whether the scenario has the same instruction as each stem file, per species.
#
# A scenario product counts when it came from a file (a HprProductKey or a SourceXml); products
# the user added are ignored. For a species the scenario has the same instruction as a file when
# there is at least one such product and every one matches a product of that species in the file:
# same ProductKey (a .pin product without key: same ProductUserID, or any when the file product
# has none) and same product name (a renamed duplicate "Name (GROUP)" counts as its original).
#
# @param instr       hpr_instruction_table() rows of all files, with `file` (default 1).
# @param species_map Named character: species group key -> "spruce", "pine" or "none".
# @param defs        The scenario's assortment table.
# @param files       File numbers to answer for (default: those in `instr`).
# @return data.frame(file, species, same).
hpr_same_instruction <- function(instr, species_map, defs, files = NULL) {
  if (is.null(instr)) instr <- data.frame(key = character(0), name = character(0), uid = character(0),
                                          groups = character(0), file = integer(0))
  if (is.null(instr$file)) instr$file <- rep(1L, nrow(instr))
  if (is.null(files)) files <- sort(unique(instr$file))
  from_file <- if (NROW(defs)) !is.na(defs$HprProductKey) | !is.na(defs$SourceXml) else logical(0)
  uid <- if (NROW(defs)) .source_uid(defs$SourceXml) else character(0)
  prod_sp <- lapply(strsplit(instr$groups, "|", fixed = TRUE), function(g) unique(unname(species_map[g])))
  out <- lapply(files, function(f) {
    sp_rows <- lapply(PRODUCT_SPECIES, function(sp) {
      rows <- which(from_file & defs$Species == sp)
      in_sp <- instr$file == f & vapply(prod_sp, function(m) sp %in% m, NA)
      fp <- instr[in_sp, , drop = FALSE]
      ok <- length(rows) > 0L && nrow(fp) > 0L && all(vapply(rows, function(i) {
        name_ok <- vapply(seq_len(nrow(fp)), function(j) .same_product_name(defs$Name[i], fp$name[j], fp$key[j]), NA)
        id_ok <- if (!is.na(defs$HprProductKey[i])) fp$key == defs$HprProductKey[i]
                 else if (!is.na(uid[i])) is.na(fp$uid) | fp$uid == uid[i]
                 else rep(TRUE, nrow(fp))
        any(name_ok & id_ok)
      }, NA))
      data.frame(file = f, species = sp, same = isTRUE(ok), stringsAsFactors = FALSE)
    })
    do.call(rbind, sp_rows)
  })
  do.call(rbind, out)
}

# Per species, the files whose grades a scenario uses (the files with its instruction). The
# caller asks only when "Use stem grades" or "Cut at grade changes" is on; with `on` FALSE it is
# NULL (all files, as before).
grade_files_of <- function(same, on) {
  if (!isTRUE(on)) return(NULL)
  setNames(lapply(PRODUCT_SPECIES, function(sp) same$file[same$species == sp & same$same]), PRODUCT_SPECIES)
}

# `hpr_grade_lengths()` rows summed to species and grade, over the given files only (R5).
#
# @param dist  hpr_grade_lengths() result.
# @param files NULL (all files) or a named list species -> file numbers to keep.
# @return data.frame(species, grade, cells) as the grade mismatch check reads it; NULL if empty.
grade_dist_by_grade <- function(dist, files = NULL) {
  if (is.null(dist) || NROW(dist) == 0L) return(NULL)
  if (!is.null(files))
    dist <- dist[vapply(seq_len(nrow(dist)), function(i) dist$file[i] %in% files[[dist$species[i]]], NA), , drop = FALSE]
  if (nrow(dist) == 0L) return(NULL)
  key <- paste(dist$species, dist$grade)
  first <- !duplicated(key)
  data.frame(species = dist$species[first], grade = dist$grade[first],
             cells = as.integer(tapply(dist$cells, factor(key, levels = key[first]), sum)),
             stringsAsFactors = FALSE)
}

# The share (text) of the stem length with a grade other than the start grade, per species, over
# the stems of files whose grades are not used (R5). Whole percent, "<1" between 0 and 0.5.
#
# @param dist hpr_grade_lengths() result.
# @param same hpr_same_instruction() result.
# @return data.frame(species, share) for species with grade information in an unused file.
grade_unused_notes <- function(dist, same) {
  none <- data.frame(species = character(0), share = character(0), stringsAsFactors = FALSE)
  if (is.null(dist) || NROW(dist) == 0L || !"file" %in% names(dist)) return(none)
  rows <- lapply(PRODUCT_SPECIES, function(sp) {
    ok_files <- same$file[same$species == sp & same$same]
    d <- dist[dist$species == sp & !dist$file %in% ok_files, , drop = FALSE]
    if (nrow(d) == 0L || !any(!is.na(d$grade))) return(NULL)
    p <- 100 * sum(d$cells[!is.na(d$grade) & !is.na(d$start) & d$grade != d$start]) / sum(d$cells)
    data.frame(species = sp, share = if (p > 0 && p < 0.5) "<1" else format(floor(p + 0.5), trim = TRUE),
               stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, NA)]
  if (length(rows)) do.call(rbind, rows) else none
}

# The note texts of grade_unused_notes() for scenario `side` in `lang`.
grade_unused_text <- function(notes, side, lang = "en") {
  if (NROW(notes) == 0L) return(character(0))
  vapply(seq_len(nrow(notes)), function(i)
    tr("grade_unused",
       tr(if (identical(notes$species[i], "pine")) "sp_pine_lc" else "sp_spruce_lc", lang = lang),
       side, notes$share[i], lang = lang), "")
}
