# ==============================================================================
# R/helpers_hpr.R
#
# Purpose : StanForD 2010 .hpr files (harvester production reports), Phase A
#           (2026-10-20): read the species groups, products, price matrices
#           and permitted grades, and turn them into a scenario (assortment
#           table and prices) for "Import from .hpr" on the Assortments and
#           prices tab. Stems are only indexed (count, species, profile type);
#           bucking .hpr stems is Phase B (docs/plans/PLAN.md).
#
# Source  : ported from optBuck (SmartForest-no/optBuck: getSpeciesGroup-
#           Definition(), getProductData(), getPriceMatrices(),
#           getPermittedGrades()), written on xml2. Deliberate departures
#           (CLAUDE.md): min top = DiameterMINTop, max top = DiameterClassMAX,
#           max butt = DiameterMAXButt; length classes round UP to whole dm;
#           "Forbidden to buck the log even manually" cells are left empty;
#           the price volume of "Length as defined in LengthClasses" products
#           uses the class lower limit (PriceLengths_cm, helpers_products.R).
#
# Privacy : never reads StemCoordinates, dates, operators, ObjectName or any
#           contact fields.
#
# Errors  : read_hpr() and hpr_to_scenario() return errors and warnings as
#           character vectors (shown in the import dialog); they do not stop.
# ==============================================================================

if (!exists("or_default", mode = "function"))   # helpers_utils.R
  source(file.path(dirname(if (is.null(sys.frame(1L)$ofile)) "." else sys.frame(1L)$ofile),
                   "helpers_utils.R"))

#' Token prefixes that identify a species group's species (lower case).
HPR_SPECIES_PATTERNS <- list(
  spruce = c("gran", "spruce", "picea", "kuusi", "fichte"),
  pine   = c("furu", "fura", "tall", "pine", "pinus", "mänty", "manty", "kiefer"))

#' Bucking criterion of a price cell that may not be cut: left empty.
HPR_FORBIDDEN <- "Forbidden to buck the log even manually"

#' StanForD volume categories Apter computes (others become solid volume).
HPR_VOLUME_METHODS <- c("Top", "Calculated Norwegian mid", "All diameters (solid volume)")

#' Product group / name tokens of sawlogs and pulpwood (the hidden Type).
HPR_SAWLOG_TOKENS   <- c("tømmer", "tommer", "sag", "timber", "saw", "kubb", "stock")
HPR_PULPWOOD_TOKENS <- c("massevirke", "massa", "pulp", "energi", "energy", "bio")

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
  t <- strsplit(x, "[^[:alpha:]æøåäöü]+", perl = TRUE)[[1L]]
  t[nzchar(t)]
}

#' Guess Apter's species of a species group from its name and user ID:
#' "spruce", "pine", or "none" (no match, or both).
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
  if (has(tn, HPR_PULPWOOD_TOKENS)) return("pulpwood")
  custom_type(name)
}


# ---- reading -------------------------------------------------------------------------

#' Read a StanForD 2010 .hpr file: species groups, products with their price
#' matrices and permitted grades, and an index of the stems.
#'
#' @param path     File path.
#' @param filename Name shown in messages.
#' @return list(ok, errors, warnings, header, species (data.frame: key,
#'   name, user_id, best_grade, start_grade, n_products, n_stems), products
#'   (list, one per classified product), stems (data.frame: key,
#'   species_key, has_profile, diameter_category)).
read_hpr <- function(path, filename = basename(path)) {
  fail <- function(msg) list(ok = FALSE, errors = msg, warnings = character(0))
  x <- tryCatch(xml2::read_xml(path), error = function(e) NULL)
  if (is.null(x)) return(fail(sprintf("%s is not a readable XML file.", filename)))
  xml2::xml_ns_strip(x)
  if (xml2::xml_name(x) != "HarvestedProduction")
    return(fail(sprintf("%s is not a StanForD 2010 .hpr file (root element '%s', expected HarvestedProduction).",
                        filename, xml2::xml_name(x))))
  att <- xml2::xml_attrs(x)
  mt <- att["messageType"]
  if (!is.na(mt) && mt != "hpr")
    return(fail(sprintf("%s is a StanForD '%s' message, not hpr.", filename, mt)))
  errors <- character(0); warnings <- character(0)
  du <- unname(att["diameterUnit"]); lu <- unname(att["lengthUnit"])
  if (!is.na(du) && du != "mm") errors <- c(errors, sprintf("Diameter unit is '%s'; Apter reads mm only (no conversion).", du))
  if (!is.na(lu) && lu != "cm") errors <- c(errors, sprintf("Length unit is '%s'; Apter reads cm only (no conversion).", lu))
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
    stringsAsFactors = FALSE)
  if (nrow(species) == 0L) errors <- c(errors, "The file defines no species groups.")
  if (any(duplicated(species$key)))
    errors <- c(errors, sprintf("Species group key(s) defined twice: %s.",
                                paste(unique(species$key[duplicated(species$key)]), collapse = ", ")))

  # Products (classified only; 999999 "Unclassified" has no definition)
  pds <- xml2::xml_find_all(machine, "./ProductDefinition[ClassifiedProductDefinition]")
  products <- lapply(pds, function(p) {
    cp <- xml2::xml_find_first(p, "./ClassifiedProductDefinition")
    dd <- "./DiameterDefinition"; dc <- paste0(dd, "/DiameterClasses")
    items <- xml2::xml_find_all(cp, "./ProductMatrixes/ProductMatrixItem")
    grades <- .hall(cp, "./PermittedGradesDefinition/PermittedGradeNumber")
    list(
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
      grades = if (inherits(xml2::xml_find_first(cp, "./PermittedGradesDefinition"), "xml_missing")) NULL
               else suppressWarnings(as.numeric(grades)),
      cells = data.frame(
        d_mm  = suppressWarnings(as.numeric(xml2::xml_attr(items, "diameterClassLowerLimit"))),
        l_cm  = suppressWarnings(as.numeric(xml2::xml_attr(items, "lengthClassLowerLimit"))),
        price = suppressWarnings(as.numeric(vapply(items, .hx1, "", "./Price"))),
        criteria = vapply(items, .hx1, "", "./BuckingCriteria"),
        stringsAsFactors = FALSE))
  })
  pkeys <- vapply(products, `[[`, "", "key")
  if (length(products) == 0L) errors <- c(errors, "The file defines no products with prices (ClassifiedProductDefinition).")
  if (any(duplicated(pkeys)))
    errors <- c(errors, sprintf("Product key(s) defined twice: %s.",
                                paste(unique(pkeys[duplicated(pkeys)]), collapse = ", ")))
  names(products) <- pkeys

  # Stem index only (Phase B bucks the stems): no coordinates, dates or logs.
  stems <- xml2::xml_find_all(machine, "./Stem")
  sd <- xml2::xml_find_first(stems, "./SingleTreeProcessedStem/StemDiameters")
  stems_df <- data.frame(
    key = trimws(xml2::xml_text(xml2::xml_find_first(stems, "./StemKey"))),
    species_key = trimws(xml2::xml_text(xml2::xml_find_first(stems, "./SpeciesGroupKey"))),
    has_profile = !is.na(xml2::xml_name(sd)),
    diameter_category = xml2::xml_attr(sd, "diameterCategory"),
    stringsAsFactors = FALSE)

  if (nrow(species) > 0L) {
    species$n_products <- vapply(species$key, function(k)
      sum(vapply(products, function(p) k %in% p$species_keys, logical(1))), integer(1))
    species$n_stems <- vapply(species$key, function(k) sum(stems_df$species_key == k), integer(1))
  }
  unknown <- unlist(lapply(products, function(p)
    if (length(p$species_keys) == 0L || !all(p$species_keys %in% species$key)) p$name))
  if (length(unknown) > 0L)
    warnings <- c(warnings, sprintf("Product(s) with a missing or unknown species group, not imported: %s.",
                                    paste(unknown, collapse = ", ")))

  list(ok = length(errors) == 0L, errors = errors, warnings = warnings,
       header = list(file = filename, version = unname(att["version"]),
                     n_stems = nrow(stems_df)),
       species = species, products = products, stems = stems_df)
}


# ---- products and the grade filter ----------------------------------------------------

#' Whether a product permits its species group's best grade (the lowest
#' GradeNumber): TRUE / FALSE, NA when the product has no permitted grades.
hpr_best_grade_ok <- function(p, species) {
  if (is.null(p$grades) || length(p$grades) == 0L) return(NA)
  best <- species$best_grade[species$key %in% p$species_keys]
  best <- best[!is.na(best)]
  if (length(best) == 0L) return(NA)
  min(best) %in% p$grades
}

# "130–360 mm (14 classes)", "305 cm (1 class)".
.hpr_range <- function(x, unit) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return("–")
  n <- sprintf("(%d class%s)", length(x), if (length(x) == 1L) "" else "es")
  if (length(x) == 1L) sprintf("%g %s %s", x, unit, n)
  else sprintf("%g–%g %s %s", min(x), max(x), unit, n)
}

#' One row per product for the import dialog: its species (after the
#' species map), forbidden cells, and whether it is imported by default
#' (not if its species is not used, it lacks the best grade, or it has no
#' priced cells), with the reason.
#'
#' @param species_map Named character: species group key -> "spruce",
#'   "pine" or "none".
hpr_product_table <- function(hpr, species_map) {
  if (length(hpr$products) == 0L) return(data.frame())
  rows <- lapply(hpr$products, function(p) {
    sp <- unique(unname(species_map[p$species_keys])); sp <- sp[!is.na(sp) & sp != "none"]
    grade <- hpr_best_grade_ok(p, hpr$species)
    priced <- sum(!is.na(p$cells$price) & p$cells$criteria != HPR_FORBIDDEN)
    reason <- if (length(sp) == 0L) "Species group not used"
              else if (priced == 0L) "No priced cells"
              else if (isFALSE(grade)) "Does not allow the best grade"
              else ""
    data.frame(key = p$key, name = p$name,
               group = paste(hpr$species$name[match(p$species_keys, hpr$species$key)], collapse = ", "),
               species = if (length(sp)) paste(sp, collapse = ", ") else "none",
               type = .hpr_type(p$group, p$name),
               diameters = .hpr_range(p$dia_classes, "mm"),
               lengths = .hpr_range(p$len_classes, "cm"),
               forbidden = sum(p$cells$criteria == HPR_FORBIDDEN, na.rm = TRUE),
               default = !nzchar(reason), reason = reason,
               grade_ok = grade, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}


# ---- one product -> assortment row and prices ------------------------------------------

# The price grid of one product in Apter's layout: whole-cm diameter rows x
# whole-dm length columns. Returns list(pm (long), limits (kept class lower
# limits, cm), lengths (cm), errors, warnings).
.hpr_grid <- function(p, key) {
  w <- character(0)
  cells <- p$cells[!is.na(p$cells$d_mm) & !is.na(p$cells$l_cm), , drop = FALSE]
  forbidden <- cells$criteria %in% HPR_FORBIDDEN
  n_forb <- sum(forbidden)
  ok_cells <- cells[!is.na(cells$price) & !forbidden, , drop = FALSE]
  if (n_forb > 0L) w <- c(w, sprintf("%d forbidden cell(s) left empty (never chosen).", n_forb))
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
    w <- c(w, sprintf("Length classes rounded up to whole dm: %s.",
                      paste(sprintf("%g→%g", L[keep_l & L %% 10 != 0], cut[keep_l & L %% 10 != 0]),
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
    w <- c(w, sprintf("Diameter class limits not in whole cm (%s mm): prices change at whole cm.",
                      paste(D_all[D_all %% 10 != 0], collapse = ", ")))
  rows_cm <- seq(floor(min(D) / 10), floor(max(D) / 10))
  cls <- vapply(rows_cm, function(cc) { u <- D_all[D_all <= cc * 10 & D_all >= min(D)]
                                        if (length(u)) max(u) else min(D) }, 0)
  above <- D_all[D_all > max(D)]
  max_top_mm <- if (length(above)) min(above) - 1 else NA_real_
  min_top_mm <- if (any(D_all < min(D))) min(D) else NA_real_
  if (length(above))
    w <- c(w, sprintf("Diameter class(es) from %g mm fully forbidden: max top diameter set to %g mm.",
                      min(above), max_top_mm))
  g <- expand.grid(i = seq_along(rows_cm), j = seq_along(L), KEEP.OUT.ATTRS = FALSE)
  price <- ok_cells$price[match(paste(cls[g$i], L[g$j]), paste(ok_cells$d_mm, ok_cells$l_cm))]
  long <- data.frame(ProductKey = key, LengthClass_dm = as.integer(cut[g$j] / 10),
                     PriceDiameterClass_cm = as.integer(rows_cm[g$i]),
                     PriceNOKperM3 = price, stringsAsFactors = FALSE)
  # Columns without any priced cell are dropped (that length is not cut);
  # a whole-cm row without a priced cell stays as an empty row (a fully
  # forbidden inner class: never chosen), as pm_drop_empty_cells() keeps it.
  priced_cols <- unique(long$LengthClass_dm[!is.na(long$PriceNOKperM3)])
  long <- pm_drop_empty_cells(long[long$LengthClass_dm %in% priced_cols, , drop = FALSE])
  empty <- setdiff(rows_cm, unique(long$PriceDiameterClass_cm[!is.na(long$PriceNOKperM3)]))
  if (length(empty) > 0L)
    w <- c(w, sprintf("Diameter class(es) %s cm fully forbidden: empty rows (never chosen).",
                      paste(empty, collapse = ", ")))
  kept <- (cut / 10) %in% priced_cols             # columns with a priced cell
  list(pm = long, limits = L[kept], lengths = cut[kept], min_top_mm = min_top_mm,
       max_top_mm = max_top_mm, warnings = w, errors = character(0))
}

#' Turn a read_hpr() result into a scenario: the assortment table and prices
#' of the chosen products (one row per product and species), plus a 0-priced
#' Waste row per species, checked with check_price_set().
#'
#' @param hpr          read_hpr() result.
#' @param species_map  Named character: species group key -> "spruce",
#'   "pine" or "none".
#' @param keep         Product keys to import (default: those
#'   hpr_product_table() imports by default).
#' @param base_defs    The base assortment table (its Waste rows are
#'   added); default from load_products().
#' @return list(ok, defs, pm, errors, warnings, notes).
hpr_to_scenario <- function(hpr, species_map, keep = NULL, base_defs = NULL) {
  fail <- function(msg, w = character(0)) list(ok = FALSE, errors = msg, warnings = w)
  if (!isTRUE(hpr$ok)) return(fail(hpr$errors))
  tab <- hpr_product_table(hpr, species_map)
  if (is.null(keep)) keep <- tab$key[tab$default]
  if (!any(species_map %in% PRODUCT_SPECIES))
    return(fail("Map at least one species group to Spruce or Pine."))
  w <- character(0)

  # Several groups as one species: their products all go to that species.
  for (sp in PRODUCT_SPECIES) {
    g <- names(species_map)[species_map == sp]
    if (length(g) > 1L)
      w <- c(w, sprintf("Species groups %s are all %s: a %s stem can be bucked into any of their products.",
                        paste(hpr$species$name[match(g, hpr$species$key)], collapse = " and "), sp, sp))
  }
  base <- if (is.null(base_defs)) defs_from_products(load_products()) else base_defs   # Waste rows
  rows <- list(); pms <- list(); used_keys <- character(0)
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
        w <- c(w, sprintf("Renamed a second '%s' for %s to '%s'.", trimws(p$name), sp, name))
      }
      grid <- .hpr_grid(p, ak)
      if (length(grid$errors) > 0L) {
        w <- c(w, sprintf("%s not imported: %s.", p$name, grid$errors)); next
      }
      pw <- grid$warnings
      method <- p$volume_method
      if (is.na(method) || !method %in% HPR_VOLUME_METHODS) {
        pw <- c(pw, sprintf("Volume method '%s' is not available; solid volume used.", method))
        method <- "All diameters (solid volume)"
      }
      if (method != "All diameters (solid volume)" &&
          !identical(p$volume_adjustment, VOLUME_ROUND_DOWN_CM))
        pw <- c(pw, sprintf("Volume diameter '%s': Apter rounds the top diameter down to whole cm for this method.",
                            p$volume_adjustment))
      len_rule <- or_default(p$volume_length, "Physical length cm")
      class_len <- identical(len_rule, "Length as defined in LengthClasses") &&
        method != "All diameters (solid volume)"
      if (!len_rule %in% c("Length as defined in LengthClasses", "Physical length cm",
                           "Rounded downwards to nearest dm-module"))
        pw <- c(pw, sprintf("Volume length '%s' is not available; the cut length is used.", len_rule))
      if (!identical(tolower(or_default(p$dia_under_bark, "true")), "true"))
        pw <- c(pw, "Diameters over bark in the file; Apter reads all limits under bark.")
      if (!identical(tolower(or_default(p$volume_under_bark, "true")), "true"))
        pw <- c(pw, "Price volume over bark in the file; Apter prices volume under bark.")
      if (!is.na(p$volume_top_pos) && !is.na(p$top_pos) && p$volume_top_pos != p$top_pos)
        pw <- c(pw, sprintf("Volume top position %g cm differs from the diameter top position %g cm; %g cm used.",
                            p$volume_top_pos, p$top_pos, p$top_pos))
      if (any(p$len_margins > 0, na.rm = TRUE))
        pw <- c(pw, "Length class margins ignored (logs are cut at the class length).")
      if (length(pw)) w <- c(w, paste0(name, ": ", pw))

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
  pm <- pm_minimal(do.call(rbind, lapply(pms, pm_minimal)))
  v <- check_price_set(defs, pm)
  if (!v$ok) return(fail(v$errors, w))
  list(ok = TRUE, defs = defs, pm = v$pm, errors = character(0),
       warnings = c(w, v$warnings), notes = HPR_NOTE)
}

#' What an .hpr import does not take over (one line in the dialog).
HPR_NOTE <- paste(
  "Not taken over: the file's bark functions (Apter uses barkNOR), grades (stems are",
  "defect-free; only the best-grade filter applies), length margins, distribution and",
  "limitation settings, and log colour marks. Prices are read as NOK/m³ under bark.")
