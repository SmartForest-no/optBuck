# ==============================================================================
# audits/audit_35_stanford_export.R
#
# Purpose : R4 D6 (2026-11-02): the .pin and .hpr exports are schema-valid
#           StanForD 2010 files that keep what an imported product carries
#           (ProductUserID, SpeciesGroupUserID, ModificationDate, metadata,
#           ManualFreeBuck, TopLogFreeBuck, ProductCondition, LimitationDefinition,
#           ...) and the file's own diameter classes. R/helpers_hpr_export.R
#           (hpr_export_products(), source_xml_build(), product_fingerprint()),
#           R/helpers_pin_export.R, R/helpers_hpr.R, pm_from_grid(by_class).
#
# XSD     : the Skogforsk StanForD 2010 schemas in C:/optbuck_private/stanford_xsd/
#           (v33/ProductInstruction_V3p3.xsd for .pin, v31/HarvestedProduction_V3p1.xsd
#           for the .hpr of file5), downloaded from skogforsk.se; they carry no
#           licence text and are never committed. Checks that need them are
#           REVIEW when they are missing.
#
# Private : C:/optbuck_private/hpr/REDACTED28 (file5) for checks 9-11 (REVIEW
#           when missing). Checks 1-8 use the synthetic pin_full_synthetic.pin
#           and the app's default assortments only.
#
# Checks  :
#  1.  The XSD files are available (REVIEW otherwise)
#  2.  An optBuck-made scenario (default assortments plus a custom one with
#      Norwegian letters) exports a valid .pin: neutral values, ASCII unique
#      ProductUserIDs, SpeciesGroupUserID Spruce / Pine, ModificationDate = export time
#  3.  pin_full fixture: ProductUserID and SpeciesGroupUserID survive import -> export,
#      the metadata, ManualFreeBuck, TopLogFreeBuck, ProductCondition and
#      LimitationDefinition are the source's, and the export is valid
#  4.  Two products with the same ID: the first keeps it, the other gets _2 (all unique)
#  5.  ModificationDate: unchanged products keep the source date; a product with
#      one edited price, one edited target, or no fingerprint gets the export time
#  6.  The hidden source fields survive copy assortment, the scenario xlsx and a
#      price edit
#  7.  Price edit per file class: an edited cell sets every whole-cm row of the
#      same FileDiameterClass_mm and length (a row without a class changes alone);
#      a bulk edit (by_class = FALSE) changes the cell only
#  8.  The diameter classes written are the file's own (FileDiameterClass_mm)
#  9.  file5: import -> .pin export -> import gives identical prices, targets
#      and classes; the .pin is valid; IDs and dates survive (REVIEW without file5)
# 10.  file5: a run's .hpr export is valid against the .hpr schema and keeps the
#      ProductUserIDs (REVIEW without file5)
# 11.  file5: the exported .pin has the same diameter classes and the same number
#      of matrix cells per product as the source file (REVIEW without file5)
# 12.  A stored ModificationDate that is not an xs:dateTime ("vasket", as in the
#      anonymised test files) is replaced by the export time; the export is valid
# 13.  file5 import -> .pin export: every product has exactly the source's diameter
#      class lower limits (also classes without a whole-cm row), and the file is valid
#      (REVIEW without file5)
#
# Verdict : PASS / ACCEPTED WITH REVIEW / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/stanford_export/audit_35_stanford_export.csv
#   outputs/stanford_export/audit_35_stanford_export.txt
#
# Usage   : Rscript audits/audit_35_stanford_export.R
# ==============================================================================

root_dir <- local({
  # OPTBUCK_ROOT -> walk up from getwd() -> stop()
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  env <- Sys.getenv("OPTBUCK_ROOT", unset = "")
  if (nzchar(env)) {
    d <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (!is_root(d))
      stop("Project root '", d, "' (from OPTBUCK_ROOT) does not contain ",
           "R/helpers_paths.R.", call. = FALSE)
  } else {
    d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    while (!is_root(d) && dirname(d) != d) d <- dirname(d)
    if (!is_root(d))
      stop("Cannot locate the project root: set OPTBUCK_ROOT, or run from ",
           "inside the project directory.", call. = FALSE)
  }
  d
})
Sys.setenv(OPTBUCK_ROOT = root_dir)

suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)
source(file.path(root_dir, "audits", "helpers_audit.R"))


out_dir <- path_output("stanford_export", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_35_stanford_export.txt")
audit_csv <- file.path(out_dir, "audit_35_stanford_export.csv")
init_audit_log(audit_txt)

E <- new.env(parent = emptyenv())
rec <- function(check, result, detail = "")
  record_check(E, check, result, detail, log_path = audit_txt)
PASS <- "PASS"; FAIL <- "FAIL"; REVIEW <- "REVIEW"
ok <- function(x) if (isTRUE(x)) PASS else FAIL
section <- function(title) {
  cat(sprintf("\n--- %s ---\n", title), file = audit_txt, append = TRUE)
  cat(sprintf("\n--- %s ---\n", title))
}
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)
cat("  Audit: StanForD exports (R4 D6)\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

XSD_DIR <- "C:/optbuck_private/stanford_xsd"
XSD_PIN <- file.path(XSD_DIR, "v33", "ProductInstruction_V3p3.xsd")
XSD_HPR <- file.path(XSD_DIR, "v31", "HarvestedProduction_V3p1.xsd")
have_xsd <- file.exists(XSD_PIN) && file.exists(XSD_HPR)
F5 <- "C:/optbuck_private/hpr/REDACTED28"
have5 <- file.exists(F5)
FULL <- file.path(root_dir, "inst", "extdata", "inputs", "test_fixtures", "pin_full_synthetic.pin")

# XSD validation: list(ok, n, first errors). NA ok when the schemas are missing.
xsd_check <- function(f, xsd) {
  if (!have_xsd) return(list(ok = NA, n = NA_integer_, first = "XSD missing"))
  r <- xml2::xml_validate(xml2::read_xml(f, options = "HUGE"), xml2::read_xml(xsd))
  e <- attr(r, "errors")
  list(ok = isTRUE(r), n = length(e), first = paste(utils::head(unique(gsub("\\{[^}]*\\}", "", e)), 3L), collapse = " / "))
}
# PASS/FAIL, or REVIEW when the schemas are missing (a non-XSD part must hold either way).
with_xsd <- function(label, rest_ok, v, detail) {
  res <- if (!isTRUE(rest_ok)) FAIL else if (is.na(v$ok)) REVIEW else ok(v$ok)
  rec(label, res, detail)
}
doc_of <- function(f) { d <- xml2::read_xml(f, options = "HUGE"); xml2::xml_ns_strip(d); d }
txt_of <- function(d, xp) xml2::xml_text(xml2::xml_find_all(d, xp))
now_ok <- function(x, t0, t1) {
  t <- as.POSIXct(substr(x, 1L, 19L), format = "%Y-%m-%dT%H:%M:%S")
  !is.na(t) & t >= t0 - 120 & t <= t1 + 120
}
guess_map <- function(h) setNames(guess_hpr_species(h$species$name, h$species$user_id), h$species$key)
import <- function(f) { h <- read_product_file(f); hpr_to_scenario(h, guess_map(h)) }
pin_of <- function(defs, pm) {
  p <- tempfile(fileext = ".pin"); e <- pin_export_scenario(defs, pm_minimal(pm), p); stopifnot(isTRUE(e$ok)); list(path = p, e = e)
}
by_name <- function(d, tag) {   # ProductName -> text of the product's child `tag` (NA when it has none)
  ps <- xml2::xml_find_all(d, "//ClassifiedProductDefinition")
  nm <- vapply(ps, function(p) xml2::xml_text(xml2::xml_find_first(p, "./ProductName")), "")
  v <- vapply(ps, function(p) { x <- xml2::xml_find_first(p, paste0("./", tag))
                                if (inherits(x, "xml_missing")) NA_character_ else xml2::xml_text(x) }, "")
  setNames(v, nm)
}

# ==============================================================================
section("01. The schemas")
rec("Check 01: The StanForD 2010 XSD files (.pin 3.3, .hpr 3.1) are in C:/optbuck_private/stanford_xsd/",
    if (have_xsd) PASS else REVIEW,
    sprintf("pin schema %s, hpr schema %s%s", file.exists(XSD_PIN), file.exists(XSD_HPR),
            if (have_xsd) "" else " (download them from skogforsk.se, Released schemas; they are not committed)"))

# ==============================================================================
section("02. An optBuck-made scenario")
t0 <- Sys.time()
defs0 <- defs_from_products(load_products()); pm0 <- pm_minimal(load_price_matrix_long())
cust <- .type_assortment_table(data.frame(
  Name = "Sagt\u00f8mmer \u00c5s", Type = custom_type("Sagt\u00f8mmer \u00c5s"), Species = "spruce",
  MinTopDiameter_cm = 15, MaxTopDiameter_cm = Inf, MinButtDiameter_cm = 0, MaxButtDiameter_cm = Inf,
  MinLength_cm = 400, MaxLength_cm = 500, DiameterTopPosition_cm = 0,
  VolumeDiameterCategory = "Top cylinder", PriceType = "flat", stringsAsFactors = FALSE))
defs2 <- rbind(defs0, cust)
pm2 <- pm_minimal(pm_bind(pm0, pm_start_prices(assortment_table_keys(cust), "flat", 500)))
x2 <- pin_of(defs2, pm2); t1 <- Sys.time()
d2 <- doc_of(x2$path)
ids2 <- txt_of(d2, "//ProductDefinition/ProductUserID")
v2 <- xsd_check(x2$path, XSD_PIN)
ok2 <- length(ids2) == sum(defs2$Type != "waste") && !anyDuplicated(ids2) && all(grepl("^[A-Za-z0-9_-]+$", ids2)) &&
  "Sagtommer_Aas" %in% ids2 &&
  all(txt_of(d2, "//ClassifiedProductDefinition/SpeciesGroupUserID") %in% c("Spruce", "Pine")) &&
  all(now_ok(txt_of(d2, "//ClassifiedProductDefinition/ModificationDate"), t0, t1)) &&
  all(txt_of(d2, "//ManualFreeBuck") == "false") && all(txt_of(d2, "//TopLogFreeBuck") == "false") &&
  all(txt_of(d2, "//ProductCondition") == "Buck always") &&
  all(txt_of(d2, "//LimitationDefinition/LimitationCategory") == "No limitation") &&
  !any(grepl("Latitude|Longitude|Coordinate", paste(readLines(x2$path, warn = FALSE), collapse = "")))
with_xsd("Check 02: An optBuck-made scenario exports a valid .pin with neutral values, unique ASCII ProductUserIDs, SpeciesGroupUserID Spruce/Pine and the export time as ModificationDate",
         ok2, v2, sprintf("%d products | ids %s | XSD errors %s %s", length(ids2), paste(ids2, collapse = ","), v2$n, v2$first))

# ==============================================================================
section("03. IDs and source elements survive")
imp <- import(FULL); defs3 <- imp$defs; pm3 <- pm_minimal(imp$pm)
x3 <- pin_of(defs3, pm3)
d3 <- doc_of(x3$path); s3 <- doc_of(FULL)
v3 <- xsd_check(x3$path, XSD_PIN)
same_by <- function(tag) {     # equal where the source has the element
  a <- by_name(d3, tag); b <- by_name(s3, tag); k <- names(b)[!is.na(b)]
  length(k) > 0L && identical(a[k], b[k])
}
sg_out <- setNames(txt_of(d3, "//ClassifiedProductDefinition/SpeciesGroupUserID"), txt_of(d3, "//ClassifiedProductDefinition/ProductName"))
sg_src <- setNames(txt_of(s3, "//ClassifiedProductDefinition/SpeciesGroupUserID"), txt_of(s3, "//ClassifiedProductDefinition/ProductName"))
uid_out <- setNames(txt_of(d3, "//ProductDefinition/ProductUserID"), txt_of(d3, "//ClassifiedProductDefinition/ProductName"))
uid_src <- setNames(txt_of(s3, "//ProductDefinition/ProductUserID"), txt_of(s3, "//ClassifiedProductDefinition/ProductName"))
ok3 <- identical(uid_out[sort(names(uid_out))], uid_src[sort(names(uid_src))]) &&
  identical(sg_out[sort(names(sg_out))], sg_src[sort(names(sg_src))]) &&
  same_by("ModificationDate") && same_by("ProductGroupName") &&
  same_by("ManualFreeBuck") && same_by("TopLogFreeBuck") && same_by("ProductCondition") &&
  same_by("LimitationDefinition/LimitationCategory") &&
  identical(txt_of(d3, "//ProductBuyer/BusinessName")[1L], txt_of(s3, "//ProductBuyer/BusinessName")[1L])
with_xsd("Check 03: ProductUserID, SpeciesGroupUserID, ModificationDate, metadata, ManualFreeBuck, TopLogFreeBuck, ProductCondition and LimitationDefinition survive import -> .pin export; the export is valid",
         ok3, v3, sprintf("ids %s | species ids %s | XSD errors %s %s", paste(uid_out, collapse = ","), paste(sg_out, collapse = ","), v3$n, v3$first))

# ==============================================================================
section("04. Unique ProductUserIDs")
k_p <- assortment_table_keys(defs3)[match("Pine Pulpwood Full", defs3$Name)]
dup_d <- defs3[match("Pine Pulpwood Full", defs3$Name), , drop = FALSE]; dup_d$Name <- "Pine Pulpwood Copy"
dup_k <- assortment_table_keys(dup_d)
dup_pm <- pm3[pm3$ProductKey == k_p, , drop = FALSE]; dup_pm$ProductKey <- dup_k
x4 <- pin_of(rbind(defs3, dup_d), pm_bind(pm3, dup_pm))
d4 <- doc_of(x4$path); ids4 <- txt_of(d4, "//ProductDefinition/ProductUserID")
v4 <- xsd_check(x4$path, XSD_PIN)
nm4 <- txt_of(d4, "//ClassifiedProductDefinition/ProductName")
ok4 <- !anyDuplicated(ids4) && ids4[match("Pine Pulpwood Full", nm4)] == "PULP_Pine" &&
  ids4[match("Pine Pulpwood Copy", nm4)] == "PULP_Pine_2"
with_xsd("Check 04: A copied assortment (same ProductUserID) gets _2; the original keeps its ID; all IDs are unique and the file is valid",
         ok4, v4, sprintf("ids %s | XSD errors %s %s", paste(ids4, collapse = ","), v4$n, v4$first))

# ==============================================================================
section("05. ModificationDate")
# Both products get a source date (the fixture's spruce product has none).
k_s <- assortment_table_keys(defs3)[match("Spruce Sawlog Full", defs3$Name)]
k_p <- assortment_table_keys(defs3)[match("Pine Pulpwood Full", defs3$Name)]
defs5 <- defs3; is5 <- match(k_s, assortment_table_keys(defs5))
defs5$SourceXml[is5] <- sub("<src>", "<src><md>2025-01-02T03:04:05.000+01:00</md>", defs5$SourceXml[is5], fixed = TRUE)
src_md <- c(`Pine Pulpwood Full` = "2025-10-22T17:56:46.413+02:00", `Spruce Sawlog Full` = "2025-01-02T03:04:05.000+01:00")
t0 <- Sys.time()
md_a <- by_name(doc_of(pin_of(defs5, pm3)$path), "ModificationDate")
# (b) one price edited (pine); (c) one target edited (spruce); (d) no fingerprint
pm5b <- pm3; i5 <- which(pm5b$ProductKey == k_p & !is.na(pm5b$PriceNOKperM3))[1L]; pm5b$PriceNOKperM3[i5] <- pm5b$PriceNOKperM3[i5] + 1
md_b <- by_name(doc_of(pin_of(defs5, pm5b)$path), "ModificationDate")
pm5c <- pm3; if (!PM_DISTRIBUTION_COL %in% names(pm5c)) pm5c[[PM_DISTRIBUTION_COL]] <- NA_real_
j5 <- which(pm5c$ProductKey == k_s & !is.na(pm5c$PriceNOKperM3))[1L]; pm5c[[PM_DISTRIBUTION_COL]][j5] <- 37
md_c <- by_name(doc_of(pin_of(defs5, pm5c)$path), "ModificationDate")
d5d <- defs5; d5d$SourceFingerprint <- NA_character_
md_d <- by_name(doc_of(pin_of(d5d, pm3)$path), "ModificationDate"); t1 <- Sys.time()
ok5 <- identical(md_a[names(src_md)], src_md) &&
  now_ok(md_b[["Pine Pulpwood Full"]], t0, t1) && md_b[["Spruce Sawlog Full"]] == src_md[["Spruce Sawlog Full"]] &&
  now_ok(md_c[["Spruce Sawlog Full"]], t0, t1) && md_c[["Pine Pulpwood Full"]] == src_md[["Pine Pulpwood Full"]] &&
  all(now_ok(md_d, t0, t1)) && !any(md_d %in% src_md)
rec("Check 05: Unchanged products keep the source ModificationDate; an edited price, an edited target or a missing fingerprint gives the export time",
    ok(ok5), sprintf("source %s | price edited: %s | target edited: %s | no fingerprint: %s",
                     paste(src_md, collapse = " / "), paste(md_b, collapse = " / "), paste(md_c, collapse = " / "), paste(md_d, collapse = " / ")))

# ==============================================================================
section("06. The hidden source fields travel")
cp <- copy_assortment(defs3, pm3, defs0, pm0, k_s)             # copy / make equal
x6 <- pin_of(cp$defs, cp$pm)
ids6 <- txt_of(doc_of(x6$path), "//ProductDefinition/ProductUserID")
xl <- tempfile(fileext = ".xlsx"); scenario_export(defs3, pm3, xl); ri <- scenario_import(xl)
ed <- pm_from_grid(pm_to_grid(pm3, k_s), k_s, pm3)              # a price edit leaves defs alone
ok6 <- "SAW_Spruce" %in% ids6 && isTRUE(ri$ok) &&
  identical(ri$defs$SourceXml, defs3$SourceXml) && identical(ri$defs$SourceFingerprint, defs3$SourceFingerprint) &&
  identical(cp$defs$SourceXml[match(k_s, assortment_table_keys(cp$defs))], defs3$SourceXml[match(k_s, assortment_table_keys(defs3))]) &&
  identical(product_fingerprint(pm_minimal(ed), k_s), defs3$SourceFingerprint[match(k_s, assortment_table_keys(defs3))]) &&
  !any(is.na(defs3$SourceXml[defs3$Type != "waste"])) && all(is.na(defs3$SourceXml[defs3$Type == "waste"]))
rec("Check 06: SourceXml and SourceFingerprint survive copy assortment, the scenario xlsx and a price edit; the copy exports the source ProductUserID",
    ok(ok6), sprintf("copied scenario ids %s | xlsx round trip %s | fingerprint after an unchanged grid write %s",
                     paste(ids6, collapse = ","), identical(ri$defs$SourceXml, defs3$SourceXml),
                     identical(product_fingerprint(pm_minimal(ed), k_s), defs3$SourceFingerprint[match(k_s, assortment_table_keys(defs3))])))

# ==============================================================================
section("07. Price edit per file class")
pmc <- pm_minimal(data.frame(ProductKey = "k", LengthClass_dm = rep(c(30L, 40L), each = 4L),
                             PriceDiameterClass_cm = rep(18:21, 2L), PriceNOKperM3 = c(100, 100, 110, 120, 200, 200, 210, 220),
                             FileDiameterClass_mm = rep(c(180, 180, 200, NA), 2L), stringsAsFactors = FALSE))
gc <- pm_to_grid(pmc, "k"); gc["18", "30"] <- 555
e_cls <- pm_from_grid(gc, "k", pmc, by_class = TRUE)
e_one <- pm_from_grid(gc, "k", pmc)                                   # a bulk edit: the cell only
pr_of <- function(pm, cm, dm) pm$PriceNOKperM3[pm$PriceDiameterClass_cm == cm & pm$LengthClass_dm == dm]
gc2 <- pm_to_grid(pmc, "k"); gc2["21", "40"] <- 999                   # a row without a class changes alone
e_none <- pm_from_grid(gc2, "k", pmc, by_class = TRUE)
ok7 <- pr_of(e_cls, 18, 30) == 555 && pr_of(e_cls, 19, 30) == 555 && pr_of(e_cls, 20, 30) == 110 &&
  pr_of(e_cls, 19, 40) == 200 && pr_of(e_one, 19, 30) == 100 && pr_of(e_one, 18, 30) == 555 &&
  pr_of(e_none, 21, 40) == 999 && pr_of(e_none, 20, 40) == 210 &&
  identical(pm_prices(pm_from_grid(pm_to_grid(pmc, "k"), "k", pmc, by_class = TRUE)), pm_prices(pmc))
rec("Check 07: A grid edit sets all whole-cm rows of the same file class and length; a row without a class and a bulk edit change the cell only",
    ok(ok7), sprintf("class 180 length 30: rows 18/19 = %s/%s, class 200 row 20 = %s, bulk edit row 19 = %s, row without class 21 length 40 = %s",
                     pr_of(e_cls, 18, 30), pr_of(e_cls, 19, 30), pr_of(e_cls, 20, 30), pr_of(e_one, 19, 30), pr_of(e_none, 21, 40)))

# ==============================================================================
section("08. The file's own diameter classes")
cls_src <- lapply(xml2::xml_find_all(s3, "//ClassifiedProductDefinition"), function(p)
  as.numeric(xml2::xml_text(xml2::xml_find_all(p, ".//DiameterClass/DiameterClassLowerLimit"))))
cls_out <- lapply(xml2::xml_find_all(d3, "//ClassifiedProductDefinition"), function(p)
  as.numeric(xml2::xml_text(xml2::xml_find_all(p, ".//DiameterClass/DiameterClassLowerLimit"))))
nm_src <- txt_of(s3, "//ClassifiedProductDefinition/ProductName"); nm_out <- txt_of(d3, "//ClassifiedProductDefinition/ProductName")
ok8 <- identical(cls_out[order(nm_out)], cls_src[order(nm_src)]) && any(lengths(cls_src) > 0L) &&
  any(vapply(cls_src, function(x) any(diff(x) > 10), NA))     # a class wider than one cm is kept as one class
rec("Check 08: The exported diameter classes are the file's own classes (a class wider than 1 cm stays one class)",
    ok(ok8), sprintf("source %s | exported %s", paste(vapply(cls_src, paste, "", collapse = "/"), collapse = " ; "),
                     paste(vapply(cls_out, paste, "", collapse = "/"), collapse = " ; ")))

# ==============================================================================
section("12. An invalid ModificationDate")
defs12 <- defs3; i12 <- match(k_s, assortment_table_keys(defs12))
defs12$SourceXml[i12] <- sub("<src>", "<src><md>vasket</md>", defs12$SourceXml[i12], fixed = TRUE)
t0 <- Sys.time(); x12 <- pin_of(defs12, pm3); t1 <- Sys.time()
md12 <- by_name(doc_of(x12$path), "ModificationDate"); v12 <- xsd_check(x12$path, XSD_PIN)
ok12 <- now_ok(md12[["Spruce Sawlog Full"]], t0, t1) && md12[["Pine Pulpwood Full"]] == src_md[["Pine Pulpwood Full"]] &&
  !any(grepl("vasket", readLines(x12$path, warn = FALSE)))
with_xsd("Check 12: A stored ModificationDate that is not a valid xs:dateTime ('vasket') is replaced by the export time; the export is valid",
         ok12, v12, sprintf("dates %s | XSD errors %s %s", paste(md12, collapse = " / "), v12$n, v12$first))

# ==============================================================================
# Private file5
skip5 <- function(n, what) rec(sprintf("Check %d: %s", n, what), REVIEW, sprintf("skipped: file5 (%s) is not available", F5))
if (!have5) {
  skip5(9, "file5 import -> .pin -> import is identical, valid, IDs and dates survive")
  skip5(10, "file5 .hpr export is valid and keeps ProductUserIDs")
  skip5(11, "file5 .pin has the file's diameter classes and cell counts")
  skip5(13, "file5 .pin has exactly the source's diameter classes")
} else {
  h5 <- read_hpr(F5); st5 <- read_hpr_stems(F5)
  map5 <- setNames(guess_hpr_species(st5$species$name, st5$species$user_id), st5$species$key)
  sc5 <- hpr_to_scenario(h5, map5); pm5 <- pm_minimal(sc5$pm)
  x9 <- pin_of(sc5$defs, pm5); d9 <- doc_of(x9$path)
  v9 <- xsd_check(x9$path, XSD_PIN)
  back <- import(x9$path)
  # per assortment (species + name): the sorted cells (file class, length, price, target) must be equal
  cells <- function(defs, pm, k) {
    r <- pm[pm$ProductKey == k, , drop = FALSE]
    fc <- if (PM_FILE_CLASS_COL %in% names(r)) r[[PM_FILE_CLASS_COL]] else rep(NA_real_, nrow(r))
    di <- if (PM_DISTRIBUTION_COL %in% names(r)) r[[PM_DISTRIBUTION_COL]] else rep(NA_real_, nrow(r))
    di[is.na(di)] <- 0
    u <- unique(data.frame(cls = ifelse(is.na(fc), r$PriceDiameterClass_cm * 10, fc), len = r$LengthClass_dm,
                           price = r$PriceNOKperM3, dist = di))
    u[order(u$cls, u$len), , drop = FALSE]
  }
  k5 <- assortment_table_keys(sc5$defs); kb <- assortment_table_keys(back$defs)
  com <- intersect(k5[sc5$defs$Type != "waste"], kb)
  same_cells <- vapply(com, function(k) {
    a <- cells(sc5$defs, pm5, k); b <- cells(back$defs, pm_minimal(back$pm), k)
    nrow(a) == nrow(b) && isTRUE(all.equal(unname(as.matrix(a)), unname(as.matrix(b)), check.attributes = FALSE))
  }, NA)
  fields <- c("MinTopDiameter_cm", "MaxTopDiameter_cm", "MaxButtDiameter_cm", "MinLength_cm", "MaxLength_cm",
              "VolumeDiameterCategory", "DiameterTopPosition_cm", "PermittedGrades", DISTRIBUTION_DEF_COLS)
  same_def <- vapply(com, function(k) isTRUE(all.equal(sc5$defs[match(k, k5), fields], back$defs[match(k, kb), fields],
                                                        check.attributes = FALSE)), NA)
  # The source's own values, from the product texts read_hpr() keeps (not a parse of the 141 MB file).
  src_uid <- sort(vapply(h5$products, function(p) { x <- .hx_text(p$src_uid, "ProductUserID"); Encoding(x) <- "UTF-8"; x }, ""))
  src_uid <- unname(src_uid)
  src_md <- unname(vapply(h5$products, function(p) .hx_text(gsub(">[[:space:]]+<", "><", p$src_cp), "ModificationDate"), ""))
  out_uid <- sort(txt_of(d9, "//ProductDefinition/ProductUserID"))
  out_md <- txt_of(d9, "//ProductDefinition/ClassifiedProductDefinition/ModificationDate")
  # the exported products are the non-waste, non-duplicated ones
  ids_ok <- all(out_uid %in% src_uid | grepl("_[0-9]+$", out_uid)) && !anyDuplicated(out_uid)
  md_ok <- all(out_md %in% src_md)
  ok9 <- length(com) == sum(sc5$defs$Type != "waste") && all(same_cells) && all(same_def) && ids_ok && md_ok
  with_xsd("Check 09: file5 import -> .pin export -> import gives identical prices, targets, classes and definitions; the .pin is valid; ProductUserIDs and ModificationDates survive",
           ok9, v9, sprintf("%d assortments, identical cells %d, identical definitions %d | ids from the source %s, unique | dates unchanged %s | XSD errors %s %s",
                            length(com), sum(same_cells), sum(same_def), ids_ok, md_ok, v9$n, v9$first))

  # 10. the .hpr export of a run
  if (!have_xsd) {
    rec("Check 10: file5 .hpr export is valid and keeps ProductUserIDs", REVIEW, "skipped: the XSD files are not available")
  } else {
    pp5 <- products_from_defs(sc5$defs, pm5)
    pmf <- tempfile(fileext = ".rds"); saveRDS(pm5, pmf)
    r5 <- run_hpr_bucking(hpr_stem_profiles(st5, map5), price_matrix_path = pmf, products = pp5$pd, length_classes = pp5$lc)
    sbn5 <- setNames(unname(map5), tolower(trimws(st5$species$name)))
    H <- tempfile(fileext = ".hpr")
    xh <- hpr_export_result(H, data.frame(path = F5, name = "file5.hpr"), sbn5, r5, sc5$defs, pm5)
    vh <- if (isTRUE(xh$ok)) xsd_check(H, XSD_HPR) else list(ok = FALSE, n = NA_integer_, first = paste(xh$errors, collapse = " "))
    hid <- if (isTRUE(xh$ok)) {
      raw <- readBin(H, "raw", 3e6); tx <- iconv(rawToChar(raw[raw != as.raw(0L)]), "UTF-8", "UTF-8", sub = "")
      sort(sub(">.*$", "", sub("^<ProductUserID[^>]*>", "", regmatches(tx, gregexpr("<ProductUserID[^>]*>[^<]*", tx))[[1L]])))
    } else character(0)
    # every imported product's ProductUserID is in the file (the waste rows are optBuck's own: neutral IDs)
    want <- sort(unlist(lapply(sc5$defs$SourceXml[!is.na(sc5$defs$SourceXml)], function(x) {
      e <- source_xml_part(x, "uid"); if (nzchar(e)) { u <- .hx_text(e, "ProductUserID"); Encoding(u) <- "UTF-8"; u } else NULL })))
    ok10 <- isTRUE(xh$ok) && length(want) > 0L && all(want %in% hid) && !anyDuplicated(hid)
    with_xsd("Check 10: A file5 run's .hpr export is valid against the .hpr schema and keeps the ProductUserIDs",
             ok10, vh, sprintf("%s stems, %s logs | %d ProductUserIDs, the %d imported products' IDs all there %s, unique %s | XSD errors %s %s",
                               xh$n_stems, xh$n_logs, length(hid), length(want), all(want %in% hid), !anyDuplicated(hid), vh$n, vh$first))
    unlink(H)
  }

  # 11. the exported classes are the file's own classes (by ProductUserID). A class that is
  # forbidden in every cell is narrowed away by the import (documented), so the classes
  # with a priced cell must all be there and no other class may be.
  src_cls <- lapply(h5$products, function(p) {
    c_ok <- p$cells[!is.na(p$cells$price) & !(p$cells$criteria %in% HPR_UNAVAILABLE), , drop = FALSE]
    pr <- sort(unique(c_ok$d_mm)); all_c <- sort(p$dia_classes)
    # Whole-cm rows: a class needs a multiple of 10 mm inside it (and not above the max top diameter), else the
    # import has no row for it (documented; a class narrower than 1 cm or above the limit).
    nxt <- c(all_c[-1L], Inf); mx <- if (is.na(p$dia_class_max)) Inf else p$dia_class_max
    first10 <- ceiling(pr / 10) * 10
    has_row <- first10 < nxt[match(pr, all_c)] & first10 <= mx
    list(all = all_c, priced = pr[has_row])
  })
  names(src_cls) <- vapply(h5$products, function(p) { x <- .hx_text(p$src_uid, "ProductUserID"); Encoding(x) <- "UTF-8"; x }, "")
  pd9 <- xml2::xml_find_all(d9, "//ProductDefinition")
  out_cls <- lapply(pd9, function(p) as.numeric(xml2::xml_text(xml2::xml_find_all(p, ".//DiameterClass/DiameterClassLowerLimit"))))
  out_n <- vapply(pd9, function(p) length(xml2::xml_find_all(p, ".//ProductMatrixItem")), 0L)
  out_len <- vapply(pd9, function(p) length(xml2::xml_find_all(p, ".//LengthClass")), 0L)
  names(out_cls) <- xml2::xml_text(xml2::xml_find_first(pd9, "./ProductUserID"))
  common <- intersect(names(src_cls)[!duplicated(names(src_cls))], names(out_cls))
  eq_cls <- vapply(common, function(n) all(src_cls[[n]]$priced %in% out_cls[[n]]) && all(out_cls[[n]] %in% src_cls[[n]]$all), NA)
  full <- vapply(common, function(n) out_n[[match(n, names(out_cls))]] == length(out_cls[[n]]) * out_len[[match(n, names(out_cls))]], NA)
  rec("Check 11: The exported file5 .pin has the file's own diameter classes (all classes with a priced cell, none that the file lacks) and a cell for every class and length",
      ok(length(common) >= 15L && all(eq_cls) && all(full)),
      sprintf("%d products compared by ProductUserID, same classes %d, a cell per class and length %d", length(common), sum(eq_cls), sum(full)))
}

if (have5) {
  # 13. every source diameter class is written (also those without a whole-cm row), none else
  src13 <- lapply(h5$products, function(p) sort(unique(c(p$dia_classes, p$cells$d_mm[!is.na(p$cells$d_mm)]))))
  names(src13) <- vapply(h5$products, function(p) { x <- .hx_text(p$src_uid, "ProductUserID"); Encoding(x) <- "UTF-8"; x }, "")
  com13 <- intersect(names(src13)[!duplicated(names(src13))], names(out_cls))
  eq13 <- vapply(com13, function(n) identical(as.numeric(src13[[n]]), as.numeric(sort(out_cls[[n]]))), NA)
  rec("Check 13: The exported file5 .pin has exactly the source's diameter class lower limits for every product (also classes without a whole-cm row); valid",
      if (!length(com13) || !all(eq13)) FAIL else if (is.na(v9$ok)) REVIEW else ok(v9$ok),
      sprintf("%d products compared, equal %d | %s | XSD errors %s %s", length(com13), sum(eq13),
              if (all(eq13)) "all equal" else paste("different:", paste(names(eq13)[!eq13], collapse = ", ")), v9$n, v9$first))
}

write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 35 complete.\n")
