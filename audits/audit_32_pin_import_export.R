# ==============================================================================
# audits/audit_32_pin_import_export.R
#
# Purpose : Phase 5b (2026-10-29): products and prices from StanForD 2010 .pin
#           files (ProductInstruction) into a scenario, and a scenario exported
#           as .pin. R/helpers_hpr.R (read_pin(), read_product_file(),
#           hpr_to_scenario()), R/helpers_pin_export.R, hpr_export_products(
#           style = "pin"). Reads only the synthetic fixtures
#           inputs/test_fixtures/pin_minimal_synthetic.pin and
#           pin_full_synthetic.pin (scripts/make_pin_test_fixture.R) and
#           variants of them written to tempfiles. No real file is used.
#
# Checks  :
#  1.  Both layouts import (minimal and full): assortments, limits, lengths,
#      volume methods, prices
#  2.  Key fallbacks: ProductUserID, else ProductName, else _2, _3
#  3.  BuckingCriteria: "Only logs bucked manually allowed" and "Forbidden to
#      buck the log even manually" cells are left empty, "The log is excluded
#      from apportionment bucking, only bucking according to value allowed"
#      stays priced; the same for an .hpr (fixture)
#  4.  Species guess from SpeciesGroupUserID; an unknown ID is not imported by
#      default
#  5.  A missing PriceDefinition gives "Top cylinder" with no message; a
#      missing DiameterMINTop gives the smallest diameter class
#  6.  Permitted grades fill PermittedGrades; ReferenceGrades and HprProductKey
#      stay NA for .pin products (and are still set for an .hpr)
#  7.  Export -> import round trip reproduces assortments, classes and prices
#      (the documented differences: Top cylinder is written as Top)
#  8.  The export: no waste rows, unique ProductUserIDs (spaces as underscores),
#      PermittedGradesDefinition only with grades, SpeciesGroupUserID, fixed
#      minimal header, no ProductKey or SpeciesGroupKey
#  9.  Privacy: invented coordinates in an input .pin reach no output; the
#      scan deletes an export that holds a coordinate element
# 10.  The dispatcher decides by root element; a .pin is never kept for the
#      ".hpr stems" source (app wiring; audit 17 check 42 drives it)
# 11.  Errors: wrong root, units, message type, no products, not XML; their
#      texts translate to Norsk
# 12.  .hpr import and export are unchanged (audits 25 and 31 pass; here: the
#      default .hpr import still keys products by ProductKey)
#
# Verdict : PASS / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/pin_import_export/audit_32_pin_import_export.csv
#   outputs/pin_import_export/audit_32_pin_import_export.txt
#
# Usage   : Rscript audits/audit_32_pin_import_export.R
# ==============================================================================

root_dir <- local({
  # APTER_ROOT -> walk up from getwd() -> stop()
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  env <- Sys.getenv("APTER_ROOT", unset = "")
  if (nzchar(env)) {
    d <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (!is_root(d))
      stop("Project root '", d, "' (from APTER_ROOT) does not contain ",
           "R/helpers_paths.R.", call. = FALSE)
  } else {
    d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    while (!is_root(d) && dirname(d) != d) d <- dirname(d)
    if (!is_root(d))
      stop("Cannot locate the project root: set APTER_ROOT, or run from ",
           "inside the project directory.", call. = FALSE)
  }
  d
})
Sys.setenv(APTER_ROOT = root_dir)

source(file.path(root_dir, "R", "helpers_paths.R"))
source(file.path(root_dir, "R", "helpers_audit.R"))
source(file.path(root_dir, "scripts", "app_backend",
                 "01_run_sampled_plot_bucking.R"), local = FALSE)
if (!exists("tr_msg", mode = "function")) source(file.path(root_dir, "R", "helpers_i18n.R"))

out_dir <- path_output("pin_import_export", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_32_pin_import_export.txt")
audit_csv <- file.path(out_dir, "audit_32_pin_import_export.csv")
init_audit_log(audit_txt)

E <- new.env(parent = emptyenv())
rec <- function(check, result, detail = "")
  record_check(E, check, result, detail, log_path = audit_txt)
PASS <- "PASS"; FAIL <- "FAIL"
ok <- function(x) if (isTRUE(x)) PASS else FAIL
section <- function(title) {
  cat(sprintf("\n--- %s ---\n", title), file = audit_txt, append = TRUE)
  cat(sprintf("\n--- %s ---\n", title))
}
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)
cat("  Audit: .pin import and export (Phase 5b)\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

FX <- file.path(root_dir, "inputs", "test_fixtures")
MINI <- file.path(FX, "pin_minimal_synthetic.pin")
FULL <- file.path(FX, "pin_full_synthetic.pin")
HPRFX <- file.path(FX, "hpr_example_synthetic.hpr")
rd <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
variant <- function(f, fn, ext = ".pin") {                 # a changed copy in a tempfile
  p <- tempfile(fileext = ext); con <- file(p, "wb"); writeBin(charToRaw(enc2utf8(fn(rd(f)))), con); close(con); p
}
guess_map <- function(h) setNames(guess_hpr_species(h$species$name, h$species$user_id), h$species$key)
import <- function(f, keep = NULL) {
  h <- read_product_file(f); s <- hpr_to_scenario(h, guess_map(h), keep)
  list(h = h, s = s)
}
grid_of <- function(s, name) pm_to_grid(s$pm, assortment_table_keys(s$defs)[match(name, s$defs$Name)])
row_of <- function(s, name) s$defs[match(name, s$defs$Name), ]

mini <- import(MINI); full <- import(FULL)

# ==============================================================================
section("01. Both layouts import")
names_mini <- c("Pine Sawlog", "Pine Pulpwood", "Spruce Sawlog", "Spruce Pulpwood")
names_full <- c("Pine Pulpwood Full", "Spruce Sawlog Full")
g_ps <- grid_of(mini$s, "Pine Sawlog")
r_ps <- row_of(mini$s, "Pine Sawlog")
g_pu <- grid_of(full$s, "Pine Pulpwood Full")
g_ss <- grid_of(full$s, "Spruce Sawlog Full")
r_ss <- row_of(full$s, "Spruce Sawlog Full")
ok1a <- isTRUE(mini$s$ok) && identical(mini$h$kind, "pin") &&
  setequal(mini$s$defs$Name[mini$s$defs$Type != "waste"], names_mini) &&
  sum(mini$s$defs$Type == "waste") == 2L &&
  r_ps$MinLength_cm == 310 && r_ps$MaxLength_cm == 430 && r_ps$MaxTopDiameter_cm == 50 &&
  g_ps["18", "31"] == 62 && g_ps["25", "43"] == 72 && g_ps["32", "43"] == 74
ok1b <- isTRUE(full$s$ok) && setequal(full$s$defs$Name[full$s$defs$Type != "waste"], names_full) &&
  r_ss$MinTopDiameter_cm == 18 && r_ss$MaxTopDiameter_cm == 60 && r_ss$MaxButtDiameter_cm == 70 &&
  r_ss$VolumeDiameterCategory == "Top" &&
  row_of(full$s, "Pine Pulpwood Full")$VolumeDiameterCategory == "All diameters (solid volume)" &&
  row_of(full$s, "Pine Pulpwood Full")$DiameterTopPosition_cm == 10 &&
  g_pu["5", "33"] == 40 + 5 && g_pu["7", "36"] == 65 && g_ss["18", "31"] == 600 && g_ss["32", "43"] == 800
rec("Check 01: Both layouts import (minimal and full): assortments, limits, lengths, volume methods and prices",
    ok(ok1a && ok1b), sprintf("minimal %s (%d rows, Pine Sawlog %g-%g cm, price 18/31 = %s) | full %s (%d rows, Spruce Sawlog Full min top %g, max top %g, max butt %g, method %s)",
                              ok1a, nrow(mini$s$defs), r_ps$MinLength_cm, r_ps$MaxLength_cm, g_ps["18", "31"],
                              ok1b, nrow(full$s$defs), r_ss$MinTopDiameter_cm, r_ss$MaxTopDiameter_cm,
                              r_ss$MaxButtDiameter_cm, r_ss$VolumeDiameterCategory))

# ==============================================================================
section("02. Key fallbacks")
no_uid <- variant(MINI, function(t) sub("<ProductUserID agency=\"Sample\">PS1</ProductUserID>", "", t, fixed = TRUE))
rep_uid <- variant(MINI, function(t) sub("PP1</ProductUserID>", "PS1</ProductUserID>", t, fixed = TRUE))
same_name <- variant(MINI, function(t) {
  t <- gsub("<ProductUserID agency=\"Sample\">[A-Z0-9]+</ProductUserID>", "", t)   # no IDs at all
  gsub("<ProductName>Spruce (Sawlog|Pulpwood)</ProductName>", "<ProductName>Pine Sawlog</ProductName>", t)
})
k_of <- function(f) names(read_product_file(f)$products)
k_no <- k_of(no_uid); k_rep <- k_of(rep_uid); k_same <- k_of(same_name)
ok2 <- identical(k_of(MINI), c("PS1", "PP1", "SS1", "SP1")) &&
  identical(k_no, c("Pine Sawlog", "PP1", "SS1", "SP1")) &&
  identical(k_rep, c("Pine Sawlog", "Pine Pulpwood", "SS1", "SP1")) &&
  identical(k_same, c("Pine Sawlog", "Pine Pulpwood", "Pine Sawlog_2", "Pine Sawlog_3"))
rec("Check 02: ProductUserID is the key; a missing or repeated one falls back to ProductName, a repeated name gets _2, _3",
    ok(ok2), sprintf("normal %s | no ID: %s | repeated ID: %s | no IDs, three equal names: %s",
                     paste(k_of(MINI), collapse = ","), paste(k_no, collapse = ","),
                     paste(k_rep, collapse = ","), paste(k_same, collapse = ",")))

# ==============================================================================
section("03. BuckingCriteria")
hp_full <- full$h
tab_f <- hpr_product_table(hp_full, guess_map(hp_full))
pulp_cells <- hp_full$products[["PULP_Pine"]]$cells
saw_cells <- hp_full$products[["SAW_Spruce"]]$cells
ok3a <- all(tab_f$forbidden == 1L) &&                                    # one unavailable cell in each product
  g_pu["3", "29"] %in% NA && !is.na(g_pu["3", "33"]) &&                  # (30 mm, 290 cm) manual: empty
  !is.na(g_pu["5", "36"]) && g_pu["5", "36"] == 50 &&                    # (50 mm, 360 cm) excluded from apportionment: priced
  is.na(g_ss["32", "31"]) && g_ss["32", "43"] == 800                     # (320 mm, 310 cm) forbidden: empty
# The same on an .hpr (the fixture has manual-only cells in several products).
hp <- read_hpr(HPRFX)
map_h <- guess_map(hp)
tab_h <- hpr_product_table(hp, map_h)
n_unavail <- vapply(hp$products, function(p) sum(p$cells$criteria %in% c(HPR_FORBIDDEN, HPR_MANUAL)), 0L)
n_forb <- vapply(hp$products, function(p) sum(p$cells$criteria == HPR_FORBIDDEN), 0L)
n_man <- vapply(hp$products, function(p) sum(p$cells$criteria == HPR_MANUAL), 0L)
ok3b <- identical(unname(tab_h$forbidden), unname(n_unavail[tab_h$key])) && sum(n_man) > 0L
sh <- hpr_to_scenario(hp, map_h)
k8019 <- assortment_table_keys(sh$defs)
ok3c <- isTRUE(sh$ok)
rec("Check 03: Manual-only and forbidden cells are left empty, 'excluded from apportionment' stays priced (.pin and .hpr)",
    ok(ok3a && ok3b && ok3c),
    sprintf(".pin: unavailable cells per product %s, manual cell empty %s, excluded cell priced %s, forbidden cell empty %s | .hpr fixture: %d manual-only cells in %d products, %d forbidden, product table counts both %s",
            paste(tab_f$forbidden, collapse = "/"), is.na(g_pu["3", "29"]), !is.na(g_pu["5", "36"]), is.na(g_ss["32", "31"]),
            sum(n_man), sum(n_man > 0L), sum(n_forb), ok3b))

# ==============================================================================
section("04. Species guess")
sp_full <- guess_map(full$h); sp_mini <- guess_map(mini$h)
unk <- variant(MINI, function(t) sub("<SpeciesGroupUserID agency=\"Sample\">Spruce</SpeciesGroupUserID>",
                                      "<SpeciesGroupUserID agency=\"Sample\">SE1_Birk</SpeciesGroupUserID>", t, fixed = TRUE))
hu <- read_product_file(unk); tu <- hpr_product_table(hu, guess_map(hu))
ok4 <- identical(unname(sp_full[c("SE1_Tall_SoundKnot", "SE1_Gran_Sound")]), c("pine", "spruce")) &&
  identical(unname(sp_mini[c("Pine", "Spruce")]), c("pine", "spruce")) &&
  identical(unname(guess_map(hu)["SE1_Birk"]), "none") &&
  all(!tu$default[tu$species == "none"]) && any(tu$species == "none")
rec("Check 04: The species is guessed from SpeciesGroupUserID; an unknown ID is not imported by default",
    ok(ok4), sprintf("full: %s | minimal: %s | unknown ID: %s, default ticks %s",
                     paste(sp_full, collapse = "/"), paste(sp_mini, collapse = "/"),
                     paste(guess_map(hu), collapse = "/"), paste(tu$default, collapse = "/")))

# ==============================================================================
section("05. Missing PriceDefinition and DiameterMINTop")
mw <- c(mini$s$warnings, mini$s$info)
ok5 <- all(mini$s$defs$VolumeDiameterCategory[mini$s$defs$Type != "waste"] == "Top cylinder") &&
  !any(grepl("Volume", c(mini$s$warnings, mini$s$info), fixed = TRUE)) && length(mini$s$warnings) == 0L &&
  r_ps$MinTopDiameter_cm == 18 &&                                         # smallest diameter class (180 mm)
  row_of(mini$s, "Pine Pulpwood")$MinTopDiameter_cm == 6 &&
  row_of(full$s, "Pine Pulpwood Full")$MinTopDiameter_cm == 3             # 30 mm: no DiameterMINTop in the product
rec("Check 05: A missing PriceDefinition gives 'Top cylinder' with no message; a missing DiameterMINTop gives the smallest class",
    ok(ok5), sprintf("methods %s | warnings %d, volume infos %d | min tops %g / %g / %g cm",
                     paste(unique(mini$s$defs$VolumeDiameterCategory[mini$s$defs$Type != "waste"]), collapse = ","),
                     length(mini$s$warnings), sum(grepl("Volume", mw, fixed = TRUE)),
                     r_ps$MinTopDiameter_cm, row_of(mini$s, "Pine Pulpwood")$MinTopDiameter_cm,
                     row_of(full$s, "Pine Pulpwood Full")$MinTopDiameter_cm))

# ==============================================================================
section("06. Grades and hidden columns")
pd <- full$s$defs[full$s$defs$Type != "waste", ]
sh_defs <- sh$defs[sh$defs$Type != "waste", ]
ok6 <- identical(row_of(full$s, "Pine Pulpwood Full")$PermittedGrades, "1;2;3;4;5;6;7") &&
  identical(row_of(full$s, "Spruce Sawlog Full")$PermittedGrades, "1;2") &&
  all(is.na(mini$s$defs$PermittedGrades)) &&
  all(is.na(c(full$s$defs$ReferenceGrades, mini$s$defs$ReferenceGrades))) &&
  all(is.na(c(full$s$defs$HprProductKey, mini$s$defs$HprProductKey))) &&
  all(!is.na(sh_defs$HprProductKey))                                      # an .hpr import still sets the key
rec("Check 06: Permitted grades fill PermittedGrades; ReferenceGrades and HprProductKey stay NA for .pin products",
    ok(ok6), sprintf("grades %s | %s | reference NA %s, key NA %s | .hpr import keys set %s",
                     row_of(full$s, "Pine Pulpwood Full")$PermittedGrades, row_of(full$s, "Spruce Sawlog Full")$PermittedGrades,
                     all(is.na(full$s$defs$ReferenceGrades)), all(is.na(full$s$defs$HprProductKey)),
                     all(!is.na(sh_defs$HprProductKey))))

# ==============================================================================
section("07. Export -> import round trip")
round_trip <- function(s) {
  f <- tempfile(fileext = ".pin"); x <- pin_export_scenario(s$defs, s$pm, f)
  if (!isTRUE(x$ok)) return(list(ok = FALSE, msg = x$errors))
  h2 <- read_product_file(f); s2 <- hpr_to_scenario(h2, guess_map(h2))
  if (!isTRUE(s2$ok)) return(list(ok = FALSE, msg = s2$errors))
  a <- s$defs[s$defs$Type != "waste", ]; b <- s2$defs[s2$defs$Type != "waste", ]
  ka <- assortment_table_keys(a); kb <- assortment_table_keys(b)
  b <- b[match(ka, kb), ]
  cols <- c("Name", "Species", "Type", "MinTopDiameter_cm", "MaxTopDiameter_cm", "MaxButtDiameter_cm",
            "MinLength_cm", "MaxLength_cm", "DiameterTopPosition_cm", "PermittedGrades")
  a$VolumeDiameterCategory <- sub("^Top cylinder$", "Top", a$VolumeDiameterCategory)   # the documented difference
  eq_defs <- isTRUE(all.equal(a[, c(cols, "VolumeDiameterCategory")], b[, c(cols, "VolumeDiameterCategory")],
                              check.attributes = FALSE))
  # Matrix-priced assortments only: a flat price is written as a one-class matrix (documented).
  mk <- ka[as.character(a$PriceType) == "matrix"]
  eq_pm <- length(mk) > 0L && all(vapply(mk, function(k) same_prices(s$pm, s2$pm, k), NA))
  list(ok = !anyNA(b$Name) && eq_defs && eq_pm && isTRUE(s2$ok), defs = eq_defs, prices = eq_pm,
       msg = if (!eq_defs) all.equal(a[, cols], b[, cols], check.attributes = FALSE) else "")
}
rt_mini <- round_trip(mini$s); rt_full <- round_trip(full$s)
dflt <- list(defs = defs_from_products(load_products()), pm = pm_minimal(load_price_matrix_long(root_dir)))
rt_dflt <- round_trip(dflt)
rec("Check 07: Export -> import reproduces assortments, classes and prices (Top cylinder is written as Top)",
    ok(rt_mini$ok && rt_full$ok && rt_dflt$ok),
    sprintf("minimal: defs %s prices %s | full: defs %s prices %s | default scenario: defs %s prices %s %s",
            rt_mini$defs, rt_mini$prices, rt_full$defs, rt_full$prices, rt_dflt$defs, rt_dflt$prices,
            paste(utils::head(unlist(rt_dflt$msg), 2), collapse = " ")))

# ==============================================================================
section("08. The exported file")
OUTP <- tempfile(fileext = ".pin")
x8 <- pin_export_scenario(full$s$defs, full$s$pm, OUTP)
d8 <- xml2::read_xml(OUTP); xml2::xml_ns_strip(d8)
txt8 <- rd(OUTP)
hdr <- xml2::xml_name(xml2::xml_children(xml2::xml_find_first(d8, "/ProductInstruction/ProductInstructionHeader")))
uids <- xml2::xml_text(xml2::xml_find_all(d8, "/ProductInstruction/ProductDefinition/ProductUserID"))
sg <- xml2::xml_text(xml2::xml_find_all(d8, "//SpeciesGroupUserID"))
n_grades <- length(xml2::xml_find_all(d8, "//PermittedGradesDefinition"))
x8b <- pin_export_scenario(mini$s$defs, mini$s$pm, tempfile(fileext = ".pin"))
d8b <- xml2::read_xml(x8b$path); xml2::xml_ns_strip(d8b)
# Two assortments named alike, one with a space, and a waste row.
dd <- dflt$defs
dup <- rbind(dd[dd$Type != "waste", ][1:2, ], dd[dd$Type == "waste", ][1, ])
dup$Name[1:2] <- "Same name"; dup$Species[2] <- "spruce"
ids_dup <- pin_product_ids(dup$Name[dup$Type != "waste"])
# R4 D6: an imported product keeps its own ProductUserID, SpeciesGroupUserID and metadata (buyer, Extension, ...);
# the header is Apter's own with the elements the schema requires.
ok8 <- isTRUE(x8$ok) && x8$n_products == 2L && setequal(uids, c("PULP_Pine", "SAW_Spruce")) &&
  identical(hdr, c("SenderApplication", "CreationDate", "ModificationDate", "ApplicationVersionCreated",
                   "ApplicationVersionModified", "CountryCode")) &&
  identical(xml2::xml_text(xml2::xml_find_first(d8, "//SenderApplication")), "optBuck") &&
  xml2::xml_attr(d8, "messageType") == "pin" &&
  n_grades == 2L && length(xml2::xml_find_all(d8, "//PermittedGradeNumber")) > 0L &&
  length(xml2::xml_find_all(d8b, "//PermittedGradeNumber")) == 0L &&   # the element is required; its grades only when present
  setequal(sg, c("SE1_Tall_SoundKnot", "SE1_Gran_Sound")) &&   # the source's own SpeciesGroupUserIDs (R4 D6)
  !grepl("<ProductKey>|<SpeciesGroupKey>", txt8) && grepl("<ProductBuyer>", txt8, fixed = TRUE) && grepl("<Extension>", txt8, fixed = TRUE) &&
  !any(grepl("^Waste", xml2::xml_text(xml2::xml_find_all(d8, "//ProductName")))) &&
  identical(ids_dup, c("Same_name", "Same_name_2")) &&
  identical(pin_product_ids(c("A b", "A_b", "A b")), c("A_b", "A_b_2", "A_b_3"))
rec("Check 08: The export has no waste rows, unique ProductUserIDs (the source's own), grades only when present, SpeciesGroupUserID, the source's metadata and a fixed header with the elements the schema requires",
    ok(ok8), sprintf("ids %s | header %s | grade blocks %d (full) / %d (minimal) | species %s | duplicates %s",
                     paste(uids, collapse = ","), paste(hdr, collapse = "/"), n_grades,
                     length(xml2::xml_find_all(d8b, "//PermittedGradesDefinition")), paste(sg, collapse = "/"),
                     paste(ids_dup, collapse = ",")))

# ==============================================================================
section("09. Privacy")
LAT <- "61.123456"; LON <- "11.654321"
coords <- variant(FULL, function(t) {
  t <- sub("<CountryCode>", paste0('<CreationCoordinates receiverPosition="Base machine position" coordinateReferenceSystem="WGS84">',
                                   '<Latitude latitudeCategory="North">', LAT, '</Latitude><Longitude longitudeCategory="East">', LON,
                                   "</Longitude><Altitude>123</Altitude></CreationCoordinates><CountryCode>"), t, fixed = TRUE)
  sub("<ProductBuyer>", "<ProductBuyer><BusinessID>INVENTED-BUYER-ID</BusinessID>", t, fixed = TRUE)
})
ic <- import(coords)
f9 <- tempfile(fileext = ".pin"); x9 <- pin_export_scenario(ic$s$defs, ic$s$pm, f9)
t9 <- rd(f9)
leak <- grepl(paste(c(LAT, LON, "Latitude", "Longitude", "Altitude", "CreationCoordinates"), collapse = "|"), t9)
# The invented buyer ID sits in a product's own metadata, which an export keeps (R4 D6); the header is never read.
keep_cols <- setdiff(names(full$s$defs), "SourceXml")
same_as_plain <- identical(ic$s$defs[, keep_cols], full$s$defs[, keep_cols]) && identical(ic$s$pm, full$s$pm) &&
  grepl("INVENTED-BUYER-ID", ic$s$defs$SourceXml[1L], fixed = TRUE)
# The scan removes a file that holds a coordinate element.
real_scan <- .hpr_export_has_coordinates
assign(".hpr_export_has_coordinates", function(path, ...) TRUE, envir = globalenv())
f9b <- tempfile(fileext = ".pin"); x9b <- pin_export_scenario(full$s$defs, full$s$pm, f9b)
assign(".hpr_export_has_coordinates", real_scan, envir = globalenv())
scan_txt <- tempfile(fileext = ".pin"); writeLines("<A><Latitude>1</Latitude></A>", scan_txt)
ok9 <- isTRUE(x9$ok) && !leak && same_as_plain && !isTRUE(x9b$ok) && !file.exists(f9b) &&
  real_scan(scan_txt) && !real_scan(f9)
rec("Check 09: Invented header coordinates in an input .pin reach no output (a product's own metadata is kept); the scan deletes an export with a coordinate element",
    ok(ok9), sprintf("import equals the plain import %s | export holds coordinates: %s | scan failure removes the file %s (%s) | scan finds a coordinate element %s, none in the export %s",
                     same_as_plain, leak, !file.exists(f9b), paste(x9b$errors, collapse = " "), real_scan(scan_txt), !real_scan(f9)))

# ==============================================================================
section("10. Dispatcher and app wiring")
as_pin_name <- variant(HPRFX, function(t) t, ext = ".pin")           # an .hpr under a .pin name
as_hpr_name <- variant(MINI, function(t) t, ext = ".hpr")            # a .pin under an .hpr name
src <- paste(readLines(file.path(root_dir, "app", "server_scenarios.R"), warn = FALSE), collapse = "\n")
ui_src <- paste(readLines(file.path(root_dir, "app", "app_ui.R"), warn = FALSE), collapse = "\n")
ok10 <- identical(read_product_file(MINI)$kind, "pin") && identical(read_product_file(as_hpr_name)$kind, "pin") &&
  identical(read_product_file(HPRFX)$kind, "hpr") && identical(read_product_file(as_pin_name)$kind, "hpr") &&
  grepl("if (!identical(data$kind, \"pin\")) hpr_kept(", src, fixed = TRUE) && !grepl("hpr_imported", src, fixed = TRUE) &&
  grepl('accept = c(".xlsx", ".csv", ".hpr", ".pin", ".gz", ".zip")', ui_src, fixed = TRUE)
rec("Check 10: The root element decides; a .pin is not kept for the .hpr stems source (audit 17 check 42 drives the app)",
    ok(ok10), sprintf("kinds: pin %s, pin as .hpr %s, hpr %s, hpr as .pin %s | app: pin branch present %s, hpr_imported gone %s",
                      read_product_file(MINI)$kind, read_product_file(as_hpr_name)$kind, read_product_file(HPRFX)$kind,
                      read_product_file(as_pin_name)$kind, grepl("identical(data$kind, \"pin\")", src, fixed = TRUE),
                      length(gregexpr("hpr_imported(list(", src, fixed = TRUE)[[1L]]) == 1L))

# ==============================================================================
section("11. Errors")
err_of <- function(t, ext = ".pin") { f <- tempfile(fileext = ext); writeLines(t, f, useBytes = TRUE)
  h <- read_product_file(f); if (isTRUE(h$ok)) "(no error)" else paste(h$errors, collapse = " ") }
e <- c(
  root = err_of('<Other xmlns="urn:skogforsk:stanford2010"><A/></Other>'),
  dunit = err_of(sub('diameterUnit="mm"', 'diameterUnit="cm"', rd(MINI), fixed = TRUE)),
  lunit = err_of(sub('lengthUnit="cm"', 'lengthUnit="m"', rd(MINI), fixed = TRUE)),
  msgtype = err_of(sub('messageType="pin"', 'messageType="hpr"', rd(MINI), fixed = TRUE)),
  noprod = err_of(sub("(?s)<ProductDefinition>.*</ProductDefinition>", "", rd(MINI), perl = TRUE)),
  notxml = err_of("this is not xml <<"))
want <- c(root = "expected HarvestedProduction or ProductInstruction", dunit = "Diameter unit", lunit = "Length unit",
          msgtype = "not pin", noprod = "no products", notxml = "not a readable XML")
hit <- vapply(names(want), function(n) grepl(want[[n]], e[[n]], fixed = TRUE), NA)
i18n_reset_misses()
tr_e <- tr_msg(unname(e), "no")
ok11 <- all(hit) && !length(i18n_misses()) && all(tr_e != e)
rec("Check 11: Errors block the import (wrong root, units, message type, no products, not XML) and translate to Norsk",
    ok(ok11), paste(sprintf("%s: %s", names(want), ifelse(hit, "ok", e[names(want)])), collapse = " | "))

# ==============================================================================
section("12. .hpr unchanged")
keys_h <- names(read_product_file(HPRFX)$products)
ok12 <- identical(read_product_file(HPRFX)$kind, "hpr") && identical(keys_h, names(hp$products)) &&
  all(grepl("^[0-9]+$", keys_h)) && all(!is.na(sh$defs$HprProductKey[sh$defs$Type != "waste"]))
rec("Check 12: An .hpr is still read by ProductKey and its assortments keep HprProductKey",
    ok(ok12), sprintf("%d product keys, all numeric %s", length(keys_h), all(grepl("^[0-9]+$", keys_h))))

# ==============================================================================
section("13. Files under 512 bytes")
tiny_pin <- paste0('<ProductInstruction messageType="pin" diameterUnit="mm" lengthUnit="cm"><ProductDefinition>',
                   '<ClassifiedProductDefinition><ProductName>P</ProductName><SpeciesGroupUserID>Pine</SpeciesGroupUserID>',
                   '<ProductMatrixes><ProductMatrixItem diameterClassLowerLimit="180" lengthClassLowerLimit="310">',
                   '<Price>62</Price></ProductMatrixItem></ProductMatrixes></ClassifiedProductDefinition></ProductDefinition></ProductInstruction>')
tiny_root <- '<Other xmlns="urn:skogforsk:stanford2010"><A/></Other>'
f13a <- tempfile(fileext = ".pin"); writeBin(charToRaw(tiny_pin), f13a)
f13b <- tempfile(fileext = ".pin"); writeBin(charToRaw(tiny_root), f13b)
t13 <- import(f13a)
e13 <- read_product_file(f13b)
g13 <- grid_of(t13$s, "P")
ok13 <- file.size(f13a) < 512L && file.size(f13b) < 512L && isTRUE(t13$h$ok) && identical(t13$h$kind, "pin") &&
  identical(names(t13$h$products), "P") && nrow(t13$s$defs) == 2L &&   # the product + the base waste row
  isTRUE(all(g13[!is.na(g13)] == 62)) && sum(!is.na(g13)) == 1L &&
  !isTRUE(e13$ok) && any(grepl("expected HarvestedProduction or ProductInstruction", e13$errors, fixed = TRUE)) &&
  !any(grepl("not a readable XML", e13$errors, fixed = TRUE))
rec("Check 13: A valid .pin under 512 bytes imports; a wrong-root file under 512 bytes gives the wrong-root error",
    ok(ok13), sprintf(".pin %d bytes, ok %s, %d assortments, priced cells %d | wrong root %d bytes: %s",
                      file.size(f13a), t13$h$ok, nrow(t13$s$defs), sum(!is.na(g13)), file.size(f13b), paste(e13$errors, collapse = " ")))

unlink(c(f13a, f13b, no_uid, rep_uid, same_name, unk, coords, OUTP, f9, as_pin_name, as_hpr_name))
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 32 complete.\n")