# ==============================================================================
# audits/audit_41_example_hpr.R  (2026-11-09)
#
# Purpose : The anonymised example file inst/extdata/example.hpr (made by scripts/make_example_hpr.R from one of
#           the owner's real files): it is small, valid, anonymous and usable in the package.
#
# Checks  :
#  1.  The file exists and is at most 5,000,000 bytes
#  2.  It is well-formed XML; it has no coordinate elements (StemCoordinates, Latitude, Longitude, Altitude)
#  3.  Identity fields (machine, owner, contractor, operator, object, sub-object, organisation, buyer, destination)
#      hold only neutral placeholders, empty values or numbers
#  4.  read_hpr(), read_hpr_stems() and read_hpr_logs() work; every classified product has at least one recorded log
#  5.  bucking_check() works and gives a yield index between 0.5 and 1.1 (stems checked > 0)
#  6.  The file is valid against the StanForD 2010 HarvestedProduction schema 3.1 (REVIEW when the schema is missing)
#  7.  The scripted search for the original's identifying strings (made when the file was built, in the private
#      folder) found none (REVIEW when that record is missing)
#
# Verdict : PASS / ACCEPTED WITH REVIEW / FAIL (helpers_audit.R)
# Outputs : outputs/example_hpr/audit_41_example_hpr.{csv,txt}
# Usage   : Rscript audits/audit_41_example_hpr.R
# ==============================================================================

root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  env <- Sys.getenv("OPTBUCK_ROOT", unset = "")
  if (nzchar(env)) {
    d <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (!is_root(d)) stop("Project root '", d, "' (from OPTBUCK_ROOT) does not contain R/helpers_paths.R.", call. = FALSE)
  } else {
    d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    while (!is_root(d) && dirname(d) != d) d <- dirname(d)
    if (!is_root(d)) stop("Cannot locate the project root: set OPTBUCK_ROOT, or run from inside the project directory.", call. = FALSE)
  }
  d
})
Sys.setenv(OPTBUCK_ROOT = root_dir)

suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))
source(file.path(root_dir, "audits", "helpers_audit.R"))

out_dir <- path_output("example_hpr", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_41_example_hpr.txt")
audit_csv <- file.path(out_dir, "audit_41_example_hpr.csv")
init_audit_log(audit_txt)

E <- new.env(parent = emptyenv())
rec <- function(check, result, detail = "") record_check(E, check, result, detail, log_path = audit_txt)
PASS <- "PASS"; FAIL <- "FAIL"; REVIEW <- "REVIEW"
ok <- function(x) if (isTRUE(x)) PASS else FAIL
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)
cat("  Audit: the anonymised example file inst/extdata/example.hpr\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

ex <- file.path(root_dir, "inst", "extdata", "example.hpr")

# ---- 1 ------------------------------------------------------------------------------------------
size <- if (file.exists(ex)) file.size(ex) else NA_real_
rec("Check 01: example.hpr exists and is at most 5,000,000 bytes", ok(!is.na(size) && size <= 5e6), sprintf("%s bytes", format(size, big.mark = ",")))
if (is.na(size)) { write_audit_outputs(E$results, audit_csv, audit_txt); quit(save = "no", status = 0) }

# ---- 2 ------------------------------------------------------------------------------------------
doc <- tryCatch(xml2::read_xml(ex, options = "HUGE"), error = function(e) e)
if (inherits(doc, "error")) {
  rec("Check 02: well-formed XML, no coordinate elements", FAIL, conditionMessage(doc))
} else {
  xml2::xml_ns_strip(doc)
  bad <- xml2::xml_find_all(doc, paste0("//*[self::StemCoordinates or self::Latitude or self::Longitude or self::Altitude or ",
    "self::DefaultCoordinate or self::GeographicalRestriction or self::RestrictionPoint or @coordinateReferenceSystem or @receiverPosition]"))
  rec("Check 02: well-formed XML, no coordinate elements", ok(length(bad) == 0L), sprintf("coordinate elements: %d", length(bad)))

  # ---- 3 ----------------------------------------------------------------------------------------
  paths <- c("Machine/MachineKey", "Machine/MachineUserID", "Machine/MachineOwnerID", "Machine/MachineOwner/BusinessID",
             "Machine/BaseMachineManufacturerID", "Machine/LoggingContractor/BusinessName", "Machine/OperatorDefinition/OperatorUserID",
             "Machine/OperatorDefinition/ContactInformation/FirstName", "Machine/ObjectDefinition/ObjectName",
             "Machine/ObjectDefinition/ObjectUserID", "Machine/ObjectDefinition/ContractNumber", "Machine/ObjectDefinition/TextFromMachine",
             "Machine/ObjectDefinition/SubObject/SubObjectName", "Machine/ObjectDefinition/SubObject/SubObjectUserID",
             "Machine/ObjectDefinition/LoggingOrganisation/ContactInformation/BusinessName",
             "Machine/SpeciesGroupDefinition/LoggingOrganisation/ContactInformation/BusinessName",
             "ClassifiedProductDefinition/ProductBuyer/BusinessName", "ClassifiedProductDefinition/ProductDestination/BusinessName",
             "ClassifiedProductDefinition/LoggingOrganisation/ContactInformation/BusinessName",
             "HPRCMSettings/CopySettings", "HPRCMSettings/Description", "HPRCMSettings/HPRCMSettingsID",
             "HPRCMSettings/HPRCMSettingsTitle", "HPRCMSettings/HPRCMSettingsItem/Title")
  vals <- lapply(paths, function(p) {
    xp <- paste0("//", paste(sprintf("*[local-name()='%s']", strsplit(p, "/", fixed = TRUE)[[1L]]), collapse = "/"))
    trimws(xml2::xml_text(xml2::xml_find_all(doc, xp)))
  })
  allv <- unlist(vals)
  neutral <- grepl("^(Example|Operator|Owner|Machine|BaseMachine|Contract|Object|SubObject|Property)", allv) | grepl("^[0-9]*$", allv)
  rec("Check 03: identity fields hold only neutral placeholders, empty values or numbers", ok(all(neutral) && length(allv) > 0L),
      sprintf("%d values in %d fields; not neutral: %d", length(allv), length(paths), sum(!neutral)))
}

# ---- 4 ------------------------------------------------------------------------------------------
h  <- read_hpr(ex)
hs <- read_hpr_stems(ex)
rl <- read_hpr_logs(ex, defs = h)
used <- unique(rl$logs$ProductKey[!rl$logs$waste])
keys <- vapply(h$products, function(p) p$key, "")
rec("Check 04: read_hpr(), read_hpr_stems(), read_hpr_logs() work; every classified product has a recorded log",
    ok(isTRUE(h$ok) && isTRUE(hs$ok) && isTRUE(rl$ok) && length(keys) > 0L && all(keys %in% used)),
    sprintf("products %d, with logs %d; logs %d on %d stems", length(keys), sum(keys %in% used), nrow(rl$logs), length(unique(rl$logs$stem_key))))

# ---- 5 ------------------------------------------------------------------------------------------
t0 <- Sys.time()
cc <- bucking_check(ex)
secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
kf <- if (isTRUE(cc$ok)) cc$key_figures else NULL
yi <- if (!is.null(kf)) kf$yield[1L] else NA_real_
rec("Check 05: bucking_check() works; yield index between 0.5 and 1.1", ok(isTRUE(cc$ok) && is.finite(yi) && yi > 0.5 && yi < 1.1 && kf$stems[1L] > 0),
    sprintf("stems checked %s, yield index %.4f, %.1f s", if (!is.null(kf)) kf$stems[1L] else NA, yi, secs))

# ---- 6 ------------------------------------------------------------------------------------------
xsd <- "C:/optbuck_private/stanford_xsd/v31/HarvestedProduction_V3p1.xsd"
if (file.exists(xsd)) {
  v <- xml2::xml_validate(xml2::read_xml(ex, options = "HUGE"), xml2::read_xml(xsd))
  rec("Check 06: valid against the StanForD 2010 HarvestedProduction schema 3.1", ok(isTRUE(v)), sprintf("errors: %d", length(attr(v, "errors"))))
} else {
  rec("Check 06: valid against the StanForD 2010 HarvestedProduction schema 3.1", REVIEW, "the schema is not on this machine (C:/optbuck_private/stanford_xsd/): skipped")
}

# ---- 7 ------------------------------------------------------------------------------------------
rds <- "C:/optbuck_private/example_check/example_build.rds"
if (file.exists(rds)) {
  b <- readRDS(rds)
  rec("Check 07: the scripted search for the original's identifying strings found none", ok(length(b$hit) > 0L && !any(b$hit)),
      sprintf("%d strings searched when the file was built, %d found", length(b$strings), sum(b$hit)))
} else {
  rec("Check 07: the scripted search for the original's identifying strings found none", REVIEW, "the build record is not on this machine: skipped")
}

write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 41 complete.\n")
