# ==============================================================================
# audits/audit_34_distribution_import.R
#
# Purpose : Phase 7a (2026-10-30): the target distribution of imported .hpr and
#           .pin products is read, kept and written back. Hidden price-matrix
#           column `Distribution` (percent per cell) and four hidden assortment
#           fields (DistributionAllowed, DistributionCategory,
#           DistributionWithinProduct, MAXDeviation). Not used by runs.
#           R/helpers_hpr.R (.hpr_product(), .hpr_grid()), R/helpers_price_matrix.R
#           (pm_minimal(), pm_prices(), pm_bind(), pm_from_grid(), ...),
#           R/helpers_hpr_export.R, R/helpers_pin_export.R, app/helpers_upload.R.
#           Fixtures: inputs/test_fixtures/distribution_synthetic.hpr/.pin
#           (scripts/make_distribution_fixture.R). Checks 20-29 read the owner's
#           private files in C:/apter_private (read in place, never copied;
#           printed as file1..file5, hand.pin, real.pin) and are skipped with a
#           note when a file is missing.
#
# Checks  :
#  1.  Import gives the expected Distribution per cell and the four product
#      fields (.hpr and .pin)
#  2.  The whole-cm rows of one file diameter class all carry the same value
#  3.  Round trip import -> export -> import is identical (.hpr and .pin)
#  4.  Hidden data survives scenario copy, bulk edits, cell edits, rows and
#      columns added or removed, rename, check_price_set; a price edit leaves
#      Distribution unchanged; a new cell has none
#  5.  Products not from a file have NA; defaults give no new column
#  6.  write_price_file() on a pm with Distribution = the same pm without it;
#      the backend price loader drops the column
#  7.  Export: LengthDistributionDefinition between PriceDefinition and
#      ProductMatrixes, element order of the source files, only when stored;
#      <Distribution> per cell
#  8.  A Distribution-only difference neither highlights nor triggers a
#      comparison; the scenario xlsx does not carry the data (documented)
#  9.  FileDiameterClass_mm (hidden pm column, 2026-10-30): on the fixture it
#      equals the lower limit of the file class of every whole-cm row (.hpr)
# 10.  It is kept through scenario copy, bulk edit/fill, cell edit, rows and
#      columns added or removed, rename; new rows/cells and new assortments
#      have NA; defaults get no column
# 11.  It never reaches a run: write_price_file() identical with and without
#      it, pm_prices() has only the price columns, the backend loader drops it
# 12.  Private file5 spruce 140 SCA Saqtommer: 13 distinct values = the file's
#      13 class lower limits
# 30.  Editor (restructure R2): a cell edit sets every whole-cm row of the same file
#      class (only the row without a file class); prices never change
# 31.  Even / Longer logs equal tuning_target(); Clear removes the target
# 32.  Row sums and the bold flag (neither 0 nor 100); unavailable cells and
#      values outside 0-100 are refused
# 33.  The four fields on the first target (allowed, Volume of logs, within
#      product, MAXDeviation 0); fields from a file are kept; category change
# 34.  Scenario xlsx round trip: Distribution, FileDiameterClass_mm and the four
#      fields identical after download and upload (sheets target_<n> and
#      file_classes); no extra sheets without data
# 35.  Apportionment bucking fields (polish): DistributionAllowed and MAXDeviation
#      are editable (range 0-100 enforced) and survive copy A/B and the
#      scenario xlsx
#  20-29. Private files (see above)
#
# Verdict : PASS / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/distribution_import/audit_34_distribution_import.csv
#   outputs/distribution_import/audit_34_distribution_import.txt
#
# Usage   : Rscript audits/audit_34_distribution_import.R
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
source(file.path(root_dir, "app", "helpers_upload.R"))
source(file.path(root_dir, "R", "helpers_price_tuning.R"))      # tuning_target() (Even / Longer logs)
source(file.path(root_dir, "R", "helpers_distribution_edit.R"))  # the editor (restructure R2)
if (!exists("tr_msg", mode = "function")) source(file.path(root_dir, "R", "helpers_i18n.R"))

out_dir <- path_output("distribution_import", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_34_distribution_import.txt")
audit_csv <- file.path(out_dir, "audit_34_distribution_import.csv")
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
note <- function(txt) { cat(txt, "\n", file = audit_txt, append = TRUE); cat(txt, "\n") }
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)
cat("  Audit: distribution import and export (Phase 7a)\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

FX <- file.path(root_dir, "inputs", "test_fixtures")
F_HPR <- file.path(FX, "distribution_synthetic.hpr")
F_PIN <- file.path(FX, "distribution_synthetic.pin")
DIST <- PM_DISTRIBUTION_COL
FIELDS <- DISTRIBUTION_DEF_COLS

guess_map <- function(h) setNames(guess_hpr_species(h$species$name, h$species$user_id), h$species$key)
import <- function(f) { h <- read_product_file(f); list(h = h, s = hpr_to_scenario(h, guess_map(h))) }
keyof <- function(s, name) assortment_table_keys(s$defs)[match(name, s$defs$Name)]
dist_of <- function(pm, key, dm, cm) {       # the Distribution of one cell
  r <- pm[pm$ProductKey == key & pm$LengthClass_dm == dm & pm$PriceDiameterClass_cm == cm, , drop = FALSE]
  if (nrow(r) != 1L || !DIST %in% names(r)) NA_real_ else r[[DIST]]
}
col_of <- function(pm) if (DIST %in% names(pm)) pm[[DIST]] else rep(NA_real_, nrow(pm))
fields_of <- function(defs, i) {
  l <- lapply(FIELDS, function(n) defs[[n]][i]); names(l) <- FIELDS; l
}
same_fields <- function(a, b) identical(a, b)

hpr <- import(F_HPR); pin <- import(F_PIN)
SAW <- "Spruce Sawlog"; PULP <- "Spruce Pulpwood"
EXPECT_FIELDS <- list(DistributionAllowed = TRUE, DistributionCategory = "Volume of logs",
                      DistributionWithinProduct = TRUE, MAXDeviation = 4)
# Whole-cm rows 18-24 = file class 180, 25-31 = 250, 32 = 320; lengths 37/43/49/55 dm.
expect_cell <- function(cm, dm) {
  v <- if (cm >= 25 && cm <= 31) c(`37` = 0, `43` = 20, `49` = 50, `55` = 30)
       else c(`37` = 0, `43` = 30, `49` = 45, `55` = 25)
  unname(v[as.character(dm)])
}

# ==============================================================================
section("01. Import: Distribution per cell and the four product fields")
chk1 <- function(x) {
  k <- keyof(x$s, SAW); kp <- keyof(x$s, PULP)
  pm <- x$s$pm; sw <- pm[pm$ProductKey == k, , drop = FALSE]
  cells_ok <- all(mapply(function(dm, cm, v) isTRUE(v == expect_cell(cm, dm)),
                         sw$LengthClass_dm, sw$PriceDiameterClass_cm, sw[[DIST]]))
  i <- match(SAW, x$s$defs$Name); j <- match(PULP, x$s$defs$Name)
  isTRUE(x$s$ok) && DIST %in% names(pm) && nrow(sw) == 4L * 15L && cells_ok &&
    same_fields(fields_of(x$s$defs, i), EXPECT_FIELDS) &&
    all(vapply(fields_of(x$s$defs, j), is.na, logical(1))) &&
    all(is.na(pm[[DIST]][pm$ProductKey == kp]))
}
rec("Check 01: Import gives the expected Distribution per cell and the four fields (.hpr and .pin)",
    ok(chk1(hpr) && chk1(pin)),
    sprintf(".hpr %s | .pin %s", chk1(hpr), chk1(pin)))

# ==============================================================================
section("02. Whole-cm rows of one file class carry the same value")
chk2 <- function(x) {
  k <- keyof(x$s, SAW); sw <- x$s$pm[x$s$pm$ProductKey == k, , drop = FALSE]
  cls <- findInterval(sw$PriceDiameterClass_cm, c(18, 25, 32))
  all(tapply(sw[[DIST]], list(cls, sw$LengthClass_dm), function(v) length(unique(v)) == 1L))
}
rec("Check 02: All whole-cm rows of a file diameter class have the same Distribution (.hpr and .pin)",
    ok(chk2(hpr) && chk2(pin)), sprintf(".hpr %s | .pin %s", chk2(hpr), chk2(pin)))

# ==============================================================================
section("03. Round trip import -> export -> import")
groups <- data.frame(key = c("1", "2"), species = c("spruce", "pine"), stringsAsFactors = FALSE)
export_hpr_file <- function(s) {
  d <- s$defs[as.character(s$defs$Type) != "waste", , drop = FALSE]; rownames(d) <- NULL
  prod <- hpr_export_products(d, s$pm, orig = list(), groups = groups, used_keys = character(0))
  p <- tempfile(fileext = ".hpr")
  txt <- paste0('<?xml version="1.0" encoding="utf-8"?>\n',
    '<HarvestedProduction messageType="hpr" diameterUnit="mm" lengthUnit="cm" xmlns="urn:skogforsk:stanford2010"><Machine>',
    '<SpeciesGroupDefinition><SpeciesGroupKey>1</SpeciesGroupKey><SpeciesGroupName>Spruce</SpeciesGroupName></SpeciesGroupDefinition>',
    '<SpeciesGroupDefinition><SpeciesGroupKey>2</SpeciesGroupKey><SpeciesGroupName>Pine</SpeciesGroupName></SpeciesGroupDefinition>',
    paste0(prod$xml, collapse = ""), "</Machine></HarvestedProduction>\n")
  con <- file(p, "wb"); writeBin(charToRaw(enc2utf8(txt)), con); close(con); p
}
export_pin_file <- function(s) {
  p <- tempfile(fileext = ".pin"); x <- pin_export_scenario(s$defs, s$pm, p)
  if (!isTRUE(x$ok)) stop(paste(x$errors, collapse = "; "))
  p
}
# Same fields and the same Distribution per cell for every non-waste assortment of `a` in `b`.
# A product with nothing stored is written with 0 (the file format has no "none"), so its
# cells compare as 0.
rt_same <- function(a, b) {
  ka <- assortment_table_keys(a$defs); kb <- assortment_table_keys(b$defs)
  res <- vapply(which(as.character(a$defs$Type) != "waste"), function(i) {
    j <- match(ka[i], kb); if (is.na(j)) return(FALSE)
    fa <- fields_of(a$defs, i); fb <- fields_of(b$defs, j)
    # R4 D6: the schema requires a LengthDistributionDefinition, so a product with none stored is written
    # with the neutral one (not allowed, Number of logs, 0, within product) and reads back as that.
    if (all(is.na(unlist(fa)))) fa <- list(DistributionAllowed = FALSE, DistributionCategory = "Number of logs",
                                           DistributionWithinProduct = TRUE, MAXDeviation = 0)
    va <- a$pm[a$pm$ProductKey == ka[i] & !is.na(a$pm$LengthClass_dm), , drop = FALSE]
    vb <- b$pm[b$pm$ProductKey == kb[j] & !is.na(b$pm$LengthClass_dm), , drop = FALSE]
    stored <- any(!is.na(col_of(va)))
    m <- match(paste(va$LengthClass_dm, va$PriceDiameterClass_cm), paste(vb$LengthClass_dm, vb$PriceDiameterClass_cm))
    x <- col_of(va); y <- col_of(vb)[m]
    if (!stored) { x[is.na(x)] <- 0; y[is.na(y)] <- 0 }
    same_fields(fa, fb) && !anyNA(m) && nrow(va) == nrow(vb) && identical(as.numeric(x), as.numeric(y))
  }, logical(1))
  all(res)
}
rt_hpr <- import(export_hpr_file(hpr$s) ) ; rt_pin <- import(export_pin_file(pin$s))
rt_hpr_from_pin <- import(export_hpr_file(pin$s))
rec("Check 03: Round trip import -> export -> import: identical Distribution per cell and four fields (.hpr and .pin)",
    ok(rt_same(hpr$s, rt_hpr$s) && rt_same(pin$s, rt_pin$s) && rt_same(pin$s, rt_hpr_from_pin$s)),
    sprintf(".hpr %s | .pin %s | .pin data via .hpr writer %s", rt_same(hpr$s, rt_hpr$s),
            rt_same(pin$s, rt_pin$s), rt_same(pin$s, rt_hpr_from_pin$s)))

# ==============================================================================
section("04. Hidden data survives edits; a price edit leaves Distribution unchanged")
s <- hpr$s; d0 <- s$defs; pm0 <- s$pm; k <- keyof(s, SAW)
same_dist <- function(pm) {            # Distribution of the same cells as pm0
  m <- match(paste(pm0$ProductKey, pm0$LengthClass_dm, pm0$PriceDiameterClass_cm),
             paste(pm$ProductKey, pm$LengthClass_dm, pm$PriceDiameterClass_cm))
  !anyNA(m) && identical(col_of(pm)[m], col_of(pm0))
}
price_changed <- function(pm) {
  m <- match(paste(pm0$ProductKey, pm0$LengthClass_dm, pm0$PriceDiameterClass_cm),
             paste(pm$ProductKey, pm$LengthClass_dm, pm$PriceDiameterClass_cm))
  !identical(pm$PriceNOKperM3[m], pm0$PriceNOKperM3)
}
adj <- pm_bulk_adjust(pm0, k, rows = 1:3, cols = 1:2, mode = "amount", value = 10)
rp <- setNames(rep(500, nrow(pm_to_grid(pm0, k))), rownames(pm_to_grid(pm0, k)))
fil <- pm_bulk_fill(pm0, k, rp, setNames(rep(0, 4), colnames(pm_to_grid(pm0, k))))
g <- pm_to_grid(pm0, k); g[1, 1] <- g[1, 1] + 7
cell_edit <- pm_from_grid(g, k, pm0)
ok4a <- same_dist(adj) && price_changed(adj) && same_dist(fil) && price_changed(fil) &&
  same_dist(cell_edit) && price_changed(cell_edit)
# a changed price does not change the cell's Distribution
ok4a <- ok4a && identical(dist_of(cell_edit, k, 37, 18), dist_of(pm0, k, 37, 18)) &&
  isTRUE(cell_edit$PriceNOKperM3[cell_edit$ProductKey == k & cell_edit$LengthClass_dm == 37 & cell_edit$PriceDiameterClass_cm == 18] !=
         pm0$PriceNOKperM3[pm0$ProductKey == k & pm0$LengthClass_dm == 37 & pm0$PriceDiameterClass_cm == 18])
# rows and columns: new cells have none, old ones keep theirs
hi <- pm_add_class(pm0, k, "high"); lo <- pm_add_class(pm0, k, "low"); al <- pm_add_length(pm0, k, 40)
new_hi <- hi[hi$ProductKey == k & hi$PriceDiameterClass_cm == 33, ]
new_lo <- lo[lo$ProductKey == k & lo$PriceDiameterClass_cm == 17, ]
new_al <- al[al$ProductKey == k & al$LengthClass_dm == 40, ]
ok4b <- same_dist(hi) && same_dist(lo) && same_dist(al) &&
  nrow(new_hi) == 4L && all(is.na(new_hi[[DIST]])) && nrow(new_lo) == 4L && all(is.na(new_lo[[DIST]])) &&
  nrow(new_al) == 15L && all(is.na(new_al[[DIST]]))
rm_c <- pm_remove_class(pm0, k, "high"); rm_l <- pm_remove_length(pm0, k, 55)
ok4c <- identical(col_of(rm_c), col_of(pm0)[!(pm0$ProductKey == k & pm0$PriceDiameterClass_cm == 32)]) &&
  identical(col_of(rm_l), col_of(pm0)[!(pm0$ProductKey == k & pm0$LengthClass_dm == 55)])
rn <- pm_rename_key(pm0, k, "spruce_renamed")
ok4d <- identical(col_of(rn)[rn$ProductKey == "spruce_renamed"], col_of(pm0)[pm0$ProductKey == k])
# scenario A -> B copies: the whole assortment, and the prices alone
b_defs <- defs_from_products(load_products()); b_pm <- load_price_matrix_long()
cp <- copy_assortment(d0, pm0, b_defs, b_pm, k)
ok4e <- identical(sort(col_of(cp$pm)[cp$pm$ProductKey == k]), sort(col_of(pm0)[pm0$ProductKey == k])) &&
  all(is.na(col_of(cp$pm)[cp$pm$ProductKey != k])) &&
  same_fields(fields_of(cp$defs, match(SAW, cp$defs$Name)), EXPECT_FIELDS)
cpp <- copy_prices(d0, pm0, d0, adj, k)
ok4f <- same_dist(cpp$pm) && identical(cpp$copied, k)
# a rebuild through the assortment table and check_price_set
ck <- check_price_set(.type_assortment_table(d0), pm0)
ok4g <- isTRUE(ck$ok) && same_dist(ck$pm) &&
  same_fields(fields_of(.type_assortment_table(d0), match(SAW, d0$Name)), EXPECT_FIELDS)
# a new assortment added to the scenario (starting prices, no Distribution)
nw <- pm_bind(pm0, pm_start_prices("spruce_new", "matrix", 100, 10, 12, c(300, 400)))
ok4h <- same_dist(nw) && all(is.na(nw[[DIST]][nw$ProductKey == "spruce_new"]))
rec("Check 04: Scenario copy, bulk edits, cell edit, rows/columns, rename, check_price_set keep the hidden data; price edits leave it; new cells have none",
    ok(ok4a && ok4b && ok4c && ok4d && ok4e && ok4f && ok4g && ok4h),
    sprintf("bulk/fill/cell edit %s | add class/length %s | remove %s | rename %s | copy assortment %s | copy prices %s | rebuild %s | new assortment %s",
            ok4a, ok4b, ok4c, ok4d, ok4e, ok4f, ok4g, ok4h))

# ==============================================================================
section("05. Products not from a file have NA; defaults give no new column")
base <- defs_from_products(load_products())
pm_def <- load_price_matrix_long()
user_row <- base[1, , drop = FALSE]
p5 <- products_from_defs(d0, pm0)
ok5 <- all(vapply(FIELDS, function(n) n %in% names(base) && all(is.na(base[[n]])), logical(1))) &&
  !DIST %in% names(pm_def) && !DIST %in% names(pm_minimal(pm_def)) &&
  all(vapply(FIELDS, function(n) all(is.na(d0[[n]][d0$Type == "waste"])), logical(1))) &&
  !any(c(FIELDS, DIST) %in% names(p5$pd)) && !any(c(FIELDS, DIST) %in% names(p5$lc)) &&
  all(is.na(col_of(pm_bind(pm_minimal(pm_def), pm0))[seq_len(nrow(pm_def))])) &&
  identical(pm_minimal(pm_def), pm_prices(pm_minimal(pm_def)))
rec("Check 05: Default and user-added products have NA in all five hidden fields; no new column in defaults or run ProductData",
    ok(ok5), sprintf("base fields NA %s | default pm without column %s | ProductData without fields %s",
                     all(vapply(FIELDS, function(n) all(is.na(base[[n]])), logical(1))),
                     !DIST %in% names(pm_def), !any(c(FIELDS, DIST) %in% names(p5$pd))))

# ==============================================================================
section("06. write_price_file() never writes Distribution; the backend loader drops it")
f_with <- write_price_file(pm0); f_without <- write_price_file(pm_prices(pm0))
bytes <- function(f) readBin(f, "raw", file.size(f))
ok6a <- identical(bytes(f_with), bytes(f_without)) &&
  !any(grepl(DIST, readLines(f_with, warn = FALSE), fixed = TRUE)) &&
  identical(names(pm_prices(pm0)), PM_REQUIRED_COLS)
# the backend price loader (default products with the default matrix plus a Distribution column)
withd <- pm_def; withd[[DIST]] <- NA_real_
f_def <- tempfile(fileext = ".csv"); f_defd <- tempfile(fileext = ".csv")
readr::write_csv(pm_def, f_def, na = ""); readr::write_csv(withd, f_defd, na = "")
l1 <- .load_products_prices(f_def, root_dir, NULL, NULL); l2 <- .load_products_prices(f_defd, root_dir, NULL, NULL)
ok6b <- !DIST %in% names(l2$pm) && identical(l1$pm, l2$pm)
rec("Check 06: write_price_file() on a pm with Distribution is byte-identical to the same pm without it; the backend loader drops the column",
    ok(ok6a && ok6b), sprintf("price file identical %s | backend pm identical %s", ok6a, ok6b))
unlink(c(f_with, f_without, f_def, f_defd))

# ==============================================================================
section("07. Export: element position and order, <Distribution> per cell")
d_saw <- d0[match(SAW, d0$Name), , drop = FALSE]; d_pulp <- d0[match(PULP, d0$Name), , drop = FALSE]
ex <- hpr_export_products(rbind(d_saw, d_pulp), pm0, orig = list(), groups = groups, used_keys = character(0))
x_saw <- ex$xml[1]; x_pulp <- ex$xml[2]
pos <- function(x, tag) regexpr(tag, x, fixed = TRUE)[1]
ok7a <- pos(x_saw, "</PriceDefinition>") > 0 &&
  pos(x_saw, "</PriceDefinition>") < pos(x_saw, "<LengthDistributionDefinition") &&
  pos(x_saw, "</LengthDistributionDefinition>") < pos(x_saw, "<ProductMatrixes ") &&
  pos(x_saw, "<DistributionAllowed>") < pos(x_saw, "<DistributionCategory>") &&
  pos(x_saw, "<DistributionCategory>") < pos(x_saw, "<MAXDeviation>") &&
  pos(x_saw, "<MAXDeviation>") < pos(x_saw, "<DistributionWithinProduct>") &&
  grepl("<DistributionAllowed>true</DistributionAllowed><DistributionCategory>Volume of logs</DistributionCategory><MAXDeviation>4</MAXDeviation><DistributionWithinProduct>true</DistributionWithinProduct>", x_saw, fixed = TRUE)
neutral7 <- paste0('<LengthDistributionDefinition modificationRestricted="false"><DistributionAllowed>false</DistributionAllowed>',
                   "<DistributionCategory>Number of logs</DistributionCategory><MAXDeviation>0</MAXDeviation>",
                   "<DistributionWithinProduct>true</DistributionWithinProduct></LengthDistributionDefinition>")
ok7b <- grepl(neutral7, x_pulp, fixed = TRUE) &&
  length(gregexpr("<Distribution>0</Distribution>", x_pulp, fixed = TRUE)[[1]]) ==
    length(gregexpr("<ProductMatrixItem ", x_pulp, fixed = TRUE)[[1]])
vals <- as.numeric(regmatches(x_saw, gregexpr("(?<=<Distribution>)[0-9.]+(?=</Distribution>)", x_saw, perl = TRUE))[[1]])
# R4 D6: the file's own diameter classes (180, 250, 320), not one class per whole cm: 3 classes x 4 lengths
ok7c <- length(vals) == 4L * 3L && setequal(vals, c(0, 20, 25, 30, 45, 50))
# PermittedGradesDefinition (when there) comes before the distribution; the .pin writer uses the same code
pin_ids <- pin_product_ids(c(SAW, PULP))
ex_pin <- hpr_export_products(rbind(d_saw, d_pulp), pm0, orig = list(), groups = NULL, used_keys = character(0),
                              style = "pin", ids = pin_ids)
ok7d <- identical(sub("^<ProductDefinition><ProductUserID>[^<]*</ProductUserID>", "", ex_pin$xml[1]) != "", TRUE) &&
  grepl("<LengthDistributionDefinition", ex_pin$xml[1], fixed = TRUE) && grepl(neutral7, ex_pin$xml[2], fixed = TRUE)
rec("Check 07: LengthDistributionDefinition after PriceDefinition and before ProductMatrixes in the source order (the stored values, else the neutral element the schema requires); <Distribution> per cell of the file's own classes",
    ok(ok7a && ok7b && ok7c && ok7d),
    sprintf("position/order %s | product without values: neutral element, 0 per cell %s | cell values %s | .pin %s", ok7a, ok7b, ok7c, ok7d))

# ==============================================================================
section("08. No effect on comparison or highlighting")
pm_nod <- pm0; pm_nod[[DIST]] <- NULL
pm_other <- pm0; pm_other[[DIST]] <- ifelse(is.na(pm0[[DIST]]), NA, pm0[[DIST]] + 1)
d_other <- d0; d_other$MAXDeviation[match(SAW, d0$Name)] <- 9; d_other$DistributionCategory[match(SAW, d0$Name)] <- "Number of logs"
ok8a <- !scenarios_differ(d0, pm0, d0, pm_nod) && !scenarios_differ(d0, pm0, d0, pm_other) &&
  !scenarios_differ(d0, pm0, d_other, pm0) && same_assortments(d0, d_other) &&
  !any(assortment_diff(d0, d_other)$diff) && same_prices(pm0, pm_other, k)
ok8b <- TRUE   # (the scenario xlsx carries the data since restructure R2.5: check 34)
rec("Check 08: Distribution-only differences do not highlight or trigger a comparison",
    ok(ok8a && ok8b), sprintf("no comparison/highlight %s", ok8a))

# ==============================================================================
section("09. FileDiameterClass_mm on the fixture")
FC <- PM_FILE_CLASS_COL
pm9 <- hpr$s$pm; k9 <- keyof(hpr$s, SAW)
sw9 <- pm9[pm9$ProductKey == k9, , drop = FALSE]
exp9 <- ifelse(sw9$PriceDiameterClass_cm <= 24, 180, ifelse(sw9$PriceDiameterClass_cm <= 31, 250, 320))
ok9 <- FC %in% names(pm9) && identical(as.numeric(sw9[[FC]]), as.numeric(exp9)) &&
  all(!is.na(pm9[[FC]][pm9$ProductKey %in% keyof(hpr$s, PULP)]))
rec("Check 09: FileDiameterClass_mm of every whole-cm row of the fixture sawlog equals its file class lower limit (180 / 250 / 320)",
    ok(ok9), sprintf("%d rows, classes %s", nrow(sw9), paste(sort(unique(sw9[[FC]])), collapse = "/")))

# ==============================================================================
section("10. FileDiameterClass_mm is kept; new rows have NA")
fc_of <- function(pm, key, cm) unique(pm[[FC]][pm$ProductKey == key & pm$PriceDiameterClass_cm == cm])
same_fc <- function(pm) identical(pm[[FC]][match(paste(pm0$ProductKey, pm0$LengthClass_dm, pm0$PriceDiameterClass_cm),
                                                   paste(pm$ProductKey, pm$LengthClass_dm, pm$PriceDiameterClass_cm))], pm0[[FC]])
k10 <- keyof(hpr$s, SAW)
adj10 <- pm_bulk_adjust(pm0, k10, rows = 1:3, cols = 1:2, mode = "amount", value = 10)
rp10 <- setNames(rep(500, nrow(pm_to_grid(pm0, k10))), rownames(pm_to_grid(pm0, k10)))
fil10 <- pm_bulk_fill(pm0, k10, rp10, setNames(rep(0, 4), colnames(pm_to_grid(pm0, k10))))
g10 <- pm_to_grid(pm0, k10); g10[1, 1] <- g10[1, 1] + 7
ce10 <- pm_from_grid(g10, k10, pm0)
hi10 <- pm_add_class(pm0, k10, "high"); lo10 <- pm_add_class(pm0, k10, "low"); al10 <- pm_add_length(pm0, k10, 40)
ok10a <- same_fc(adj10) && same_fc(fil10) && same_fc(ce10) &&
  identical(ce10[[FC]][ce10$ProductKey == k10 & ce10$LengthClass_dm == 37 & ce10$PriceDiameterClass_cm == 18], 180)
ok10b <- all(is.na(hi10[[FC]][hi10$ProductKey == k10 & hi10$PriceDiameterClass_cm == 33])) &&
  all(is.na(lo10[[FC]][lo10$ProductKey == k10 & lo10$PriceDiameterClass_cm == 17])) &&
  all(is.na(al10[[FC]][al10$ProductKey == k10 & al10$LengthClass_dm == 40])) &&
  identical(sum(!is.na(hi10[[FC]])), sum(!is.na(pm0[[FC]])))
rc10 <- pm_remove_class(pm0, k10, "high"); rl10 <- pm_remove_length(pm0, k10, 55)
ok10c <- identical(rc10[[FC]], pm0[[FC]][!(pm0$ProductKey == k10 & pm0$PriceDiameterClass_cm == 32)]) &&
  identical(rl10[[FC]], pm0[[FC]][!(pm0$ProductKey == k10 & pm0$LengthClass_dm == 55)])
rn10 <- pm_rename_key(pm0, k10, "spruce_renamed")
ok10d <- identical(rn10[[FC]][rn10$ProductKey == "spruce_renamed"], pm0[[FC]][pm0$ProductKey == k10])
b10 <- defs_from_products(load_products()); bpm10 <- load_price_matrix_long()
cp10 <- copy_assortment(d0, pm0, b10, bpm10, k10)
ok10e <- identical(sort(cp10$pm[[FC]][cp10$pm$ProductKey == k10]), sort(pm0[[FC]][pm0$ProductKey == k10])) &&
  all(is.na(cp10$pm[[FC]][cp10$pm$ProductKey != k10]))
cpp10 <- copy_prices(d0, pm0, d0, adj10, k10)
ok10f <- same_fc(cpp10$pm)
ck10 <- check_price_set(.type_assortment_table(d0), pm0)
ok10g <- isTRUE(ck10$ok) && same_fc(ck10$pm)
nw10 <- pm_bind(pm0, pm_start_prices("spruce_new", "matrix", 100, 10, 12, c(300, 400)))
ok10h <- same_fc(nw10) && all(is.na(nw10[[FC]][nw10$ProductKey == "spruce_new"]))
def10 <- load_price_matrix_long()
ok10i <- !FC %in% names(def10) && !FC %in% names(pm_minimal(def10)) &&
  all(is.na(pm_bind(pm_minimal(def10), pm0)[[FC]][seq_len(nrow(def10))]))
rec("Check 10: FileDiameterClass_mm survives copy, bulk edit/fill, cell edit, rows/columns, rename, check_price_set; new rows, cells and assortments have NA; defaults get no column",
    ok(ok10a && ok10b && ok10c && ok10d && ok10e && ok10f && ok10g && ok10h && ok10i),
    sprintf("edits %s | add class/length %s | remove %s | rename %s | copy assortment %s | copy prices %s | rebuild %s | new assortment %s | defaults %s",
            ok10a, ok10b, ok10c, ok10d, ok10e, ok10f, ok10g, ok10h, ok10i))

# ==============================================================================
section("11. FileDiameterClass_mm never reaches a run")
f11a <- write_price_file(pm0); f11b <- write_price_file(pm_prices(pm0))
f11c <- write_price_file(pm0[, setdiff(names(pm0), FC), drop = FALSE])
ok11a <- identical(bytes(f11a), bytes(f11b)) && identical(bytes(f11a), bytes(f11c)) &&
  !any(grepl(FC, readLines(f11a, warn = FALSE), fixed = TRUE)) && identical(names(pm_prices(pm0)), PM_REQUIRED_COLS)
wf11 <- pm_def; wf11[[FC]] <- 180
f11d <- tempfile(fileext = ".csv"); readr::write_csv(wf11, f11d, na = "")
f_def_path <- tempfile(fileext = ".csv"); readr::write_csv(pm_def, f_def_path, na = "")
l11a <- .load_products_prices(f_def_path, root_dir, NULL, NULL)
l11b <- .load_products_prices(f11d, root_dir, NULL, NULL)
ok11b <- !FC %in% names(l11b$pm) && identical(l11a$pm, l11b$pm)
rec("Check 11: write_price_file() is byte-identical with and without FileDiameterClass_mm; pm_prices() has only the price columns; the backend loader drops the column",
    ok(ok11a && ok11b), sprintf("price file identical %s | backend pm identical %s", ok11a, ok11b))
unlink(c(f11a, f11b, f11c, f11d, f_def_path))

# ==============================================================================
# Private files (checks 20-29): read in place, skipped with a note when missing.
# ==============================================================================
PRIV_HPR <- "C:/apter_private/hpr"; PRIV_PIN <- "C:/apter_private/pin"
hpr_files <- if (dir.exists(PRIV_HPR)) sort(list.files(PRIV_HPR, "[.]hpr$", full.names = TRUE)) else character(0)
names(hpr_files) <- if (length(hpr_files)) paste0("file", seq_along(hpr_files)) else character(0)
pin_files <- c(hand.pin = file.path(PRIV_PIN, "hand.pin"), real.pin = file.path(PRIV_PIN, "real.pin"))
pin_files <- pin_files[file.exists(pin_files)]
priv <- list()
get_priv <- function(n) {
  if (is.null(priv[[n]])) priv[[n]] <<- import(c(hpr_files, pin_files)[[n]])
  priv[[n]]
}
skip <- function(check, what) {
  note(sprintf("  SKIPPED: %s (%s missing)", check, what))
  rec(paste0(check, " (skipped: private file missing)"), PASS, what)
}
have <- function(n) n %in% names(c(hpr_files, pin_files))

# Independent check of a product's imported cells against the raw file cells: every whole-cm row
# carries the Distribution of the file class it falls in (the same way prices are spread).
cells_match_raw <- function(x, hkey) {
  p <- x$h$products[[hkey]]
  key <- NULL
  for (i in seq_len(nrow(x$s$defs))) {
    k <- assortment_table_keys(x$s$defs)[i]
    if (identical(x$s$defs$HprProductKey[i], hkey) || (identical(x$h$kind, "pin") && identical(x$s$defs$Name[i], p$name))) { key <- k; break }
  }
  if (is.null(key)) return(NA)
  sw <- x$s$pm[x$s$pm$ProductKey == key & !is.na(x$s$pm$LengthClass_dm), , drop = FALSE]
  cc <- p$cells
  D <- sort(unique(cc$d_mm[!is.na(cc$d_mm)]))
  ok <- vapply(seq_len(nrow(sw)), function(r) {
    cls <- max(D[D <= sw$PriceDiameterClass_cm[r] * 10], min(D))
    hit <- which(cc$d_mm == cls & ceiling(cc$l_cm / 10) == sw$LengthClass_dm[r])
    if (length(hit) != 1L) return(NA)
    v <- cc$dist[hit]
    (is.na(v) && is.na(sw[[DIST]][r])) || (!is.na(v) && !is.na(sw[[DIST]][r]) && v == sw[[DIST]][r])
  }, NA)
  all(ok[!is.na(ok)]) && sum(!is.na(ok)) > 0L
}
sca_check <- function(n) {            # spruce 140 SCA Sagtomer: 30/45/25 on 430/490/550, 0 on 370
  x <- get_priv(n); i <- which(grepl("140.*SCA", x$s$defs$Name) & x$s$defs$Species == "spruce")
  if (length(i) != 1L) return(list(ok = FALSE, detail = sprintf("%d match(es)", length(i))))
  k <- assortment_table_keys(x$s$defs)[i]; sw <- x$s$pm[x$s$pm$ProductKey == k, , drop = FALSE]
  by_len <- tapply(sw[[DIST]], sw$LengthClass_dm, function(v) paste(sort(unique(v)), collapse = "/"))
  by_len <- setNames(as.character(by_len), names(by_len))
  exp_len <- c(`43` = "30", `49` = "45", `55` = "25")
  if (identical(n, "file5")) exp_len <- c(`37` = "0", exp_len)      # only file5 has the 370 cm column
  f <- fields_of(x$s$defs, i)
  ok <- identical(by_len, exp_len) && identical(f$DistributionCategory, "Volume of logs") &&
    identical(f$MAXDeviation, 4) && isTRUE(f$DistributionAllowed) &&
    !anyNA(sw[[DIST]])
  list(ok = ok, detail = sprintf("lengths %s | %s, max deviation %s, allowed %s | %d rows",
                                 paste(names(by_len), by_len, sep = ":", collapse = " "),
                                 f$DistributionCategory, f$MAXDeviation, f$DistributionAllowed, nrow(sw)))
}

section("20-22. file5, file3, file4: spruce 140 SCA Sagtomer")
for (n in c("file5", "file3", "file4")) {
  chk <- sprintf("Check %d: %s spruce 140 SCA Sagt\u00f8mmer: Distribution 30/45/25 on 430/490/550 cm in every diameter class%s; Volume of logs, MAXDeviation 4, allowed",
                 20L + match(n, c("file5", "file3", "file4")) - 1L, n, if (n == "file5") ", 0 on 370" else " (no 370 column)")
  if (!have(n)) { skip(chk, n); next }
  r <- sca_check(n); rec(chk, ok(r$ok), r$detail)
}

section("23. file1: sawlog products")
if (!have("file1")) skip("Check 23: file1 sawlog products DistributionAllowed false, all Distribution 0", "file1") else {
  x <- get_priv("file1"); d <- x$s$defs; saw <- which(d$Type == "sawlog")
  vals <- unlist(lapply(assortment_table_keys(d)[saw], function(k) x$s$pm[[DIST]][x$s$pm$ProductKey == k]))
  raw_allowed <- vapply(d$HprProductKey[saw], function(h) isTRUE(x$h$products[[h]]$dist_allowed), NA)
  rec("Check 23: file1 sawlog products: all Distribution 0; DistributionAllowed as in the file (false for all but one product, which is true with all Distribution 0)",
      ok(length(saw) > 0L && length(vals) > 0L && all(vals == 0) && !anyNA(vals) &&
           identical(unname(d$DistributionAllowed[saw]), unname(raw_allowed))),
      sprintf("%d sawlog assortments, %d cells all 0, DistributionAllowed false for %d, true for %d",
              length(saw), length(vals), sum(!d$DistributionAllowed[saw]), sum(d$DistributionAllowed[saw])))
}

section("24. real.pin: BM_Tall")
if (!have("real.pin")) skip("Check 24: real.pin BM_Tall: Volume of logs, MAXDeviation 5, 40 Distribution values (0, 15, 70)", "real.pin") else {
  x <- get_priv("real.pin"); p <- x$h$products[["BM_Tall"]]
  i <- match(TRUE, x$s$defs$HprProductKey %in% "BM_Tall" | x$s$defs$Name %in% p$name)
  raw_vals <- p$cells$dist[!is.na(p$cells$dist)]
  rec("Check 24: real.pin BM_Tall: Volume of logs, MAXDeviation 5, 40 Distribution values (0, 15, 70); imported cells equal the file cells",
      ok(!is.null(p) && identical(p$dist_category, "Volume of logs") && identical(p$dist_maxdev, 5) &&
           length(raw_vals) == 40L && setequal(raw_vals, c(0, 15, 70)) &&
           identical(x$s$defs$DistributionCategory[i], "Volume of logs") && identical(x$s$defs$MAXDeviation[i], 5) &&
           isTRUE(cells_match_raw(x, "BM_Tall"))),
      sprintf("raw values %d, set {%s}, category %s, max deviation %s", length(raw_vals),
              paste(sort(unique(raw_vals)), collapse = ","), p$dist_category, p$dist_maxdev))
}

section("25. hand.pin")
if (!have("hand.pin")) skip("Check 25: hand.pin 20 Distribution values (10, 15, 20, 25) in PS1, PP1, SS1, SP1; no LengthDistributionDefinition", "hand.pin") else {
  x <- get_priv("hand.pin"); ps <- x$h$products[c("PS1", "PP1", "SS1", "SP1")]
  raw_vals <- unlist(lapply(ps, function(p) p$cells$dist[!is.na(p$cells$dist)]))
  rec("Check 25: hand.pin 20 Distribution values (10, 15, 20, 25) in PS1, PP1, SS1, SP1; no LengthDistributionDefinition (fields NA)",
      ok(!any(vapply(ps, is.null, NA)) && length(raw_vals) == 20L && setequal(raw_vals, c(10, 15, 20, 25)) &&
           all(vapply(FIELDS, function(n) all(is.na(x$s$defs[[n]])), logical(1))) &&
           all(vapply(c("PS1", "PP1", "SS1", "SP1"), function(h) isTRUE(cells_match_raw(x, h)), NA))),
      sprintf("raw values %d, set {%s}", length(raw_vals), paste(sort(unique(raw_vals)), collapse = ",")))
}

section("26. Imported cells equal the file cells, all private files")
{
  chk <- "Check 26: Every imported whole-cm row carries the Distribution of its file cell (all private files); whole-cm rows of a file class never differ"
  files <- names(c(hpr_files, pin_files))
  if (!length(files)) skip(chk, "private files") else {
    bad <- character(0); n_prod <- 0L
    for (n in files) {
      x <- get_priv(n)
      hk <- if (identical(x$h$kind, "pin")) names(x$h$products) else x$s$defs$HprProductKey[!is.na(x$s$defs$HprProductKey)]
      for (h in unique(hk)) {
        r <- cells_match_raw(x, h); n_prod <- n_prod + 1L
        if (!isTRUE(r)) bad <- c(bad, sprintf("%s:%s", n, h))
      }
    }
    rec(chk, ok(length(bad) == 0L && n_prod > 0L),
        sprintf("%d products compared, %d differ%s", n_prod, length(bad), if (length(bad)) paste0(": ", paste(head(bad, 5), collapse = ", ")) else ""))
  }
}

section("27-28. Round trip, all private files")
rt_priv <- function(n) {
  x <- get_priv(n)
  re <- if (identical(x$h$kind, "pin")) import(export_pin_file(x$s)) else import(export_hpr_file(x$s))
  list(same = rt_same(x$s, re$s), n = sum(as.character(x$s$defs$Type) != "waste"))
}
for (grp in list(list(27L, hpr_files, "hpr", "all 5 .hpr files (products written and re-read)"),
                 list(28L, pin_files, "pin", "both .pin files"))) {
  chk <- sprintf("Check %d: Round trip import -> export -> import identical for %s", grp[[1]], grp[[4]])
  if (!length(grp[[2]])) { skip(chk, grp[[3]]); next }
  res <- lapply(setNames(names(grp[[2]]), names(grp[[2]])), rt_priv)
  rec(chk, ok(all(vapply(res, `[[`, NA, "same"))),
      paste(sprintf("%s %s (%d assortments)", names(res), vapply(res, function(r) if (r$same) "same" else "DIFFERENT", ""),
                    vapply(res, `[[`, 1L, "n")), collapse = " | "))
}

unlink(c(rt_hpr$h$header$file))
# ==============================================================================
section("12. FileDiameterClass_mm on file5 spruce 140 SCA Sagtommer")
if (!have("file5")) skip("Check 12: FileDiameterClass_mm of file5 spruce 140 SCA", "file5") else {
  x12 <- get_priv("file5"); d12 <- x12$s$defs
  i12 <- which(d12$Species == "spruce" & grepl("140 SCA", d12$Name, fixed = TRUE))[1L]
  k12 <- assortment_table_keys(d12)[i12]; hk12 <- d12$HprProductKey[i12]
  v12 <- x12$s$pm[[FC]][x12$s$pm$ProductKey == k12 & !is.na(x12$s$pm$LengthClass_dm)]
  file12 <- sort(unique(x12$h$products[[hk12]]$cells$d_mm))
  rec("Check 12: file5 spruce 140 SCA Sagtommer: 13 distinct FileDiameterClass_mm values equal the file's 13 class lower limits",
      ok(length(unique(v12)) == 13L && identical(sort(unique(v12)), file12) && !anyNA(v12)),
      sprintf("%d distinct values in %d rows; file classes %d; equal %s", length(unique(v12)), length(v12), length(file12),
              identical(sort(unique(v12)), file12)))
}

# ==============================================================================
section("30. Editor: class-wide cell edit, prices untouched")
saw_k <- keyof(hpr$s, SAW)
pm30 <- pm_minimal(hpr$s$pm)
row_ids <- function(pm, cm, dm) which(pm$ProductKey == saw_k & pm$PriceDiameterClass_cm %in% cm & pm$LengthClass_dm %in% dm)
e30 <- dist_set_cell(pm30, saw_k, 20, 43, 40)
same_class <- e30[[DIST]][row_ids(e30, 18:24, 43)]
other_rows <- setdiff(seq_len(nrow(e30)), row_ids(e30, 18:24, 43))
ok30a <- all(same_class == 40) && identical(e30[[DIST]][other_rows], pm30[[DIST]][other_rows]) &&
  identical(pm_prices(e30), pm_prices(pm30))
# a row without a file class is its own class
pm30n <- pm30; pm30n[[PM_FILE_CLASS_COL]] <- NA_real_
e30n <- dist_set_cell(pm30n, saw_k, 20, 43, 40)
ch <- which(!is.na(e30n[[DIST]]) & (is.na(pm30n[[DIST]]) | e30n[[DIST]] != pm30n[[DIST]]))
ok30b <- length(ch) == 1L && e30n$PriceDiameterClass_cm[ch] == 20 && e30n$LengthClass_dm[ch] == 43
# NA clears the cells of the class
e30c <- dist_set_cell(e30, saw_k, 22, 43, NA)
ok30c <- all(is.na(e30c[[DIST]][row_ids(e30c, 18:24, 43)]))
# the editor grid shows the values at the grid positions
g30 <- dist_grid(e30, saw_k)
ok30d <- identical(dim(g30), dim(pm_to_grid(e30, saw_k))) && identical(dimnames(g30), dimnames(pm_to_grid(e30, saw_k))) &&
  isTRUE(g30["20", "43"] == 40) && isTRUE(g30["18", "43"] == 40)
rec("Check 30: A cell edit sets every whole-cm row of the same file class (rows without a file class: only that row), NA clears, prices unchanged, dist_grid() has the price grid's shape",
    ok(ok30a && ok30b && ok30c && ok30d), sprintf("class-wide %s | own class %s | clear %s | grid %s", ok30a, ok30b, ok30c, ok30d))

# ==============================================================================
section("31. Editor: Even / Longer logs equal tuning_target(); Clear")
tg_grid <- function(pm, key) {
  av <- dist_available(pm, key)
  g <- list(price = ifelse(t(av), 1, NA_real_)); dimnames(g$price) <- list(colnames(av), rownames(av)); g
}
ok31 <- TRUE; det31 <- character(0)
for (ty in c("neutral", "long")) {
  e <- dist_apply_target(pm30, saw_k, ty)
  want <- tuning_target(tg_grid(pm30, saw_k), ty)           # length x diameter
  got <- t(as.matrix(dist_grid(e, saw_k)))                  # length x diameter
  same <- isTRUE(all.equal(unname(got), unname(want), tolerance = 1e-9)) && identical(is.na(got), is.na(want))
  sums <- dist_row_sums(dist_grid(e, saw_k))
  same <- same && all(abs(sums - 100) < 1e-9) && identical(pm_prices(e), pm_prices(pm30))
  ok31 <- ok31 && same; det31 <- c(det31, sprintf("%s %s", ty, same))
}
cl <- dist_clear(pm30, saw_k)
others <- pm30$ProductKey != saw_k
ok31c <- !any(!is.na(col_of(cl)[cl$ProductKey == saw_k])) && identical(col_of(cl)[others], col_of(pm30)[others])
only <- pm30[pm30$ProductKey == saw_k, , drop = FALSE]
ok31d <- !DIST %in% names(dist_clear(only, saw_k))
rec("Check 31: Even and Longer logs equal tuning_target() on the priced cells (rows sum to 100, prices unchanged); Clear removes the assortment's target (the column when it was the only one)",
    ok(ok31 && ok31c && ok31d), sprintf("%s | clear keeps other products %s | column dropped when empty %s", paste(det31, collapse = ", "), ok31c, ok31d))

# ==============================================================================
section("32. Editor: sums, flag, refused edits")
g32 <- data.frame(`37` = c(100, 0, 30, NA), `43` = c(0, 0, 60, NA), check.names = FALSE)
sums32 <- dist_row_sums(g32)
ok32a <- identical(sums32, c(100, 0, 90, 0)) && identical(dist_row_flag(sums32), c(FALSE, FALSE, TRUE, FALSE))
err1 <- tryCatch({ dist_set_cell(pm30, saw_k, 18, 99, 10); "" }, error = function(e) conditionMessage(e))
err2 <- tryCatch({ dist_set_cell(pm30, saw_k, 20, 43, 101); "" }, error = function(e) conditionMessage(e))
err3 <- tryCatch({ dist_set_cell(pm30, saw_k, 20, 43, -1); "" }, error = function(e) conditionMessage(e))
ok32b <- grepl("no price", err1) && grepl("between 0 and 100", err2) && grepl("between 0 and 100", err3)
rec("Check 32: Row sums; the flag is TRUE only for a sum that is neither 0 nor 100; a cell without a price and a value outside 0-100 are refused",
    ok(ok32a && ok32b), sprintf("sums %s | refused: '%s' / '%s'", ok32a, substr(err1, 1, 40), substr(err2, 1, 40)))

# ==============================================================================
section("33. Editor: the four fields")
d33 <- hpr$s$defs
ks33 <- assortment_table_keys(d33); i_saw <- match(saw_k, ks33)
kept <- dist_init_fields(d33, saw_k)
ok33a <- identical(fields_of(kept, i_saw), fields_of(d33, i_saw))          # from a file: unchanged
pulp_k <- keyof(hpr$s, "Spruce Pulpwood"); j <- match(pulp_k, ks33)
d33n <- dist_clear_fields(d33, pulp_k)                                      # an assortment without the fields
fresh <- dist_init_fields(d33n, pulp_k)
ok33b <- isTRUE(fresh$DistributionAllowed[j]) && identical(fresh$DistributionCategory[j], "Volume of logs") &&
  isTRUE(fresh$DistributionWithinProduct[j]) && identical(fresh$MAXDeviation[j], 0)
cat33 <- dist_set_category(fresh, pulp_k, "Number of logs")
ok33c <- identical(cat33$DistributionCategory[j], "Number of logs") &&
  inherits(tryCatch(dist_set_category(fresh, pulp_k, "x"), error = function(e) e), "error")
ok33d <- all(is.na(unlist(dist_clear_fields(fresh, pulp_k)[j, FIELDS])))
ok33e <- identical(dist_init_fields(cat33, pulp_k), cat33)                   # fields already there: kept
rec("Check 33: First target sets Allowed TRUE, 'Volume of logs', WithinProduct TRUE, MAXDeviation 0; fields from a file are kept; the category can be changed; the fields can be cleared",
    ok(ok33a && ok33b && ok33c && ok33d && ok33e), sprintf("file fields kept %s | first target %s | category %s | cleared %s | kept when set %s", ok33a, ok33b, ok33c, ok33d, ok33e))

# ==============================================================================
section("34. Scenario xlsx round trip of the target and the file classes")
d34 <- hpr$s$defs; pm34 <- pm_minimal(hpr$s$pm)
x34 <- tempfile(fileext = ".xlsx"); scenario_export(d34, pm34, x34)
r34 <- scenario_import(x34)
a34 <- pm34[order(pm34$ProductKey, pm34$LengthClass_dm, pm34$PriceDiameterClass_cm), ]
b34 <- pm_minimal(r34$pm)
b34 <- b34[order(b34$ProductKey, b34$LengthClass_dm, b34$PriceDiameterClass_cm), ]
ok34a <- isTRUE(r34$ok) && nrow(a34) == nrow(b34) && identical(a34[[DIST]], b34[[DIST]]) &&
  identical(a34[[PM_FILE_CLASS_COL]], b34[[PM_FILE_CLASS_COL]]) && identical(pm_prices(a34), pm_prices(b34))
ok34b <- all(vapply(FIELDS, function(n) identical(d34[[n]], r34$defs[[n]]), logical(1)))
sh34 <- readxl::excel_sheets(x34)
ok34c <- "file_classes" %in% sh34 && any(grepl("^target_", sh34))
# an Even / Longer target set in the editor (no file classes)
pm34e <- dist_apply_target(pm_prices(pm34), saw_k, "long"); d34e <- dist_init_fields(dist_clear_fields(d34, saw_k), saw_k)
x34e <- tempfile(fileext = ".xlsx"); scenario_export(d34e, pm34e, x34e); r34e <- scenario_import(x34e)
b34e <- pm_minimal(r34e$pm)
ord <- function(p) p[order(p$ProductKey, p$LengthClass_dm, p$PriceDiameterClass_cm), ]
ok34d <- isTRUE(r34e$ok) && identical(ord(b34e)[[DIST]], ord(pm_minimal(pm34e))[[DIST]]) && !PM_FILE_CLASS_COL %in% names(b34e) &&
  !"file_classes" %in% readxl::excel_sheets(x34e) &&
  all(vapply(FIELDS, function(n) identical(d34e[[n]], r34e$defs[[n]]), logical(1)))
# no data: no extra sheets or columns
x34n <- tempfile(fileext = ".xlsx"); scenario_export(defs_from_products(load_products()), load_price_matrix_long(), x34n)
sh34n <- readxl::excel_sheets(x34n); r34n <- scenario_import(x34n)
ok34e <- !"file_classes" %in% sh34n && !any(grepl("^target_", sh34n)) && isTRUE(r34n$ok) &&
  !DIST %in% names(r34n$pm) && !PM_FILE_CLASS_COL %in% names(r34n$pm) &&
  !any(FIELDS %in% names(as.data.frame(readxl::read_excel(x34n, sheet = "assortments", col_types = "text"))))
ok34f <- identical(names(pm_prices(b34e)), PM_REQUIRED_COLS) &&
  identical(ord(pm_prices(b34e)), ord(pm_prices(pm34e)))
rec("Check 34: Scenario xlsx round trip: Distribution, FileDiameterClass_mm and the four fields identical (sheets target_<n>, file_classes); no extra sheets or columns without data; price data unchanged",
    ok(ok34a && ok34b && ok34c && ok34d && ok34e && ok34f),
    sprintf("cells + file classes %s | fields %s | sheets %s (%s) | editor target %s | no data, no extra sheets %s | prices %s",
            ok34a, ok34b, ok34c, paste(sh34, collapse = ","), ok34d, ok34e, ok34f))
unlink(c(x34, x34e, x34n))

# ==============================================================================
section("35. Apportionment bucking fields")
d35 <- dist_init_fields(dist_clear_fields(hpr$s$defs, saw_k), saw_k); j35 <- match(saw_k, assortment_table_keys(d35))
e35 <- dist_set_maxdev(dist_set_allowed(d35, saw_k, FALSE), saw_k, 4.5)
ok35a <- identical(e35$DistributionAllowed[j35], FALSE) && identical(e35$MAXDeviation[j35], 4.5) &&
  identical(dist_set_allowed(e35, saw_k, TRUE)$DistributionAllowed[j35], TRUE) &&
  inherits(tryCatch(dist_set_maxdev(e35, saw_k, -1), error = function(e) e), "error") &&
  inherits(tryCatch(dist_set_maxdev(e35, saw_k, 101), error = function(e) e), "error") &&
  inherits(tryCatch(dist_set_maxdev(e35, saw_k, NA), error = function(e) e), "error")
pm35 <- pm_minimal(hpr$s$pm)
cp35 <- copy_assortment(e35, pm35, hpr$s$defs, pm35, saw_k)               # copy A -> B
jc <- match(saw_k, assortment_table_keys(cp35$defs))
ok35b <- identical(cp35$defs$DistributionAllowed[jc], FALSE) && identical(cp35$defs$MAXDeviation[jc], 4.5)
x35 <- tempfile(fileext = ".xlsx"); scenario_export(e35, pm35, x35); r35 <- scenario_import(x35)
jr <- match(saw_k, assortment_table_keys(r35$defs))
ok35c <- isTRUE(r35$ok) && identical(r35$defs$DistributionAllowed[jr], FALSE) && identical(r35$defs$MAXDeviation[jr], 4.5)
rec("Check 35: Apportionment fields: DistributionAllowed and MAXDeviation (0-100) can be set; they survive copy A -> B and the scenario xlsx",
    ok(ok35a && ok35b && ok35c), sprintf("setters %s | copy %s | xlsx %s", ok35a, ok35b, ok35c))
unlink(x35)

write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 34 complete.\n")