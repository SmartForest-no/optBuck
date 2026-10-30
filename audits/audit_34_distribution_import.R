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
  pos(x_saw, "</LengthDistributionDefinition>") < pos(x_saw, "<ProductMatrixes>") &&
  pos(x_saw, "<DistributionAllowed>") < pos(x_saw, "<DistributionCategory>") &&
  pos(x_saw, "<DistributionCategory>") < pos(x_saw, "<MAXDeviation>") &&
  pos(x_saw, "<MAXDeviation>") < pos(x_saw, "<DistributionWithinProduct>") &&
  grepl("<DistributionAllowed>true</DistributionAllowed><DistributionCategory>Volume of logs</DistributionCategory><MAXDeviation>4</MAXDeviation><DistributionWithinProduct>true</DistributionWithinProduct>", x_saw, fixed = TRUE)
ok7b <- !grepl("LengthDistributionDefinition", x_pulp, fixed = TRUE) &&
  length(gregexpr("<Distribution>0</Distribution>", x_pulp, fixed = TRUE)[[1]]) ==
    length(gregexpr("<ProductMatrixItem ", x_pulp, fixed = TRUE)[[1]])
vals <- as.numeric(regmatches(x_saw, gregexpr("(?<=<Distribution>)[0-9.]+(?=</Distribution>)", x_saw, perl = TRUE))[[1]])
ok7c <- length(vals) == 4L * 15L && setequal(vals, c(0, 20, 25, 30, 45, 50))
# PermittedGradesDefinition (when there) comes before the distribution; the .pin writer uses the same code
pin_ids <- pin_product_ids(c(SAW, PULP))
ex_pin <- hpr_export_products(rbind(d_saw, d_pulp), pm0, orig = list(), groups = NULL, used_keys = character(0),
                              style = "pin", ids = pin_ids)
ok7d <- identical(sub("^<ProductDefinition><ProductUserID>[^<]*</ProductUserID>", "", ex_pin$xml[1]) != "", TRUE) &&
  grepl("<LengthDistributionDefinition", ex_pin$xml[1], fixed = TRUE) && !grepl("<LengthDistributionDefinition", ex_pin$xml[2], fixed = TRUE)
rec("Check 07: LengthDistributionDefinition after PriceDefinition and before ProductMatrixes in the source order, only with stored values; <Distribution> per cell",
    ok(ok7a && ok7b && ok7c && ok7d),
    sprintf("position/order %s | product without values: no element, 0 per cell %s | cell values %s | .pin %s", ok7a, ok7b, ok7c, ok7d))

# ==============================================================================
section("08. No effect on comparison or highlighting; not in the scenario xlsx")
pm_nod <- pm0; pm_nod[[DIST]] <- NULL
pm_other <- pm0; pm_other[[DIST]] <- ifelse(is.na(pm0[[DIST]]), NA, pm0[[DIST]] + 1)
d_other <- d0; d_other$MAXDeviation[match(SAW, d0$Name)] <- 9; d_other$DistributionCategory[match(SAW, d0$Name)] <- "Number of logs"
ok8a <- !scenarios_differ(d0, pm0, d0, pm_nod) && !scenarios_differ(d0, pm0, d0, pm_other) &&
  !scenarios_differ(d0, pm0, d_other, pm0) && same_assortments(d0, d_other) &&
  !any(assortment_diff(d0, d_other)$diff) && same_prices(pm0, pm_other, k)
xl <- tempfile(fileext = ".xlsx"); scenario_export(d0, pm0, xl)
sh <- as.data.frame(readxl::read_excel(xl, sheet = "assortments", col_types = "text"))
imp <- scenario_import(xl)
ok8b <- !any(c(FIELDS, DIST) %in% names(sh)) && isTRUE(imp$ok) && !DIST %in% names(imp$pm) &&
  all(vapply(FIELDS, function(n) all(is.na(imp$defs[[n]])), logical(1)))
rec("Check 08: Distribution-only differences do not highlight or trigger a comparison; the scenario xlsx does not carry the data (documented limitation)",
    ok(ok8a && ok8b), sprintf("no comparison/highlight %s | xlsx without data and re-import ok %s", ok8a, ok8b))
unlink(xl)

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
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 34 complete.\n")
