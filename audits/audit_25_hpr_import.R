# ==============================================================================
# audits/audit_25_hpr_import.R
#
# Purpose : Import from .hpr, Phase A (2026-10-20): R/helpers_hpr.R reads a
#           StanForD 2010 .hpr file (species groups, products, price matrices,
#           permitted grades) and turns it into a scenario (assortment table
#           and prices). Runs on inputs/test_fixtures/hpr_example_synthetic.hpr
#           (optBuck's example.hpr definitions and prices, no stem or personal
#           data; scripts/make_hpr_test_fixture.R) and on small broken
#           variants of it.
#
# Checks  :
#  1.  Reading: 4 species groups (445 FURU, 446 GRAN, 447 LAUV, 448
#      BIOVIRKE; best grade 1), 8 classified products (999999 skipped), read
#      from the part before the first stem; stems are not read
#  2.  Product definitions as in the file (limits, classes, volume method,
#      grades, cells, forbidden cells) for all 8 products
#  3.  Species guess: FURU pine, GRAN spruce, LAUV and BIOVIRKE not used;
#      also "Gran tynning", "Tall", user ID only, and a mixed "Furu/Gran" name
#  4.  Grade filter: only 100 Energi (grade 7 only) is unticked by default,
#      with "Only for grades set by the operator (special orders)"; a product
#      without permitted grades is kept. Invented groups: reference grades =
#      the most commonly permitted (ties kept), else StartGrade, else none;
#      a sawlog-group product with only grade 8 is unticked (no exemption)
#  5.  Mapping: 5 assortments + Waste per species; min top from
#      DiameterMINTop (14, 14.2), max top from DiameterClassMAX (50), max butt
#      from DiameterMAXButt (60); top position 10; volume methods; types;
#      price lengths only for REDACTED23; the scenario passes
#      check_price_set()
#  6.  Lengths round up to whole dm (365 -> 37 ... 545 -> 55; 305 -> 31); a
#      class without a whole-dm length below the next class is dropped
#  7.  Forbidden cells are empty: REDACTED23 has no 13 cm row and
#      no 40/46 dm cells below 21 cm; every other cell has the file's price;
#      whole-cm rows take the largest class at or below them (20 cm -> 190 mm,
#      21 cm -> 207 mm; 14 cm -> 142 mm for SCA Sagtømmer)
#  8.  Price length: a 37 dm REDACTED23 candidate has the Top
#      volume of 365 cm, not 370; other products and the base products use
#      the cut length; base LengthClasses get no PriceLength_cm column
#  9.  Errors: not XML, wrong root, another message type, diameter unit cm,
#      no Machine, duplicate product key, no species groups, no products, no
#      species group mapped
# 10.  Warnings: unknown species group, over-bark diameters, over-bark price
#      volume, unsupported volume method (an invented one; solid volume
#      used), volume top
#      position differs, length margins; two groups as one species with a
#      renamed duplicate are noted in info only (no warning)
# 11.  Run (100-tree list) with the imported scenario: every product used,
#      logs inside their limits and lengths, no forbidden cell chosen, price
#      volumes of REDACTED23 on the class lower limit, no
#      coordinate-like columns in any result table
# 12.  Scenario file round trip keeps the price lengths; a scenario without
#      them differs, and a comparison prices REDACTED23 on 365 cm in
#      A and 370 cm in B (separate candidate passes)
# 13.  A fully forbidden inner diameter class (all 120 mm cells of 197
#      Massevirke) stays as empty rows 12-15 cm: imported (noted in info, not shown),
#      valid, kept by the scenario xlsx and long CSV round trips and by a grid
#      edit elsewhere, and never chosen in a run (chosen without it)
# 14.  Large files: only the part before the first stem record is parsed;
#      a stem before the products makes it read the whole file; "<Stem>"
#      inside a biomass function is not a stem; .hpr.gz and .hpr in a .zip
#      give the same definitions; a zip without an .hpr is an error
# 15.  Import message: the fixture import shows no warnings (rounding,
#      whole-cm classes, the mm volume diameter and forbidden cells are info);
#      a skipped product, a dropped length class and a replaced volume
#      method are warnings
# 16.  Imported Mid product (2026-10-21): REDACTED23 switched to
#      VolumeDiameterCategory Mid imports as Mid without a warning (keeps its
#      price lengths); a 37 dm candidate is priced on the under-bark diameter
#      at the middle of the cut log over 365 cm, price class from the top;
#      a run with the scenario chooses it
#
# Verdict : PASS / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/hpr_import/audit_25_hpr_import.csv
#   outputs/hpr_import/audit_25_hpr_import.txt
#
# Usage   : Rscript audits/audit_25_hpr_import.R
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
source(file.path(root_dir, "scripts", "performance_tree_lists.R"))

out_dir <- path_output("hpr_import", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_25_hpr_import.txt")
audit_csv <- file.path(out_dir, "audit_25_hpr_import.csv")
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
cat("  Audit: import from .hpr (Phase A)\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

FIXTURE <- file.path(root_dir, "inputs", "test_fixtures", "hpr_example_synthetic.hpr")
fx <- readLines(FIXTURE, warn = FALSE, encoding = "UTF-8")
# A broken variant of the fixture: `edit` takes and returns its lines.
variant <- function(edit) {
  f <- tempfile(fileext = ".hpr")
  writeLines(edit(fx), f, useBytes = TRUE)
  f
}
# Replace the first match of `pattern` in the lines (fixed = TRUE).
sub1 <- function(lines, pattern, repl) {
  i <- grep(pattern, lines, fixed = TRUE)[1L]
  lines[i] <- sub(pattern, repl, lines[i], fixed = TRUE)
  lines
}
# The lines of product `key`'s ProductDefinition block.
prod_lines <- function(lines, key) {
  s <- grep(sprintf("<ProductKey>%s</ProductKey>", key), lines, fixed = TRUE)[1L]
  e <- s + grep("</ProductDefinition>", lines[s:length(lines)], fixed = TRUE)[1L] - 1L
  s:e
}
# Edit within product `key`'s block only.
in_prod <- function(lines, key, pattern, repl, all = FALSE) {
  r <- prod_lines(lines, key)
  b <- lines[r]
  if (all) b <- gsub(pattern, repl, b, fixed = TRUE)
  else { i <- grep(pattern, b, fixed = TRUE)[1L]; b[i] <- sub(pattern, repl, b[i], fixed = TRUE) }
  lines[r] <- b
  lines
}

h <- read_hpr(FIXTURE)
sp_map <- setNames(guess_hpr_species(h$species$name, h$species$user_id), h$species$key)
base_defs <- defs_from_products(load_products(root_dir))
r <- hpr_to_scenario(h, sp_map, base_defs = base_defs)
keys <- assortment_table_keys(r$defs)
K_PINE_SAW <- "pine_240_sag_REDACTED34"
K_SCA <- keys[startsWith(keys, "spruce_140_sca_sag")]


# ==============================================================================
section("01. Reading")
pk <- names(h$products)
ok1 <- isTRUE(h$ok) && identical(h$species$key, c("445", "446", "447", "448")) &&
  identical(h$species$name, c("FURU", "GRAN", "LAUV", "BIOVIRKE")) &&
  all(h$species$best_grade == 1) &&
  setequal(pk, c("7949", "7953", "8015", "8016", "8017", "8019", "7973", "7974")) &&
  is.null(h$stems) && identical(h$header$read, "prefix")
rec("Check 01: Species groups and classified products, read from the part before the first stem",
    ok(ok1), sprintf("ok %s | groups %s | best grades %s | products %s | read %s (%s of %s MB), stems not read %s",
                     isTRUE(h$ok), paste(h$species$key, h$species$name, collapse = ", "),
                     paste(h$species$best_grade, collapse = ","), paste(pk, collapse = ","),
                     h$header$read, h$header$mb_read, h$header$mb_file, is.null(h$stems)))


# ==============================================================================
section("02. Product definitions")
# key, min top, max top (class MAX), max butt, top position, length classes,
# length MAX, volume method, grades, cells, forbidden cells.
exp2 <- list(
  "7949" = list(140, 500, 600, 10, c(365, 377, 395, 410, 425, 455, 485, 515, 545), 560, "Top", 1:2, 126, 35),
  "7953" = list(40, 700, 700, 10, c(300, 340, 370, 400, 430, 460, 490, 520), 550, "All diameters (solid volume)", 1:6, 40, 0),
  "8015" = list(40, 700, 700, 10, c(300, 340, 370, 400, 430, 460, 490, 520), 520, "All diameters (solid volume)", 1:6, 40, 0),
  "8016" = list(40, 700, 700, 10, 300, 310, "All diameters (solid volume)", 7, 10, 0),
  "8017" = list(120, 239, 300, 10, 305, 315, "All diameters (solid volume)", 1:2, 6, 0),
  "8019" = list(142, 600, 650, 10, c(370, 430, 490, 550), 560, "Calculated Norwegian mid", 1:2, 52, 0),
  "7973" = list(50, 900, 900, 10, c(300, 340, 370, 400, 430, 460, 490, 520, 550), 560, "All diameters (solid volume)", 1:8, 63, 0),
  "7974" = list(50, 700, 750, 10, c(340, 370, 400, 430, 460, 490, 520), 580, "All diameters (solid volume)", 1:8, 49, 0))
bad2 <- character(0)
for (k in names(exp2)) {
  p <- h$products[[k]]; e <- exp2[[k]]
  got <- list(p$min_top, p$dia_class_max, p$max_butt, p$top_pos, p$len_classes, p$len_class_max,
              p$volume_method, sort(p$grades), nrow(p$cells), sum(p$cells$criteria == HPR_FORBIDDEN))
  same <- mapply(function(a, b) isTRUE(all.equal(as.numeric(a), as.numeric(b))) || identical(a, b), got, e)
  if (!all(same)) bad2 <- c(bad2, sprintf("%s (field %s)", k, paste(which(!same), collapse = ",")))
}
rec("Check 02: Product definitions as in the file (8 products)",
    ok(length(bad2) == 0L), if (length(bad2)) paste("differ:", paste(bad2, collapse = "; "))
                            else "limits, classes, volume methods, grades, cells and forbidden cells all as in the file")


# ==============================================================================
section("03. Species guess")
g3 <- guess_hpr_species(c("FURU", "GRAN", "LAUV", "BIOVIRKE", "Gran tynning", "Tall", "X", "Furu/Gran blanding"),
                        c("NO1_FURU", "NO2_Gran", "NO3_Lauv", "NO4_Biovirke", NA, NA, "SE_gran", NA))
exp3 <- c("pine", "spruce", "none", "none", "spruce", "pine", "spruce", "none")
rec("Check 03: Species guessed from group names and user IDs",
    ok(identical(g3, exp3)), sprintf("FURU, GRAN, LAUV, BIOVIRKE, 'Gran tynning', 'Tall', user ID 'SE_gran', 'Furu/Gran blanding' -> %s",
                                     paste(g3, collapse = ", ")))


# ==============================================================================
section("04. Grade filter")
tab <- hpr_product_table(h, sp_map)
off <- tab$key[!tab$default & tab$species != "none"]
h4 <- h; h4$products[["8016"]]$grades <- NULL
tab4 <- hpr_product_table(h4, sp_map)
# The rule on invented groups (decided 2026-10-22): reference grades = the
# grade(s) permitted by the most products of the group (ties kept), else
# StartGrade, else the lowest grade; no exemption by product group or name.
gp <- function(key, sp, grades, group = "Massevirke")
  list(key = key, name = key, group = group, species_keys = sp, grades = grades)
h4s <- list(
  species = data.frame(key = c("1", "2", "3", "4"), best_grade = c(1, 1, 1, NA),
                       start_grade = c(2, 2, 3, NA), stringsAsFactors = FALSE),
  products = list(
    gp("saw_mode",  "1", 2, "Tømmer"),          # a sawlog permitting only the mode grade 2
    gp("saw_mode2", "1", 2, "Tømmer"),
    gp("pulp_wide", "1", c(1, 2, 3)),
    gp("special",   "1", 8, "Tømmer"),          # special order: a sawlog group, grade 8 only
    gp("energy",    "1", 5),                    # energy wood, grade 5 only
    gp("no_grades", "1", NULL),
    gp("tie_a", "2", c(1, 2)), gp("tie_b", "2", c(3, 4)),   # a tie: every grade counts once
    gp("start_only", "3", NULL)))                            # group 3: no product lists grades
ref4 <- hpr_reference_grades(h4s)
ok_of <- vapply(h4s$products, function(p) as.character(hpr_grade_ok(p, ref4)), "")
names(ok_of) <- vapply(h4s$products, `[[`, "", "key")
exp_ok <- c(saw_mode = "TRUE", saw_mode2 = "TRUE", pulp_wide = "TRUE", special = "FALSE",
            energy = "FALSE", no_grades = NA, tie_a = "TRUE", tie_b = "TRUE", start_only = NA)
ok_ref <- identical(ref4[["1"]], 2) && identical(ref4[["2"]], c(1, 2, 3, 4)) &&
  identical(ref4[["3"]], 3) && identical(ref4[["4"]], numeric(0))
ok4 <- identical(off, "8016") && identical(tab$reason[tab$key == "8016"], HPR_GRADE_REASON) &&
  identical(HPR_GRADE_REASON, "Only for grades set by the operator (special orders)") &&
  isTRUE(tab4$default[tab4$key == "8016"]) && is.na(tab4$grade_ok[tab4$key == "8016"]) &&
  ok_ref && identical(ok_of, exp_ok)
rec("Check 04: Products without a reference grade (most commonly permitted) unticked by default",
    ok(ok4), sprintf("fixture: unticked (mapped species) %s ('%s'); without permitted grades kept %s | invented groups: reference grades %s | permits one: %s",
                     paste(off, collapse = ","), tab$reason[tab$key == "8016"],
                     isTRUE(tab4$default[tab4$key == "8016"]),
                     paste(names(ref4), vapply(ref4, function(x) paste(x, collapse = "/"), ""), sep = ":", collapse = ", "),
                     paste(names(ok_of), ok_of, sep = "=", collapse = ", ")))


# ==============================================================================
section("05. Mapping onto the assortment table")
d5 <- r$defs; row <- function(k) d5[keys == k, , drop = FALSE]
exp_keys <- c(K_PINE_SAW, "pine_200_massevirke", "spruce_197_massevirke", "spruce_154_kubb", K_SCA,
              "pine_waste", "spruce_waste")
ps <- row(K_PINE_SAW); sca <- row(K_SCA)
ok5 <- isTRUE(r$ok) && setequal(keys, exp_keys) && length(K_SCA) == 1L &&
  ps$MinTopDiameter_cm == 14 && ps$MaxTopDiameter_cm == 50 && ps$MaxButtDiameter_cm == 60 &&
  ps$DiameterTopPosition_cm == 10 && ps$VolumeDiameterCategory == "Top" && ps$Type == "sawlog" &&
  sca$MinTopDiameter_cm == 14.2 && sca$VolumeDiameterCategory == "Calculated Norwegian mid" &&
  row("spruce_197_massevirke")$Type == "pulpwood" && row("spruce_154_kubb")$Type == "sawlog" &&
  all(d5$Type[d5$Name == "Waste"] == "waste") &&
  identical(which(!is.na(d5$PriceLengths_cm)), which(keys == K_PINE_SAW)) &&
  all(d5$PriceType[d5$Name != "Waste"] == "matrix") && isTRUE(check_price_set(d5, r$pm)$ok)
rec("Check 05: Assortments, limits, volume methods, types and price lengths; the scenario is valid",
    ok(ok5), sprintf("keys %s | pine sawlog top %g-%g, butt <= %g, top position %g, %s | SCA min top %g, %s | price lengths on: %s | valid %s",
                     paste(keys, collapse = ","), ps$MinTopDiameter_cm, ps$MaxTopDiameter_cm,
                     ps$MaxButtDiameter_cm, ps$DiameterTopPosition_cm, ps$VolumeDiameterCategory,
                     sca$MinTopDiameter_cm, sca$VolumeDiameterCategory,
                     paste(keys[!is.na(d5$PriceLengths_cm)], collapse = ","),
                     isTRUE(check_price_set(d5, r$pm)$ok)))


# ==============================================================================
section("06. Lengths round up")
cols <- function(pm, k) sort(unique(pm$LengthClass_dm[pm$ProductKey == k & !is.na(pm$LengthClass_dm)]))
c_ps <- cols(r$pm, K_PINE_SAW); c_kubb <- cols(r$pm, "spruce_154_kubb")
# Narrow class: 8017 Kubb gets a 301 cm class before its 305 cm class.
f6 <- variant(function(l) in_prod(l, "8017", 'lengthClassLowerLimit="305"', 'lengthClassLowerLimit="301"'))
l6 <- readLines(f6, warn = FALSE)
# add back a 305 column: duplicate the 301 items as 305
it <- grep('lengthClassLowerLimit="301"', l6, fixed = TRUE)
dup <- unlist(lapply(it, function(i) {
  e <- i + grep("</ProductMatrixItem>", l6[i:length(l6)], fixed = TRUE)[1L] - 1L
  sub('lengthClassLowerLimit="301"', 'lengthClassLowerLimit="305"', l6[i:e], fixed = TRUE)
}))
last <- max(it); last_e <- last + grep("</ProductMatrixItem>", l6[last:length(l6)], fixed = TRUE)[1L] - 1L
l6 <- append(l6, dup, after = last_e); writeLines(l6, f6, useBytes = TRUE)
r6 <- hpr_to_scenario(read_hpr(f6), sp_map, base_defs = base_defs)
w6 <- grep("Kubb.*301 cm dropped", r6$warnings, value = TRUE)
ok6 <- identical(c_ps, c(37L, 38L, 40L, 41L, 43L, 46L, 49L, 52L, 55L)) && identical(c_kubb, 31L) &&
  isTRUE(r6$ok) && length(w6) == 1L && identical(cols(r6$pm, "spruce_154_kubb"), 31L)
rec("Check 06: Length classes round up to whole dm; a class without a whole-dm length is dropped",
    ok(ok6), sprintf("REDACTED23 %s dm | Kubb %s dm | 301+305 cm classes: '%s', columns %s",
                     paste(c_ps, collapse = ","), paste(c_kubb, collapse = ","),
                     paste(w6, collapse = ""), paste(cols(r6$pm, "spruce_154_kubb"), collapse = ",")))


# ==============================================================================
section("07. Forbidden cells and diameter rows")
g7 <- pm_to_grid(r$pm, K_PINE_SAW)
cell <- function(g, d, l) g[as.character(d), as.character(l)]
p_hpr <- h$products[["7949"]]$cells
hp <- function(d, l) p_hpr$price[p_hpr$d_mm == d & p_hpr$l_cm == l]
# Every cell: forbidden in the file <-> empty; otherwise the file's price.
L7 <- c(365, 377, 395, 410, 425, 455, 485, 515, 545); dm7 <- ceiling(L7 / 10)
D7 <- sort(unique(p_hpr$d_mm[p_hpr$criteria != HPR_FORBIDDEN]))
bad7 <- 0L; n_empty <- 0L
for (cc in as.integer(rownames(g7))) for (j in seq_along(L7)) {
  cls <- max(D7[D7 <= cc * 10])
  forb <- p_hpr$criteria[p_hpr$d_mm == cls & p_hpr$l_cm == L7[j]] == HPR_FORBIDDEN
  v <- cell(g7, cc, dm7[j])
  if (forb) { n_empty <- n_empty + 1L; if (!is.na(v)) bad7 <- bad7 + 1L }
  else if (!isTRUE(v == hp(cls, L7[j]))) bad7 <- bad7 + 1L
}
g_sca <- pm_to_grid(r$pm, K_SCA)
ok7 <- !"13" %in% rownames(g7) && min(as.integer(rownames(g7))) == 14L &&
  max(as.integer(rownames(g7))) == 36L && bad7 == 0L &&
  all(is.na(g7[as.character(14:20), c("40", "46")])) &&
  cell(g7, 20, 37) == hp(190, 365) && cell(g7, 21, 37) == hp(207, 365) &&
  min(as.integer(rownames(g_sca))) == 14L && cell(g_sca, 14, 43) == h$products[["8019"]]$cells$price[
    h$products[["8019"]]$cells$d_mm == 142 & h$products[["8019"]]$cells$l_cm == 430]
rec("Check 07: Forbidden cells empty, all other cells the file's price; whole-cm rows",
    ok(ok7), sprintf("REDACTED23 rows %s-%s cm (no 13), %d empty cells, %d cells wrong | 20 cm @37 dm = %g (190 mm), 21 cm = %g (207 mm) | SCA first row %s cm = %g (142 mm)",
                     min(rownames(g7)), max(as.integer(rownames(g7))), n_empty, bad7,
                     cell(g7, 20, 37), cell(g7, 21, 37), min(as.integer(rownames(g_sca))), cell(g_sca, 14, 43)))


# ==============================================================================
section("08. Price length")
p8 <- products_from_defs(r$defs, r$pm)
pv <- list(treid = "t", plot_id = NA, species_group = "pine", DBH = 300, height_m = 18, height_cm = 1800L,
           diameterPosition = seq(0L, 1800L, 10L), DiameterValue = 330 - seq(0, 1800, 10) * 0.15)
cand <- generate_candidate_logs_one_tree(pv, p8$pd, p8$lc, stump_height_cm = 10L)
c37 <- cand[cand$ProductKey == K_PINE_SAW & cand$LogLength == 370 & cand$eligible, ][1L, ]
v365 <- price_volume_m3("Top", c37$TopDiameter, 365, adjustment = VOLUME_ROUND_DOWN_CM)
v370 <- price_volume_m3("Top", c37$TopDiameter, 370, adjustment = VOLUME_ROUND_DOWN_CM)
c41 <- cand[cand$ProductKey == K_PINE_SAW & cand$LogLength == 410 & cand$eligible, ][1L, ]
pb <- products_from_defs(base_defs, pm_minimal(load_price_matrix_long(root_dir)))
ok8 <- isTRUE(all.equal(c37$price_volume_m3, v365)) && !isTRUE(all.equal(v365, v370)) &&
  isTRUE(all.equal(c41$price_volume_m3, price_volume_m3("Top", c41$TopDiameter, 410, adjustment = VOLUME_ROUND_DOWN_CM))) &&
  identical(p8$lc$PriceLength_cm[p8$lc$ProductKey == K_SCA], p8$lc$LogLength[p8$lc$ProductKey == K_SCA]) &&
  identical(names(pb$lc), c("ProductKey", "LogLength", "LogLength_dm"))
rec("Check 08: Price volume on the class lower limit for REDACTED23 only; base products unchanged",
    ok(ok8), sprintf("37 dm log, top %.1f mm: price volume %.5f (365 cm: %.5f, 370 cm: %.5f) | 41 dm on 410 cm: %s | SCA on cut length: %s | base LengthClasses columns: %s",
                     c37$TopDiameter, c37$price_volume_m3, v365, v370,
                     isTRUE(all.equal(c41$price_volume_m3, price_volume_m3("Top", c41$TopDiameter, 410, adjustment = VOLUME_ROUND_DOWN_CM))),
                     identical(p8$lc$PriceLength_cm[p8$lc$ProductKey == K_SCA], p8$lc$LogLength[p8$lc$ProductKey == K_SCA]),
                     paste(names(pb$lc), collapse = ",")))


# ==============================================================================
section("09. Errors")
not_xml <- tempfile(fileext = ".hpr"); writeLines("not xml at all", not_xml)
err <- function(f) { x <- read_hpr(f); if (isTRUE(x$ok)) "(no error)" else paste(x$errors, collapse = " / ") }
e9 <- c(
  not_xml   = err(not_xml),
  root      = err(variant(function(l) gsub("HarvestedProduction", "ProductionReport", l, fixed = TRUE))),
  message   = err(variant(function(l) sub1(l, 'messageType="hpr"', 'messageType="pri"'))),
  unit      = err(variant(function(l) sub1(l, 'diameterUnit="mm"', 'diameterUnit="cm"'))),
  machine   = err(variant(function(l) gsub("Machine", "Machinery", l, fixed = TRUE))),
  dup_key   = err(variant(function(l) sub1(l, "<ProductKey>7953</ProductKey>", "<ProductKey>7949</ProductKey>"))),
  no_groups = err(variant(function(l) gsub("SpeciesGroupDefinition", "SpeciesGroupDef", l, fixed = TRUE))),
  no_prods  = err(variant(function(l) gsub("ClassifiedProductDefinition", "OtherDefinition", l, fixed = TRUE))))
none_map <- setNames(rep("none", 4L), h$species$key)
e9["no_species"] <- paste(hpr_to_scenario(h, none_map, base_defs = base_defs)$errors, collapse = " / ")
pat9 <- c(not_xml = "not a readable XML", root = "not a StanForD 2010", message = "'pri' message",
          unit = "Diameter unit is 'cm'", machine = "no Machine", dup_key = "defined twice: 7949",
          no_groups = "no species groups", no_prods = "no products", no_species = "Spruce or Pine")
hit9 <- mapply(function(t, p) grepl(p, t, fixed = TRUE), e9[names(pat9)], pat9)
rec("Check 09: File errors are reported and block the import",
    ok(all(hit9)), paste(sprintf("%s: %s", names(e9), ifelse(hit9[names(e9)], "ok", e9)), collapse = " | "))


# ==============================================================================
section("10. Warnings")
map_both_spruce <- sp_map; map_both_spruce["445"] <- "spruce"   # FURU and GRAN as spruce
# Forbid every 120 mm price cell of product 8015.
forbid_class_8015_120 <- function(l) {
  r <- prod_lines(l, "8015")
  for (i in r[grepl('diameterClassLowerLimit="120"', l[r], fixed = TRUE)]) {
    j <- i + grep("<BuckingCriteria>", l[i:length(l)], fixed = TRUE)[1L] - 1L
    l[j] <- sub("<BuckingCriteria>[^<]*</BuckingCriteria>",
                sprintf("<BuckingCriteria>%s</BuckingCriteria>", HPR_FORBIDDEN), l[j])
  }
  l
}
warn_of <- function(f, map = sp_map) {
  x <- read_hpr(f); s <- hpr_to_scenario(x, map, base_defs = base_defs)
  c(x$warnings, s$warnings)
}
w10 <- list(
  unknown_group = warn_of(variant(function(l) in_prod(l, "7953", "<SpeciesGroupKey>445</SpeciesGroupKey>",
                                                      "<SpeciesGroupKey>999</SpeciesGroupKey>"))),
  dia_over_bark = warn_of(variant(function(l) in_prod(l, "8015", "<DiameterUnderBark>true", "<DiameterUnderBark>false"))),
  vol_over_bark = warn_of(variant(function(l) in_prod(l, "8015", "<VolumeUnderBark>true", "<VolumeUnderBark>false"))),
  method        = warn_of(variant(function(l) in_prod(l, "8019", ">Calculated Norwegian mid<", ">Unsupported method<"))),
  top_position  = warn_of(variant(function(l) in_prod(l, "8019", 'volumeDiameterTopPosition="10"',
                                                      'volumeDiameterTopPosition="20"'))),
  margin        = warn_of(variant(function(l) in_prod(l, "8015", "<LengthClassMargin>0", "<LengthClassMargin>5"))))
pat10 <- c(unknown_group = "missing or unknown species group", dia_over_bark = "Diameters over bark",
           vol_over_bark = "Price volume over bark", method = "'Unsupported method' is not available",
           top_position = "Volume top position 20 cm", margin = "margins ignored")
hit10 <- mapply(function(w, p) any(grepl(p, w, fixed = TRUE)), w10[names(pat10)], pat10)
# Two groups as one species with a duplicate name: it is renamed, and that is
# noted in info (not shown); so is the merged species.
dup <- hpr_to_scenario(read_hpr(variant(function(l) in_prod(l, "7953", "200 Massevirke", "197 Massevirke"))),
                       map_both_spruce, base_defs = base_defs)
renamed <- isTRUE(dup$ok) && any(grepl("Renamed a second '197 Massevirke'", dup$info, fixed = TRUE)) &&
  any(grepl("are all spruce", dup$info, fixed = TRUE)) && length(dup$warnings) == 0L
rec("Check 10: Partial problems are warnings and the rest imports",
    ok(all(hit10) && renamed),
    paste(c(sprintf("%s: %s", names(pat10), ifelse(hit10, "ok", "MISSING")),
            sprintf("duplicate renamed and merged species noted in info, no warning: %s", renamed)), collapse = " | "))


# ==============================================================================
section("11. Run with the imported scenario")
tl <- perf_tree_list(100L)
f11 <- tempfile(fileext = ".csv"); pm_export(r$pm, f11)
res <- tryCatch(run_tree_list_bucking(tl, f11, root_dir, products = p8$pd, length_classes = p8$lc),
                error = function(e) conditionMessage(e))
ran <- is.list(res)
if (ran) {
  lg <- res$selected_logs_all
  pd <- p8$pd[match(lg$ProductKey, p8$pd$ProductKey), ]
  lim_ok <- all(lg$TopDiameter_ub >= pd$MinTopDiameter - 1e-9 & lg$TopDiameter_ub <= pd$MaxTopDiameter + 1e-9)
  len_ok <- all(paste(lg$ProductKey, lg$LogLength) %in% paste(p8$lc$ProductKey, p8$lc$LogLength))
  mat <- lg[p8$pd$PriceType[match(lg$ProductKey, p8$pd$ProductKey)] == "matrix", ]
  priced <- paste(r$pm$ProductKey, r$pm$LengthClass_dm, r$pm$PriceDiameterClass_cm)
  forb_ok <- all(paste(mat$ProductKey, mat$LengthClass_dm, mat$PriceDiameterClass_cm_used) %in% priced)
  ps11 <- lg[lg$ProductKey == K_PINE_SAW, ]
  pv_ok <- nrow(ps11) > 0L && isTRUE(all.equal(ps11$price_volume_m3,
    price_volume_m3("Top", ps11$TopDiameter_ub, price_lengths(row(K_PINE_SAW)$PriceLengths_cm, ps11$LogLength),
                    adjustment = VOLUME_ROUND_DOWN_CM)))
  used <- setdiff(keys[r$defs$Name != "Waste"], unique(lg$ProductKey))
  tabs <- res[vapply(res, is.data.frame, logical(1))]
  coord_cols <- unique(unlist(lapply(tabs, function(t) grep("lat|lon|coord|altitude", names(t), value = TRUE,
                                                             ignore.case = TRUE))))
}
ok11 <- ran && lim_ok && len_ok && forb_ok && pv_ok && length(used) == 0L && length(coord_cols) == 0L
rec("Check 11: A tree-list run with the imported scenario respects limits, lengths and forbidden cells",
    ok(ok11), if (!ran) paste("run failed:", res) else
      sprintf("%d logs | products not used: %s | top limits %s | lengths %s | no forbidden cell %s | REDACTED23 price volume on class limits %s (%d logs) | coordinate-like columns: %s",
              nrow(lg), if (length(used)) paste(used, collapse = ",") else "none", lim_ok, len_ok,
              forb_ok, pv_ok, nrow(ps11), if (length(coord_cols)) paste(coord_cols, collapse = ",") else "none"))


# ==============================================================================
section("12. Scenario file and comparison")
f12 <- tempfile(fileext = ".xlsx"); scenario_export(r$defs, r$pm, f12)
back <- scenario_import(f12)
d_nopl <- r$defs; d_nopl$PriceLengths_cm <- NA_character_
pb12 <- products_from_defs(d_nopl, r$pm)
cmp <- tryCatch(run_tree_list_bucking(tl, f11, root_dir, products = p8$pd, length_classes = p8$lc,
                                      scenario = list(price_matrix_path = f11, products = pb12$pd,
                                                      length_classes = pb12$lc)),
                error = function(e) conditionMessage(e))
cmp_ok <- FALSE
if (is.list(cmp)) {
  a12 <- cmp$selected_logs_all; b12 <- cmp$scenario$selected_logs_all
  a12 <- a12[a12$ProductKey == K_PINE_SAW & a12$LogLength == 370, ]
  b12 <- b12[b12$ProductKey == K_PINE_SAW & b12$LogLength == 370, ]
  top_a <- price_volume_m3("Top", a12$TopDiameter_ub, 365, adjustment = VOLUME_ROUND_DOWN_CM)
  top_b <- price_volume_m3("Top", b12$TopDiameter_ub, 370, adjustment = VOLUME_ROUND_DOWN_CM)
  cmp_ok <- nrow(a12) > 0L && nrow(b12) > 0L && isTRUE(all.equal(a12$price_volume_m3, top_a)) &&
    isTRUE(all.equal(b12$price_volume_m3, top_b))
}
ok12 <- isTRUE(back$ok) && identical(back$defs$PriceLengths_cm, r$defs$PriceLengths_cm) &&
  isTRUE(scenarios_differ(r$defs, r$pm, d_nopl, r$pm)) && !isTRUE(scenarios_differ(r$defs, r$pm, back$defs, back$pm)) &&
  cmp_ok
rec("Check 12: Price lengths survive the scenario file; a scenario without them differs and is priced on the cut",
    ok(ok12), sprintf("round trip %s, price lengths kept %s | differs without them %s, equal after round trip %s | comparison: A on 365 cm, B on 370 cm %s",
                      isTRUE(back$ok), identical(back$defs$PriceLengths_cm, r$defs$PriceLengths_cm),
                      isTRUE(scenarios_differ(r$defs, r$pm, d_nopl, r$pm)),
                      !isTRUE(scenarios_differ(r$defs, r$pm, back$defs, back$pm)),
                      if (is.list(cmp)) cmp_ok else paste("failed:", cmp)))


# ==============================================================================
section("13. Fully forbidden inner diameter class")
# Every 120 mm cell of 8015 (197 Massevirke) forbidden, between priced 80 and
# 160 mm classes: rows 12-15 cm stay as empty rows and are never chosen.
r13 <- hpr_to_scenario(read_hpr(variant(forbid_class_8015_120)), sp_map, base_defs = base_defs)
K13 <- "spruce_197_massevirke"
ok13 <- FALSE; det13 <- "import failed"
if (isTRUE(r13$ok)) {
  g13 <- pm_to_grid(r13$pm, K13)
  rows13 <- as.integer(rownames(g13))
  empty_ok <- identical(rows13, 4:20) && all(is.na(as.matrix(g13[as.character(12:15), ]))) &&
    all(!is.na(as.matrix(g13[as.character(c(4:11, 16:20)), ])))
  warn13 <- any(grepl("12, 13, 14, 15 cm fully forbidden: empty rows", r13$info, fixed = TRUE)) &&
    !any(grepl("fully forbidden", r13$warnings, fixed = TRUE))   # info, not shown
  valid13 <- isTRUE(check_price_set(r13$defs, r13$pm)$ok)
  # Scenario file (wide sheets) and long CSV round trips keep the empty rows.
  fx13 <- tempfile(fileext = ".xlsx"); scenario_export(r13$defs, r13$pm, fx13)
  b13 <- scenario_import(fx13)
  xlsx_ok <- isTRUE(b13$ok) && same_prices(b13$pm, r13$pm, K13)
  fl13 <- tempfile(fileext = ".csv"); pm_export(r13$pm, fl13)
  up13 <- read_price_matrix_upload(fl13, fl13, r13$pm, matrix_keys = assortment_table_keys(r13$defs))
  long_ok <- length(up13$errors) == 0L && isTRUE(check_price_set(r13$defs, up13$long)$ok) &&
    same_prices(pm_minimal(up13$long), r13$pm, K13)
  # Editing another cell (as the grid does) keeps them.
  g_ed <- g13; g_ed["16", "30"] <- 999
  pm_ed <- pm_from_grid(g_ed, K13, r13$pm)
  edit_ok <- isTRUE(check_price_set(r13$defs, pm_ed)$ok) &&
    all(is.na(as.matrix(pm_to_grid(pm_ed, K13)[as.character(12:15), ])))
  # A run never chooses the empty classes; without the forbidden class it
  # does. Spruce sawlogs (Kubb, SCA) are left out of both runs, so pulpwood
  # is the spruce product at 12-15 cm.
  no_saw <- function(s) {
    k <- assortment_table_keys(s$defs)
    drop <- k %in% c("spruce_154_kubb", K_SCA)
    list(defs = s$defs[!drop, , drop = FALSE], pm = s$pm[!s$pm$ProductKey %in% k[drop], , drop = FALSE])
  }
  run_of <- function(s) {
    p <- products_from_defs(s$defs, s$pm); f <- tempfile(fileext = ".csv"); pm_export(s$pm, f)
    out <- tryCatch(run_tree_list_bucking(tl, f, root_dir, products = p$pd, length_classes = p$lc),
                    error = function(e) conditionMessage(e))
    unlink(f); out
  }
  run13 <- run_of(no_saw(r13)); ref13 <- run_of(no_saw(r))
  if (is.list(run13) && is.list(ref13)) {
    l13 <- run13$selected_logs_all; l13 <- l13[l13$ProductKey == K13, ]
    in_empty <- sum(l13$PriceDiameterClass_cm_used %in% 12:15)
    l_ref <- ref13$selected_logs_all; l_ref <- l_ref[l_ref$ProductKey == K13, ]
    ref_empty <- sum(l_ref$PriceDiameterClass_cm_used %in% 12:15)
    run_ok <- nrow(l13) > 0L && in_empty == 0L && ref_empty > 0L
  } else { run_ok <- FALSE; in_empty <- NA; ref_empty <- NA }
  unlink(c(fx13, fl13))
  ok13 <- empty_ok && warn13 && valid13 && xlsx_ok && long_ok && edit_ok && run_ok
  det13 <- sprintf("imported, rows %d-%d, 12-15 cm empty and the rest priced %s | warning %s | valid %s | scenario xlsx round trip %s | long CSV round trip %s | grid edit keeps them %s | run: %s logs in 12-15 cm (%s without the forbidden class), %d 197 Massevirke logs",
                   min(rows13), max(rows13), empty_ok, warn13, valid13, xlsx_ok, long_ok, edit_ok,
                   in_empty, ref_empty, if (is.list(run13)) nrow(l13) else 0L)
}
rec("Check 13: A fully forbidden inner diameter class stays as empty rows, valid and never chosen",
    ok(ok13), det13)


# ==============================================================================
section("14. Large files: the part before the first stem, .gz and .zip")
same_defs <- function(a, b) isTRUE(a$ok) && identical(a$species, h$species) && identical(a$products, h$products)
# A stem before the products: the definitions are not all before it, so the
# whole file is read.
stem_first <- function(l) {
  s <- grep("<Stem>", l, fixed = TRUE)[1L]; e <- grep("</Stem>", l, fixed = TRUE)[1L]
  p <- grep("<ProductDefinition>", l, fixed = TRUE)[1L]
  append(l[-(s:e)], l[s:e], after = p - 1L)
}
h_whole <- read_hpr(variant(stem_first))
# "<Stem>" inside a species group's biomass function is not a stem record.
biomass <- function(l) {
  i <- grep("<SpeciesGroupName>", l, fixed = TRUE)[1L]
  append(l, "<BioMassFunctions><Detailed><Stem>370</Stem></Detailed></BioMassFunctions>", after = i)
}
h_bio <- read_hpr(variant(biomass))
gz <- tempfile(fileext = ".hpr.gz"); con <- gzfile(gz, "wb"); writeLines(fx, con, useBytes = TRUE); close(con)
h_gz <- read_hpr(gz, "fixture.hpr.gz")
zd <- tempfile("zipsrc"); dir.create(zd); file.copy(FIXTURE, file.path(zd, "fixture.hpr"))
zf <- tempfile(fileext = ".zip"); zip::zipr(zf, file.path(zd, "fixture.hpr"))
h_zip <- read_hpr(zf, "fixture.zip")
writeLines("no hpr here", file.path(zd, "readme.txt"))
zn <- tempfile(fileext = ".zip"); zip::zipr(zn, file.path(zd, "readme.txt"))
h_nozip <- read_hpr(zn, "other.zip")
ok14 <- identical(h$header$read, "prefix") && h$header$mb_read < h$header$mb_file &&
  identical(h_whole$header$read, "whole") && same_defs(h_whole) &&
  identical(h_bio$header$read, "prefix") && isTRUE(h_bio$ok) && identical(h_bio$products, h$products) &&
  identical(h_gz$header$read, "prefix") && same_defs(h_gz) &&
  same_defs(h_zip) && !isTRUE(h_nozip$ok) && any(grepl("contains no .hpr file", h_nozip$errors, fixed = TRUE))
rec("Check 14: Only the part before the first stem is read (whole file if needed); .gz and .zip",
    ok(ok14), sprintf("fixture: %s, %s of %s MB | stem before the products: %s, same definitions %s | <Stem> in a biomass function: %s, same products %s | .gz: %s, same %s | .zip: same %s | zip without .hpr: '%s'",
                      h$header$read, h$header$mb_read, h$header$mb_file, h_whole$header$read, same_defs(h_whole),
                      h_bio$header$read, identical(h_bio$products, h$products), h_gz$header$read, same_defs(h_gz),
                      same_defs(h_zip), paste(h_nozip$errors, collapse = " ")))
unlink(c(gz, zf, zn, zd), recursive = TRUE)


# ==============================================================================
section("15. Import message: warnings only for what is lost or replaced")
# The fixture import loses nothing: no warnings; rounding, whole-cm classes,
# the mm volume diameter and forbidden cells are info (documented rules).
info_pat <- c("rounded up to whole dm", "not in whole cm", "forbidden cell", "Volume diameter")
in_info <- vapply(info_pat, function(p) any(grepl(p, r$info, fixed = TRUE)), logical(1))
in_warn <- vapply(info_pat, function(p) any(grepl(p, r$warnings, fixed = TRUE)), logical(1))
# Something lost: a product without priced cells (ticked anyway), a length
# class without a whole-dm length; replaced: an unsupported volume method.
no_cells <- variant(function(l) in_prod(l, "8017", "<BuckingCriteria>No limit</BuckingCriteria>",
                                        sprintf("<BuckingCriteria>%s</BuckingCriteria>", HPR_FORBIDDEN), all = TRUE))
h15 <- read_hpr(no_cells)
r15 <- hpr_to_scenario(h15, sp_map, keep = c("7949", "8017", "8019"), base_defs = base_defs)
w_skip <- any(grepl("154 Kubb not imported: no priced cells", r15$warnings, fixed = TRUE))
w_drop <- any(grepl("301 cm dropped", r6$warnings, fixed = TRUE))
w_meth <- any(grepl("solid volume used", w10$method, fixed = TRUE))
ok15 <- length(r$warnings) == 0L && all(in_info) && !any(in_warn) && w_skip && w_drop && w_meth
rec("Check 15: The import shows warnings only when something is lost or replaced",
    ok(ok15), sprintf("fixture import: %d warnings, info has %s | skipped product warned %s | dropped length class warned %s | replaced volume method warned %s",
                      length(r$warnings), paste(sprintf("%s=%s", info_pat, in_info), collapse = ", "),
                      w_skip, w_drop, w_meth))


# ==============================================================================
section("16. Imported Mid product")
# REDACTED23 (pine, length as defined in LengthClasses, diameter in
# mm) with VolumeDiameterCategory Mid, like the Mid sawlogs of a real file.
f16 <- variant(function(l) in_prod(l, "7949", ">Top</VolumeDiameterCategory>", ">Mid</VolumeDiameterCategory>"))
h16 <- read_hpr(f16)
r16 <- hpr_to_scenario(h16, sp_map, base_defs = base_defs)
d16 <- r16$defs[assortment_table_keys(r16$defs) == K_PINE_SAW, , drop = FALSE]
imp16 <- isTRUE(r16$ok) && identical(h16$products[["7949"]]$volume_method, "Mid") &&
  length(c(h16$warnings, r16$warnings)) == 0L && nrow(d16) == 1L &&
  identical(d16$VolumeDiameterCategory, "Mid") &&
  identical(d16$PriceLengths_cm, row(K_PINE_SAW)$PriceLengths_cm) &&
  any(grepl("REDACTED23: Volume diameter 'Measured diameter in mm': Apter rounds the mid diameter",
            r16$info, fixed = TRUE))
# Candidates on check 8's pine stem: a 37 dm log is priced on the under-bark
# diameter at the middle of the cut log, over the class lower limit (365 cm);
# the price class still comes from the top diameter.
p16 <- products_from_defs(r16$defs, r16$pm)
c16 <- generate_candidate_logs_one_tree(pv, p16$pd, p16$lc, stump_height_cm = 10L)
c16 <- c16[c16$ProductKey == K_PINE_SAW & c16$LogLength == 370 & c16$eligible, ][1L, ]
pvu <- add_under_bark_profile(pv)
mid16 <- approx(pvu$diameterPosition, pvu$DiameterValueUB, (c16$StartPos + c16$StopPos) / 2)$y
v16 <- price_volume_m3("Mid", NA, 365, adjustment = VOLUME_ROUND_DOWN_CM, mid_d_mm = mid16)
v16cut <- price_volume_m3("Mid", NA, 370, adjustment = VOLUME_ROUND_DOWN_CM, mid_d_mm = mid16)
cand16 <- isTRUE(all.equal(c16$price_volume_m3, v16)) && !isTRUE(all.equal(v16, v16cut)) &&
  !isTRUE(all.equal(v16, c37$price_volume_m3)) &&
  identical(c16$PriceDiameterClass_cm, as.integer(floor(c16$TopDiameter / 10)))
# A run with the imported scenario chooses the Mid product.
f16pm <- tempfile(fileext = ".csv"); pm_export(r16$pm, f16pm)
res16 <- tryCatch(run_tree_list_bucking(tl, f16pm, root_dir, products = p16$pd, length_classes = p16$lc),
                  error = function(e) conditionMessage(e))
run16 <- is.list(res16) && any(res16$selected_logs_all$ProductKey == K_PINE_SAW) &&
  all(!is.na(res16$selected_logs_all$price_volume_m3[res16$selected_logs_all$ProductKey == K_PINE_SAW]))
rec("Check 16: An imported Mid product maps to Mid without a warning, is priced on the mid diameter over the class lower limit, and is chosen in a run",
    ok(imp16 && cand16 && run16),
    sprintf("import: Mid %s, %d warnings, price lengths kept %s | 37 dm log from %d cm: mid %.1f mm, top %.1f mm (class %s), price volume %.5f (365 cm: %.5f, cut 370 cm: %.5f, Top: %.5f) | run: %s",
            identical(d16$VolumeDiameterCategory, "Mid"), length(c(h16$warnings, r16$warnings)),
            identical(d16$PriceLengths_cm, row(K_PINE_SAW)$PriceLengths_cm), c16$StartPos, mid16,
            c16$TopDiameter, c16$PriceDiameterClass_cm, c16$price_volume_m3, v16, v16cut, c37$price_volume_m3,
            if (is.list(res16)) sprintf("%d Mid logs", sum(res16$selected_logs_all$ProductKey == K_PINE_SAW))
            else paste("failed:", res16)))
unlink(c(f11, f12, f16, f16pm))


# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 25 complete.\n")
