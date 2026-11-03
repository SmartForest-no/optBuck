# ==============================================================================
# audits/audit_28_translations.R
#
# Purpose : The Norwegian language option (roadmap Phase 1, 2026-10-27): the
#           one translation table inputs/i18n/translations.csv and
#           R/helpers_i18n.R, without a browser (audit 17 drives the app in
#           both languages).
#
# Checks  :
#  1.  Table: columns id, kind, en, no, note; ids unique; kind ui / msg /
#      value; en and no non-empty and valid UTF-8; the same sprintf
#      placeholders in the same order in en and no (positional %1$s allowed
#      in no); value and msg rows with the same English agree in Norsk
#  2.  Every text id the code uses exists (literal ids in app/ and
#      R/helpers_result_tables.R, and the ids built in code: tree counts,
#      legends, maturity classes, plural and A/B variants), and no ui id is
#      unused
#  3.  No English left in the app code: every prose-like string literal in
#      app/*.R is an English message or value of the table (translated when
#      shown), apart from a short allowlist of non-text strings
#  4.  Every message template round-trips: an example text in English
#      matches its own template and translates without a miss; tr() and
#      tr_value() leave English unchanged; unknown values pass through
#  5.  Backend messages users see translate without a miss: tree-list
#      errors, warnings, excluded reasons and height sources; .hpr import
#      (fixture, product table reasons, broken files); .hpr stems (fixture
#      drops, warnings, info); price uploads, scenario files and checks;
#      matrix row/column edits
#  6.  Downloads: write_download_csv() in English equals write.csv; in Norsk
#      it reads back (";", decimal comma, BOM) to the same values;
#      log_tally_xlsx() in English keeps its sheet names and headers, in
#      Norsk the headers, values and the long sheet's name are translated
#      and the numbers are the same
#  7.  Display helpers: tree_counts() in English gives the text of the
#      version before Phase 1 (fixed cases), Norsk inflects per species;
#      fmt_num() in both languages
#
# Verdict : PASS / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/i18n/audit_28_translations.csv
#   outputs/i18n/audit_28_translations.txt
#
# Usage   : Rscript audits/audit_28_translations.R
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
# Pure app helpers (no Shiny session): uploads and tree counts.
source(file.path(root_dir, "app", "helpers_upload.R"))
source(file.path(root_dir, "app", "helpers_validation.R"))

out_dir <- path_output("i18n", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_28_translations.txt")
audit_csv <- file.path(out_dir, "audit_28_translations.csv")
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
cat("  Audit: translations (Norsk / English)\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

tbl <- i18n_table(file.path(root_dir, "inputs", "i18n", "translations.csv"))
shorten <- function(x, n = 6L) if (length(x) == 0L) "none" else
  paste(c(utils::head(x, n), if (length(x) > n) sprintf("... (%d)", length(x))), collapse = " | ")


# ==============================================================================
section("01. The table")
ph_seq <- function(s) {
  p <- regmatches(s, gregexpr(.I18N_PH, s, perl = TRUE))[[1L]]
  pos <- suppressWarnings(as.integer(sub("^%([0-9]+)\\$.*$", "\\1", p)))
  if (all(!is.na(pos)) && length(p)) p <- p[order(pos)]
  substring(p, nchar(p))                       # the conversions, in argument order
}
cols_ok <- identical(names(tbl), c("id", "kind", "en", "no", "note"))
dup_ids <- tbl$id[duplicated(tbl$id)]
bad_kind <- tbl$id[!tbl$kind %in% c("ui", "msg", "value")]
empty <- tbl$id[!nzchar(tbl$en) | !nzchar(tbl$no)]
bad_utf8 <- tbl$id[!validUTF8(tbl$en) | !validUTF8(tbl$no)]
ph_bad <- tbl$id[vapply(seq_len(nrow(tbl)), function(i)
  !identical(ph_seq(tbl$en[i]), ph_seq(tbl$no[i])), logical(1))]
mv <- tbl[tbl$kind %in% c("msg", "value"), , drop = FALSE]
conflict <- unique(mv$en[duplicated(mv$en) & !duplicated(mv[, c("en", "no")])])
ok1 <- cols_ok && !length(dup_ids) && !length(bad_kind) && !length(empty) && !length(bad_utf8) &&
  !length(ph_bad) && !length(conflict)
rec("Check 01: table columns, unique ids, kinds, non-empty UTF-8 texts, same placeholders, no conflicting duplicates",
    ok(ok1), sprintf("%d rows (%s) | duplicated ids: %s | bad kinds: %s | empty: %s | invalid UTF-8: %s | placeholders differ: %s | conflicting English: %s",
                     nrow(tbl), paste(names(table(tbl$kind)), table(tbl$kind), collapse = ", "),
                     shorten(dup_ids), shorten(bad_kind), shorten(empty), shorten(bad_utf8),
                     shorten(ph_bad), shorten(conflict)))


# ==============================================================================
section("02. Ids used in the code")
code_files <- c(Sys.glob(file.path(root_dir, "app", "*.R")),
                file.path(root_dir, "R", c("helpers_result_tables.R", "helpers_hpr_stems.R")))   # warning texts: grade_warning_text()
code <- unlist(lapply(code_files, readLines, encoding = "UTF-8", warn = FALSE))
js <- readLines(file.path(root_dir, "app", "www", "apter.js"), warn = FALSE)
# Literal ids in tr(), t_(), L(), M(), T_() calls (also c("a", "b") inside them).
calls <- regmatches(code, gregexpr("\\b(tr|t_|L|M|T_)\\((c\\()?\"[a-z][a-z0-9_]*\"(, *\"[a-z][a-z0-9_]*\")*", code, perl = TRUE))
lit_ids <- unique(unlist(lapply(unlist(calls), function(x)
  regmatches(x, gregexpr("(?<=\")[a-z][a-z0-9_]*(?=\")", x, perl = TRUE))[[1L]])))
# Ids the code builds (paste0 / sprintf / unit_id()).
built <- c(
  as.vector(outer(c("tc_spruce_", "tc_pine_"), c("tree", "trees", "stem", "stems"), paste0)),
  as.vector(outer(c("tc_spruce_", "tc_pine_"), c("tree_last", "trees_last", "stem_last", "stems_last",
                                                 "tree_last_of_many", "stem_last_of_many"), paste0)),
  "tc_none_tree", "tc_none_stem",
  paste0("leg_", c("sawlog", "pulpwood", "waste", "custom")), paste0("hkl_", 3:5),
  "cmp_all_unchanged", "cmp_all_unchanged_1", "cmp_n_unchanged", "cmp_n_unchanged_1",
  "stem_note_tree", "stem_note_tree_ab", "stem_note_stem", "stem_note_stem_ab",
  "lbl_trees", "lbl_trees_stems", "lbl_trees_with_logs", "lbl_trees_with_logs_stems",
  "lbl_trees_with_logs_ab", "lbl_trees_with_logs_ab_stems",
  "msg_added", "msg_added_both", "msg_imported", "msg_imported_1", "stem_title_tree", "stem_title_stem",
  "logs_preview", "logs_preview_ab", "tally_caption", "tally_caption_flat",
  "tl_heights_line", "tl_heights_line_ratio",
  "window_title", "run_starting")                        # read by www/apter.js
used <- unique(c(lit_ids, built))
missing_ids <- setdiff(used, tbl$id)
quoted <- unique(unlist(regmatches(c(code, js), gregexpr("[\"'][a-z][a-z0-9_]*[\"']", c(code, js)))))
quoted <- gsub("[\"']", "", quoted)
unused <- setdiff(tbl$id[tbl$kind == "ui"], c(used, quoted))
js_ok <- all(c("window_title", "run_starting") %in% tbl$id) && any(grepl("t('run_starting')", js, fixed = TRUE))
ok2 <- !length(missing_ids) && !length(unused) && js_ok && length(lit_ids) > 200L
rec("Check 02: every id the code uses exists; no ui id unused",
    ok(ok2), sprintf("%d literal ids, %d built | missing: %s | unused ui ids: %s | JS ids: %s",
                     length(lit_ids), length(built), shorten(missing_ids), shorten(unused), js_ok))


# ==============================================================================
section("03. No English text left in the app code")
# Prose-like string constants (two or more words, or a capitalised word with
# a space) in app/*.R; allowed only if the table translates them at display
# (msg / value rows: backend stage names, the one English error kept for the
# backend's messages).
# helpers_root.R is left out: its errors stop the app before it starts
# (developer messages, never in the browser).
app_files <- setdiff(Sys.glob(file.path(root_dir, "app", "*.R")), file.path(root_dir, "app", "helpers_root.R"))
lits <- do.call(rbind, lapply(app_files, function(f) {
  pd <- utils::getParseData(parse(f, keep.source = TRUE, encoding = "UTF-8"))
  s <- pd[pd$token == "STR_CONST", c("line1", "text")]
  if (!nrow(s)) return(NULL)
  data.frame(file = basename(f), line = s$line1,
             text = substring(s$text, 2L, nchar(s$text) - 1L), stringsAsFactors = FALSE)
}))
prose <- grepl("[A-Za-z]{3,}", lits$text) &
  (grepl("^[A-Z][a-z]+ [A-Za-z]", lits$text) | grepl("[a-z]{2,} [a-z]{2,} [a-z]{2,}", lits$text))
# Non-text strings that look like prose: JS code, the language names on the
# switch (each in its own language), the switch's aria-label (both
# languages) and county names (Norwegian proper names, setup.R).
ALLOW <- c("Språk / Language", "Norsk", "English", "Sogn og Fjordane",
           "function(rowInfo) { Shiny.setInputValue('asm_click', rowInfo.original['.key'], {priority: 'event'}); }",
           "Handsontable.renderers.NumericRenderer.apply(this, arguments);")
known <- c(tbl$en[tbl$kind %in% c("msg", "value")])
is_known <- lits$text %in% known
left <- lits[prose & !is_known & !lits$text %in% ALLOW & !grepl("^\\s*function|Handsontable|instance\\.|td\\.", lits$text), ]
ok3 <- nrow(left) == 0L
rec("Check 03: no untranslated English string literals in app/*.R",
    ok(ok3), sprintf("%d string literals, %d prose-like, %d translated at display | left: %s",
                     nrow(lits), sum(prose), sum(prose & is_known),
                     shorten(sprintf("%s:%d '%s'", left$file, left$line, left$text))))


# ==============================================================================
section("04. Templates round-trip")
i18n_reset_misses()
tp <- tbl[tbl$kind %in% c("msg", "value") & grepl(.I18N_PH, tbl$en, perl = TRUE), , drop = FALSE]
fill_example <- function(s) {
  i <- 0L
  gsub_fn <- function(m) { i <<- i + 1L
    switch(substring(m, nchar(m)), d = , i = as.character(10L + i), f = , g = sprintf("%.1f", 1.5 + i),
           paste0("Xq", i)) }
  out <- s; m <- gregexpr(.I18N_PH, s, perl = TRUE)[[1L]]
  if (m[1L] < 0) return(gsub("%%", "%", s, fixed = TRUE))
  parts <- regmatches(s, list(m))[[1L]]
  regmatches(out, list(m)) <- list(vapply(parts, gsub_fn, ""))
  gsub("%%", "%", out, fixed = TRUE)
}
rt <- vapply(seq_len(nrow(tp)), function(i) {
  ex <- fill_example(tp$en[i])
  out <- tr_msg(ex, "no")
  !identical(out, ex) || identical(tp$en[i], tp$no[i])
}, logical(1))
rt_fail <- tp$id[!rt]
misses4 <- i18n_misses()
en_same <- identical(tr_msg(c("Total", "spruce"), "en"), c("Total", "spruce")) &&
  identical(tr_value(c("Spruce", "Top cylinder"), "en"), c("Spruce", "Top cylinder")) &&
  identical(tr("btn_run", lang = "en"), "Run optimizer")
pass_through <- identical(tr_value(c("SCA 140 Sagtømmer", NA), "no"), c("SCA 140 Sagtømmer", NA))
ok4 <- !length(rt_fail) && !length(misses4) && en_same && pass_through
rec("Check 04: every message template round-trips; English unchanged; unknown values pass through",
    ok(ok4), sprintf("%d templates | failed: %s | misses: %s | English unchanged %s | pass-through %s",
                     nrow(tp), shorten(rt_fail), shorten(misses4), en_same, pass_through))


# ==============================================================================
section("05. Backend messages users see")
i18n_reset_misses()
seen <- character(0)
see <- function(...) seen <<- c(seen, unlist(list(...)))

# Tree lists: read errors, validation errors and warnings, excluded reasons,
# height sources and notes.
wcsv <- function(df) { f <- tempfile(fileext = ".csv"); utils::write.csv(df, f, row.names = FALSE); f }
see(read_tree_list(tempfile(fileext = ".txt"), "x.txt")$errors)
see(read_tree_list(wcsv(data.frame(a = 1)), "a.csv")$errors)
bad <- data.frame(tree_id = c("1", "1", NA, "4", "5", "6"), species = c("spruce", "spruce", "pine", "birch", "pine", "pine"),
                  dbh = c("25", "x", "20", "30", NA, "-1"), height = c(20, 21, 18, 22, 0, 15), extra = 1)
p_bad <- prepare_tree_list(read_tree_list(wcsv(bad), "bad.csv")$data)
see(p_bad$errors, p_bad$warnings)
mm <- data.frame(tree_id = 1:12, species = rep(c("spruce", "pine"), 6), dbh = seq(150, 400, length.out = 12),
                 height = seq(150, 300, length.out = 12))
p_mm <- prepare_tree_list(mm); see(p_mm$errors, p_mm$warnings)
ok_list <- data.frame(tree_id = 1:14, species = rep(c("spruce", "pine"), 7),
                      dbh = c(3, 4, seq(12, 52, length.out = 12)), height = c(NA, 3, rep(NA, 12)))
p_ok <- prepare_tree_list(ok_list)
see(p_ok$errors, p_ok$warnings, p_ok$trees$excluded_reason[!is.na(p_ok$trees$excluded_reason)],
    p_ok$trees$height_source, p_ok$heights$method, p_ok$heights$note[nzchar(p_ok$heights$note)])
cal <- data.frame(tree_id = 1:6, species = "spruce", dbh = c(15, 20, 25, 30, 35, 40),
                  height = c(16, NA, 22, NA, 26, NA))
p_cal <- prepare_tree_list(cal); see(p_cal$warnings, p_cal$trees$height_source, p_cal$heights$method)
tl_n <- length(seen)

# .hpr import: the fixture, its product table, broken files and an empty map.
FIXTURE <- file.path(root_dir, "inputs", "test_fixtures", "hpr_example_synthetic.hpr")
h <- read_hpr(FIXTURE, "fixture.hpr"); see(h$errors, h$warnings)
map_all <- stats::setNames(guess_hpr_species(h$species$name, h$species$user_id), h$species$key)
tab <- hpr_product_table(h, map_all); see(tab$reason[nzchar(tab$reason)])
see(hpr_product_table(h, stats::setNames(rep("none", nrow(h$species)), h$species$key))$reason)
res <- hpr_to_scenario(h, map_all, tab$key[tab$default], base_defs = defs_from_products(load_products(root_dir)))
see(res$errors, res$warnings, res$info)
see(hpr_to_scenario(h, stats::setNames(rep("none", nrow(h$species)), h$species$key))$errors)
fx <- readLines(FIXTURE, encoding = "UTF-8", warn = FALSE)
variant <- function(lines) { f <- tempfile(fileext = ".hpr"); writeLines(lines, f, useBytes = TRUE); f }
see(read_hpr(variant("not xml at all"), "broken.hpr")$errors)
see(read_hpr(variant(c("<?xml version='1.0'?>", "<Foo/>")), "foo.hpr")$errors)
see(read_hpr(variant(sub('diameterUnit="mm"', 'diameterUnit="cm"', fx, fixed = TRUE)), "cm.hpr")$errors)
see(read_hpr(variant(sub('messageType="hpr"', 'messageType="pri"', fx, fixed = TRUE)), "pri.hpr")$errors)
zd <- tempfile("zipsrc"); dir.create(zd); writeLines("x", file.path(zd, "readme.txt"))
zf <- tempfile(fileext = ".zip"); zip::zipr(zf, file.path(zd, "readme.txt"))
see(read_hpr(zf, "nohpr.zip")$errors)
hpr_n <- length(seen) - tl_n

# .hpr stems: the fixture's stems with drops, warnings and info.
st <- read_hpr_stems(FIXTURE, "fixture.hpr"); see(st$errors, st$warnings, st$info)
ck <- hpr_stem_checks(st, map_all); see(ck$errors, ck$warnings, ck$info)
ck0 <- hpr_stem_checks(st, stats::setNames(rep("none", length(map_all)), names(map_all)))
see(ck0$errors, ck0$warnings, ck0$info)
# Cleaning: one stem per check (broken top, no fall, a 40 mm step).
sk <- st$stems$species_key[st$stems$kind == "single"][1L]; s10 <- seq(0, 1000, by = 10)
stc <- list(ok = TRUE, errors = character(0), warnings = character(0), info = character(0), file = NA_character_,
            species = st$species, stems = data.frame(stem_key = c("1", "2", "3"), species_key = sk, kind = "single",
                                                    category = "Over bark", n_points = 101L, stringsAsFactors = FALSE),
            idx = rep(1:3, each = 101L), pos = rep(s10, 3L),
            d = c(330 - 0.1 * s10, rep(150, 101L), ifelse(s10 < 500, 180, 140)))
see(hpr_stem_checks(stc, stats::setNames("spruce", sk))$warnings)
see(read_hpr_stems(variant(fx[!grepl("<Stem>|<StemKey>|</Stem>", fx)]), "nostems.hpr")$errors)
stems_n <- length(seen) - tl_n - hpr_n

# Prices: uploads, scenario files, checks and matrix edits.
defs <- defs_from_products(load_products(root_dir)); pm <- pm_minimal(load_price_matrix_long(root_dir))
f_bad <- wcsv(data.frame(a = 1, b = 2)); see(upload_price_set(f_bad, "bad.csv", defs, pm)$errors)
neg <- pm; neg$PriceNOKperM3[1] <- -5; see(check_price_set(defs, neg)$errors)
gap <- pm[!(pm$ProductKey == "spruce_sawlog" & pm$PriceDiameterClass_cm %in% 30), ]
see(check_price_set(defs, gap)$errors, check_price_set(defs, gap)$warnings)
see(upload_price_set(wcsv(transform(pm, ProductKey = "pine_poles")), "poles.csv", defs, pm)$errors)
see(upload_assortment_prices(wcsv(pm), "all.csv", defs, pm, "nope_key")$errors)
see(scenario_import(f_bad)$errors)
xl <- tempfile(fileext = ".xlsx"); writexl::write_xlsx(list(other = data.frame(a = 1)), xl)
see(scenario_import(xl)$errors)
see(upload_price_set(tempfile(fileext = ".rds"), "x.rds", defs, pm)$errors)
see(upload_price_set(tempfile(fileext = ".txt"), "x.txt", defs, pm)$errors)
custom <- rbind(defs, transform(defs[1, ], Name = "Poles", Type = "poles"))
see(check_price_set(custom, pm)$errors)
see(check_price_set(defs[0, ], pm)$errors)
err_of <- function(expr) tryCatch({ expr; character(0) }, error = function(e) conditionMessage(e))
see(err_of(pm_add_class(pm, "spruce_sawlog", "high", max_top_cm = 50)),
    err_of(pm_add_length(pm, "spruce_sawlog", 34)), err_of(pm_add_length(pm, "spruce_sawlog", -2)),
    err_of(pm_remove_length(pm, "spruce_sawlog", 99)),
    err_of(copy_assortment(defs[0, ], pm[0, ], defs, pm, "spruce_sawlog")))
price_n <- length(seen) - tl_n - hpr_n - stems_n

seen <- unique(seen[!is.na(seen) & nzchar(seen)])
out <- tr_msg(seen, "no")
misses5 <- i18n_misses()
untouched <- seen[out == seen & grepl("[A-Za-z]{3,} [a-z]{3,}", seen)]
ok5 <- length(seen) >= 60L && !length(misses5) && !length(untouched)
rec("Check 05: backend messages (tree list, .hpr import, .hpr stems, prices) translate without a miss",
    ok(ok5), sprintf("%d distinct messages (tree list %d, .hpr import %d, .hpr stems %d, prices %d) | misses: %s | unchanged: %s",
                     length(seen), tl_n, hpr_n, stems_n, price_n, shorten(misses5, 4L), shorten(untouched, 4L)))
writeLines(c("English | Norsk", paste(seen, "|", out)), file.path(out_dir, "audit_28_messages.txt"), useBytes = TRUE)


# ==============================================================================
section("06. Downloads")
df6 <- data.frame(`Verdi (kr)` = c(1234.5, NA), Treslag = c("Gran", "Furu"), n = c(3L, 4L), check.names = FALSE)
f_en <- tempfile(fileext = ".csv"); f_ref <- tempfile(fileext = ".csv")
write_download_csv(df6, f_en, "en"); utils::write.csv(df6, f_ref, row.names = FALSE, fileEncoding = "UTF-8")
en_same6 <- identical(readBin(f_en, "raw", 1e5), readBin(f_ref, "raw", 1e5))
f_no <- tempfile(fileext = ".csv"); write_download_csv(df6, f_no, "no")
raw_no <- readBin(f_no, "raw", 1e5)
back <- utils::read.csv2(f_no, fileEncoding = "UTF-8-BOM", check.names = FALSE)
no_ok6 <- identical(raw_no[1:3], as.raw(c(0xEF, 0xBB, 0xBF))) && identical(names(back), names(df6)) &&
  isTRUE(all.equal(back[[1]], df6[[1]])) && identical(back$Treslag, df6$Treslag) &&
  grepl("1234,5", rawToChar(raw_no[-(1:3)]), fixed = TRUE)
# Log tally xlsx on a small synthetic tally.
logs <- data.frame(ProductKey = c("spruce_sawlog", "spruce_sawlog", "spruce_pulpwood"),
                   ProductType = c("sawlog", "sawlog", "pulpwood"), species_group = "spruce",
                   PriceDiameterClass_cm_used = c(20L, 22L, NA), LengthClass_dm = c(43L, 46L, 30L),
                   commercial_volume_m3 = c(0.2, 0.3, 0.05))
tl6 <- list(A = log_tally(logs, pm))
x_en <- tempfile(fileext = ".xlsx"); x_no <- tempfile(fileext = ".xlsx")
log_tally_xlsx(x_en, tl6, list(A = pm)); log_tally_xlsx(x_no, tl6, list(A = pm), lang = "no")
sh_en <- readxl::excel_sheets(x_en); sh_no <- readxl::excel_sheets(x_no)
s1_en <- as.data.frame(readxl::read_excel(x_en, sh_en[1])); s1_no <- as.data.frame(readxl::read_excel(x_no, sh_no[1]))
num_cols <- names(s1_en)[vapply(s1_en, is.numeric, logical(1))]
xl_ok <- identical(sh_en[length(sh_en)], "long") && identical(sh_no[length(sh_no)], "liste") &&
  "Total" %in% names(s1_en) && "Totalt" %in% names(s1_no) && "Diameter (cm)" %in% names(s1_en) &&
  isTRUE(all.equal(unname(s1_en[num_cols]), unname(s1_no[match(tr_value(num_cols, "no"), names(s1_no))]))) &&
  any(grepl("Stokker", s1_no[[1]])) &&
  identical(names(readxl::read_excel(x_no, "liste"))[1:3], c("Scenario", "Sortiment", "Type"))
ok6 <- en_same6 && no_ok6 && xl_ok
rec("Check 06: downloads: English CSV as write.csv; Norsk CSV with ';', decimal comma and BOM reads back; tally xlsx in both languages",
    ok(ok6), sprintf("English CSV identical %s | Norsk CSV %s | xlsx sheets en '%s' / no '%s', headers and numbers %s",
                     en_same6, no_ok6, paste(sh_en, collapse = "', '"), paste(sh_no, collapse = "', '"), xl_ok))


# ==============================================================================
section("07. Display helpers")
# tree_counts() as before Phase 1 (texts of main, 2026-10-23), and in Norsk.
cases <- list(list(c("spruce", "spruce", "pine"), 3L, "tree", "3 plots · 2 spruce · 1 pine trees",
                   "3 prøveflater · 2 graner · 1 furu"),
              list("pine", NA, "stem", "1 pine stem", "1 furustamme"),
              list(c("spruce", "pine", "pine"), 1L, "stem", "1 plot · 1 spruce · 2 pine stems",
                   "1 prøveflate · 1 granstamme · 2 furustammer"),
              list(character(0), 5L, "tree", "No trees", "Ingen trær"),
              list(rep("spruce", 1500), 655L, "tree", "655 plots · 1,500 spruce trees",
                   "655 prøveflater · 1 500 graner"),
              list("spruce", 1L, "tree", "1 plot · 1 spruce tree", "1 prøveflate · 1 gran"))
tc_bad <- Filter(Negate(is.null), lapply(cases, function(cc) {
  en <- tree_counts(cc[[1]], cc[[2]], cc[[3]])$text
  no <- tree_counts(cc[[1]], cc[[2]], cc[[3]], lang = "no")$text
  if (!identical(en, cc[[4]]) || !identical(no, cc[[5]])) sprintf("'%s' / '%s'", en, no)
}))
fmt_ok <- identical(fmt_num(c(1234.56, NA, -0.5), 1L, "en"), c("1,234.6", "–", "-0.5")) &&
  identical(fmt_num(c(1234.56, NA, -0.5), 1L, "no"), c("1 234,6", "–", "-0,5")) &&
  identical(fmt_num(12.25, 2L, "no", big = FALSE), "12,25")
ok7 <- !length(tc_bad) && fmt_ok
rec("Check 07: tree_counts() in English as before Phase 1 and inflected in Norsk; fmt_num()",
    ok(ok7), sprintf("%d tree-count cases, wrong: %s | fmt_num %s", length(cases), shorten(unlist(tc_bad)), fmt_ok))

section("08. Product names from files keep their case")
dn <- function(sp, nm, l) assortment_display_name(sp, nm, l)
ok8 <- identical(dn("spruce", "SAG - REDACTED35", "no"), "Gran SAG - REDACTED35") &&
  identical(dn("spruce", "SAG - REDACTED35", "en"), "Spruce SAG - REDACTED35") &&
  identical(dn("pine", "MASSE FRISK", "no"), "Furu MASSE FRISK") &&
  identical(dn("spruce", "REDACTED25", "no"), "Gran REDACTED25") &&
  identical(dn("spruce", "eNERGI 50-90%", "en"), "Spruce eNERGI 50-90%") &&
  identical(dn("spruce", "Sawlog", "en"), "Spruce sawlog") && identical(dn("spruce", "Sawlog", "no"), "Gran sagtømmer") &&
  identical(dn("pine", "Waste", "no"), "Furu vrak")
rec("Check 08: a product name from a file is shown as written ('SAG - REDACTED35' -> 'Gran SAG - ...' / 'Spruce SAG - ...', 'REDACTED25' unchanged); only the base names (Sawlog, Pulpwood, Waste) start in lower case",
    ok(ok8), paste(dn("spruce", "SAG - REDACTED35", "no"), "|", dn("spruce", "SAG - REDACTED35", "en"), "|", dn("spruce", "Sawlog", "no")))


# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 28 complete.\n")
