# ==============================================================================
# audits/audit_38_check_texts.R
#
# Purpose : The texts of the bucking check (tab 5, roadmap 8) and of the NFI profile build: every id the
#           code uses exists in inputs/i18n/translations.csv in English and Norsk, every chk_* text is
#           used, the backend's check messages have msg rows, the progress texts translate. (Literal
#           user text in app/ files is audit 28 check 3.)
#
# Checks  :
#  1.  Every id used in app/check.R and app/check_report.R (L("..."), t_("...")) is a ui row with an English and a Norsk text
#  2.  Every chk_* ui row is used in app/check.R or app/check_report.R
#  3.  Every reason R/helpers_check.R writes for a stem left out, and the failure message of the app,
#      has a msg row (English text matched), with a Norsk text
#  4.  Other ids of roadmap 8: tab_check, help_use_5, asm_c_butt, v_butt_only, v_butt_not
#  5.  The progress texts of the background jobs (NFI profile build, bucking check) translate to Norsk
#  6.  Placeholders: the English and the Norsk text of a row have the same conversions in the same order
#
# Verdict : PASS / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/check/audit_38_check_texts.csv
#   outputs/check/audit_38_check_texts.txt
#
# Usage   : Rscript audits/audit_38_check_texts.R
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

suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)
source(file.path(root_dir, "audits", "helpers_audit.R"))

out_dir <- path_output("check", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_38_check_texts.txt")
audit_csv <- file.path(out_dir, "audit_38_check_texts.csv")
init_audit_log(audit_txt)

E <- new.env(parent = emptyenv())
rec <- function(check, result, detail = "") record_check(E, check, result, detail, log_path = audit_txt)
PASS <- "PASS"; FAIL <- "FAIL"
ok <- function(x) if (isTRUE(x)) PASS else FAIL
section <- function(title) {
  cat(sprintf("\n--- %s ---\n", title), file = audit_txt, append = TRUE)
  cat(sprintf("\n--- %s ---\n", title))
}
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)
cat("  Audit: texts of the bucking check (roadmap 8)\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

tbl <- i18n_table(file.path(root_dir, "inst", "extdata", "inputs", "i18n", "translations.csv"))
src_check <- paste(unlist(lapply(c("check.R", "check_report.R"), function(f) readLines(file.path(root_dir, "inst", "app", f), warn = FALSE, encoding = "UTF-8"))), collapse = "\n")
src_logic <- paste(readLines(file.path(root_dir, "R", "helpers_check.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
shorten <- function(x, n = 8L) if (length(x) == 0L) "none" else
  paste(c(utils::head(x, n), if (length(x) > n) sprintf("... (%d)", length(x))), collapse = " | ")
filled <- function(id, kind) { r <- tbl[tbl$id == id & tbl$kind == kind, , drop = FALSE]
  nrow(r) == 1L && nzchar(r$en) && nzchar(r$no) }

section("01. Ids used by app/check.R")
used <- unique(unlist(regmatches(src_check, gregexpr("(?<=\\b(?:L|t_)\\(\")[A-Za-z0-9_]+(?=\")", src_check, perl = TRUE))))
used <- c(used, unlist(regmatches(src_check, gregexpr("(?<=\")chk_[A-Za-z0-9_]+(?=\")", src_check, perl = TRUE))))
io_ids <- unlist(regmatches(src_check, gregexpr("(?<=(?:Output|downloadButton|fileInput|actionButton|checkboxInput)\\(\")chk_[A-Za-z0-9_]+(?=\")", src_check, perl = TRUE)))
used <- setdiff(unique(used[nzchar(used)]), io_ids)      # output and input ids are not texts
missing1 <- used[!vapply(used, filled, NA, kind = "ui")]
rec("Check 01: every id app/check.R uses is a ui row with an English and a Norsk text",
    ok(length(used) > 40L && length(missing1) == 0L), sprintf("%d ids; missing or empty: %s", length(used), shorten(missing1)))

section("02. No unused chk_* text")
chk_ids <- tbl$id[tbl$kind == "ui" & startsWith(tbl$id, "chk_")]
unused <- chk_ids[!vapply(chk_ids, function(i) grepl(sprintf("\"%s\"", i), src_check, fixed = TRUE), NA)]
rec("Check 02: every chk_* ui row is used in app/check.R", ok(length(chk_ids) > 60L && length(unused) == 0L),
    sprintf("%d rows; unused: %s", length(chk_ids), shorten(unused)))

section("03. Backend messages")
reasons <- unique(unlist(regmatches(src_logic, gregexpr("(?<=stems\\$reason\\[[^\\]]{1,160}\\] <- \")[^\"]+(?=\")", src_logic, perl = TRUE))))
reasons <- c(reasons, unlist(regmatches(src_logic, gregexpr("(?<=reason = \")[^\"]+(?=\")", src_logic, perl = TRUE))))
reasons <- unique(c(reasons, "The check failed: %s"))
has_msg <- vapply(reasons, function(r) { k <- tbl[tbl$kind == "msg" & tbl$en == r, , drop = FALSE]; nrow(k) >= 1L && all(nzchar(k$no)) }, NA)
rec("Check 03: every reason the check gives for a stem left out, and the failure message, has a msg row in both languages",
    ok(length(reasons) >= 5L && all(has_msg)), sprintf("%d messages; without a row: %s", length(reasons), shorten(reasons[!has_msg])))

section("04. Other ids")
others <- list(c("tab_check", "ui"), c("chk_dl_pdf", "ui"), c("chk_left_out", "ui"), c("chk_lim_g", "ui"), c("chk_lim_h", "ui"), c("chk_lim_j", "ui"), c("help_use_5", "ui"), c("asm_c_butt", "ui"), c("v_butt_only", "value"), c("v_butt_not", "value"),
               c("v_stage_nfi_build", "value"), c("v_stage_nfi_save", "value"))
bad4 <- vapply(others, function(o) !filled(o[1L], o[2L]), NA)
rec("Check 04: tab_check, chk_lim_j (recorded value can exceed best possible), help_use_5, asm_c_butt, v_butt_only, v_butt_not and the two NFI build progress texts exist in both languages",
    ok(!any(bad4)), sprintf("missing: %s", shorten(vapply(others[bad4], `[`, "", 1L))))

section("05. Progress texts")
prog_en <- c("Building NFI stem profiles (first use only)", "Saving NFI stem profiles", "Loading NFI data and taper profiles", "Bucking trees")
prog_no <- vapply(prog_en, tr_value, "", lang = "no")
chk_prog <- c("chk_p_read", "chk_p_opt", "chk_p_val", "chk_p_tab")
rec("Check 05: the progress texts of the NFI profile build and of the job translate to Norsk (and differ from English); the four check steps have both texts",
    ok(all(prog_no != prog_en) && all(vapply(chk_prog, filled, NA, kind = "ui"))), sprintf("Norsk: %s", shorten(unname(prog_no), 4L)))

section("06. Placeholders")
ph <- function(s) { p <- regmatches(s, gregexpr("%[0-9$]*[sdf]", s))[[1L]]; p }
subt <- tbl[(startsWith(tbl$id, "chk_") | tbl$id %in% c("v_stage_nfi_build", "v_stage_nfi_save")), ]
bad6 <- subt$id[vapply(seq_len(nrow(subt)), function(i) !identical(gsub("[0-9]+\\$", "", ph(subt$en[i])), gsub("[0-9]+\\$", "", ph(subt$no[i]))), NA)]
rec("Check 06: English and Norsk text of every chk_* row have the same placeholders", ok(length(bad6) == 0L), sprintf("differing: %s", shorten(bad6)))

write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 38 complete.\n")
