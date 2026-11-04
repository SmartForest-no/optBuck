# ==============================================================================
# audits/audit_37_check_vs_old.R
#
# Purpose : R3 (roadmap 8, task 2): the new reader, profiles and permitted grades against the SAVED
#           output of the old optBuck (C:/apter_private/optbuck_reference/, never re-run), on the owner's
#           private files file1, file3, file4 and file5 (patched copy file5p). Private files: REVIEW
#           when they are absent. The work is scripts/verify_r3.R (parts a-d; part e, the ablation on
#           file4, is a report there and not asserted); this audit runs it and asserts:
#
# Checks  :
#  1.  Reader (a): every field of read_hpr_logs() equals the old getLogs() in at least 99.9 % of the
#      logs (ProductKey, LogLength, m3sub, m3sob, the six diameters), the same logs in both; the two
#      differences that remain are named: StartPos (the old value is the cumulative sum of the
#      previous lengths, the new one the file's recorded StartPos, 1-9 cm) and file5's unclassified
#      key (the patched copy renamed 2147483647 to 999999); multi-tree stems are read by the new
#      reader only
#  2.  Profiles (b): for every stem both bucked, the new profile has the old points with the old
#      diameters (positions and diameters equal in 100 %)
#  3.  Permitted grades (c): identical per product; the only difference is the unclassified product
#  4.  B5-3(i) (d): the new optimum is never below the new value of an old optimal pattern the
#      optimiser could have chosen (0 violations); the number of eligible stems is reported (the old
#      patterns cut lengths off Apter's length lattice in most stems, so the test has little power on
#      file1 and file5)
#
# Verdict : PASS / FAIL, REVIEW without the private files (helpers_audit.R)
#
# Outputs :
#   outputs/check/audit_37_check_vs_old.csv
#   outputs/check/audit_37_check_vs_old.txt
#   outputs/check/audit_37_r3.csv         (aggregates only)
#
# Usage   : Rscript audits/audit_37_check_vs_old.R
# ==============================================================================

root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  env <- Sys.getenv("APTER_ROOT", unset = "")
  if (nzchar(env)) {
    d <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (!is_root(d)) stop("Project root '", d, "' (from APTER_ROOT) does not contain R/helpers_paths.R.", call. = FALSE)
  } else {
    d <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    while (!is_root(d) && dirname(d) != d) d <- dirname(d)
    if (!is_root(d)) stop("Cannot locate the project root: set APTER_ROOT, or run from inside the project directory.", call. = FALSE)
  }
  d
})
Sys.setenv(APTER_ROOT = root_dir)

source(file.path(root_dir, "R", "helpers_paths.R"))
source(file.path(root_dir, "R", "helpers_audit.R"))

out_dir <- path_output("check", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_37_check_vs_old.txt")
audit_csv <- file.path(out_dir, "audit_37_check_vs_old.csv")
init_audit_log(audit_txt)

E <- new.env(parent = emptyenv())
rec <- function(check, result, detail = "") record_check(E, check, result, detail, log_path = audit_txt)
PASS <- "PASS"; FAIL <- "FAIL"; REVIEW <- "REVIEW"
ok <- function(x) if (isTRUE(x)) PASS else FAIL
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)
cat("  Audit: bucking check against the saved old optBuck output (R3)\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

ref <- "C:/apter_private/optbuck_reference"; dirp <- "C:/apter_private/hpr"
need <- c(file.path(ref, c("file1", "file3", "file4", "file5p"), "ck_getLogs.rds"), file.path(dirp, c("REDACTED33", "REDACTED27", "REDACTED28")))
checks <- c("Check 01: Reader against the old getLogs() (>= 99.9 % per field; StartPos and file5's unclassified key named)",
            "Check 02: Stem profiles equal the old getStemprofile() points",
            "Check 03: Permitted grades equal the old table except the unclassified product",
            "Check 04: B5-3(i): the new optimum is never below the new value of an eligible old optimal pattern")
if (!all(file.exists(need)) || length(list.files(dirp, "^REDACTED46_REDACTED45.*[.]hpr$")) == 0L) {
  for (ck in checks) rec(ck, REVIEW, "the private files (C:/apter_private/hpr, C:/apter_private/optbuck_reference) are not here")
} else {
  Sys.setenv(R3_SOURCE_ONLY = "1")
  res <- tryCatch({ source(file.path(root_dir, "scripts", "verify_r3.R"), local = TRUE); NULL }, error = function(e) e)
  Sys.setenv(R3_SOURCE_ONLY = "")
  if (!inherits(res, "error") || !grepl("R3 functions loaded", conditionMessage(res))) {
    for (ck in checks) rec(ck, FAIL, paste("verify_r3.R did not load:", if (inherits(res, "error")) conditionMessage(res) else "no stop"))
  } else {
    files <- names(cfg); parts <- c("a", "b", "c", "d")
    for (p in c("a", "b", "c", "d")) for (nm in files) tryCatch(get(paste0("part_", p))(nm), error = function(e) say("%s %s FAILED: %s", p, nm, conditionMessage(e)))
    ff <- c("ProductKey", "LogLength", "m3sub", "m3sob", "Butt_ob", "Butt_ub", "Mid_ob", "Mid_ub", "Top_ob", "Top_ub")
    min_eq <- min(unlist(lapply(R3$a, function(x) unlist(x[ff]))))
    sp_ok <- all(vapply(R3$a, function(x) isTRUE(x$StartPos >= 0.999) || isTRUE(x$startpos_old_is_cumulative == 1), NA))
    pk_ok <- all(vapply(R3$a, function(x) isTRUE(x$ProductKey >= 0.999) || isTRUE(x$productkey_only_unclassified), NA))
    n_un <- sum(vapply(R3$a, function(x) x$unmatched, 0))
    ff1 <- ff[ff != "ProductKey"]
    min_eq2 <- min(unlist(lapply(R3$a, function(x) unlist(x[ff1]))))
    rec(checks[1], ok(min_eq2 >= 0.999 && sp_ok && pk_ok && n_un == 0L && length(R3$a) == 4L),
        sprintf("min share equal over the nine non-key fields %.4f %% | StartPos named cause in %s | ProductKey named cause in %s | unmatched single-tree logs %d | StartPos equal: %s",
                100 * min_eq2, sp_ok, pk_ok, n_un, paste(sprintf("%s %.1f %%", names(R3$a), 100 * vapply(R3$a, function(x) x$StartPos, 0)), collapse = ", ")))
    rec(checks[2], ok(all(vapply(R3$b, function(x) x[["identical"]] == 1 && x[["points_found"]] == 1 && x[["diameters_equal"]] == 1, NA)) && length(R3$b) == 4L),
        paste(sprintf("%s identical %.4f", names(R3$b), vapply(R3$b, function(x) x[["identical"]], 0)), collapse = ", "))
    rec(checks[3], ok(all(vapply(R3$c, function(x) all(x$differing %in% x$unclassified | x$differing %in% c("999999", "2147483647")), NA)) && length(R3$c) == 4L),
        paste(sprintf("%s differs: %s", names(R3$c), vapply(R3$c, function(x) paste(x$differing, collapse = ","), "")), collapse = " | "))
    rec(checks[4], ok(all(vapply(R3$d, function(x) x[["violations"]] == 0, NA)) && length(R3$d) == 4L),
        paste(sprintf("%s: %d of %d old patterns eligible, %d violations", names(R3$d), vapply(R3$d, function(x) as.integer(x[["eligible"]]), 0L),
                      vapply(R3$d, function(x) as.integer(x[["stems"]]), 0L), vapply(R3$d, function(x) as.integer(x[["violations"]]), 0L)), collapse = "; "))
    agg <- data.frame(file = names(R3$d), old_patterns = vapply(R3$d, function(x) x[["stems"]], 0), eligible = vapply(R3$d, function(x) x[["eligible"]], 0),
                      violations = vapply(R3$d, function(x) x[["violations"]], 0),
                      profiles_identical = vapply(R3$b[names(R3$d)], function(x) x[["identical"]], 0))
    utils::write.csv(agg, file.path(out_dir, "audit_37_r3.csv"), row.names = FALSE)
  }
}
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 37 complete.\n")
