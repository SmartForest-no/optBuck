# ==============================================================================
# audits/audit_22_performance_equivalence.R
#
# Purpose : The Performance phase (2026-10-13) must not change any result.
#           Checks that results do not depend on the number of workers or on
#           chunking, that a comparison (one candidate pass for both runs)
#           equals two separate runs, that everything equals main before the
#           phase, and that the app's background jobs give the same results.
#
# Checks  :
#  1.  1 worker vs 15 workers (default chunks): identical() result tables,
#      unsorted, for an NFI stratum and the 1000-tree list
#  2.  Chunk sizes 1, 7 and the default (100-tree list with custom
#      assortments, as a comparison): identical() to 1 worker
#  3.  Comparison = two separate runs: 1000-tree list (default vs changed
#      matrix) and custom assortments with UseIn default / scenario / both;
#      identical() per run
#  4.  Runs whose shared products are defined differently get their own
#      candidate pass (.prepare_runs() makes two groups) and still equal
#      separate runs
#  5.  Every case of scripts/performance_cases.R equals the main baseline
#      (outputs/performance/baseline_main_3fa0a9d.rds, taken on 3fa0a9d
#      before the phase; rows sorted, since main bucked trees in arrow's
#      read order; identical, or within 1e-12 for sums over trees). All
#      cases run with the pre-Phase-4c base products (pre_phase4c_limits():
#      the baseline predates the Phase 4c diameter limits)
#  6.  workers = 1 runs in this process: no future plan is set
#  7.  The app's background job (app/helpers_jobs.R, run_job_async()) gives
#      the same results as the backend runner, as a comparison
#  8.  The backend's usable candidates (.candidate_columns(usable_only =
#      TRUE) + .candidate_details()) equal the eligible rows of
#      generate_candidate_logs_one_tree() with a price volume, for default
#      and custom products (50 trees, with and without under-bark profiles)
#
# Verdict : PASS / FAIL (helpers_audit.R)
#
# Outputs :
#   outputs/performance/audit_22_performance_equivalence.csv
#   outputs/performance/audit_22_performance_equivalence.txt
#
# Usage   : Rscript audits/audit_22_performance_equivalence.R
#           Uses 15 workers (fewer if the machine has fewer cores).
# ==============================================================================

suppressPackageStartupMessages(library(readr))

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

suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)
source(file.path(root_dir, "audits", "helpers_audit.R"))

source(file.path(root_dir, "scripts", "performance_tree_lists.R"))
source(file.path(root_dir, "scripts", "performance_cases.R"))

out_dir <- path_output("performance", root_dir = root_dir)
ensure_dir(out_dir)
audit_txt <- file.path(out_dir, "audit_22_performance_equivalence.txt")
audit_csv <- file.path(out_dir, "audit_22_performance_equivalence.csv")
init_audit_log(audit_txt)

E <- new.env(parent = emptyenv())
rec <- function(check, result, detail = "")
  record_check(E, check, result, detail, log_path = audit_txt)
PASS <- "PASS"; REVIEW <- "REVIEW"; FAIL <- "FAIL"
section <- function(title) {
  cat(sprintf("\n--- %s ---\n", title), file = audit_txt, append = TRUE)
  cat(sprintf("\n--- %s ---\n", title))
}
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)
cat("  Audit: performance equivalence\n", file = audit_txt, append = TRUE)
cat(sprintf("  Run: %s\n", Sys.time()), file = audit_txt, append = TRUE)
cat(paste(rep("=", 61), collapse = ""), "\n", file = audit_txt, append = TRUE)

N_WORKERS <- min(15L, max(2L, parallel::detectCores() - 1L))
TABLES <- names(PERF_TABLE_KEYS)
# Phase 4c changed the default diameter limits; the baseline predates them,
# so every case runs with the pre-Phase-4c base products (scripts/
# phase4c_limits_change_report.R reports what the new limits change).
cases  <- perf_cases(root_dir, limits = "pre_phase4c")
# Roadmap 8: the baseline with the NFI tree numbers translated to the permuted ids (values untouched).
base_f <- file.path(root_dir, "outputs", "performance", "baseline_main_3fa0a9d_newids.rds")
if (!file.exists(base_f)) base_f <- file.path(root_dir, "outputs", "performance", "baseline_main_3fa0a9d.rds")
base   <- readRDS(base_f)

# Result tables identical, unsorted; names of the tables that differ.
differs <- function(a, b) TABLES[!vapply(TABLES, function(k) identical(a[[k]], b[[k]]), logical(1))]
same_txt <- function(d) if (length(d) == 0L) "all tables identical" else paste("differ:", paste(d, collapse = ", "))
with_chunk <- function(size, expr) {
  old <- options(apter.chunk_size = size); on.exit(options(old)); expr
}


# ==============================================================================
section("01. 1 worker vs N workers")
r1_nfi <- perf_run_case(cases$nfi_both, root_dir, workers = 1L)
rN_nfi <- perf_run_case(cases$nfi_both, root_dir, workers = N_WORKERS)
r1_tl  <- perf_run_case(cases$tl1000, root_dir, workers = 1L)
rN_tl  <- perf_run_case(cases$tl1000, root_dir, workers = N_WORKERS)
d1 <- c(differs(r1_nfi, rN_nfi), differs(r1_tl, rN_tl))
rec(sprintf("Check 01: 1 vs %d workers: NFI stratum and 1000-tree list identical (unsorted)", N_WORKERS),
    if (length(d1) == 0L && rN_tl$diagnostics$workers == N_WORKERS &&
        r1_tl$diagnostics$workers == 1L) PASS else FAIL,
    sprintf("%s | workers recorded: %d and %d | trees: %d and %d", same_txt(d1),
            r1_tl$diagnostics$workers, rN_tl$diagnostics$workers,
            nrow(rN_nfi$tree_summary), nrow(rN_tl$tree_summary)))


# ==============================================================================
section("02. Chunk sizes")
cd <- cases$tl100_custom_default; cs <- cases$tl100_custom_scenario
cmp1 <- perf_run_comparison(cd, cs, root_dir, workers = 1L)
d2 <- character(0)
for (size in list(1L, 7L, NULL)) {
  rr <- with_chunk(size, perf_run_comparison(cd, cs, root_dir, workers = N_WORKERS))
  lab <- if (is.null(size)) "default" else as.character(size)
  d2 <- c(d2, paste0(lab, ":", c(differs(cmp1, rr), paste0("scenario/", differs(cmp1$scenario, rr$scenario)))))
}
d2 <- d2[!grepl(":$|:scenario/$", d2)]
rec("Check 02: Chunk sizes 1, 7 and default give identical results (comparison with custom assortments)",
    if (length(d2) == 0L) PASS else FAIL, same_txt(d2))


# ==============================================================================
section("03. Comparison = two separate runs")
cmp_tl <- perf_run_comparison(cases$tl1000, cases$tl1000_scn, root_dir, workers = N_WORKERS)
sep_scn <- perf_run_case(cases$tl1000_scn, root_dir, workers = N_WORKERS)
sep_cd  <- perf_run_case(cd, root_dir, workers = N_WORKERS)
sep_cs  <- perf_run_case(cs, root_dir, workers = N_WORKERS)
d3 <- c(paste0("tl1000 A/", differs(cmp_tl, rN_tl)),
        paste0("tl1000 B/", differs(cmp_tl$scenario, sep_scn)),
        paste0("custom A/", differs(cmp1, sep_cd)),
        paste0("custom B/", differs(cmp1$scenario, sep_cs)))
d3 <- d3[!grepl("/$", d3)]
use_in <- sort(unique(base$inputs$defs$UseIn))
keys_a <- unique(cmp1$selected_logs_all$ProductKey)
keys_b <- unique(cmp1$scenario$selected_logs_all$ProductKey)
rec("Check 03: A comparison equals two separate runs (1000-tree list; custom assortments with UseIn default/scenario/both)",
    if (length(d3) == 0L && identical(use_in, c("both", "default", "scenario")) &&
        "spruce_poles" %in% keys_a && !"spruce_poles" %in% keys_b &&
        "spruce_large_sawlog" %in% keys_b && !"spruce_large_sawlog" %in% keys_a) PASS else FAIL,
    sprintf("%s | UseIn: %s | A logs: %s | B logs: %s", same_txt(d3),
            paste(use_in, collapse = "/"), paste(sort(keys_a), collapse = ","),
            paste(sort(keys_b), collapse = ",")))


# ==============================================================================
section("04. Differently defined shared products: separate candidate passes")
pd_b <- load_products(root_dir); lc_b <- load_length_classes(root_dir)
pd_b$MinTopDiameter[pd_b$ProductKey == "spruce_sawlog"] <- 160
lk <- build_price_lookup(load_price_matrix_long(root_dir))
lc_b$LogLength_dm <- lc_b$LogLength / 10
pd_a <- load_products(root_dir); lc_a <- lc_b
groups4 <- length(.prepare_runs(list(list(pd = pd_a, lc = lc_a, lookup = lk),
                                     list(pd = pd_b, lc = lc_b, lookup = lk)))$groups)
groups4s <- length(.prepare_runs(list(list(pd = pd_a, lc = lc_a, lookup = lk),
                                      list(pd = pd_a, lc = lc_a, lookup = lk)))$groups)
case4 <- list(source = "list", n_trees = 100L, pd = pd_b, lc = lc_b)
cmp4 <- perf_run_comparison(cases$tl100, case4, root_dir, workers = N_WORKERS)
sep4 <- perf_run_case(case4, root_dir, workers = N_WORKERS)
r100 <- perf_run_case(cases$tl100, root_dir, workers = N_WORKERS)
d4 <- c(paste0("A/", differs(cmp4, r100)), paste0("B/", differs(cmp4$scenario, sep4)))
d4 <- d4[!grepl("/$", d4)]
rec("Check 04: Shared products defined differently -> two candidate passes; results equal separate runs",
    if (groups4 == 2L && groups4s == 1L && length(d4) == 0L &&
        !identical(cmp4$tree_summary$total_value_nok, cmp4$scenario$tree_summary$total_value_nok))
      PASS else FAIL,
    sprintf("groups: %d (same definitions: %d) | %s", groups4, groups4s, same_txt(d4)))


# ==============================================================================
section("05. Main baseline")
det5 <- character(0); ok5 <- TRUE
for (nm in names(cases)) {
  r <- if (nm == "tl1000") rN_tl else if (nm == "nfi_both") rN_nfi
       else if (nm == "tl1000_scn") sep_scn else if (nm == "tl100_custom_default") sep_cd
       else if (nm == "tl100_custom_scenario") sep_cs else if (nm == "tl100") r100
       else perf_run_case(cases[[nm]], root_dir, workers = N_WORKERS)
  cmp <- perf_compare(r, base[[nm]])
  if (any(cmp == "DIFFERS")) ok5 <- FALSE
  det5 <- c(det5, sprintf("%s: %s", nm, if (all(cmp == "identical")) "identical"
                          else paste(sprintf("%s %s", names(cmp)[cmp != "identical"],
                                             cmp[cmp != "identical"]), collapse = ", ")))
}
rec(sprintf("Check 05: All %d cases equal the main baseline (commit %s, rows sorted)",
            length(cases), substr(base$meta$commit, 1, 7)),
    if (ok5) PASS else FAIL, paste(det5, collapse = "; "))


# ==============================================================================
section("06. workers = 1 runs in this process")
future::plan(future::sequential)
r6 <- perf_run_case(cases$tl100, root_dir, workers = 1L)
plan6 <- if (inherits(future::plan(), "sequential")) "sequential" else class(future::plan())[1L]
rec("Check 06: workers = 1 sets no future plan and records 1 worker",
    if (inherits(future::plan(), "sequential") && r6$diagnostics$workers == 1L &&
        length(differs(r6, r100)) == 0L) PASS else FAIL,
    sprintf("plan after the run: %s | workers recorded: %d | %s", plan6,
            r6$diagnostics$workers, same_txt(differs(r6, r100))))


# ==============================================================================
section("07. App background job")
ROOT <- root_dir
source(file.path(root_dir, "inst", "app", "helpers_jobs.R"))
old_w <- options(apter.workers = N_WORKERS)
await <- function(p) {
  done <- FALSE; val <- NULL; err <- NULL
  promises::then(p, function(v) { val <<- v; done <<- TRUE },
                 function(e) { err <<- e; done <<- TRUE })
  while (!done) later::run_now(0.05)
  if (!is.null(err)) stop(err)
  val
}
pm_file <- function(pm) { f <- tempfile(fileext = ".csv"); pm_export(pm, f); f }
n_prog <- 0L
r7 <- tryCatch(await(run_job_async(
  list(src = "list", tl = perf_tree_list(100L), list_params = list(),
       path = pm_file(cd$pm), pd = cd$pd, lc = cd$lc,
       scenario = list(price_matrix_path = pm_file(cs$pm), products = cs$pd,
                       length_classes = cs$lc)),
  function(done, total, what) n_prog <<- n_prog + 1L)), error = function(e) e)
r7n <- tryCatch(await(run_job_async(
  list(src = "nfi", params = cases$nfi_both$params, path = "default_2021"))),
  error = function(e) e)
options(old_w)
ok7 <- !inherits(r7, "error") && !inherits(r7n, "error")
d7 <- if (ok7) c(paste0("list A/", differs(r7, cmp1)), paste0("list B/", differs(r7$scenario, cmp1$scenario)),
                 paste0("nfi/", differs(r7n, rN_nfi))) else character(0)
d7 <- d7[!grepl("/$", d7)]
rec("Check 07: The app's background job (run_job_async) equals the backend runner (tree-list comparison, NFI stratum)",
    if (ok7 && length(d7) == 0L && n_prog > 0L) PASS else FAIL,
    if (!ok7) paste("error:", conditionMessage(if (inherits(r7, "error")) r7 else r7n))
    else sprintf("%s | progress updates: %d", same_txt(d7), n_prog))


# ==============================================================================
section("08. Usable candidates = eligible rows of generate_candidate_logs_one_tree()")
tl8 <- perf_tree_list(50L)
td8 <- tree_list_tree_data(tl8)
pvs8 <- profiles_to_profile_vectors(generate_taper_profiles_for_trees(td8, quiet = TRUE))
cols8 <- c("ProductKey", "ProductType", "StartPos", "StopPos", "LogLength", "price_volume_m3",
           "LengthClass_dm", "PriceDiameterClass_cm", "TopDiameter_ob", "TopDiameter_ub",
           "DiameterUnderBark", "ProductPriority")
n_bad8 <- 0L; n8 <- 0L
for (set in list(list(pd_a, lc_a), list(cs$pd, cs$lc))) {
  plan8 <- .candidate_plan(set[[1]], set[[2]])
  for (pv0 in pvs8) for (pv in list(pv0, add_under_bark_profile(pv0))) {
    a <- generate_candidate_logs_one_tree(pv, set[[1]], set[[2]])
    e <- a[a$eligible & !is.na(a$price_volume_m3), cols8]; rownames(e) <- NULL
    u <- .candidate_columns(pv, plan8[[pv$species_group]], usable_only = TRUE)
    g <- if (is.null(u$cols)) e[0, ] else
      as.data.frame(.candidate_details(u, seq_along(u$cols$StartPos))[cols8],
                    stringsAsFactors = FALSE)
    n8 <- n8 + 1L
    if (!identical(e, g) || sum(u$n_all) != nrow(a)) n_bad8 <- n_bad8 + 1L
  }
}
rec("Check 08: Backend candidates equal the eligible rows of generate_candidate_logs_one_tree() (default and custom products)",
    if (n_bad8 == 0L) PASS else FAIL,
    sprintf("%d of %d tree/product-set cases differ", n_bad8, n8))


# ==============================================================================
write_audit_outputs(E$results, audit_csv, audit_txt)
cat("Audit 22 complete.\n")
cat("Results written to:", out_dir, "\n")
