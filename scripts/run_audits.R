# ==============================================================================
# scripts/run_audits.R
#
# Purpose : Run a set of audits (and scripts/performance_check.R) as separate
#           Rscript processes, a few at a time, and print one result line per
#           item plus the total time. Exit status 1 if anything FAILs.
#           (2026-10-28, speedup: the sequential full run took 9 min 39 s.)
#
# Usage   : Rscript scripts/run_audits.R [options] [what ...]
#
#   what        audit numbers (16 20), ranges (14-20), names (audit_16_...),
#               `perf` (scripts/performance_check.R) or `all`.
#               Default: all = every audits/audit_NN_*.R from 14 up and perf.
#   --jobs N    processes at a time (default 3, audit 17 included; audit 40, the install gate, always runs alone and last)
#   --workers N future workers per process (default max(2, (cores - 1) %/%
#               jobs), passed as OPTBUCK_WORKERS so the processes do not
#               oversubscribe the cores)
#   --17-last   run audit 17 alone after all others, instead of first and
#               alongside them (the fallback if it is flaky under load)
#   --log-dir D where each item's output goes (default outputs/audit_runs/,
#               gitignored)
#
# Order   : audit 17 (the browser audit, about 4 min) starts first and takes
#           one of the slots; the others follow, longest first, in the
#           remaining slots (jobs - 1 while 17 runs).
#
# Result  : PASS    exit 0 and "Verdict: PASS" (perf: "OK: every case matches")
#           REVIEW  "Verdict: ACCEPTED WITH REVIEW" (not a failure)
#           FAIL    anything else; the [FAIL] lines of its log are printed
#
# Needs   : processx (a dependency of shinytest2).
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
if (!requireNamespace("processx", quietly = TRUE))
  stop("run_audits.R needs the 'processx' package (install.packages(\"processx\")).", call. = FALSE)

# ---- arguments -------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  i <- match(name, args)
  if (is.na(i)) return(default)
  if (i == length(args)) stop("Missing value for ", name, call. = FALSE)
  v <- args[i + 1L]; args <<- args[-c(i, i + 1L)]; v
}
jobs    <- as.integer(opt("--jobs", 3L))
cores   <- suppressWarnings(parallel::detectCores()); if (is.na(cores)) cores <- 2L
workers <- as.integer(opt("--workers", max(2L, (cores - 1L) %/% max(1L, jobs))))
log_dir <- opt("--log-dir", file.path(root_dir, "outputs", "audit_runs"))
last17  <- "--17-last" %in% args; args <- setdiff(args, "--17-last")
if (is.na(jobs) || jobs < 1L) stop("--jobs must be a positive integer", call. = FALSE)

# ---- the items -------------------------------------------------------------------
files <- list.files(file.path(root_dir, "audits"), "^audit_[0-9]{2}_.*[.]R$")
files <- files[as.integer(sub("^audit_([0-9]+)_.*", "\\1", files)) >= 14L]   # 14 up: later audits are included
items <- data.frame(name = sub("[.]R$", "", files), num = as.integer(sub("^audit_([0-9]+)_.*", "\\1", files)),
                    script = file.path(root_dir, "audits", files), stringsAsFactors = FALSE)
items <- rbind(items, data.frame(name = "performance_check", num = NA_integer_,
                                 script = file.path(root_dir, "scripts", "performance_check.R"),
                                 stringsAsFactors = FALSE))
# Typical times (s, 2026-10-28), for the longest-first order.
weight <- c(`16` = 90, `22` = 68, `19` = 41, performance_check = 24, `25` = 18, `26` = 15)

want <- if (length(args) == 0L) "all" else args
pick <- unique(unlist(lapply(want, function(w) {
  if (w == "all") return(items$name)
  if (w == "perf") return("performance_check")
  if (grepl("^[0-9]+-[0-9]+$", w)) {
    r <- as.integer(strsplit(w, "-", fixed = TRUE)[[1L]])
    return(items$name[!is.na(items$num) & items$num >= r[1L] & items$num <= r[2L]])
  }
  if (grepl("^[0-9]+$", w)) return(items$name[!is.na(items$num) & items$num == as.integer(w)])
  hit <- items$name[startsWith(items$name, w)]
  if (length(hit) == 0L) stop("Unknown audit: ", w, call. = FALSE)
  hit
})))
if (length(pick) == 0L) stop("No audit selected.", call. = FALSE)
items <- items[match(pick, items$name), , drop = FALSE]
is17 <- !is.na(items$num) & items$num == 17L
w <- ifelse(is.na(items$num), weight[["performance_check"]], weight[as.character(items$num)])
w[is.na(w)] <- 10
rest <- items[!is17, , drop = FALSE][order(-w[!is17]), , drop = FALSE]
queue <- if (last17) rbind(rest, items[is17, , drop = FALSE]) else rbind(items[is17, , drop = FALSE], rest)
# Audit 40 (the install gate) times tab 5, so it runs alone, after everything else (2026-11-06).
is40 <- !is.na(queue$num) & queue$num == 40L
queue <- rbind(queue[!is40, , drop = FALSE], queue[is40, , drop = FALSE])

dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

# ---- run -------------------------------------------------------------------------
classify <- function(it, status, log) {
  txt <- if (file.exists(log)) readLines(log, warn = FALSE, encoding = "UTF-8") else character(0)
  if (it$name == "performance_check")
    return(if (status == 0L && any(grepl("OK: every case matches the main baseline", txt, fixed = TRUE))) "PASS" else "FAIL")
  if (any(grepl("Verdict: ACCEPTED WITH REVIEW", txt, fixed = TRUE)) && status == 0L) return("REVIEW")
  if (status == 0L && any(grepl("Verdict: PASS", txt, fixed = TRUE))) "PASS" else "FAIL"
}
start_one <- function(it) {
  log <- file.path(log_dir, paste0(it$name, ".log"))
  p <- processx::process$new(
    rscript, it$script, wd = root_dir, stdout = log, stderr = "2>&1",
    env = c("current", OPTBUCK_ROOT = root_dir, OPTBUCK_WORKERS = as.character(workers), NOT_CRAN = "true"))
  list(it = it, p = p, log = log, t0 = Sys.time())
}

# ---- pre-flight: orphaned headless Chromes (report only) -------------------------
# A headless Chrome (--headless and --remote-debugging-port on its command line)
# whose parent process no longer exists was left by an R process that was killed
# or lost its session. Nothing is killed here: the browser audits (17, 29) start
# their own Chrome with a private profile and close only that one. Windows only.
orphan_chromes <- function() {
  if (.Platform$OS.type != "windows") return(character(0))
  ps1 <- tempfile(fileext = ".ps1")
  writeLines(c(
    "Get-CimInstance Win32_Process -Filter \"Name='chrome.exe'\" | Where-Object {",
    "  $_.CommandLine -match '--headless' -and $_.CommandLine -match '--remote-debugging-port' } |",
    "  ForEach-Object { if (-not (Get-Process -Id $_.ParentProcessId -ErrorAction SilentlyContinue)) { $_.ProcessId } }"), ps1)
  on.exit(unlink(ps1))
  out <- tryCatch(system2("powershell", c("-NoProfile", "-ExecutionPolicy", "Bypass", "-File", shQuote(ps1)),
                          stdout = TRUE, stderr = FALSE), error = function(e) character(0))
  trimws(out[grepl("^\\s*[0-9]+\\s*$", out)])
}
orphans <- orphan_chromes()
if (length(orphans))
  cat(sprintf(paste0("Note: %d orphaned headless Chrome process(es) from earlier runs: PID %s.\n",
                     "      Not touched. To remove them yourself: taskkill /F /T %s\n"),
              length(orphans), paste(orphans, collapse = ", "),
              paste0("/PID ", orphans, collapse = " ")))

cat(sprintf("Running %d item(s), %d at a time, %d worker(s) each; logs in %s\n",
            nrow(queue), jobs, workers, log_dir))
t_all <- Sys.time()
running <- list(); results <- character(0); k <- 0L
# On an interrupt, an error or the normal end, kill the process trees of this
# script's own children that are still running (and nothing else).
tryCatch(repeat {
  # Finished processes: print their line.
  # One is_alive() snapshot per round: a process that exits after this point is
  # handled next round. (Asking again for the filter below could drop a process
  # that died in between without ever printing or recording it.)
  finished <- vapply(running, function(r) !r$p$is_alive(), NA)
  for (r in running[finished]) {
    el <- as.numeric(difftime(Sys.time(), r$t0, units = "secs"))
    res <- tryCatch(classify(r$it, r$p$get_exit_status(), r$log), error = function(e) "FAIL")
    results[r$it$name] <- res
    cat(sprintf("%-6s %-46s %7.1f s\n", res, r$it$name, el))
    if (res == "FAIL") {
      txt <- tryCatch(readLines(r$log, warn = FALSE, encoding = "UTF-8"),
                      error = function(e) "(the log could not be read)")
      bad <- grep("\\[FAIL|^Error|Execution halted", txt, value = TRUE)
      cat(paste0("       ", utils::tail(if (length(bad)) bad else txt, 6L), collapse = "\n"), "\n", sep = "")
    }
  }
  running <- running[!finished]
  # Start more. While audit 17 runs it takes one slot (the others get jobs - 1).
  alone_next <- function() k < nrow(queue) && !is.na(queue$num[k + 1L]) && (queue$num[k + 1L] == 40L || (last17 && queue$num[k + 1L] == 17L))
  alone17 <- alone_next()
  while (k < nrow(queue) && length(running) < jobs && !(alone17 && length(running) > 0L)) {
    k <- k + 1L
    running[[length(running) + 1L]] <- start_one(queue[k, , drop = FALSE])
    alone17 <- alone_next()
  }
  if (k >= nrow(queue) && length(running) == 0L) break
  Sys.sleep(0.5)
}, finally = {
  for (r in running) if (r$p$is_alive()) try(r$p$kill_tree(), silent = TRUE)
})
total <- as.numeric(difftime(Sys.time(), t_all, units = "secs"))
# Every started item must be in the summary: one without a result is a FAIL.
for (nm in setdiff(queue$name[seq_len(k)], names(results))) {
  results[nm] <- "FAIL"
  cat(sprintf("%-6s %-46s %7s\n       no result recorded\n", "FAIL", nm, "-"))
}
n <- table(factor(results, c("PASS", "REVIEW", "FAIL")))
cat(sprintf("\n%d PASS, %d REVIEW, %d FAIL; total %.0f s (%.1f min)\n",
            n[["PASS"]], n[["REVIEW"]], n[["FAIL"]], total, total / 60))
quit(save = "no", status = if (n[["FAIL"]] > 0L) 1L else 0L)
