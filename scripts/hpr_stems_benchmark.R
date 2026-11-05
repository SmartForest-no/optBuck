# ==============================================================================
# scripts/hpr_stems_benchmark.R
#
# Purpose : Time and memory of the ".hpr stems" source (hpr Phase B) on real
#           .hpr files: reading the stems (read_hpr_stems()), and a full A/B
#           run (run_hpr_bucking(): A = default 2021 prices, B = sawlog x 1.3,
#           the species groups as guessed). Each file runs in its own R
#           process, so the memory peaks are that file's own.
#
# Privacy : LOCAL ONLY. Real .hpr files hold exact stem coordinates and
#           names: keep them outside the repository, and never commit this
#           script's output (it is printed, and written only where asked).
#           Only counts, times and memory are reported.
#
# Usage   : Rscript scripts/hpr_stems_benchmark.R <folder with .hpr files> [workers] [out.txt]
# ==============================================================================

args <- commandArgs(TRUE)
if (length(args) < 1L) stop("Usage: Rscript scripts/hpr_stems_benchmark.R <folder> [workers] [out.txt]")
root <- local({
  d <- normalizePath(Sys.getenv("APTER_ROOT", getwd()), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "R", "helpers_paths.R")) && dirname(d) != d) d <- dirname(d)
  d
})
Sys.setenv(APTER_ROOT = root)

if (identical(args[1L], "--one")) {
  # One file, in this process: one line of results.
  f <- args[2L]; workers <- as.integer(args[3L])
  suppressMessages(pkgload::load_all(root, quiet = TRUE))   # the package (R/, inst/)
  mem <- function() ps::ps_memory_info()
  mb <- function(x) x / 1048576
  w0 <- mem()[["wset"]]
  t0 <- Sys.time()
  st <- read_hpr_stems(f)
  t_read <- as.numeric(difftime(Sys.time(), t0, units = "secs")); m_read <- mem()[["peak_wset"]]
  if (!isTRUE(st$ok)) {
    cat(sprintf("%s | %.1f MB | %s stems | read %.1f s, +%.0f MB | not bucked: %s\n", basename(f),
                mb(file.size(f)), prettyNum(nrow(or_default(st$stems, data.frame())), big.mark = ","),
                t_read, mb(m_read - w0), paste(st$errors, collapse = " ")))
    quit(save = "no")
  }
  map <- setNames(guess_hpr_species(st$species$name, st$species$user_id), st$species$key)
  pm0 <- pm_minimal(load_price_matrix_long(root))
  pm1 <- pm0; saw <- grepl("sawlog", pm1$ProductKey); pm1$PriceNOKperM3[saw] <- pm1$PriceNOKperM3[saw] * 1.3
  fb <- tempfile(fileext = ".csv"); pm_export(pm1, fb)
  t1 <- Sys.time()
  p <- hpr_stem_profiles(st, map)
  r <- run_hpr_bucking(p, scenario = list(price_matrix_path = fb), workers = workers)
  cmp <- compare_cutting_patterns(r, r$scenario, pm0, pm1)
  t_run <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
  kids <- ps::ps_children(ps::ps_handle())
  wk <- sum(vapply(kids, function(k) tryCatch(ps::ps_memory_info(k)[["peak_wset"]], error = function(e) 0), 0))
  dropped <- if (nrow(p$excluded)) {
    tab <- sort(table(p$excluded$excluded_reason), decreasing = TRUE)
    paste(sprintf("%d %s", as.integer(tab), names(tab)), collapse = "; ")
  } else "none"
  cat(sprintf(paste("%s | %.1f MB | %s stems, %s used | read %.1f s, +%.0f MB | A/B run %.1f s",
                    "(bucking %.1f s), main peak +%.0f MB, %d workers %.0f MB | changed %s | NOK/m3 %.1f -> %.1f | dropped: %s\n"),
              basename(f), mb(file.size(f)), prettyNum(p$n_read, big.mark = ","),
              prettyNum(p$n_used, big.mark = ","), t_read, mb(m_read - w0), t_run,
              sum(r$diagnostics$stage_timings$elapsed_seconds[r$diagnostics$stage_timings$stage %in%
                    c("candidate_generation", "price_joining", "dp_optimization")]) / max(1L, workers),
              mb(mem()[["peak_wset"]] - w0), length(kids), mb(wk), prettyNum(sum(cmp$changed), big.mark = ","),
              r$per_m3_summary$nok_per_m3[1], r$scenario$per_m3_summary$nok_per_m3[1], dropped))
  quit(save = "no")
}

folder <- args[1L]
workers <- if (length(args) >= 2L) as.integer(args[2L]) else max(1L, parallel::detectCores() - 1L)
files <- list.files(folder, pattern = "\\.(hpr|gz|zip)$", full.names = TRUE, ignore.case = TRUE)
files <- files[order(file.size(files))]
rscript <- file.path(R.home("bin"), "Rscript")
self <- normalizePath(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)[1L]))
lines <- unlist(lapply(files, function(f)
  system2(rscript, c(shQuote(self), "--one", shQuote(f), workers), stdout = TRUE, stderr = FALSE)))
lines <- lines[grepl(" \\| ", lines)]
cat(lines, sep = "\n")
if (length(args) >= 3L) writeLines(lines, args[3L])
