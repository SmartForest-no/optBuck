# ==============================================================================
# scripts/phase4c_limits_change_report.R
#
# Purpose : Phase 4c set the default maximum top and butt diameters (spruce
#           sawlog 60/65 cm, pine sawlog 50/60 cm, pulpwood 70/70 cm, from
#           optBuck's example.hpr). This is the one intended result change of
#           the phase. The report runs the performance cases
#           (scripts/performance_cases.R) and audit 20's three strata with
#           the new limits and compares them with the baselines taken before
#           (outputs/performance/baseline_main_3fa0a9d.rds,
#           outputs/assortments/baseline_phase4b_default.rds): logs and trees
#           that changed, and NOK/m3 in total and by assortment. Audits 20
#           (check 8) and 22 (check 5) show that with the old limits nothing
#           changes.
#
# Outputs : outputs/performance/phase4c_limits_change.csv     (per case)
#           outputs/performance/phase4c_limits_change_by_assortment.csv
#           outputs/performance/phase4c_limits_change.txt
#
# Usage   : Rscript scripts/phase4c_limits_change_report.R [workers]
# ==============================================================================

root_dir <- local({
  r <- Sys.getenv("OPTBUCK_ROOT", "")
  if (nzchar(r)) return(normalizePath(r, winslash = "/"))
  d <- normalizePath(getwd(), winslash = "/")
  repeat {
    if (file.exists(file.path(d, "R", "helpers_paths.R"))) return(d)
    p <- dirname(d); if (identical(p, d)) stop("Set OPTBUCK_ROOT to the project root.")
    d <- p
  }
})
Sys.setenv(OPTBUCK_ROOT = root_dir)
suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)
source(file.path(root_dir, "scripts", "performance_tree_lists.R"))
source(file.path(root_dir, "scripts", "performance_cases.R"))

args    <- commandArgs(trailingOnly = TRUE)
workers <- if (length(args) > 0L) as.integer(args[1L]) else min(15L, max(1L, parallel::detectCores() - 1L))
out_dir <- file.path(root_dir, "outputs", "performance")

base_perf <- readRDS(file.path(out_dir, "baseline_main_3fa0a9d.rds"))
base_4b   <- readRDS(file.path(root_dir, "outputs", "assortments", "baseline_phase4b_default.rds"))

log_key  <- function(l) paste(l$plot_id_github, l$treid, l$StartPos, l$StopPos, l$ProductKey)
tree_key <- function(d) paste(d$plot_id_github, d$treid)
total_m3 <- function(r) r$per_m3_summary[r$per_m3_summary$scope == "Total", ]

compare_run <- function(name, new, old) {
  kn <- log_key(new$selected_logs_all); ko <- log_key(old$selected_logs_all)
  changed_logs <- c(setdiff(kn, ko), setdiff(ko, kn))
  tn <- split(kn, tree_key(new$selected_logs_all))
  to <- split(ko, tree_key(old$selected_logs_all))
  trees <- union(names(tn), names(to))
  tree_changed <- vapply(trees, function(t)
    !setequal(or_default(tn[[t]], character(0)), or_default(to[[t]], character(0))), logical(1))
  tn_ <- total_m3(new); to_ <- total_m3(old)
  data.frame(case = name, trees = nrow(new$tree_summary), trees_changed = sum(tree_changed),
             logs_before = length(ko), logs_after = length(kn),
             logs_removed = length(setdiff(ko, kn)), logs_added = length(setdiff(kn, ko)),
             value_nok_before = round(sum(old$selected_logs_all$value_nok), 2),
             value_nok_after  = round(sum(new$selected_logs_all$value_nok), 2),
             commercial_m3_before = round(to_$commercial_volume_m3, 4),
             commercial_m3_after  = round(tn_$commercial_volume_m3, 4),
             nok_per_m3_before = round(to_$nok_per_m3, 2),
             nok_per_m3_after  = round(tn_$nok_per_m3, 2),
             delta_nok_per_m3  = round(tn_$nok_per_m3 - to_$nok_per_m3, 2),
             stringsAsFactors = FALSE)
}

by_assortment <- function(name, new, old) {
  a <- old$per_m3_by_assortment; b <- new$per_m3_by_assortment
  k <- function(d) paste(d$scope, d$assortment)
  keys <- union(k(a), k(b))
  pick <- function(d, col) { v <- d[[col]][match(keys, k(d))]; v[is.na(v)] <- 0; v }
  sp <- strsplit(keys, " ", fixed = TRUE)
  data.frame(case = name, scope = vapply(sp, `[`, "", 1L),
             assortment = vapply(sp, function(x) paste(x[-1L], collapse = " "), ""),
             nok_per_m3_before = round(pick(a, "nok_per_m3_contribution"), 2),
             nok_per_m3_after  = round(pick(b, "nok_per_m3_contribution"), 2),
             delta_nok_per_m3  = round(pick(b, "nok_per_m3_contribution") -
                                         pick(a, "nok_per_m3_contribution"), 2),
             share_pct_before = round(100 * pick(a, "volume_share"), 2),
             share_pct_after  = round(100 * pick(b, "volume_share"), 2),
             stringsAsFactors = FALSE)
}

rows <- list(); rows_a <- list()
cases <- perf_cases(root_dir)
for (nm in names(cases)) {
  cat("case", nm, "\n")
  r <- perf_run_case(cases[[nm]], root_dir, workers = workers)
  rows[[nm]] <- compare_run(nm, r, base_perf[[nm]])
  rows_a[[nm]] <- by_assortment(nm, r, base_perf[[nm]])
}
for (n in setdiff(names(base_4b), "meta")) {
  nm <- paste0("audit20_", n); cat("case", nm, "\n")
  r <- run_sampled_plot_bucking(base_4b$meta$strata[[n]], "default_2021", root_dir, workers = workers)
  rows[[nm]] <- compare_run(nm, r, base_4b[[n]])
  rows_a[[nm]] <- by_assortment(nm, r, base_4b[[n]])
}
res   <- do.call(rbind, rows);   rownames(res) <- NULL
res_a <- do.call(rbind, rows_a); rownames(res_a) <- NULL
res_a <- res_a[res_a$delta_nok_per_m3 != 0 | res_a$share_pct_before != res_a$share_pct_after, ]

readr::write_csv(res, file.path(out_dir, "phase4c_limits_change.csv"))
readr::write_csv(res_a, file.path(out_dir, "phase4c_limits_change_by_assortment.csv"))
txt <- file.path(out_dir, "phase4c_limits_change.txt")
sink(txt)
cat("Phase 4c default diameter limits: results against the baselines\n")
cat(sprintf("Run: %s, %d workers\n\n", format(Sys.time()), workers))
print(res[, c("case", "trees", "trees_changed", "logs_before", "logs_after", "logs_removed",
              "logs_added", "nok_per_m3_before", "nok_per_m3_after", "delta_nok_per_m3")],
      row.names = FALSE)
cat("\nBy assortment (rows that changed):\n")
print(res_a, row.names = FALSE)
sink()
cat(readLines(txt), sep = "\n")
