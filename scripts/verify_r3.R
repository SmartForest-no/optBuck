# ==============================================================================
# scripts/verify_r3.R  (roadmap 8, task 2, 2026-11-04)
#
# Purpose : Verification of the new reader and valuation against the SAVED output of the old optBuck
#           (C:/apter_private/optbuck_reference/<file>/, nothing is re-run), on the owner's private
#           files. R3 counts as passed when a-d pass:
#             a  read_hpr_logs() vs the old getLogs() per log and field (target >= 99.9 % equal)
#             b  new stem profile points vs the old getStemprofile() per stem
#             c  hpr_permitted_grades() vs the old permitted_grades.csv
#             d  B5-3(i): the new optimum is never below the new value of an eligible old optimal pattern
#             e  ablation on file4: old bark rule and old over-bark price lookup move the recorded total
#                toward the old total (report, no threshold)
# Output  : C:/apter_private/diag/r3_verification.txt (printed too) and r3_*.csv with the listed differences
#           (private; stem keys). Only aggregates go into PLAN.md.
# Usage   : Rscript scripts/verify_r3.R [a b c d e] [file1 file3 file4 file5]
# ==============================================================================
root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  d <- normalizePath(Sys.getenv("APTER_ROOT", unset = getwd()), winslash = "/", mustWork = FALSE)
  while (!is_root(d) && dirname(d) != d) d <- dirname(d)
  d
})
Sys.setenv(APTER_ROOT = root_dir)
source(file.path(root_dir, "R", "helpers_paths.R"))
source(file.path(root_dir, "scripts", "app_backend", "01_run_sampled_plot_bucking.R"), local = FALSE)
dirp <- "C:/apter_private/hpr"; ref <- "C:/apter_private/optbuck_reference"; dg <- "C:/apter_private/diag"
dir.create(dg, showWarnings = FALSE, recursive = TRUE)
cfg <- list(file1 = list(path = list.files(dirp, "^REDACTED46_REDACTED45.*[.]hpr$", full.names = TRUE)[1L], ref = "file1"),
            file3 = list(path = file.path(dirp, "REDACTED33"), ref = "file3"),
            file4 = list(path = file.path(dirp, "REDACTED27"), ref = "file4"),
            file5 = list(path = file.path(dirp, "REDACTED28"), ref = "file5p"))
args <- commandArgs(trailingOnly = TRUE)
parts <- intersect(args, c("a", "b", "c", "d", "e")); if (!length(parts)) parts <- c("a", "b", "c", "d", "e")
files <- intersect(args, names(cfg)); if (!length(files)) files <- names(cfg)
R3 <- list(a = list(), b = list(), c = list(), d = list(), e = list())   # results for audit 37
out <- character(0); say <- function(...) { l <- sprintf(...); out <<- c(out, l); cat(l, "\n", sep = "") }
pc <- function(a, b) if (b > 0) sprintf("%.2f %%", 100 * a / b) else "NA"
refp <- function(nm, f) file.path(ref, cfg[[nm]]$ref, f)
skey <- function(x) sprintf("%.0f", as.numeric(as.character(x)))   # as.character(26000000) is "2.6e+07"
same <- function(x, y, tol) (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & abs(x - y) <= tol)

# ---- a. reader --------------------------------------------------------------------------------------
part_a <- function(nm) {
  h <- read_hpr(cfg[[nm]]$path, "x"); new <- read_hpr_logs(cfg[[nm]]$path, "x", defs = h)$logs
  old <- as.data.frame(readRDS(refp(nm, "ck_getLogs.rds"))); old$StemKey <- skey(old$StemKey)
  new$k <- paste(new$stem_key, new$LogKey); old$k <- paste(old$StemKey, old$LogKey)
  m <- match(old$k, new$k)
  say("a. %s: old logs %d, new logs %d; only in old %d, only in new %d (new reads multi-tree stems: %d of the new-only logs)", nm, nrow(old), nrow(new),
      sum(is.na(m)), sum(!new$k %in% old$k), sum(!new$k %in% old$k & new$kind == "multi"))
  fl <- list(ProductKey = c("ProductKey", 0), StartPos = c("StartPos", 0), LogLength = c("LogLength", 0), m3sub = c("m3sub", 1e-9),
             m3sob = c("m3sob", 1e-9), Butt_ob = c("ButtOb", 0), Butt_ub = c("ButtUb", 0), Mid_ob = c("MidOb", 0), Mid_ub = c("MidUb", 0),
             Top_ob = c("TopOb", 0), Top_ub = c("TopUb", 0))
  ok <- !is.na(m); rows <- list(); R3$a[[nm]] <<- list()
  for (f in names(fl)) {
    x <- as.numeric(old[[f]][ok]); y <- as.numeric(new[[fl[[f]][1L]]][m[ok]]); eq <- same(x, y, as.numeric(fl[[f]][2L]))
    cause <- ifelse(eq, "equal", ifelse(is.na(x) & !is.na(y), "old missing, new has a value",
                    ifelse(!is.na(x) & is.na(y), "new missing, old has a value",
                    ifelse(abs(x - y) <= 1e-4 + 0.5, "differs by at most 0.5 (rounding)", "differs by more"))))
    R3$a[[nm]][[f]] <<- mean(eq)
    say("   %-10s equal %s (%d of %d); %s", f, pc(sum(eq), length(eq)), sum(eq), length(eq),
        paste(sprintf("%s: %d", names(table(cause[!eq])), as.integer(table(cause[!eq]))), collapse = "; "))
    if (any(!eq)) rows[[f]] <- data.frame(field = f, stem = old$StemKey[ok][!eq], log = old$LogKey[ok][!eq], old = x[!eq], new = y[!eq], cause = cause[!eq])
  }
  if (length(rows)) utils::write.csv(do.call(rbind, rows), file.path(dg, sprintf("r3_reader_%s.csv", nm)), row.names = FALSE)
  # causes of the two differences that remain
  R3$a[[nm]]$startpos_old_is_cumulative <<- if ("StartPos" %in% names(rows)) { d <- rows$StartPos
    cumv <- stats::ave(new$LogLength, new$stem_key, FUN = function(v) c(0, cumsum(v)[-length(v)]))[match(paste(d$stem, d$log), new$k)]; mean(d$old == cumv) } else 1
  R3$a[[nm]]$productkey_only_unclassified <<- if ("ProductKey" %in% names(rows)) all(rows$ProductKey$old == 999999 & rows$ProductKey$new %in% h$unclassified_keys) else TRUE
  R3$a[[nm]]$unmatched <<- sum(is.na(m)) + sum(!new$k %in% old$k & new$kind != "multi")
  if ("StartPos" %in% names(rows)) { d <- rows$StartPos
    cum <- stats::ave(new$LogLength, new$stem_key, FUN = function(v) c(0, cumsum(v)[-length(v)]))[match(paste(d$stem, d$log), new$k)]
    say("   StartPos: the old value equals the cumulative sum of the previous lengths in %s of the %d differences; the new value is the file's recorded StartPos (Extension) in %s; differences 1 to 9 cm in %s",
        pc(sum(d$old == cum), nrow(d)), nrow(d),   pc(sum(new$start_recorded[match(paste(d$stem, d$log), new$k)] %in% TRUE), nrow(d)), pc(sum(abs(d$old - d$new) >= 1 & abs(d$old - d$new) <= 9), nrow(d))) }
  if ("ProductKey" %in% names(rows)) { d <- rows$ProductKey
    say("   ProductKey: old %s -> new %s in %d logs (the patched copy file5p renamed the unclassified key 2147483647 to 999999)", paste(unique(d$old), collapse = ","), paste(unique(d$new), collapse = ","), nrow(d)) }
  if (any(is.na(m)) || any(!new$k %in% old$k)) say("   unmatched logs (stem/log): old only %s; new only %s", paste(utils::head(old$k[is.na(m)], 5), collapse = "; "), paste(utils::head(new$k[!new$k %in% old$k], 5), collapse = "; "))
  # the unit of the reader test: logs with every field equal
  all_eq <- Reduce(`&`, lapply(names(fl), function(f) same(as.numeric(old[[f]][ok]), as.numeric(new[[fl[[f]][1L]]][m[ok]]), as.numeric(fl[[f]][2L]))))
  say("   all fields equal in %s of the matched logs", pc(sum(all_eq), length(all_eq)))
  invisible(NULL)
}

# ---- b. profiles ------------------------------------------------------------------------------------
part_b <- function(nm) {
  h <- read_hpr(cfg[[nm]]$path, "x")
  smap <- setNames(guess_hpr_species(h$species$name, h$species$user_id), h$species$key)
  st <- read_hpr_stems(cfg[[nm]]$path, "x", defs = h); pr <- hpr_stem_profiles(st, smap)$profiles
  old <- as.data.frame(readRDS(refp(nm, "ck_getStemprofile.rds")))[, c("StemKey", "diameterPosition", "DiameterValue")]
  old$StemKey <- skey(old$StemKey)
  new <- data.frame(StemKey = pr$treid, diameterPosition = pr$diameterPosition, DiameterValue = pr$DiameterValue, stringsAsFactors = FALSE)
  so <- unique(as.character(old$StemKey)); sn <- unique(as.character(new$StemKey))
  say("b. %s: stems old %d, new %d; only old %d (species Not used, dropped by the checks or cleaning), only new %d", nm, length(so), length(sn),
      length(setdiff(so, sn)), length(setdiff(sn, so)))
  both <- intersect(so, sn)
  o <- old[as.character(old$StemKey) %in% both, ]; n <- new[as.character(new$StemKey) %in% both, ]
  ko <- paste(o$StemKey, o$diameterPosition); kn <- paste(n$StemKey, n$diameterPosition)
  mo <- match(ko, kn)
  d_ok <- !is.na(mo); dd <- abs(o$DiameterValue[d_ok] - n$DiameterValue[mo[d_ok]])
  po <- tapply(rep(1L, nrow(o)), as.character(o$StemKey), sum); pn <- tapply(rep(1L, nrow(n)), as.character(n$StemKey), sum)
  bad_pos <- tapply(is.na(mo), as.character(o$StemKey), sum)[both]; extra_new <- (pn[both] - (po[both] - bad_pos))
  bad_d <- tapply(c(dd > 0), as.character(o$StemKey[d_ok]), sum)[both]; bad_d[is.na(bad_d)] <- 0
  ident <- bad_pos == 0 & extra_new == 0 & bad_d == 0
  say("   points: old %d, new %d, old positions found in new %d (%s), diameters equal at those %s (max difference %.1f mm)", nrow(o), nrow(n), sum(d_ok), pc(sum(d_ok), nrow(o)),
      pc(sum(dd == 0), length(dd)), if (length(dd)) max(dd) else 0)
  say("   stems with identical profiles: %s (%d of %d); with old points missing in new: %d; with extra new points: %d; with a different diameter: %d",
      pc(sum(ident), length(ident)), sum(ident), length(ident), sum(bad_pos > 0), sum(extra_new > 0), sum(bad_d > 0))
  # cause of non-identical stems: where the old extra points are (above the new top?)
  if (any(bad_pos > 0)) {
    miss <- o[is.na(mo), ]; top_new <- tapply(n$diameterPosition, as.character(n$StemKey), max)
    above <- miss$diameterPosition > top_new[as.character(miss$StemKey)]
    say("   old points missing in new: %d, of which above the new profile top: %d, inside the profile: %d", nrow(miss), sum(above), sum(!above))
  }
  R3$b[[nm]] <<- c(identical = mean(ident), points_found = sum(d_ok) / nrow(o), diameters_equal = if (length(dd)) mean(dd == 0) else 1)
  bad <- both[!ident]
  if (length(bad)) utils::write.csv(data.frame(StemKey = bad, old_missing = bad_pos[!ident], extra_new = extra_new[!ident], diam_diff = bad_d[!ident]), file.path(dg, sprintf("r3_profile_%s.csv", nm)), row.names = FALSE)
}

# ---- c. permitted grades ------------------------------------------------------------------------------
part_c <- function(nm) {
  h <- read_hpr(cfg[[nm]]$path, "x"); pg <- hpr_permitted_grades(h)
  old <- utils::read.csv(refp(nm, "permitted_grades.csv"), stringsAsFactors = FALSE)
  keys <- sort(unique(c(pg$ProductKey, as.character(old$ProductKey))))
  diffs <- character(0)
  for (k in keys) {
    g_new <- sort(unique(pg$grade[pg$ProductKey == k])); g_old <- sort(unique(old$PermittedGrade[as.character(old$ProductKey) == k]))
    if (length(g_new) == 1L && is.na(g_new)) g_new <- numeric(0)         # NA = all grades in the new table
    if (!identical(as.numeric(g_new), as.numeric(g_old))) diffs <- c(diffs, sprintf("%s: new {%s} old {%s}", k, paste(g_new, collapse = ","), paste(g_old, collapse = ",")))
  }
  unc <- h$unclassified_keys
  R3$c[[nm]] <<- list(differing = sub(":.*", "", diffs), unclassified = unc)
  say("c. %s: %d products in the new table, %d in the old; differing products: %d (%s)", nm, length(unique(pg$ProductKey)), length(unique(old$ProductKey)), length(diffs),
      if (length(diffs)) paste(diffs, collapse = " | ") else "none")
  say("   unclassified key(s) in the file: %s", paste(unc, collapse = ", "))
}

# ---- d. B5-3(i) ---------------------------------------------------------------------------------------
get_check <- function(nm, tag = "rz") {
  f <- file.path(dg, sprintf("%s_%s.rds", nm, tag))
  if (file.exists(f)) readRDS(f) else { x <- bucking_check(cfg[[nm]]$path); saveRDS(x, f); x }
}
part_d <- function(nm) {
  x <- get_check(nm); h <- read_hpr(cfg[[nm]]$path, "x")
  samp <- refp(nm, "logs_optimal_sample.csv"); if (!file.exists(samp)) samp <- refp(nm, "logs_optimal.csv")
  oldp <- utils::read.csv(samp, stringsAsFactors = FALSE)
  keys_stem <- skey(unique(oldp$StemKey))
  oldp$stem_key <- skey(oldp$StemKey); oldp$ProductKey <- as.character(oldp$ProductKey)
  oldp$waste <- oldp$ProductKey %in% h$unclassified_keys | oldp$ProductKey == "999999"
  in_job <- keys_stem[keys_stem %in% names(x$job$pvs)]
  lg <- oldp[oldp$stem_key %in% in_job, c("stem_key", "ProductKey", "StartPos", "StopPos", "LogLength", "waste")]
  # Valued with the optimiser's own price lookup (the cells it may choose): a log in a manual-only cell or in a
  # length class the import drops has no price there, so its pattern is not eligible.
  vr <- value_recorded_logs(x$job, lg, x$prodmap, hpr_products = h$products)
  vr$stem_key <- lg$stem_key
  # A pattern the optimiser could have chosen: every log on the 10 cm grid, of a length the product allows in
  # Apter's grid (the import rounds file length classes up to whole dm: a 375 cm log in class 365 is not a candidate),
  # inside the product limits, priced in the optimiser's own lookup.
  plan_len <- function(sp, ak) { pl <- x$job$prep$runs[[1L]]$own_plan[[sp]]; if (is.null(pl) || !is.null(pl$error)) return(numeric(0))
    pk <- vapply(pl$products, `[[`, "", "pk"); i <- match(ak, pk); if (is.na(i)) numeric(0) else pl$products[[i]]$lengths }
  sp_of <- vapply(lg$stem_key, function(k) x$job$pvs[[k]]$species_group, "")
  off_grid <- !lg$waste & !is.na(vr$akey) & mapply(function(sp, ak, len, st) !(len %in% plan_len(sp, ak)) || st %% 10 != 0, sp_of, vr$akey, lg$LogLength, lg$StartPos)
  bad_log <- (!lg$waste & (vr$unknown_product | vr$outside | is.na(vr$PriceNOKperM3))) | off_grid
  inel <- tapply(bad_log, lg$stem_key, any)
  by_cause <- function(flag) sum(tapply(flag, lg$stem_key, any))
  say("   stems with at least one log: off the optimiser's length lattice %d, outside the product limits %d, without a price %d, unknown product %d",
      by_cause(off_grid), by_cause(!lg$waste & vr$outside), by_cause(!lg$waste & is.na(vr$PriceNOKperM3)), by_cause(vr$unknown_product))
  val <- tapply(vr$value_nok, lg$stem_key, sum)
  st <- x$stems[match(names(val), x$stems$StemKey), ]
  ok <- st$use & !inel[names(val)]
  viol <- ok & val[names(val)] > st$value_opt + 1e-6
  say("d. %s: %d stems with an old optimal pattern (%d in the new job); eligible under the new rules: %d; ineligible (log outside limits / unpriced / unknown product): %d; not used: %d",
      nm, length(keys_stem), length(in_job), sum(ok), sum(st$use & inel[names(val)]), sum(!st$use))
  say("   VIOLATIONS (new optimum below the new value of the old pattern): %d of %d eligible stems; sum new optimum %.0f vs old patterns %.0f", sum(viol), sum(ok),
      sum(st$value_opt[ok]), sum(val[ok]))
  R3$d[[nm]] <<- c(stems = length(keys_stem), eligible = sum(ok), violations = sum(viol))
  if (any(viol)) utils::write.csv(data.frame(StemKey = names(val)[viol], old_pattern_value = val[viol], new_optimum = st$value_opt[viol]), file.path(dg, sprintf("r3_violations_%s.csv", nm)), row.names = FALSE)
}

# ---- e. ablation on file4 -------------------------------------------------------------------------------
part_e <- function() {
  nm <- "file4"; h <- read_hpr(cfg[[nm]]$path, "x")
  old <- utils::read.csv(refp(nm, "per_stem.csv"), stringsAsFactors = FALSE)
  tot <- function(x, ob = FALSE) {
    job <- x$job
    if (ob) for (sp in names(job$prep$runs[[1L]]$own_plan)) { pl <- job$prep$runs[[1L]]$own_plan[[sp]]
      if (is.null(pl$error)) { for (i in seq_along(pl$products)) pl$products[[i]]$ub <- FALSE; job$prep$runs[[1L]]$own_plan[[sp]] <- pl } }
    lg <- x$logs$recorded[, c("stem_key", "ProductKey", "StartPos", "StopPos", "LogLength", "waste")]
    vr <- value_recorded_logs(job, lg, x$prodmap, hpr_products = h$products, rec_prices = hpr_rec_prices(h, x$defs))
    s <- tapply(vr$value_nok, lg$stem_key, sum); keep <- intersect(names(s), as.character(old$StemKey[old$actual_value > 0 | TRUE]))
    list(new = sum(s[keep]), old = sum(old$actual_value[match(keep, as.character(old$StemKey))], na.rm = TRUE), n = length(keep))
  }
  x0 <- get_check(nm)
  BARK_MODELS$spruce_all <<- function(d_cm, dbh_cm, species_group) bark_model_barkNOR(d_cm, dbh_cm, rep("spruce", length(species_group)))
  options(apter.bark_model = "spruce_all"); x1 <- bucking_check(cfg[[nm]]$path); options(apter.bark_model = NULL)
  r <- list(`new rules (barkNOR per species, under-bark price)` = tot(x0), `old bark rule (spruce for all species)` = tot(x1),
            `old price lookup (over-bark top)` = tot(x0, TRUE), `both old rules` = tot(x1, TRUE))
  say("e. file4 ablation, recorded value of %d stems; old optBuck actual total %.0f", r[[1]]$n, r[[1]]$old)
  for (k in names(r)) say("   %-55s %10.0f  (%+.2f %% vs old)", k, r[[k]]$new, 100 * (r[[k]]$new / r[[k]]$old - 1))
}

if (identical(Sys.getenv("R3_SOURCE_ONLY"), "1")) stop("R3 functions loaded", call. = FALSE)   # audit 37 sources this file up to here
for (p in parts) for (nm in if (p == "e") "file4" else files) tryCatch(
  if (p == "e") part_e() else get(paste0("part_", p))(nm), error = function(e) say("%s %s FAILED: %s", p, nm, conditionMessage(e)))
writeLines(out, file.path(dg, "r3_verification.txt"))
