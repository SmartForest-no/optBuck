# ==============================================================================
# scripts/volume_ratio_check.R
#
# Purpose : 0.3.1, item 3. optBuck's price volume of the recorded logs against the
#           harvester's own `m3 (price)`, per product and per volume method, on the
#           owner's private files (C:/optbuck_private/hpr, never in the repository).
#           Run it once on the old code and once on the new code:
#             Rscript scripts/volume_ratio_check.R <repo root> <tag>
#           Output (private): C:/optbuck_private/volume_ratios/<tag>_file<N>.csv with
#           one row per product (names in the private file only) and
#           <tag>_summary.csv by file and method (no names).
# ==============================================================================
args <- commandArgs(TRUE)
root <- normalizePath(args[1], winslash = "/"); tag <- args[2]
suppressMessages(pkgload::load_all(root, quiet = TRUE))
out <- "C:/optbuck_private/volume_ratios"; dir.create(out, showWarnings = FALSE, recursive = TRUE)
fs <- list.files("C:/optbuck_private/hpr", full.names = TRUE)
summ <- list()
# Optional experiment (tag ends in "_nornd"): no rounding of the volume diameter (the files say "Measured diameter in mm").
if (grepl("_nornd$", tag)) assignInNamespace("volume_rounding", function(method) rep("None", length(method)), "optBuck")
only <- if (length(args) >= 3L) as.integer(strsplit(args[3], ",")[[1L]]) else seq_along(fs)
for (i in only) {
  r <- tryCatch(bucking_check(fs[i], root = root), error = function(e) list(ok = FALSE, errors = conditionMessage(e)))
  if (!isTRUE(r$ok) || isTRUE(r$no_profile)) { cat(sprintf("file%d: not checked (%s)\n", i, paste(r$errors, collapse = " "))); next }
  rec <- r$logs$recorded
  rec <- rec[!rec$waste & !is.na(rec$akey) & !is.na(rec$price_volume_m3) & !is.na(rec$m3_price) & rec$m3_price > 0, , drop = FALSE]
  keys <- assortment_table_keys(r$defs)
  meth <- setNames(r$defs$VolumeDiameterCategory, keys)
  d <- do.call(rbind, lapply(split(rec, rec$akey), function(x) data.frame(
    product = x$pname[1L], key = x$akey[1L], method = unname(meth[x$akey[1L]]), logs = nrow(x),
    optbuck_m3 = sum(x$price_volume_m3), harvester_m3 = sum(x$m3_price),
    ratio = sum(x$price_volume_m3) / sum(x$m3_price),
    mean_abs_pct = 100 * mean(abs(x$price_volume_m3 / x$m3_price - 1)), stringsAsFactors = FALSE)))
  utils::write.csv(d, file.path(out, sprintf("%s_file%d.csv", tag, i)), row.names = FALSE)
  s <- do.call(rbind, lapply(split(d, d$method), function(x) data.frame(
    file = paste0("file", i), method = x$method[1L], products = nrow(x), logs = sum(x$logs),
    ratio = sum(x$optbuck_m3) / sum(x$harvester_m3), stringsAsFactors = FALSE)))
  summ[[length(summ) + 1L]] <- s
  cat(sprintf("file%d done\n", i))
}
utils::write.csv(do.call(rbind, summ), file.path(out, sprintf("%s_summary.csv", tag)), row.names = FALSE)
print(do.call(rbind, summ), row.names = FALSE)
