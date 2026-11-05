# Why is the recorded value above the best possible for some stems? (roadmap 8, part A4; private files;
# reads the saved checks of scripts/diag_part_a.R, prints aggregates only.)
root_dir <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/Apter"; Sys.setenv(APTER_ROOT = root_dir)
source(file.path(root_dir, "R", "helpers_paths.R")); source(file.path(root_dir, "scripts", "app_backend", "01_run_sampled_plot_bucking.R"))
for (nm in c("file1", "file4", "file5")) {
  x <- readRDS(sprintf("C:/apter_private/diag/%s_a_after.rds", nm)); s <- x$stems; u <- s$use & s$value_opt > 0
  r <- x$logs$recorded; r <- r[r$stem_key %in% s$StemKey[u], ]
  run <- x$job$prep$runs[[1L]]
  sp <- vapply(r$stem_key, function(k) x$job$pvs[[k]]$species_group, "")
  off <- mapply(function(sp1, ak, len, st) { if (is.na(ak)) return(FALSE); pl <- run$own_plan[[sp1]]; pk <- vapply(pl$products, `[[`, "", "pk"); i <- match(ak, pk)
    if (is.na(i)) return(FALSE); !(len %in% pl$products[[i]]$lengths) || st %% 10 != 0 }, sp, r$akey, r$LogLength, r$StartPos)
  off <- as.logical(unlist(off)) & !as.logical(r$waste)
  gt <- u & s$rec_gt_opt %in% TRUE
  st_off <- tapply(off, r$stem_key, any)[s$StemKey]; st_off[is.na(st_off)] <- FALSE
  st_out <- s$outside %in% TRUE
  ex <- (s$value_rec - s$value_opt) / s$value_opt
  cat(sprintf("%s: stems %d; recorded > best in %d (%.1f %%), median excess %.2f %%, 90th percentile %.2f %%, excess total %.0f kr (%.2f %% of best)\n", nm, sum(u), sum(gt), 100 * mean(gt[u]),
      100 * median(ex[gt]), 100 * quantile(ex[gt], 0.9), sum((s$value_rec - s$value_opt)[gt]), 100 * sum((s$value_rec - s$value_opt)[gt]) / sum(s$value_opt[u])))
  cat(sprintf("   of those stems: with a recorded log off Apter's length lattice %.1f %% (of all stems %.1f %%); with a log outside the limits %.1f %%; with an unpriced log %.1f %%; with a manual cut %.1f %%\n",
      100 * mean(st_off[gt]), 100 * mean(st_off[u]), 100 * mean(st_out[gt]), 100 * mean(tapply(r$value_nok == 0 & !r$waste, r$stem_key, any)[s$StemKey[gt]], na.rm = TRUE), 100 * mean(s$has_manual[gt])))
  cat(sprintf("   share of the excess in stems with an off-lattice log: %.1f %%\n", 100 * sum((s$value_rec - s$value_opt)[gt & st_off]) / sum((s$value_rec - s$value_opt)[gt])))
}
