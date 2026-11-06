# ==============================================================================
# scripts/performance_profile.R
#
# Purpose : Where does a run's time go? (Performance phase, step 1.)
#           For synthetic tree lists of 100 and 1000 trees it times each
#           per-tree stage with high-resolution timers, by calling the
#           backend's own functions in the order .buck_profiles() calls them:
#             cand    generate_candidate_logs_one_tree()
#             filter  eligibility filter (eligible & price volume)
#             keys    prepare_price_join_keys()
#             lookup  lookup_prices() + value_nok
#             unpr    unpriced table
#             dp_in   DP input subset
#             dp      .optimize_dp_fast()
#             comvol  commercial_volume_ub()
#             row     the tree's result row and selected logs
#           plus the run-level stages (taperNOR, under bark, summaries) from
#           the real backend run, whose tree values it cross-checks. One
#           Rprof pass on 100 trees gives the hotspots inside the functions.
#           It also measures the size of the price-independent geometry (the
#           eligible candidates) that a cache would hold.
#
# Output  : outputs/performance/profile_trees_{100,1000}.csv (one row per tree)
#           outputs/performance/profile_summary.txt
#
# Usage   : Rscript scripts/performance_profile.R
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
Sys.setenv(OPTBUCK_ROOT = root_dir)
suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)
source(file.path(root_dir, "scripts", "performance_tree_lists.R"))

out_dir <- file.path(root_dir, "outputs", "performance")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
summary_txt <- file.path(out_dir, "profile_summary.txt")
cat("", file = summary_txt)
say <- function(...) {
  s <- sprintf(...)
  cat(s, "\n", sep = ""); cat(s, "\n", sep = "", file = summary_txt, append = TRUE)
}
now <- function() as.numeric(bench::hires_time())

pd <- load_products(root_dir); lc <- load_length_classes(root_dir)
lc$LogLength_dm <- lc$LogLength / 10
lookup <- build_price_lookup(load_price_matrix_long(root_dir))
STAGES <- c("cand", "filter", "keys", "lookup", "unpr", "dp_in", "dp", "comvol", "row")

# The per-tree loop of .buck_profiles(), timed stage by stage.
profile_trees <- function(pvs, tree_data) {
  rows <- vector("list", length(pvs))
  for (i in seq_along(pvs)) {
    tid <- names(pvs)[i]; pv <- pvs[[tid]]
    tmeta <- tree_data[tree_data$treid == tid, , drop = FALSE]
    t <- setNames(numeric(length(STAGES)), STAGES)
    t0 <- now(); cands <- generate_candidate_logs_one_tree(pv, pd, lc, 10L, 10L)
    t1 <- now(); t["cand"] <- t1 - t0
    elig <- cands[cands$eligible & !is.na(cands$price_volume_m3), , drop = FALSE]
    t2 <- now(); t["filter"] <- t2 - t1
    elig <- prepare_price_join_keys(elig, lookup)
    t3 <- now(); t["keys"] <- t3 - t2
    elig$PriceNOKperM3 <- lookup_prices(elig, lookup)
    elig$value_nok <- elig$price_volume_m3 * elig$PriceNOKperM3
    t4 <- now(); t["lookup"] <- t4 - t3
    up <- elig[is.na(elig$PriceNOKperM3), , drop = FALSE]
    if (nrow(up) > 0L) as.data.frame(table(ProductKey = up$ProductKey,
                                           LengthClass_dm = as.character(up$LengthClass_dm)))
    t5 <- now(); t["unpr"] <- t5 - t4
    dp_in <- elig[!is.na(elig$PriceNOKperM3) & !is.na(elig$value_nok), , drop = FALSE]
    t6 <- now(); t["dp_in"] <- t6 - t5
    res <- .optimize_dp_fast(dp_in)
    t7 <- now(); t["dp"] <- t7 - t6
    com <- if (res$n_selected > 0L)
      sum(commercial_volume_ub(pv$diameterPosition, pv$DiameterValueUB, res$selected_logs)) else 0
    t8 <- now(); t["comvol"] <- t8 - t7
    data.frame(plot_id = as.character(tmeta$plot_id), treid = tid,
               n_logs_selected = res$n_selected, total_value_nok = res$dp_best_value,
               commercial_volume_m3 = com, stringsAsFactors = FALSE)
    if (res$n_selected > 0L) { sel <- res$selected_logs; sel$plot_id <- tmeta$plot_id }
    t["row"] <- now() - t8

    # Geometry a cache would hold: all eligible columns, and a compact set.
    compact <- elig[, c("ProductKey", "StartPos", "StopPos", "LengthClass_dm",
                        "PriceDiameterClass_cm", "price_volume_m3",
                        "TopDiameter_ob", "TopDiameter_ub")]
    rows[[i]] <- data.frame(
      treid = tid, species_group = pv$species_group, dbh_cm = tmeta$dbh_cm,
      height_m = tmeta$height_m, n_cand = nrow(cands), n_elig = nrow(elig),
      n_dp = nrow(dp_in), n_sel = res$n_selected, value = res$dp_best_value,
      bytes_elig = as.numeric(utils::object.size(cands[cands$eligible &
                                                     !is.na(cands$price_volume_m3), ])),
      bytes_compact = as.numeric(utils::object.size(compact)),
      as.list(t), stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}

# Profile vectors with under-bark profiles, as run_tree_list_bucking() makes them.
prepare_pvs <- function(p) {
  trees <- p$trees[p$trees$in_range, , drop = FALSE]
  td <- data.frame(plot_id = trees$plot_id, treid = trees$treid, sesong = NA,
                   species_group = trees$species_group, dbh_cm = trees$dbh_cm,
                   height_m = trees$height_m, height_source = trees$height_source,
                   stringsAsFactors = FALSE)
  t0 <- now(); prof <- generate_taper_profiles_for_trees(td, quiet = TRUE); t_taper <- now() - t0
  t0 <- now(); pvs <- profiles_to_profile_vectors(prof); t_conv <- now() - t0
  t0 <- now(); pvs <- lapply(pvs, add_under_bark_profile, model = get_bark_model())
  t_ub <- now() - t0
  list(pvs = pvs, td = td, t = c(taper = t_taper, convert = t_conv, under_bark = t_ub))
}

for (n in c(100L, 1000L)) {
  say("\n==== %d trees ====", n)
  p <- perf_tree_list(n)
  pre <- prepare_pvs(p)
  tt <- profile_trees(pre$pvs, pre$td)
  utils::write.csv(tt, file.path(out_dir, sprintf("profile_trees_%d.csv", n)), row.names = FALSE)

  # The real backend run: run-level stages, and a cross-check of tree values.
  t0 <- now(); r <- run_tree_list_bucking(p, root = root_dir); t_run <- now() - t0
  st <- r$diagnostics$stage_timings
  m <- match(r$tree_summary$treid, tt$treid)
  same <- isTRUE(all(r$tree_summary$total_value_nok == tt$value[m])) &&
    isTRUE(all(r$tree_summary$n_logs_selected == tt$n_sel[m]))
  say("Backend run %.1f s; profiled tree values identical to the backend run: %s", t_run, same)

  per_stage <- colSums(tt[, STAGES])
  loop_tot  <- sum(per_stage)
  say("Run-level: taperNOR %.2f s, profile conversion %.2f s, under bark %.2f s, summaries %.2f s",
      pre$t["taper"], pre$t["convert"], pre$t["under_bark"],
      st$elapsed_seconds[st$stage == "summary_assembly"])
  say("Per-tree loop (profiled) %.1f s:", loop_tot)
  for (s in STAGES)
    say("  %-7s %7.2f s  %5.1f %%  %6.2f ms/tree", s, per_stage[[s]],
        100 * per_stage[[s]] / loop_tot, 1000 * per_stage[[s]] / n)
  say("Candidates per tree: all %.0f, eligible %.0f (median %.0f, max %.0f); selected logs %.1f",
      mean(tt$n_cand), mean(tt$n_elig), stats::median(tt$n_elig), max(tt$n_elig), mean(tt$n_sel))
  tot_tree <- rowSums(tt[, STAGES])
  say("Time per tree: median %.1f ms, max %.1f ms; cor(time, eligible) %.2f, cor(time, height) %.2f",
      1000 * stats::median(tot_tree), 1000 * max(tot_tree),
      stats::cor(tot_tree, tt$n_elig), stats::cor(tot_tree, tt$height_m))
  say("Geometry size per 1000 trees: eligible candidates %.0f MB, compact columns %.0f MB",
      sum(tt$bytes_elig) / n * 1000 / 2^20, sum(tt$bytes_compact) / n * 1000 / 2^20)
}

# Hotspots inside the functions: Rprof on the 100-tree loop.
say("\n==== Rprof, 100 trees (self and total time by function) ====")
pre <- prepare_pvs(perf_tree_list(100L))
prof_file <- tempfile(fileext = ".out")
Rprof(prof_file, interval = 0.005)
invisible(profile_trees(pre$pvs, pre$td))
Rprof(NULL)
sp <- summaryRprof(prof_file)
say("Top self time:")
bs <- head(sp$by.self, 20)
for (i in seq_len(nrow(bs)))
  say("  %-36s %6.2f s  %5.1f %%", rownames(bs)[i], bs$self.time[i], bs$self.pct[i])
say("Top total time:")
bt <- head(sp$by.total, 30)
for (i in seq_len(nrow(bt)))
  say("  %-36s %6.2f s  %5.1f %%", rownames(bt)[i], bt$total.time[i], bt$total.pct[i])
