# ==============================================================================
# scripts/app_backend/02_benchmark_dp_optimizer_alternatives.R
#
# Purpose : Benchmark faster pure-R DP optimizer alternatives against the
#           reference optimize_one_tree_dp() from R/helpers_dp_optimizer.R.
#
# Current bottleneck (from stage timing, n_plots=50):
#   dp_optimization : ~23 s  (44% of total)
#
# Two alternative implementations tested here:
#
#   v2 -- Pre-split candidates by StartPos + pre-extracted vectors
#         Eliminates per-position which(StartPos == cur_pos) calls.
#         Key change: build list of candidate indices keyed by StartPos once,
#         then look up in O(1) per position instead of O(n_dp).
#         Inner R for-loop over candidates retained.
#
#   v3 -- Pre-split + fully vectorised inner loop
#         Same as v2 but replaces the inner for-loop with vectorised
#         R operations (+, which.max) on pre-extracted numeric vectors.
#         Eliminates per-candidate R interpreter overhead in the hot loop.
#
# Usage:
#   Rscript scripts/app_backend/02_benchmark_dp_optimizer_alternatives.R
# ==============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
})

ROOT <- "S:/Prosjekter/52106_SFI_SmartForest/optBuck/nfiBuck"

# Source helpers and backend.
for (.h in c("helpers_paths", "helpers_products", "helpers_valuation",
             "helpers_taper", "helpers_candidate_generation",
             "helpers_dp_optimizer")) {
  source(file.path(ROOT, "R", paste0(.h, ".R")), local = FALSE)
}
source(file.path(ROOT, "scripts", "app_backend",
                 "01_run_sampled_plot_bucking.R"), local = FALSE)

cat("Helpers and backend sourced.\n\n")

# ==============================================================================
# Step 1: Reproduce priced candidate tables for benchmark trees
# ==============================================================================
# Run the backend at n_plots=10 and extract what we need for the DP benchmarks.
# We re-run the pipeline inline here so we can capture priced_candidates_list.

PARAMS <- list(species_group = "spruce", hogstkl = 41L,
               abonitet = 17L, n_plots = 10L, seed = 42L)
PM_PATH <- "default_2021"

# --- load data and sample plots (mirrors backend steps 2-5) ---
gh_dir      <- file.path(ROOT, "data_processed_github")
trees_all   <- as.data.frame(arrow::read_parquet(
  file.path(gh_dir, "nfi_tree_plot_stratum_ready_github.parquet")))
eligible_trees <- trees_all[trees_all$species_group == PARAMS$species_group &
                              trees_all$hogstkl %in% PARAMS$hogstkl &
                              trees_all$abonitet %in% PARAMS$abonitet, ]
set.seed(PARAMS$seed)
eligible_plots <- sort(unique(eligible_trees$plot_id_github))
sampled_plots  <- sort(sample(eligible_plots, PARAMS$n_plots, replace = FALSE))
trees_samp     <- eligible_trees[eligible_trees$plot_id_github %in% sampled_plots, ]

trees_samp$height_m <- as.numeric(trees_samp$beregnet_hoyde) / 10
trees_valid <- trees_samp[
  !is.na(trees_samp$dbh_cm)   & trees_samp$dbh_cm   >= 5.0 &
  !is.na(trees_samp$height_m) & trees_samp$height_m >= 4.5, ]

cat(sprintf("Trees in sample: %d\n", nrow(trees_valid)))

# --- load products and price matrix ---
pd <- load_products(ROOT)
lc <- load_length_classes(ROOT)
pm <- load_price_matrix_long(ROOT)

pm_mat_join <- pm[!is.na(pm$LengthClass_dm),
                  intersect(c("ProductKey","LengthClass_dm","PriceDiameterClass_cm",
                              "PriceNOKperM3"), names(pm)), drop = FALSE]
pm_fl_raw   <- pm[is.na(pm$LengthClass_dm),
                  intersect(c("ProductKey","PriceNOKperM3"), names(pm)), drop = FALSE]
pm_fl_join  <- pm_fl_raw[!duplicated(pm_fl_raw$ProductKey), , drop = FALSE]
pm_saw_lookup  <- setNames(as.numeric(pm_mat_join$PriceNOKperM3),
                            paste(pm_mat_join$ProductKey, pm_mat_join$LengthClass_dm,
                                  pm_mat_join$PriceDiameterClass_cm, sep = "|"))
pm_flat_lookup <- setNames(as.numeric(pm_fl_join$PriceNOKperM3),
                            as.character(pm_fl_join$ProductKey))

# --- load taper profiles ---
taper_dir <- file.path(ROOT, "outputs", "app_cache",
                        "taper_profiles_by_plot_block_github")
sampled_treids <- as.character(trees_valid$treid)
numeric_ids    <- as.integer(substr(sampled_plots, 2L, nchar(sampled_plots)))
needed_blocks  <- unique(sprintf("B%03d", floor((numeric_ids - 1L) / 500L)))

profiles_raw <- as.data.frame(
  arrow::open_dataset(taper_dir) |>
    dplyr::filter(plot_block %in% needed_blocks,
                  plot_id_github %in% sampled_plots) |>
    dplyr::select(-plot_block) |>
    dplyr::collect())
profiles_raw <- profiles_raw[profiles_raw$treid %in% sampled_treids, ]
names(profiles_raw)[names(profiles_raw) == "plot_id_github"] <- "plot_id"
pvs <- profiles_to_profile_vectors(profiles_raw)

# --- generate priced candidates for each tree ---
cat(sprintf("Building priced candidate tables for %d trees ...\n", length(pvs)))
priced_list <- vector("list", length(pvs))
tree_ids    <- names(pvs)

for (i in seq_along(pvs)) {
  pv   <- pvs[[tree_ids[i]]]
  cand <- tryCatch(
    generate_candidate_logs_one_tree(pv, pd, lc, 10L, 10L),
    error = function(e) NULL)
  if (is.null(cand)) next

  elig <- cand[cand$eligible & !is.na(cand$price_volume_m3), , drop = FALSE]
  if (nrow(elig) == 0L) next

  elig <- prepare_price_join_keys(elig, 50L)
  is_saw <- grepl("sawlog", elig$ProductKey, ignore.case = TRUE)
  elig$PriceNOKperM3 <- NA_real_
  if (any(is_saw)) {
    saw_keys <- paste(elig$ProductKey[is_saw], elig$LengthClass_dm[is_saw],
                      elig$PriceDiameterClass_cm_used[is_saw], sep = "|")
    elig$PriceNOKperM3[is_saw] <- pm_saw_lookup[saw_keys]
  }
  if (any(!is_saw)) {
    elig$PriceNOKperM3[!is_saw] <- pm_flat_lookup[elig$ProductKey[!is_saw]]
  }
  elig$value_nok <- elig$price_volume_m3 * elig$PriceNOKperM3
  priced_in <- elig[!is.na(elig$PriceNOKperM3) & !is.na(elig$value_nok), ]
  if (nrow(priced_in) > 0L) priced_list[[i]] <- priced_in
}
priced_list <- Filter(Negate(is.null), priced_list)
n_benchmark_trees <- length(priced_list)
cat(sprintf("Priced candidate tables built: %d trees with priced candidates\n\n",
            n_benchmark_trees))

# ==============================================================================
# Alternative DP implementations
# ==============================================================================

# v2: Pre-split candidates by StartPos + pre-extracted vectors.
# Replaces per-position which() with O(1) list lookup.
# Inner R for-loop over candidates is retained.
optimize_dp_v2 <- function(valued_candidates) {
  dp_in <- valued_candidates[
    !is.na(valued_candidates$value_nok) &
    !is.na(valued_candidates$price_volume_m3), , drop = FALSE]
  dp_in$StartPos <- as.integer(round(dp_in$StartPos))
  dp_in$StopPos  <- as.integer(round(dp_in$StopPos))
  n_dp <- nrow(dp_in)

  if (n_dp == 0L)
    return(list(selected_logs = data.frame(), dp_best_value = 0, n_selected = 0L,
                n_dp_candidates = 0L, optimizer_status = "no_candidates",
                optimizer_message = "No priced candidates; DP not run"))

  # Pre-extract vectors: avoids repeated $-column access in hot loop.
  sp_vec  <- dp_in$StartPos
  ep_vec  <- dp_in$StopPos
  val_vec <- dp_in$value_nok

  all_pos  <- sort(unique(c(sp_vec, ep_vec)))
  sentinel <- max(all_pos) + 1L
  all_pos  <- c(all_pos, sentinel)
  m        <- length(all_pos)

  max_pos    <- max(all_pos)
  pos_to_idx <- integer(max_pos + 1L)
  for (k in seq_len(m)) pos_to_idx[all_pos[k] + 1L] <- k

  # Pre-split candidate indices by StartPos (replaces per-position which()).
  # split() is O(n_dp) once vs O(n_dp * m) with which() per position.
  cands_by_start <- split(seq_len(n_dp), sp_vec)   # named list, char keys

  dp_val <- numeric(m)
  dp_log <- integer(m)

  for (i in seq(m - 1L, 1L)) {
    cur_pos   <- all_pos[i]
    cand_idx  <- cands_by_start[[as.character(cur_pos)]]

    best_val  <- dp_val[i + 1L]
    best_cand <- 0L

    if (!is.null(cand_idx)) {
      for (ci in cand_idx) {
        sp <- ep_vec[ci]
        if (sp > max_pos) next
        si <- pos_to_idx[sp + 1L]
        if (si == 0L) next
        v_tot <- val_vec[ci] + dp_val[si]
        if (v_tot > best_val) {
          best_val  <- v_tot
          best_cand <- ci
        }
      }
    }

    dp_val[i] <- best_val
    dp_log[i] <- best_cand
  }

  # Traceback
  sel_idx <- integer(0)
  i <- 1L
  while (i < m) {
    ci <- dp_log[i]
    if (ci == 0L) { i <- i + 1L
    } else {
      sel_idx <- c(sel_idx, ci)
      sp <- ep_vec[ci]
      if (sp > max_pos) break
      si <- pos_to_idx[sp + 1L]
      if (si == 0L) break
      i <- si
    }
  }

  n_sel <- length(sel_idx)
  if (n_sel == 0L)
    return(list(selected_logs = data.frame(), dp_best_value = dp_val[1L],
                n_selected = 0L, n_dp_candidates = n_dp,
                optimizer_status = "zero_logs",
                optimizer_message = "DP ran but selected 0 logs"))

  sel <- dp_in[sel_idx, , drop = FALSE]
  rownames(sel) <- NULL
  list(selected_logs = sel, dp_best_value = dp_val[1L], n_selected = n_sel,
       n_dp_candidates = n_dp, optimizer_status = "ok",
       optimizer_message = sprintf("Selected %d logs", n_sel))
}


# v3: Pre-split + fully vectorised inner loop.
# Replaces the inner R for-loop with vectorised C operations (+ and which.max).
# No R interpreter iteration over candidates at each position.
optimize_dp_v3 <- function(valued_candidates) {
  dp_in <- valued_candidates[
    !is.na(valued_candidates$value_nok) &
    !is.na(valued_candidates$price_volume_m3), , drop = FALSE]
  dp_in$StartPos <- as.integer(round(dp_in$StartPos))
  dp_in$StopPos  <- as.integer(round(dp_in$StopPos))
  n_dp <- nrow(dp_in)

  if (n_dp == 0L)
    return(list(selected_logs = data.frame(), dp_best_value = 0, n_selected = 0L,
                n_dp_candidates = 0L, optimizer_status = "no_candidates",
                optimizer_message = "No priced candidates; DP not run"))

  sp_vec  <- dp_in$StartPos
  ep_vec  <- dp_in$StopPos
  val_vec <- dp_in$value_nok

  all_pos  <- sort(unique(c(sp_vec, ep_vec)))
  sentinel <- max(all_pos) + 1L
  all_pos  <- c(all_pos, sentinel)
  m        <- length(all_pos)

  max_pos    <- max(all_pos)
  pos_to_idx <- integer(max_pos + 1L)
  for (k in seq_len(m)) pos_to_idx[all_pos[k] + 1L] <- k

  # Pre-split by StartPos.
  cands_by_start <- split(seq_len(n_dp), sp_vec)

  dp_val <- numeric(m)
  dp_log <- integer(m)

  for (i in seq(m - 1L, 1L)) {
    cur_pos <- all_pos[i]
    cands   <- cands_by_start[[as.character(cur_pos)]]

    default_val <- dp_val[i + 1L]

    if (!is.null(cands)) {
      # Vectorised: compute stop-position indices for all candidates at once.
      stop_idx <- pos_to_idx[ep_vec[cands] + 1L]
      valid    <- stop_idx > 0L & ep_vec[cands] <= max_pos
      if (any(valid)) {
        cv       <- cands[valid]
        sv       <- stop_idx[valid]
        total_v  <- val_vec[cv] + dp_val[sv]          # vectorised C addition
        best_k   <- which.max(total_v)                # vectorised C max search
        if (total_v[best_k] > default_val) {
          dp_val[i] <- total_v[best_k]
          dp_log[i] <- cv[best_k]
          next
        }
      }
    }
    dp_val[i] <- default_val
    # dp_log[i] stays 0 (integer default)
  }

  # Traceback
  sel_idx <- integer(0)
  i <- 1L
  while (i < m) {
    ci <- dp_log[i]
    if (ci == 0L) { i <- i + 1L
    } else {
      sel_idx <- c(sel_idx, ci)
      sp <- ep_vec[ci]
      if (sp > max_pos) break
      si <- pos_to_idx[sp + 1L]
      if (si == 0L) break
      i <- si
    }
  }

  n_sel <- length(sel_idx)
  if (n_sel == 0L)
    return(list(selected_logs = data.frame(), dp_best_value = dp_val[1L],
                n_selected = 0L, n_dp_candidates = n_dp,
                optimizer_status = "zero_logs",
                optimizer_message = "DP ran but selected 0 logs"))

  sel <- dp_in[sel_idx, , drop = FALSE]
  rownames(sel) <- NULL
  list(selected_logs = sel, dp_best_value = dp_val[1L], n_selected = n_sel,
       n_dp_candidates = n_dp, optimizer_status = "ok",
       optimizer_message = sprintf("Selected %d logs", n_sel))
}


# ==============================================================================
# Step 2: Correctness check (reference vs v2 vs v3)
# ==============================================================================
cat("=== Correctness check ===\n")
n_mismatch_v2 <- 0L
n_mismatch_v3 <- 0L

for (i in seq_along(priced_list)) {
  cands <- priced_list[[i]]
  ref <- optimize_one_tree_dp(cands)
  v2  <- optimize_dp_v2(cands)
  v3  <- optimize_dp_v3(cands)

  # Compare: n_selected, dp_best_value, StartPos/StopPos sets.
  val_ok_v2  <- abs(v2$dp_best_value  - ref$dp_best_value)  < 1e-8
  nsel_ok_v2 <- v2$n_selected  == ref$n_selected
  pos_ok_v2  <- if (ref$n_selected > 0 && v2$n_selected == ref$n_selected) {
    identical(sort(paste(ref$selected_logs$StartPos, ref$selected_logs$StopPos)),
              sort(paste(v2$selected_logs$StartPos,  v2$selected_logs$StopPos)))
  } else ref$n_selected == v2$n_selected

  val_ok_v3  <- abs(v3$dp_best_value  - ref$dp_best_value)  < 1e-8
  nsel_ok_v3 <- v3$n_selected  == ref$n_selected
  pos_ok_v3  <- if (ref$n_selected > 0 && v3$n_selected == ref$n_selected) {
    identical(sort(paste(ref$selected_logs$StartPos, ref$selected_logs$StopPos)),
              sort(paste(v3$selected_logs$StartPos,  v3$selected_logs$StopPos)))
  } else ref$n_selected == v3$n_selected

  ok_v2 <- val_ok_v2 && nsel_ok_v2 && pos_ok_v2
  ok_v3 <- val_ok_v3 && nsel_ok_v3 && pos_ok_v3

  if (!ok_v2) {
    n_mismatch_v2 <- n_mismatch_v2 + 1L
    cat(sprintf("  MISMATCH v2 tree %d: ref_val=%.4f v2_val=%.4f ref_n=%d v2_n=%d\n",
                i, ref$dp_best_value, v2$dp_best_value, ref$n_selected, v2$n_selected))
  }
  if (!ok_v3) {
    n_mismatch_v3 <- n_mismatch_v3 + 1L
    cat(sprintf("  MISMATCH v3 tree %d: ref_val=%.4f v3_val=%.4f ref_n=%d v3_n=%d\n",
                i, ref$dp_best_value, v3$dp_best_value, ref$n_selected, v3$n_selected))
  }
}

cat(sprintf("  Trees checked      : %d\n", n_benchmark_trees))
cat(sprintf("  v2 mismatches      : %d  (%s)\n", n_mismatch_v2,
            if (n_mismatch_v2 == 0) "IDENTICAL" else "FAIL"))
cat(sprintf("  v3 mismatches      : %d  (%s)\n", n_mismatch_v3,
            if (n_mismatch_v3 == 0) "IDENTICAL" else "FAIL"))


# ==============================================================================
# Step 3: Runtime benchmark (n_reps repetitions for stable timing)
# ==============================================================================
N_REPS <- 5L
cat(sprintf("\n=== Runtime benchmark (%d reps x %d trees) ===\n",
            N_REPS, n_benchmark_trees))

bench_run <- function(dp_fn, label) {
  times <- numeric(N_REPS)
  for (rep in seq_len(N_REPS)) {
    t0 <- proc.time()
    for (cands in priced_list) dp_fn(cands)
    times[rep] <- as.numeric((proc.time() - t0)["elapsed"])
  }
  med <- median(times)
  cat(sprintf("  %-12s median=%.3f s  range=[%.3f,%.3f]\n",
              label, med, min(times), max(times)))
  med
}

t_ref <- bench_run(optimize_one_tree_dp, "reference")
t_v2  <- bench_run(optimize_dp_v2,       "v2")
t_v3  <- bench_run(optimize_dp_v3,       "v3")

cat(sprintf("\n  Speedup v2 vs ref  : %.2fx\n", t_ref / t_v2))
cat(sprintf("  Speedup v3 vs ref  : %.2fx\n", t_ref / t_v3))
cat(sprintf("  Speedup v3 vs v2   : %.2fx\n", t_v2  / t_v3))

cat(sprintf("\n  Trees per second (ref): %.1f\n", n_benchmark_trees / t_ref * N_REPS / N_REPS))
cat(sprintf("  Trees per second (v3) : %.1f\n", n_benchmark_trees / t_v3 * N_REPS / N_REPS))

# Candidate stats
total_cands <- sum(vapply(priced_list, nrow, integer(1)))
avg_cands   <- total_cands / n_benchmark_trees
cat(sprintf("\n  Total priced candidates across trees: %d\n", total_cands))
cat(sprintf("  Average per tree                    : %.0f\n", avg_cands))


# ==============================================================================
# Step 4: Recommendation
# ==============================================================================
cat("\n=== Recommendation ===\n")

speedup_v3 <- t_ref / t_v3
if (speedup_v3 >= 3) {
  cat(sprintf(
    "  v3 gives %.1fx speedup. RECOMMEND integrating v3 into the backend.\n",
    speedup_v3))
  cat("  Integration: replace the optimize_one_tree_dp() call in\n")
  cat("  scripts/app_backend/01_run_sampled_plot_bucking.R with\n")
  cat("  a local .optimize_dp_fast() function.\n")
  cat("  Do NOT modify R/helpers_dp_optimizer.R.\n")
} else if (speedup_v3 >= 1.5) {
  cat(sprintf(
    "  v3 gives %.1fx speedup. Moderate improvement.\n", speedup_v3))
  cat("  Consider Rcpp for the DP loop as the next step.\n")
} else {
  cat(sprintf(
    "  v3 gives only %.1fx speedup. Pure R gains are near exhausted.\n",
    speedup_v3))
  cat("  Recommend Rcpp implementation of the backward-induction loop.\n")
}
