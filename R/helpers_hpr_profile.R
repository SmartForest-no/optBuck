# ==============================================================================
# R/helpers_hpr_profile.R  (2026-11-19)
#
# Purpose : Estimated stem profiles for .hpr files that have recorded logs but no
#           StemDiameters (older John Deere files, some others). The profile of each
#           single-tree stem is calibrated on the diameters the file does give: DBH at
#           DBHHeight, and per log Mid ob (log middle), Top ob (log top) and, where the
#           file has it, Butt ob (log start). docs/plans/logs-only-profiles.md has the
#           validation (profile RMSE 6.7 mm, yield index about 0.01 lower than with
#           measured profiles, 14 files with real profiles).
# Method  : (1) total height H: bounded 1-D least squares of taperNOR::taperNOR() at
#           the DBH of the file (H between the highest measured point + 0.1 m and 55 m);
#           (2) taperNOR::kublin_nor() (TapeR calibration on all diameters, mHt = H)
#           gives the profile on the 10 cm grid. kublin_nor uses the spruce parameters
#           for every species (known taperNOR issue); hfromd() was not used because it
#           fails on short stems (heights of several hundred metres).
# Range   : from the felling cut to the top of the last recorded log + 10 cm, rounded
#           up to 10 cm (where the John Deere profiles end), and not above H.
# Heights : above ground = position + (130 - DBHHeight), as for measured profiles
#           (hpr_stump_offset()).
# Reads   : per stem StemKey and DBH only (whitelist); the recorded logs come from
#           read_hpr_logs(). Never names, coordinates, dates.
# ==============================================================================


# Highest total height (m) the estimate accepts.
HPR_EST_MAX_HEIGHT_M <- 55

# DBH (mm) of each stem record: data.frame(stem_key, dbh_mm).
read_hpr_dbh <- function(path, filename = basename(path), chunk_bytes = HPR_STEM_CHUNK_BYTES) {
  src <- .hpr_source(path, filename)
  if (length(src$errors)) return(data.frame(stem_key = character(0), dbh_mm = numeric(0)))
  parts <- .hpr_stem_chunks(src$path, src$entry, function(r)
    data.frame(stem_key = trimws(.hs_re1(r, "<StemKey>([^<]*)</StemKey>")),
               dbh_mm = .hs_num1(r, "<DBH>\\s*([^<]*?)\\s*</DBH>"), stringsAsFactors = FALSE), chunk_bytes)
  d <- if (length(parts)) do.call(rbind, parts) else data.frame(stem_key = character(0), dbh_mm = numeric(0))
  d[!duplicated(d$stem_key), , drop = FALSE]
}

# Height above ground (m) and diameter (cm) of the points a logs-only stem gives.
# @param lg  The stem's recorded logs (read_hpr_logs()$logs rows: StartPos, StopPos, MidOb, TopOb, ButtOb in mm).
# @param dbh_mm,dbh_pos_cm  DBH and the position (cm from the felling cut) it is measured at.
# @param offset_cm  Stump offset (hpr_stump_offset()).
.hpr_est_points <- function(lg, dbh_mm, dbh_pos_cm, offset_cm) {
  P <- rbind(data.frame(pos = (lg$StartPos + lg$StopPos) / 2, d = lg$MidOb),
             data.frame(pos = lg$StopPos, d = lg$TopOb),
             data.frame(pos = lg$StartPos, d = lg$ButtOb),
             data.frame(pos = dbh_pos_cm, d = dbh_mm))
  P <- P[!is.na(P$d) & !is.na(P$pos) & P$d > 0, , drop = FALSE]
  P$h <- (P$pos + offset_cm) / 100; P$dcm <- P$d / 10
  P <- P[order(P$h), , drop = FALSE]
  P[!duplicated(round(P$h, 3)), , drop = FALSE]
}

# Profile of one stem. @return list(pos (cm from the cut, 10 cm grid), d (mm over bark), height_m) or a
# character reason it cannot be estimated.
.hpr_est_profile <- function(P, species, dbh_cm, offset_cm, end_cm) {
  if (nrow(P) < 3L) return("fewer than three diameters")
  sp <- if (identical(species, "pine")) "pine" else "spruce"
  fit <- tryCatch(stats::optimize(function(H) {
    if (H <= max(P$h) + 0.05) return(1e9)
    sum((P$dcm - suppressWarnings(taperNOR::taperNOR(P$h, dbh_cm, H, sp)))^2) }, c(max(P$h) + 0.1, HPR_EST_MAX_HEIGHT_M)),
    error = function(e) NULL)
  if (is.null(fit) || !is.finite(fit$minimum)) return("tree height not found")
  H <- fit$minimum
  top_cm <- min(end_cm, H * 100 - offset_cm)
  g <- seq(0, floor(top_cm / 10) * 10, by = HPR_PROFILE_STEP_CM)
  if (length(g) < 3L) return("profile too short")
  hx <- pmin((g + offset_cm) / 100, H - 1e-6)
  d <- tryCatch(as.numeric(taperNOR::kublin_nor(Hx = hx, Hm = P$h, Dm = P$dcm, mHt = H, sp = 1)$DHx),
                error = function(e) NULL)
  if (is.null(d) || any(!is.finite(d))) return("profile calibration failed")
  list(pos = g, d = pmax(d, 0) * 10, height_m = H)
}

# Estimate the profiles of a read_hpr_stems() result whose file has no StemDiameters.
# Only single-tree stems with recorded logs and a DBH are estimated; the rest stay without profile.
#
# @param st    The (profile-less) read_hpr_stems() result.
# @param logs  read_hpr_logs()$logs.
# @param dbh   read_hpr_dbh().
# @param species_map  Group key -> "spruce" / "pine" / "none" (default: the name guess; "none" is estimated
#   as spruce, so a later change of the choice still finds a profile).
# @return list(st (stems, idx, pos, d filled; `estimated` TRUE, category "Over bark"), n_estimated,
#   reasons (table of why stems were not estimated)).
hpr_estimate_profiles <- function(st, logs, dbh, species_map = NULL) {
  s <- st$stems; sp <- st$species
  if (is.null(species_map))
    species_map <- setNames(guess_hpr_species(sp$name, sp$user_id), sp$key)
  lgs <- split(logs, logs$stem_key)
  dbh_mm <- dbh$dbh_mm[match(s$stem_key, dbh$stem_key)]
  pos <- vector("list", nrow(s)); dd <- vector("list", nrow(s)); why <- rep(NA_character_, nrow(s))
  est_in <- vector("list", nrow(s)); est_sp <- rep(NA_character_, nrow(s))
  for (i in which(s$kind == "single")) {
    l1 <- lgs[[s$stem_key[i]]]
    if (is.null(l1) || nrow(l1) == 0L) { why[i] <- "no recorded logs"; next }
    if (is.na(dbh_mm[i]) || dbh_mm[i] <= 0) { why[i] <- "no DBH"; next }
    gi <- match(s$species_key[i], sp$key)
    off <- if (is.na(gi)) HPR_DEFAULT_STUMP_CM else sp$offset_cm[gi]
    sps <- unname(species_map[s$species_key[i]])
    P <- .hpr_est_points(l1, dbh_mm[i], HPR_DBH_ABOVE_GROUND_CM - off, off)
    end_cm <- ceiling((max(l1$StopPos) + 10) / 10) * 10
    r <- .hpr_est_profile(P, sps, dbh_mm[i] / 10, off, end_cm)
    if (is.character(r)) { why[i] <- r; next }
    pos[[i]] <- r$pos; dd[[i]] <- r$d
    # what a re-estimate needs when the species choice changes (hpr_reestimate_profiles())
    est_in[[i]] <- list(P = P, dbh_cm = dbh_mm[i] / 10, off = off, end_cm = end_cm)
    est_sp[i] <- if (identical(sps, "pine")) "pine" else "spruce"
  }
  n <- lengths(pos)
  st$stems$n_points <- as.integer(n)
  st$stems$category <- ifelse(n > 0L, "Over bark", s$category)
  st$stems$estimated <- n > 0L
  st$stems$est_species <- est_sp; st$est_in <- est_in   # est_in: a list, so not a stems column (tables are written)
  st$idx <- rep.int(seq_len(nrow(s)), n)
  st$pos <- unlist(pos, use.names = FALSE); st$d <- unlist(dd, use.names = FALSE)
  list(st = st, n_estimated = sum(n > 0L), reasons = table(why[!is.na(why)]))
}

# Re-estimate the stems whose species choice changed since they were estimated (the group's species is
# set in tab 1 after the file is read): the profile is rebuilt with the chosen species' taper curve for
# the height fit. Stems that cannot be re-estimated keep their profile. Nothing to do (and the same
# object back) for measured profiles or an unchanged choice. The last result is kept in `est_cache`
# (an environment shared by copies of `st`), so repeated calls with one map cost nothing.
#
# @param st          read_hpr_stems() / combine_hpr_stems() result.
# @param species_map Group key -> "spruce" / "pine" / "none" ("none" counts as spruce, as at read time).
# @return `st` with idx/pos/d, stems$n_points and stems$est_species updated.
hpr_reestimate_profiles <- function(st, species_map) {
  s <- st$stems
  if (is.null(s$estimated) || !any(s$estimated) || is.null(s$est_species) || is.null(st$est_in)) return(st)
  want <- ifelse(unname(species_map[s$species_key]) %in% "pine", "pine", "spruce")
  chg <- which(s$estimated & !is.na(s$est_species) & s$est_species != want)
  if (!length(chg)) return(st)
  key <- paste(chg, want[chg], collapse = ",")
  cache <- st$est_cache
  if (is.environment(cache) && identical(cache$key, key)) return(cache$st)
  pos <- vector("list", length(chg)); dd <- pos; ok <- rep(FALSE, length(chg))
  for (j in seq_along(chg)) {
    e <- st$est_in[[chg[j]]]
    r <- .hpr_est_profile(e$P, want[chg[j]], e$dbh_cm, e$off, e$end_cm)
    if (is.character(r)) next
    pos[[j]] <- r$pos; dd[[j]] <- r$d; ok[j] <- TRUE
  }
  chg <- chg[ok]; pos <- pos[ok]; dd <- dd[ok]
  if (length(chg)) {
    keep <- !(st$idx %in% chg)
    idx <- c(st$idx[keep], rep.int(chg, lengths(pos)))
    p <- c(st$pos[keep], unlist(pos, use.names = FALSE)); d <- c(st$d[keep], unlist(dd, use.names = FALSE))
    o <- order(idx, p)
    st$idx <- idx[o]; st$pos <- p[o]; st$d <- d[o]
    st$stems$n_points[chg] <- lengths(pos); st$stems$est_species[chg] <- want[chg]
  }
  if (is.environment(cache)) { cache$key <- key; cache$st <- st }
  st
}
