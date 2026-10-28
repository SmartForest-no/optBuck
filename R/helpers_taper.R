# ==============================================================================
# R/helpers_taper.R
#
# Purpose : Taper profile generation using taperNOR (over-bark, Norwegian
#           equations) for the Apter bucking simulation.
#
# Accepted conventions (see CLAUDE.md):
#   - taperNOR over-bark  (with_bark = TRUE)
#   - DiameterSource = "taperNOR_over_bark"
#   - Positions in cm above terrain
#   - Profile step default 0.10 m
#   - height_m = beregnet_hoyde / 10 when beregnet_hoyde is in decimetres
#   - No bark correction; no under-bark conversion
#
# Usage   : source("R/helpers_paths.R")
#           source("R/helpers_taper.R")
#           profile <- generate_taper_profile_one_tree(
#                        treid="242251", plot_id="C64024",
#                        species_group="spruce", dbh_cm=50.1, height_m=20.0)
#           pvs     <- profiles_to_profile_vectors(profile)
#           pv      <- pvs[["242251"]]
# ==============================================================================

if (!exists("or_default", mode = "function"))   # helpers_utils.R
  source(file.path(dirname(if (is.null(sys.frame(1L)$ofile)) "." else sys.frame(1L)$ofile),
                   "helpers_utils.R"))

if (!exists("get_root_dir", mode = "function"))
  source(file.path(dirname(or_default(sys.frame(1L)$ofile, ".")), "helpers_paths.R"))


#' Minimum DBH (cm) accepted by taperNOR.
TAPER_MIN_DBH_CM <- 5.0

#' Minimum height (m) accepted by taperNOR.
TAPER_MIN_HEIGHT_M <- 4.5

#' Profile step in cm (must equal START_POS_STEP_CM in candidate generation).
PROFILE_STEP_CM <- 10L


# ------------------------------------------------------------------------------
# Input normalisation
# ------------------------------------------------------------------------------

#' Normalise a single-row data frame or named list to the inputs required by
#' generate_taper_profile_one_tree().
#'
#' Handles both "height_m" (metres) and "beregnet_hoyde" (decimetres) columns.
#' Returns a named list with: treid, plot_id, species_group, dbh_cm, height_m.
#' Stops with a descriptive error if any required value is missing.
#'
#' @param tree_row  A one-row data.frame or named list.
#' @return Named list: treid, plot_id, species_group, dbh_cm, height_m.
normalise_tree_inputs_for_taper <- function(tree_row) {
  get_field <- function(x, ...) {
    candidates <- c(...)
    for (nm in candidates) {
      v <- x[[nm]]
      if (!is.null(v) && length(v) > 0L && !is.na(v[1L]))
        return(v[1L])
    }
    NA
  }

  treid        <- as.character(get_field(tree_row, "treid"))
  plot_id      <- as.character(or_default(get_field(tree_row, "plot_id"), NA_character_))
  species_group <- as.character(get_field(tree_row, "species_group"))

  dbh_cm <- as.numeric(get_field(tree_row, "dbh_cm", "brysth_dia"))
  if (is.na(dbh_cm))
    stop(sprintf("Tree %s: dbh_cm missing.", treid))

  # height_m takes precedence; fall back to beregnet_hoyde (dm) / 10.
  height_m <- as.numeric(get_field(tree_row, "height_m", "trehoyde", "beregnet_hoyde"))
  if (!is.na(height_m)) {
    # Detect decimetres: beregnet_hoyde > 10 usually means it is in dm.
    if ("beregnet_hoyde" %in% names(tree_row) &&
        !("height_m" %in% names(tree_row)) &&
        height_m > 10) {
      height_m <- height_m / 10
    }
  } else {
    stop(sprintf("Tree %s: height_m missing.", treid))
  }

  if (!species_group %in% c("spruce", "pine"))
    stop(sprintf("Tree %s: species_group '%s' not supported (must be spruce or pine).",
                 treid, species_group))

  list(
    treid         = treid,
    plot_id       = plot_id,
    species_group = species_group,
    dbh_cm        = dbh_cm,
    height_m      = height_m
  )
}


# ------------------------------------------------------------------------------
# One-tree profile generation
# ------------------------------------------------------------------------------

#' Generate a taper profile for one tree using taperNOR (over-bark).
#'
#' Returns a data.frame with one row per profile step, including derived
#' columns (diameterPosition in cm, DiameterValue in mm) needed by
#' candidate generation.
#'
#' @param treid          Tree ID (character or numeric).
#' @param plot_id        Plot ID (character).
#' @param species_group  "spruce" or "pine".
#' @param dbh_cm         Diameter at breast height (cm), over-bark.
#' @param height_m       Total tree height (m).
#' @param profile_step_m Profile step (m); default 0.10.
#' @param sesong         Optional season/year field (passed through).
#' @return data.frame with columns: treid, plot_id, species_group, dbh_cm,
#'   height_m, z_m, diameter_cm, DiameterValue (mm), diameterPosition (cm),
#'   DBH (mm).
generate_taper_profile_one_tree <- function(treid, plot_id = NA_character_,
                                             species_group,
                                             dbh_cm, height_m,
                                             profile_step_m = 0.10,
                                             sesong = NA) {
  if (!requireNamespace("taperNOR", quietly = TRUE))
    stop("Package 'taperNOR' is required. ",
         "Install: devtools::install_git('https://github.com/SmartForest-no/taperNOR')")

  z_vec <- seq(profile_step_m, height_m, by = profile_step_m)
  if (length(z_vec) == 0L)
    stop(sprintf("Tree %s: height_m=%.2f is too small for step %.2f m.",
                 treid, height_m, profile_step_m))

  d_cm <- taperNOR::taperNOR(
    h         = z_vec,
    dbh       = dbh_cm,
    h_top     = height_m,
    sp        = species_group,
    with_bark = TRUE
  )

  profile_step_cm <- as.integer(round(profile_step_m * 100))

  data.frame(
    treid             = as.character(treid),
    plot_id           = as.character(or_default(plot_id, NA_character_)),
    sesong            = sesong,
    species_group     = species_group,
    dbh_cm            = dbh_cm,
    height_m          = height_m,
    z_m               = z_vec,
    diameter_cm       = d_cm,
    # Derived columns for candidate generation
    DiameterValue     = d_cm * 10,                  # mm
    diameterPosition  = as.integer(
      round(z_vec * 100 / profile_step_cm) * profile_step_cm),   # cm
    DBH               = dbh_cm * 10,                # mm
    stringsAsFactors  = FALSE
  )
}


# ------------------------------------------------------------------------------
# Batch profile generation
# ------------------------------------------------------------------------------

#' Generate taper profiles for multiple trees.
#'
#' @param tree_data      data.frame with one row per tree. Required columns:
#'                       treid, species_group, dbh_cm, height_m (or
#'                       beregnet_hoyde).  Optional: plot_id, sesong.
#' @param profile_step_m Profile step (m); default 0.10.
#' @param quiet          Suppress per-tree messages.
#' @return data.frame: all profiles row-bound, plus a `profile_ok` attribute
#'   (logical vector, one per tree).
generate_taper_profiles_for_trees <- function(tree_data,
                                               profile_step_m = 0.10,
                                               quiet = FALSE) {
  n <- nrow(tree_data)
  rows_ok      <- logical(n)
  error_msgs   <- character(n)
  profile_list <- vector("list", n)

  for (i in seq_len(n)) {
    row_i <- tree_data[i, , drop = FALSE]
    result <- tryCatch({
      inp <- normalise_tree_inputs_for_taper(row_i)
      p   <- generate_taper_profile_one_tree(
               treid         = inp$treid,
               plot_id       = inp$plot_id,
               species_group = inp$species_group,
               dbh_cm        = inp$dbh_cm,
               height_m      = inp$height_m,
               profile_step_m = profile_step_m,
               sesong        = or_default(row_i[["sesong"]], NA))
      list(profile = p, ok = TRUE, msg = "")
    }, error = function(e) {
      list(profile = NULL, ok = FALSE, msg = conditionMessage(e))
    })

    rows_ok[i]    <- result$ok
    error_msgs[i] <- result$msg
    if (result$ok) {
      profile_list[[i]] <- result$profile
    } else if (!quiet) {
      warning(sprintf("Tree %s: profile failed: %s",
                      or_default(tree_data$treid[i], i), result$msg))
    }
  }

  profiles_df <- do.call(rbind, profile_list[rows_ok])
  if (is.null(profiles_df)) profiles_df <- data.frame()

  attr(profiles_df, "profile_ok")     <- rows_ok
  attr(profiles_df, "profile_errors") <- error_msgs
  profiles_df
}


# ------------------------------------------------------------------------------
# Profile vector extraction (for per-tree candidate generation)
# ------------------------------------------------------------------------------

#' Convert a profiles data.frame to a named list of per-tree profile vectors.
#'
#' Each element is a list with fields used by generate_candidate_logs_one_tree:
#'   diameterPosition  integer vector (cm, multiples of PROFILE_STEP_CM)
#'   DiameterValue     numeric vector (mm)
#'   DBH               numeric scalar (mm)
#'   height_m          numeric scalar (m)
#'   height_cm         integer scalar (cm)
#'   species_group     character scalar
#'   plot_id           character scalar
#'   treid             character scalar
#'   grade             numeric vector, one per position (optional; .hpr stem
#'                     grades, Phase 6: the grade of the cell [pos, pos + 10 cm))
#'
#' @param profiles        data.frame from generate_taper_profiles_for_trees().
#' @param profile_step_cm Profile step in cm; default PROFILE_STEP_CM (10L).
#' @return Named list keyed by treid (character).
profiles_to_profile_vectors <- function(profiles,
                                         profile_step_cm = PROFILE_STEP_CM) {
  if (is.null(profiles) || nrow(profiles) == 0L) return(list())

  required <- c("treid", "diameterPosition", "DiameterValue",
                "DBH", "height_m", "species_group")
  missing  <- setdiff(required, names(profiles))
  if (length(missing) > 0L)
    stop("profiles data.frame missing columns: ", paste(missing, collapse = ", "))

  tree_ids <- unique(as.character(profiles$treid))
  pvs      <- vector("list", length(tree_ids))
  names(pvs) <- tree_ids

  # Row indices per tree in one pass (split() keeps the original row order),
  # and the columns pulled out once: a per-tree scan of the whole table was
  # quadratic in the number of trees.
  rows_by_tree <- split(seq_len(nrow(profiles)),
                        factor(as.character(profiles$treid), levels = tree_ids))
  plot_col <- profiles$plot_id
  sp_col   <- profiles$species_group
  dbh_col  <- profiles$DBH
  h_col    <- profiles$height_m
  pos_col  <- profiles$diameterPosition
  dv_col   <- profiles$DiameterValue
  grade_col <- profiles$grade                       # NULL unless the profiles carry grades

  for (tid in tree_ids) {
    ii <- rows_by_tree[[tid]]
    ii <- ii[order(pos_col[ii])]
    i1 <- ii[1L]

    pvs[[tid]] <- list(
      treid            = tid,
      plot_id          = as.character(or_default(plot_col[i1], NA_character_)),
      species_group    = sp_col[i1],
      DBH              = dbh_col[i1],
      height_m         = h_col[i1],
      height_cm        = as.integer(round(h_col[i1] * 100)),
      diameterPosition = as.integer(pos_col[ii]),
      DiameterValue    = as.numeric(dv_col[ii])
    )
    # Stem grade per grid point (.hpr stems with grades on, Phase 6).
    if (!is.null(grade_col)) pvs[[tid]]$grade <- as.numeric(grade_col[ii])
  }
  pvs
}
