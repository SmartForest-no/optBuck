# app/setup.R
#
# Purpose : App-wide data and constants: plot metadata, product/price
#           default assortments and prices, HKL/bonitet/county choice
#           mappings, fixed sampling settings and the release version
#           (internal). Styling is in theme.R and
#           www/optbuck.css.
#           Sourced by app.R after ROOT is resolved and the backend is loaded.

# ---- load plot metadata for stratum availability preview --------------------
plots_all <- as.data.frame(arrow::read_parquet(
  extdata_path("nfi", "nfi_plots_github.parquet", root_dir = ROOT)))

# ---- default assortments and prices -------------------------------------------
# The base products as the assortment table (Phase 4c; R/helpers_products.R)
# and the default prices in optBuck's minimal long form. Both scenarios start
# from these; "Reset to defaults" and "Reset to default prices" restore them.
APP_PRODUCTS   <- load_products(ROOT)
APP_PM_DEFAULT <- pm_minimal(load_price_matrix_long(ROOT))
APP_DEFS       <- defs_from_products(APP_PRODUCTS)

# ---- NFI plots (Phase 4d) -------------------------------------------------------
# NFI runs use every plot of the selected stratum (no sampling). The test-only
# OPTBUCK_N_PLOTS samples that many plots instead (seed 42; audit 17 runs with 5).
APP_N_PLOTS <- suppressWarnings(as.integer(Sys.getenv("OPTBUCK_N_PLOTS")))   # NA = all plots
APP_SEED    <- 42L
# Test-only: the number of tuning iterations of the Price tuning tab (audit 17).
if (nzchar(Sys.getenv("OPTBUCK_TUNE_MAX_ITER")))
  options(optbuck.tune_max_iter = as.integer(Sys.getenv("OPTBUCK_TUNE_MAX_ITER")))

# The NFI trees with the columns select_nfi_trees() (R/helpers_tree_selection.R)
# needs: the sidebar's live tree counts use the run's own selection code.
trees_meta <- as.data.frame(arrow::read_parquet(
  extdata_path("nfi", "nfi_tree_plot_stratum_ready_github.parquet", root_dir = ROOT),
  col_select = c("plot_id_github", "treid", "species_group", "dbh_cm", "trehoyde",
                 "beregnet_hoyde", "hogstkl", "abonitet", "fylnr", "hoh")))

# ---- HKL group mapping (text id -> raw hogstkl codes) ---------------------
# Labels come from the translation table (inputs/i18n/translations.csv).
HKL_CHOICES <- c(hkl_3 = "3", hkl_4 = "4", hkl_5 = "5")
HKL_MAP <- list("3" = c(31L, 32L), "4" = c(41L, 42L), "5" = c(51L, 52L))
# Site class labels: the class itself, with a note for the lowest one.
bonitet_labels <- function(x, lang) ifelse(x == "6", tr("bonitet_6", lang = lang), x)
# Productive-forest plots only (raw HKL 3-5, codes 31-52)
plots_prod <- plots_all[plots_all$hogstkl %in% unlist(HKL_MAP), , drop = FALSE]

# ---- altitude slider (hoh, m a.s.l.) ------------------------------------------
# Fixed range over all HKL 3-5 plots, rounded out to the 50 m step (0-1100 m);
# the full range means no altitude filter (selected_hoh(), helpers_validation.R).
HOH_STEP_M <- 50
HOH_RANGE  <- c(floor(min(plots_prod$hoh, na.rm = TRUE) / HOH_STEP_M),
                ceiling(max(plots_prod$hoh, na.rm = TRUE) / HOH_STEP_M)) * HOH_STEP_M

# ---- County label mapping (current 2024 codes + historical fallback) --------
.FYLNR_NAMES <- c(
  "03"="Oslo", "11"="Rogaland", "15"="M\u00f8re og Romsdal",
  "18"="Nordland", "31"="\u00d8stfold", "32"="Akershus",
  "33"="Buskerud", "34"="Innlandet", "39"="Vestfold",
  "40"="Telemark", "42"="Agder", "46"="Vestland",
  "50"="Tr\u00f8ndelag", "55"="Troms", "56"="Finnmark",
  # Historical codes (kept for robustness)
  "01"="\u00d8stfold", "02"="Akershus",
  "04"="Hedmark", "05"="Oppland",
  "06"="Buskerud", "07"="Vestfold",
  "08"="Telemark", "09"="Aust-Agder",
  "10"="Vest-Agder", "12"="Hordaland",
  "14"="Sogn og Fjordane", "16"="S\u00f8r-Tr\u00f8ndelag",
  "17"="Nord-Tr\u00f8ndelag", "19"="Troms", "20"="Finnmark"
)
# County choices from the codes actually in the data, in the language of
# the moment ("(old)" for historical codes; names are Norwegian already).
.FYLNR_OLD <- c("01", "02", "04", "05", "06", "07", "08", "09", "10", "12", "14", "16", "17", "19", "20")
.DATA_FYLNR <- sort(unique(as.character(plots_prod$fylnr)))
fylnr_choices <- function(lang) {
  lbl <- vapply(.DATA_FYLNR, function(code) {
    nm <- unname(.FYLNR_NAMES[code])
    if (is.na(nm)) tr("county_unknown", code, lang = lang)
    else if (code %in% .FYLNR_OLD) tr("county_old", code, nm, lang = lang)
    else sprintf("%s - %s", code, nm)
  }, character(1))
  c(stats::setNames("__all__", tr("county_all", lang = lang)), stats::setNames(.DATA_FYLNR, lbl))
}

# ---- upload limit (Import from .hpr) -------------------------------------------
# Harvester files are large (real .hpr files: 3-151 MB). Shiny has one upload
# limit for all inputs; the other uploads are small. Default 200 MB; set the
# option optbuck.hpr_max_mb or the environment variable OPTBUCK_HPR_MAX_MB.
HPR_MAX_UPLOAD_MB <- local({
  env <- suppressWarnings(as.numeric(Sys.getenv("OPTBUCK_HPR_MAX_MB", "")))
  as.numeric(getOption("optbuck.hpr_max_mb", if (is.na(env)) 200 else env))
})
options(shiny.maxRequestSize = HPR_MAX_UPLOAD_MB * 1024^2)

# optBuck species of an .hpr species group (import dialog and ".hpr stems").
species_group_choices <- function(lang)
  stats::setNames(c("spruce", "pine", "both"), tr(c("sp_spruce", "sp_pine", "sp_both"), lang = lang))
hpr_species_choices <- function(lang)
  stats::setNames(c("spruce", "pine", "none"), tr(c("sp_spruce", "sp_pine", "sp_not_used"), lang = lang))

# ---- language (Phase 1, 2026-10-27; R/helpers_i18n.R) -------------------------
# The language the page starts in: Norsk, or the test-only OPTBUCK_LANG. The
# browser then applies the viewer's own choice (www/optbuck.js).
APP_LANG <- i18n_lang(Sys.getenv("OPTBUCK_LANG", ""))
i18n_table()

# ---- release version (DESCRIPTION; internal, not shown) -----------------------
APP_VERSION <- as.character(utils::packageVersion("optBuck"))

# ---- parallel workers (R/helpers_parallel.R) ---------------------------------
# Started once per app process and shared by all sessions; runs are
# background tasks on them (helpers_jobs.R). Option optbuck.workers sets the
# number (default detectCores() - 1, meant for one R process per machine).
APP_WORKERS <- optbuck_workers()
.ensure_plan(APP_WORKERS)
