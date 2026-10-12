# app/setup.R
#
# Purpose : App-wide data and constants: plot metadata, product/price
#           reference tables, HKL/bonitet/county choice mappings, the
#           estimate label and release version. Styling is in theme.R and
#           www/apter.css.
#           Sourced by app.R after ROOT is resolved and the backend is loaded.

# ---- load plot metadata for stratum availability preview --------------------
plots_all <- as.data.frame(arrow::read_parquet(
  file.path(ROOT, "data_processed_github", "nfi_plots_github.parquet")))

# ---- load product / price reference data (displayed read-only) -------------
APP_PRODUCTS   <- load_products(ROOT)
APP_LC         <- load_length_classes(ROOT)
APP_PRICE_LONG <- load_price_matrix_long(ROOT)

# ---- HKL group mapping (user-facing -> raw hogstkl codes) -----------------
HKL_CHOICES <- c(
  "HKL 3 - younger production forest" = "3",
  "HKL 4 - older production forest"   = "4",
  "HKL 5 - mature / old-growth forest" = "5"
)
HKL_MAP <- list("3" = c(31L, 32L), "4" = c(41L, 42L), "5" = c(51L, 52L))
BONITET_LABELS <- c("6"="6 (very low)","8"="8","11"="11","14"="14",
                    "17"="17","20"="20","23"="23","26"="26")
# Productive-forest plots only (raw HKL 3-5, codes 31-52)
plots_prod <- plots_all[plots_all$hogstkl %in% unlist(HKL_MAP), , drop = FALSE]

# ---- County label mapping (current 2024 codes + historical fallback) --------
.FYLNR_NAMES <- c(
  "03"="Oslo", "11"="Rogaland", "15"="M\u00f8re og Romsdal",
  "18"="Nordland", "31"="\u00d8stfold", "32"="Akershus",
  "33"="Buskerud", "34"="Innlandet", "39"="Vestfold",
  "40"="Telemark", "42"="Agder", "46"="Vestland",
  "50"="Tr\u00f8ndelag", "55"="Troms", "56"="Finnmark",
  # Historical codes (kept for robustness)
  "01"="\u00d8stfold (old)", "02"="Akershus (old)",
  "04"="Hedmark (old)", "05"="Oppland (old)",
  "06"="Buskerud (old)", "07"="Vestfold (old)",
  "08"="Telemark (old)", "09"="Aust-Agder (old)",
  "10"="Vest-Agder (old)", "12"="Hordaland (old)",
  "14"="Sogn og Fjordane (old)", "16"="S\u00f8r-Tr\u00f8ndelag (old)",
  "17"="Nord-Tr\u00f8ndelag (old)", "19"="Troms (old)", "20"="Finnmark (old)"
)
# Build county choices from codes actually in the data
.DATA_FYLNR <- sort(unique(as.character(plots_prod$fylnr)))
.fylnr_lbl <- function(code) {
  nm <- .FYLNR_NAMES[code]
  if (is.na(nm)) sprintf("%s - unknown county", code)
  else sprintf("%s - %s", code, nm)
}
FYLNR_CHOICES <- c(
  "All counties" = "__all__",
  setNames(.DATA_FYLNR, vapply(.DATA_FYLNR, .fylnr_lbl, character(1)))
)

ESTIMATE_LABEL <- "Sampled-plot estimate. Not ha-expanded. No NFI factors applied."

# ---- release version (repo-root VERSION file; shown in the footer) ------------
APP_VERSION <- trimws(readLines(file.path(ROOT, "VERSION"), n = 1L, warn = FALSE))
