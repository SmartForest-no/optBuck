# ==============================================================================
# R/api.R  (roadmap 8, step 8, 2026-11-05)
#
# The exported functions of optBuck, with their documentation (roxygen2) and examples on the synthetic
# fixtures. The files of the package otherwise use plain comments, so only this file makes man pages.
# ==============================================================================

#' optBuck: bucking simulator and bucking analysis for harvester files
#'
#' Reads StanForD 2010 harvester production files (.hpr) and product instructions (.pin), finds the
#' cutting pattern that gives the highest value for each stem (backward induction on the stem
#' profile), compares the harvester's recorded bucking with the best possible one, and offers a Shiny
#' app ([run_app()]) for comparing two sets of assortments and prices.
#'
#' @keywords internal
"_PACKAGE"

# The synthetic fixture of the package (invented stems, no real data).
.fixture <- function(name) extdata_path("inputs", "test_fixtures", name)

# Species group key -> "spruce", "pine" or "none", guessed from the group names.
.species_map_of <- function(h) setNames(guess_hpr_species(h$species$name, h$species$user_id), h$species$key)

#' Start the optBuck app
#'
#' Opens the app (five tabs: stems, assortments and prices, simulator, price tuning, bucking analysis).
#' The app needs the packages in `Suggests` (shiny, bslib, reactable, rhandsontable, ggplot2, ragg,
#' promises, later and others); NFI stem profiles are built on first use of the built-in NFI source
#' (about 3 minutes, once).
#'
#' @param launch.browser Passed to [shiny::runApp()].
#' @param ... Further arguments of [shiny::runApp()], for example `port`.
#' @return Does not return while the app runs.
#' @examples
#' \dontrun{
#' optBuck::run_app()
#' }
#' @export
run_app <- function(launch.browser = TRUE, ...) {
  need <- c("shiny", "bslib", "reactable", "rhandsontable", "ggplot2", "promises", "later", "htmltools", "jsonlite")
  missing <- need[!vapply(need, requireNamespace, NA, quietly = TRUE)]
  if (length(missing))
    stop("run_app() needs these packages: ", paste(missing, collapse = ", "), ". Install them with install.packages().", call. = FALSE)
  shiny::runApp(app_dir(), launch.browser = launch.browser, ...)
}

#' Read the instruction of an .hpr file
#'
#' Species groups, products, price matrices, permitted grades and length distributions of a StanForD
#' 2010 harvester production file. The stems, coordinates, dates and names of the file are not read.
#'
#' @name read_hpr
#' @param path An .hpr file, a gzipped .hpr, or a .zip with an .hpr.
#' @param filename Name used in messages (its extension decides how a .zip is read).
#' @usage read_hpr(path, filename = basename(path))
#' @return A list with `ok`, `errors`, `warnings`, `species`, `products`, `unclassified_keys`, `grade_names`.
#' @examples
#' f <- system.file("extdata", "inputs", "test_fixtures", "hpr_example_synthetic.hpr", package = "optBuck")
#' h <- read_hpr(f)
#' h$ok
#' h$species
#' @export
NULL

#' Read the measured stems of an .hpr file
#'
#' Streams the file in chunks and returns the measured stem profiles (positions and diameters from the
#' felling cut), never the stems' coordinates, dates or names.
#'
#' @name read_hpr_stems
#' @param path,filename As for [read_hpr()].
#' @param defs The result of [read_hpr()], if already read.
#' @param chunk_bytes Bytes per chunk (the result does not depend on it).
#' @usage read_hpr_stems(path, filename = basename(path), defs = NULL, chunk_bytes = HPR_STEM_CHUNK_BYTES)
#' @return A list with `ok`, `stems` (one row per stem), `species` and the points of the profiles.
#' @examples
#' f <- system.file("extdata", "inputs", "test_fixtures", "hpr_check_synthetic.hpr", package = "optBuck")
#' st <- read_hpr_stems(f)
#' st$ok
#' nrow(st$stems)
#' @export
NULL

#' Read the recorded logs of an .hpr file
#'
#' One row per recorded log: stem, product, start position (recorded, else the sum of the previous
#' lengths), length, cutting reason, volumes and diameters; recorded waste is the file's unclassified
#' product; a cut is manual unless its reason is "Automatic" or "Spinning".
#'
#' @name read_hpr_logs
#' @param path,filename,defs,chunk_bytes As for [read_hpr_stems()].
#' @usage read_hpr_logs(path, filename = basename(path), defs = NULL, chunk_bytes = HPR_STEM_CHUNK_BYTES)
#' @return A list with `ok` and `logs` (a data.frame).
#' @examples
#' f <- system.file("extdata", "inputs", "test_fixtures", "hpr_check_synthetic.hpr", package = "optBuck")
#' lg <- read_hpr_logs(f)
#' table(lg$logs$manual)
#' @export
NULL

#' Permitted quality grades of each product of an .hpr file
#'
#' @name hpr_permitted_grades
#' @param hpr The result of [read_hpr()].
#' @usage hpr_permitted_grades(hpr)
#' @return A data.frame with one row per product, species group and permitted grade (grade `NA`: all grades).
#' @examples
#' f <- system.file("extdata", "inputs", "test_fixtures", "hpr_example_synthetic.hpr", package = "optBuck")
#' head(hpr_permitted_grades(read_hpr(f)))
#' @export
NULL

#' The instruction of an .hpr file as a scenario
#'
#' Turns the products and prices of a harvester file into an assortment table and a price matrix, the
#' form the simulator and [write_pin()] use.
#'
#' @param path An .hpr or .pin file.
#' @param species_map Named character: species group key -> "spruce", "pine" or "none"; default: guessed
#'   from the group names.
#' @param manual_ok Also price the cells marked "Only logs bucked manually allowed".
#' @return A list with `ok`, `defs` (assortments), `pm` (prices), `species_map`, `warnings`.
#' @examples
#' f <- system.file("extdata", "inputs", "test_fixtures", "hpr_example_synthetic.hpr", package = "optBuck")
#' sc <- hpr_scenario(f)
#' sc$ok
#' sc$defs$Name
#' @export
hpr_scenario <- function(path, species_map = NULL, manual_ok = FALSE) {
  h <- read_product_file(path, basename(path))
  if (!isTRUE(h$ok)) return(h)
  smap <- if (is.null(species_map)) .species_map_of(h) else species_map
  sc <- hpr_to_scenario(h, smap, manual_ok = manual_ok)
  if (isTRUE(sc$ok)) sc$species_map <- smap
  sc
}

#' Buck the measured stems of an .hpr file
#'
#' Finds the best cutting pattern of every measured stem under the file's own products and prices.
#'
#' @param path An .hpr file (gzipped or zipped also).
#' @param species_map Named character: species group key -> "spruce", "pine" or "none"; default: guessed.
#' @param workers Number of workers (default: all but one core; use 1 for none).
#' @return The result list of the backend: `selected_logs_all`, `tree_summary`, `product_summary`,
#'   `per_m3_summary` and more.
#' @examples
#' if (requireNamespace("taperNOR", quietly = TRUE)) {
#'   f <- system.file("extdata", "inputs", "test_fixtures", "hpr_check_synthetic.hpr", package = "optBuck")
#'   r <- buck_hpr(f, workers = 1)
#'   head(r$tree_summary)
#' }
#' @export
buck_hpr <- function(path, species_map = NULL, workers = NULL) {
  if (!is.null(workers)) { old <- options(apter.workers = as.integer(workers)); on.exit(options(old), add = TRUE) }
  h <- read_hpr(path); if (!isTRUE(h$ok)) stop(paste(h$errors, collapse = " "), call. = FALSE)
  smap <- if (is.null(species_map)) .species_map_of(h) else species_map
  st <- read_hpr_stems(path, defs = h); if (!isTRUE(st$ok)) stop(paste(st$errors, collapse = " "), call. = FALSE)
  sc <- hpr_to_scenario(h, smap); if (!isTRUE(sc$ok)) stop(paste(sc$errors, collapse = " "), call. = FALSE)
  pm <- pm_minimal(sc$pm); pr <- products_from_defs(sc$defs, pm)
  pp <- tempfile(fileext = ".csv"); on.exit(unlink(pp), add = TRUE); pm_export(pm, pp)
  job <- prepare_hpr_stems_job(st, smap, price_matrix_path = pp, products = pr$pd, length_classes = pr$lc)
  finish_job(job, buck_job(job))
}

#' Buck trees from their diameter and height
#'
#' Stem profiles come from taperNOR (spruce or pine, Norwegian taper model); the best cutting pattern is
#' found under the default products and the 2021 price matrices.
#'
#' @param dbh_cm Diameter at breast height over bark (cm), one value per tree.
#' @param height_m Total height (m).
#' @param species "spruce" or "pine" (recycled).
#' @param workers Number of workers (default 1).
#' @return The result list of the backend (`selected_logs_all`, `tree_summary`, ...).
#' @examples
#' if (requireNamespace("taperNOR", quietly = TRUE)) {
#'   r <- buck_stem(dbh_cm = c(28, 34), height_m = c(22, 25), species = "spruce")
#'   r$selected_logs_all[, c("treid", "ProductKey", "LogLength", "value_nok")]
#' }
#' @export
buck_stem <- function(dbh_cm, height_m, species = "spruce", workers = 1L) {
  n <- length(dbh_cm)
  df <- data.frame(tree_id = as.character(seq_len(n)), species = rep_len(species, n), dbh = as.character(dbh_cm),
                   height = as.character(rep_len(height_m, n)), stringsAsFactors = FALSE)
  prep <- prepare_tree_list(df)
  if (!isTRUE(prep$ok)) stop(paste(prep$errors, collapse = " "), call. = FALSE)
  run_tree_list_bucking(prep, workers = workers)
}

#' Compare the harvester's bucking with the best possible bucking
#'
#' For each stem of an .hpr file the recorded logs are valued like candidates of the optimiser (same
#' price volume, class and price) and compared with the best possible bucking under the file's own
#' instruction: only the products with at least one recorded log are used, recorded waste logs are
#' defect zones, cells marked "Only logs bucked manually allowed" may be used with the file's price,
#' and stems whose recorded logs cover less than half of the measured stem are left out. The bucking
#' yield index (apteringsgrad) is the recorded value divided by the best possible value.
#'
#' @name bucking_check
#' @param path An .hpr file.
#' @param species_map Named character: species group key -> "spruce", "pine" or "none"; default: guessed.
#' @param filename Name for messages.
#' @param reject_zones Treat recorded waste logs as defect zones.
#' @param used_only Use only the products the harvester cut a log of.
#' @param manual_ok Let the best bucking use cells marked "Only logs bucked manually allowed".
#' @param progress `NULL` or a function called with "read", "opt", "val" and "tab".
#' @param root Package root (default: found automatically).
#' @param stage,prep,b Internal: the three stages of the app's background job.
#' @usage bucking_check(path, species_map = NULL, filename = basename(path), reject_zones = TRUE,
#'   used_only = TRUE, manual_ok = TRUE, progress = NULL, root = get_root_dir(),
#'   stage = "all", prep = NULL, b = NULL)
#' @return A list with `key_figures` (yield index per species), `stems`, `logs`, `assortments`,
#'   `manual`, `defect`, `top`, `length_dist`, `control` and more; `ok = FALSE` and `errors` when the
#'   file cannot be checked.
#' @examples
#' if (requireNamespace("taperNOR", quietly = TRUE)) {
#'   f <- system.file("extdata", "inputs", "test_fixtures", "hpr_check_synthetic.hpr", package = "optBuck")
#'   x <- bucking_check(f)
#'   x$key_figures[, c("scope", "stems", "value_rec", "value_opt", "yield")]
#' }
#' @export
NULL

#' Write a scenario as a StanForD 2010 product instruction (.pin)
#'
#' @param scenario The result of [hpr_scenario()] (a list with `defs` and `pm`).
#' @param path Output file.
#' @return A list with `ok` and `path` (or `errors`).
#' @examples
#' f <- system.file("extdata", "inputs", "test_fixtures", "hpr_example_synthetic.hpr", package = "optBuck")
#' out <- write_pin(hpr_scenario(f), tempfile(fileext = ".pin"))
#' out$ok
#' @export
write_pin <- function(scenario, path) pin_export_scenario(scenario$defs, scenario$pm, path)

#' Write the result of [buck_hpr()] as a StanForD 2010 .hpr
#'
#' The file is a copy of the source with the simulated logs instead of the recorded ones; stem
#' coordinates are never written.
#'
#' @param result The result of [buck_hpr()].
#' @param source The .hpr file that was bucked.
#' @param path Output file.
#' @param scenario The scenario the result was made with (default: [hpr_scenario()] of `source`).
#' @return A list with `ok`, `path`, `n_stems`, `n_logs`.
#' @examples
#' if (requireNamespace("taperNOR", quietly = TRUE)) {
#'   f <- system.file("extdata", "inputs", "test_fixtures", "hpr_check_synthetic.hpr", package = "optBuck")
#'   r <- buck_hpr(f, workers = 1)
#'   write_hpr(r, f, tempfile(fileext = ".hpr"))$ok
#' }
#' @export
write_hpr <- function(result, source, path, scenario = NULL) {
  h <- read_hpr(source); smap <- .species_map_of(h)
  if (is.null(scenario)) scenario <- hpr_scenario(source)
  hpr_export_result(path, data.frame(path = source, name = basename(source), stringsAsFactors = FALSE),
                    setNames(unname(smap), tolower(trimws(h$species$name))), result, scenario$defs, scenario$pm)
}

#' Build the NFI stem profile cache
#'
#' The built-in NFI source needs taperNOR profiles of 101,156 trees. They are built on first use; this
#' builds them in advance (about 3 minutes on one core, about 200 MB of parquet in
#' `tools::R_user_dir("optBuck", "cache")`, or the folder in `options(optBuck.cache_dir)`).
#'
#' @name build_nfi_cache
#' @param root Package root (default: found automatically).
#' @param out_dir Output folder.
#' @param plots `NULL` (all plots) or plot IDs.
#' @param progress `NULL` or a function(done, total).
#' @param chunk Trees per chunk.
#' @param workers Number of workers (1: sequential).
#' @usage build_nfi_cache(root = get_root_dir(), out_dir = nfi_cache_dir(root), plots = NULL,
#'   progress = NULL, chunk = 5000L, workers = 1L)
#' @return Invisibly, the number of profile rows written.
#' @examples
#' \dontrun{
#' build_nfi_cache(progress = function(done, total) message(done, " of ", total, " trees"))
#' }
#' @export
NULL

# Functions of the base packages used without a prefix, and column names used in dplyr/arrow expressions.
#' @importFrom stats setNames aggregate predict ave median quantile
#' @importFrom utils head tail
#' @noRd
NULL
utils::globalVariables(c("plot_block", "plot_id_github", "csv_lines"))
