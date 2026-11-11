# A flat price on a matrix assortment (2026-11-11) puts one price in every priced cell: lengths,
# diameter classes, empty cells and the demand matrix stay, so a bucking differs only by the price.
test_that("pm_fill_price keeps the product structure and a run changes only through the price", {
  f <- system.file("extdata", "example.hpr", package = "optBuck"); skip_if(!nzchar(f))
  hp <- read_hpr(f); expect_true(isTRUE(hp$ok))
  smap <- setNames(guess_hpr_species(hp$species$name, hp$species$user_id), hp$species$key)
  sc <- hpr_to_scenario(hp, smap); expect_true(isTRUE(sc$ok))
  d <- sc$defs; a <- pm_minimal(sc$pm)
  k <- assortment_table_keys(d)[d$Type == "sawlog"][1L]
  b <- pm_fill_price(a, k, 450)
  # everything but the price of that product is identical (cells, hidden columns, other products)
  keep <- setdiff(names(a), "PriceNOKperM3")
  expect_identical(a[keep], b[keep])
  expect_identical(a$PriceNOKperM3[a$ProductKey != k], b$PriceNOKperM3[b$ProductKey != k])
  expect_true(all(b$PriceNOKperM3[b$ProductKey == k & !is.na(b$PriceNOKperM3)] == 450))
  expect_gt(length(unique(a$PriceNOKperM3[a$ProductKey == k])), 1L)   # A is not flat
  pa <- products_from_defs(d, a); pb <- products_from_defs(d, b)
  expect_identical(pa$pd, pb$pd); expect_identical(pa$lc, pb$lc)      # same lengths and rules
  # a run: B's logs of the product use 450 and only cells A prices
  wp <- function(pm) { p <- tempfile(fileext = ".csv"); pm_export(pm_prices(pm), p); p }
  st <- read_hpr_stems(f)
  r <- run_hpr_bucking(hpr_stem_profiles(st, smap), price_matrix_path = wp(a), products = pa$pd,
                       length_classes = pa$lc, workers = 1L,
                       scenario = list(price_matrix_path = wp(b), products = pb$pd, length_classes = pb$lc))
  la <- r$selected_logs_all; lb <- r$scenario$selected_logs_all
  lk <- lb[lb$ProductKey == k, ]; expect_gt(nrow(lk), 0L)
  expect_true(all(lk$PriceNOKperM3 == 450))
  cells <- a[a$ProductKey == k & !is.na(a$PriceNOKperM3), ]
  expect_true(all(paste(lk$LengthClass_dm, lk$PriceDiameterClass_cm_used) %in%
                    paste(cells$LengthClass_dm, cells$PriceDiameterClass_cm)))
  expect_equal(sum(lb$value_nok), sum(lb$price_volume_m3 * lb$PriceNOKperM3))
})

# Products without a demand matrix (2026-11-11): Distribution 0 and DistributionAllowed false in the
# exports (never allowed with an all-zero matrix); existing demand matrices survive xlsx and .pin.
test_that("exports never allow a distribution without a demand matrix; matrices round-trip", {
  f <- system.file("extdata", "example.hpr", package = "optBuck"); skip_if(!nzchar(f))
  hp <- read_hpr(f)
  smap <- setNames(guess_hpr_species(hp$species$name, hp$species$user_id), hp$species$key)
  sc <- hpr_to_scenario(hp, smap); d <- sc$defs; pm <- pm_minimal(sc$pm)
  keys <- assortment_table_keys(d)
  has <- vapply(keys, function(k) dist_has_target(pm, k), NA)
  skip_if(!any(has), "no demand matrix in the example")
  k_has <- keys[has & d$Type == "sawlog"][1L]; k_has <- k_has[!is.na(k_has)]
  k_no <- keys[!has & d$Type != "waste"][1L]
  skip_if(is.na(k_no) || length(k_has) == 0L)
  d$DistributionAllowed[keys == k_no] <- TRUE            # allowed, but no matrix
  pm[[PM_DISTRIBUTION_COL]][pm$ProductKey == k_no] <- 0
  p <- tempfile(fileext = ".pin"); x <- pin_export_scenario(d, pm, p); expect_true(x$ok)
  expect_length(x$warnings, 1L)
  xml <- xml2::read_xml(p); xml2::xml_ns_strip(xml)
  prods <- xml2::xml_find_all(xml, "//ClassifiedProductDefinition")
  nm <- xml2::xml_text(xml2::xml_find_first(prods, ".//ProductName"))
  for (i in seq_along(prods)) {
    allowed <- xml2::xml_text(xml2::xml_find_first(prods[[i]], ".//DistributionAllowed"))
    dist <- as.numeric(xml2::xml_text(xml2::xml_find_all(prods[[i]], ".//ProductMatrixItem/Distribution")))
    if (!any(dist > 0)) expect_equal(allowed, "false", info = nm[i])
    else expect_equal(allowed, "true", info = nm[i])
  }
  expect_true(any(vapply(prods, function(p) any(as.numeric(xml2::xml_text(xml2::xml_find_all(p, ".//ProductMatrixItem/Distribution"))) > 0), NA)))
  # xlsx: a sheet only for products with a matrix, and the matrix comes back
  xf <- tempfile(fileext = ".xlsx"); scenario_export(d, pm, xf)
  sh <- readxl::excel_sheets(xf); expect_equal(sum(grepl("^target_", sh)), sum(has & TRUE) - sum(has & keys == k_no))
  back <- scenario_import(xf); expect_true(isTRUE(back$ok))
  pb <- pm_minimal(back$pm)
  expect_true(dist_has_target(pb, k_has)); expect_false(dist_has_target(pb, k_no))
  expect_equal(sort(dist_grid(pb, k_has)[!is.na(dist_grid(pb, k_has))]),
               sort(dist_grid(pm, k_has)[!is.na(dist_grid(pm, k_has))]))
})

# Keys transliterate ae/o/a instead of dropping the letters (2026-11-11); old scenario files still load.
test_that("assortment keys transliterate and old keys are still found in a scenario file", {
  expect_equal(assortment_slug("140 Sagtømmer gran"), "140_sagtommer_gran")
  expect_equal(assortment_slug("Ære år Ø"), "aere_ar_o")
  expect_equal(assortment_slug("Sawlog"), "sawlog")
  f <- system.file("extdata", "example.hpr", package = "optBuck"); skip_if(!nzchar(f))
  hp <- read_hpr(f); smap <- setNames(guess_hpr_species(hp$species$name, hp$species$user_id), hp$species$key)
  sc <- hpr_to_scenario(hp, smap)
  al <- key_aliases(sc$defs)
  expect_true("spruce_140_sagt_mmer_gran" %in% names(al))
  expect_equal(unname(al["spruce_140_sagt_mmer_gran"]), "spruce_140_sagtommer_gran")
  # a file written with the old keys (sheet names, corner cells, flat prices) imports unchanged
  xf <- tempfile(fileext = ".xlsx"); scenario_export(sc$defs, sc$pm, xf)
  back_key <- function(v) { j <- match(v, unname(al)); v[!is.na(j)] <- names(al)[j[!is.na(j)]]; v }
  old <- lapply(setNames(readxl::excel_sheets(xf), readxl::excel_sheets(xf)), function(s) {
    d <- as.data.frame(readxl::read_excel(xf, sheet = s, col_types = "text", .name_repair = "minimal"))
    names(d) <- back_key(names(d)); d[] <- lapply(d, back_key); d })
  names(old) <- back_key(names(old))
  of <- tempfile(fileext = ".xlsx"); writexl::write_xlsx(old, of)
  expect_true(any(grepl("sagt_mmer", readxl::excel_sheets(of))))
  back <- scenario_import(of)
  expect_true(isTRUE(back$ok), info = paste(back$errors, collapse = " | "))
  p0 <- pm_prices(sc$pm); p1 <- pm_prices(back$pm)
  expect_equal(nrow(p0), nrow(p1)); expect_equal(sort(unique(p0$ProductKey)), sort(unique(p1$ProductKey)))
  expect_equal(sum(p0$PriceNOKperM3, na.rm = TRUE), sum(p1$PriceNOKperM3, na.rm = TRUE))
})
