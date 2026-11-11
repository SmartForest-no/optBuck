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
