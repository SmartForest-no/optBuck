# Tuning needs one price per (file diameter class, length class). A price edited on one
# whole-cm row inside a class must be caught before tuning starts (0.3.1: backend error
# "value check failed on iteration 1").
test_that("tuning_sawlog_status flags prices that vary inside a diameter class", {
  f <- system.file("extdata", "example.hpr", package = "optBuck")
  skip_if(!nzchar(f))
  hp <- read_hpr(f); expect_true(isTRUE(hp$ok))
  smap <- setNames(guess_hpr_species(hp$species$name, hp$species$user_id), hp$species$key)
  sc <- hpr_to_scenario(hp, smap); expect_true(isTRUE(sc$ok))
  st <- tuning_sawlog_status(sc$defs, sc$pm)
  ok <- st$product_id[st$reason == "ok"]
  skip_if(length(ok) == 0L, "no tunable product in the example")
  key <- ok[1L]
  expect_true(tuning_prices_uniform(sc$defs, sc$pm, key))
  # a class that spans several whole-cm rows: change the price of one row, one length
  rows <- which(sc$pm$ProductKey == key & !is.na(sc$pm$PriceNOKperM3))
  fc <- sc$pm[[PM_FILE_CLASS_COL]][rows]
  grp <- paste(sc$pm$LengthClass_dm[rows], fc)
  multi <- rows[grp %in% names(which(table(grp) > 1L)) & !is.na(fc)]
  skip_if(length(multi) == 0L, "no file class spans several whole-cm rows")
  pm2 <- sc$pm; pm2$PriceNOKperM3[multi[1L]] <- pm2$PriceNOKperM3[multi[1L]] + 25
  expect_false(tuning_prices_uniform(sc$defs, pm2, key))
  st2 <- tuning_sawlog_status(sc$defs, pm2)
  expect_equal(st2$reason[st2$product_id == key], "mixed")
  expect_equal(tr("tn_reason_mixed", lang = "no"),
               "Prisene varierer innenfor en diameterklasse. Pristilpasning krever én pris per diameterklasse og lengde.")
})

# The assortments a scenario takes over from the .hpr of tab 1 (0.3.2): the file's default selection
# limited to the products the harvester used (>= 1 % of a species' logs).
test_that("hpr_default_keep limits the default selection to the used products", {
  f <- fixture("hpr_check_synthetic.hpr"); skip_if(!nzchar(f))
  hp <- read_hpr(f); expect_true(isTRUE(hp$ok))
  map <- setNames(guess_hpr_species(hp$species$name, hp$species$user_id), hp$species$key)
  all_default <- as.character(hpr_product_table(hp, map)$key[hpr_product_table(hp, map)$default])
  k <- hpr_default_keep(f, basename(f), hp, map)
  expect_true(k$used)
  expect_true(all(k$keep %in% all_default))
  expect_lt(length(k$keep), length(all_default))
  # a file without recorded logs keeps the default selection
  g <- fixture("hpr_example_synthetic.hpr")
  hg <- read_hpr(g); mg <- setNames(guess_hpr_species(hg$species$name, hg$species$user_id), hg$species$key)
  kg <- hpr_default_keep(g, basename(g), hg, mg)
  expect_false(kg$used)
  expect_equal(length(kg$keep), sum(hpr_product_table(hg, mg)$default))
})

# Matrix views of tab 4 (2026-11-11): the original and the tuned prices in NOK/m3, diameter classes that were not
# tuned are flagged (greyed in the app).
test_that("tuning_matrix_view has price views and flags the classes that were not tuned", {
  mk <- function(v) matrix(v, 2, 2)               # rows = lengths, columns = diameter classes
  by <- list(grid = list(lengths = c(37, 43), diam_classes = c(100, 150), price = mk(c(500, 520, 600, NA))),
             W = mk(c(50, 50, 100, NA)), P1 = mk(c(500, 520, 600, NA)), Pb = mk(c(510, 530, 600, NA)),
             S_orig = mk(c(40, 60, 100, NA)), S_tuned = mk(c(50, 50, 100, NA)),
             eligible = c("100" = TRUE, "150" = FALSE))
  po <- tuning_matrix_view(by, "price_orig"); pt <- tuning_matrix_view(by, "price_tuned")
  expect_equal(unname(po$m[1, ]), c(500, 520)); expect_equal(unname(pt$m[1, ]), c(510, 530))
  expect_true(is.na(pt$m[2, 2]))                  # a cell without a price stays empty
  expect_equal(po$untuned, c(FALSE, TRUE))
  expect_equal(tuning_matrix_view(by, "target")$untuned, c(FALSE, TRUE))
})
