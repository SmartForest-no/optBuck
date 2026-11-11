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
