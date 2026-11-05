test_that("a harvester file becomes a scenario and a product instruction", {
  sc <- hpr_scenario(fixture("hpr_example_synthetic.hpr"))
  expect_true(sc$ok)
  expect_true(any(grepl("^140 SCA Sagt.mmer$", sc$defs$Name)))
  out <- write_pin(sc, tempfile(fileext = ".pin"))
  expect_true(out$ok)
  expect_true(file.exists(out$path))
})

test_that("measured stems are bucked and written back as .hpr", {
  skip_if_not_installed("taperNOR")
  f <- fixture("hpr_check_synthetic.hpr")
  r <- buck_hpr(f, workers = 1)
  expect_gt(nrow(r$selected_logs_all), 0L)
  expect_true(all(r$selected_logs_all$value_nok >= 0))
  out <- tempfile(fileext = ".hpr")
  w <- write_hpr(r, f, out)
  expect_true(w$ok)
  txt <- paste(readLines(out, warn = FALSE), collapse = "")
  expect_false(grepl("StemCoordinates|Latitude", txt))
})

test_that("texts exist in both languages", {
  expect_equal(tr("tab_check", lang = "no"), "5 Apteringsanalyse")
  expect_equal(tr("tab_check", lang = "en"), "5 Bucking analysis")
  tb <- i18n_table()
  expect_false(any(!nzchar(tb$en)))
  expect_false(any(!nzchar(tb$no)))
})
