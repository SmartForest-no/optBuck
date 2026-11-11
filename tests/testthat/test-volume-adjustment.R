# Price volume diameter rounding follows each product's VolumeDiameterAdjustment (2026-11-11).
test_that("the file's VolumeDiameterAdjustment sets the rounding of each product", {
  v_mm <- price_volume_m3("Top cylinder", 187.6, 400, adjustment = "None")
  v_cm <- price_volume_m3("Top cylinder", 187.6, 400, adjustment = VOLUME_ROUND_DOWN_CM)
  expect_gt(v_mm, v_cm)
  expect_equal(v_mm, round(pi * (187.6 / 2)^2 * 400 / 1e8, 5))
  # Norwegian mid always floors, whatever the file says; solid volume never rounds
  expect_equal(volume_adjustment_of(c("Top cylinder", "All diameters (solid volume)"), c("None", VOLUME_ROUND_DOWN_CM)),
               c("None", "None"))
  expect_equal(volume_adjustment_of("Top", NA), VOLUME_ROUND_DOWN_CM)   # no stored value: the method's default
  f <- system.file("extdata", "example.hpr", package = "optBuck"); skip_if(!nzchar(f))
  hp <- read_hpr(f)
  smap <- setNames(guess_hpr_species(hp$species$name, hp$species$user_id), hp$species$key)
  sc <- hpr_to_scenario(hp, smap)
  adj <- products_from_defs(sc$defs, sc$pm)$pd$VolumeDiameterAdjustment
  expect_true(any(adj == "None") && any(adj == VOLUME_ROUND_DOWN_CM))
  # the hidden field survives an xlsx round trip
  xf <- tempfile(fileext = ".xlsx"); scenario_export(sc$defs, sc$pm, xf)
  back <- scenario_import(xf); expect_true(isTRUE(back$ok))
  expect_equal(products_from_defs(back$defs, back$pm)$pd$VolumeDiameterAdjustment, adj)
})
