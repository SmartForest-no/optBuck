test_that("the bucking check compares recorded and best possible bucking", {
  skip_if_not_installed("taperNOR")
  x <- bucking_check(fixture("hpr_check_synthetic.hpr"))
  expect_true(x$ok)
  kf <- x$key_figures
  expect_equal(kf$stems[1], 7)
  expect_equal(kf$yield[1], 0.857043065232, tolerance = 1e-9)
  # stems 1 and 6 are recorded as the best bucking
  s <- x$stems
  expect_equal(s$yield[s$StemKey %in% c("1", "6")], c(1, 1))
  # stem 9 is incomplete (one waste log on 8 m of profile), stem 7 is multi-tree
  expect_true(all(c("7", "9") %in% x$excluded$treid))
  expect_equal(x$n_incomplete, 1L)
  # a possible broken top is flagged, not excluded
  expect_true(s$possible_broken_top[s$StemKey == "5"] && s$use[s$StemKey == "5"])
})

test_that("only the products the harvester used are in the best possible bucking", {
  skip_if_not_installed("taperNOR")
  used <- bucking_check(fixture("hpr_check_synthetic.hpr"))
  all_p <- bucking_check(fixture("hpr_check_synthetic.hpr"), used_only = FALSE)
  expect_lt(used$key_figures$value_opt[1], all_p$key_figures$value_opt[1])
  expect_lt(used$n_products, all_p$n_products)
})
