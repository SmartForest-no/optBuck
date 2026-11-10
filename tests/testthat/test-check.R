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

test_that("a product counts as used with at least 1 % of a species' recorded logs", {
  mk <- function(n, key, sp = "1") data.frame(waste = FALSE, ProductKey = rep(key, n), species_key = sp, stringsAsFactors = FALSE)
  lg <- rbind(mk(990, "A"), mk(10, "B"), mk(9, "C"), mk(1, "D"),     # spruce: 1000 + 10 + ... logs
              mk(50, "A", "2"), mk(3, "E", "2"),                       # pine: E is 3/53 = 5.7 %
              data.frame(waste = TRUE, ProductKey = "W", species_key = "1", stringsAsFactors = FALSE))
  smap <- c("1" = "spruce", "2" = "pine")
  # spruce has 1010 logs: B has 10 (0.99 %), C 9, D 1 -> only A is used; pine: A and E
  expect_setequal(check_used_products(lg, smap), c("A", "E"))
  lg2 <- rbind(lg, mk(2, "B"))                                          # B: 12 of 1012 = 1.19 %
  expect_true("B" %in% check_used_products(lg2, smap))
  expect_equal(CHECK_USED_MIN_SHARE, 0.01)
  expect_length(check_used_products(lg[0, ], smap), 0L)
})
