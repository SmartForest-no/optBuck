test_that("the instruction of an .hpr file is read", {
  h <- read_hpr(fixture("hpr_example_synthetic.hpr"))
  expect_true(h$ok)
  expect_equal(nrow(h$species), 4L)
  expect_equal(h$unclassified_keys, "999999")
  expect_true(all(c("8019", "8015") %in% names(h$products)))
})

test_that("permitted grades are listed per product", {
  g <- hpr_permitted_grades(read_hpr(fixture("hpr_example_synthetic.hpr")))
  expect_true(all(c("ProductKey", "grade") %in% names(g)))
  expect_setequal(g$grade[g$ProductKey == "8019"], c(1, 2))
})

test_that("recorded logs are read with recorded or cumulative start positions", {
  lg <- read_hpr_logs(fixture("hpr_check_synthetic.hpr"))
  expect_true(lg$ok)
  d <- lg$logs
  expect_equal(nrow(d), 16L)
  expect_equal(length(unique(d$stem_key)), 9L)
  expect_equal(sum(d$waste), 5L)                       # the unclassified product
  expect_equal(sum(d$manual), 8L)                      # not "Automatic" or "Spinning"
  s3 <- d[d$stem_key == "3", ]
  expect_equal(s3$StartPos, c(0, 490))                 # not recorded: cumulative lengths
  expect_false(any(s3$start_recorded))
})

test_that("stems are read without coordinates, dates or names", {
  st <- read_hpr_stems(fixture("hpr_check_synthetic.hpr"))
  expect_true(st$ok)
  expect_false(any(c("latitude", "longitude", "name") %in% tolower(names(st$stems))))
})
