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

test_that("a header with windows-1252 bytes in a file declared UTF-8 is read", {
  f <- fixture("hpr_cp1252_synthetic.hpr")
  expect_false(validUTF8(rawToChar(readBin(f, "raw", 3000L))))
  a <- read_hpr(f); b <- read_hpr(fixture("hpr_example_synthetic.hpr"))
  expect_true(a$ok)
  expect_equal(a$species, b$species)
  expect_equal(names(a$products), names(b$products))
  expect_equal(nrow(read_hpr_stems(f)$stems), nrow(read_hpr_stems(fixture("hpr_example_synthetic.hpr"))$stems))
})

test_that("the literal text <U+FEFF> before the XML declaration is ignored", {
  f <- fixture("hpr_marker_synthetic.hpr")
  expect_true(startsWith(rawToChar(readBin(f, "raw", 20L)), "<U+FEFF>"))
  a <- read_hpr(f); b <- read_hpr(fixture("hpr_example_synthetic.hpr"))
  expect_true(a$ok)
  expect_equal(a$species, b$species)
  expect_equal(names(a$products), names(b$products))
  expect_equal(read_product_file(f)$ok, TRUE)
})

test_that("valid UTF-8 and real byte order marks are left alone", {
  x <- charToRaw("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<a>\u00f8</a>\n")
  expect_identical(.hpr_clean_bytes(x), x)
  bom <- c(as.raw(c(0xEF, 0xBB, 0xBF)), x)
  expect_identical(.hpr_clean_bytes(bom), bom)
  bad <- c(charToRaw("<a>Bj"), as.raw(0xF8), charToRaw("rn</a>"))
  expect_identical(rawToChar(.hpr_clean_bytes(bad)), "<a>Bj\u00f8rn</a>")
})
