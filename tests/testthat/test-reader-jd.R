# John Deere files of the archive test (2026-11-12): the header repair must not push the reader
# onto the slow whole-file path, definitions-only files get one clear message, and a failed
# check always carries its reason. Synthetic fixtures only.

test_that("a file with the literal <U+FEFF> marker is read from its prefix, not parsed whole", {
  a <- read_hpr(fixture("hpr_marker_synthetic.hpr"))
  expect_true(a$ok)
  expect_equal(a$header$read, "prefix")     # the marker used to count as an open element "U"
})

test_that("closing the open tags ignores nothing that the cleaning removes", {
  x <- c(charToRaw("<U+FEFF><?xml version=\"1.0\"?>\n<a><b>"))
  closed <- .hpr_close_tags(.hpr_clean_bytes(x))
  expect_equal(rawToChar(closed), "<?xml version=\"1.0\"?>\n<a><b></b></a>")
  expect_silent(xml2::read_xml(closed))
})

test_that("the default namespace is dropped from the bytes, other attributes are kept", {
  x <- charToRaw("<?xml version=\"1.0\"?>\n<Root xmlns=\"urn:x:y\" xmlns:xsi=\"http://w3/xsi\" a=\"1\"><c/></Root>")
  y <- .hpr_drop_default_ns(x)
  expect_equal(rawToChar(y), "<?xml version=\"1.0\"?>\n<Root xmlns:xsi=\"http://w3/xsi\" a=\"1\"><c/></Root>")
  doc <- xml2::read_xml(y)
  expect_false(any(names(xml2::xml_ns(doc)) == "d1"))
  expect_equal(length(xml2::xml_find_all(doc, "/Root/c")), 1L)
  expect_identical(.hpr_drop_default_ns(raw(0)), raw(0))
  no_ns <- charToRaw("<Root a=\"1\"/>")
  expect_identical(.hpr_drop_default_ns(no_ns), no_ns)
})

test_that("the readers still find the definitions when the file has a namespace", {
  h <- read_hpr(fixture("hpr_example_synthetic.hpr"))
  expect_true(h$ok)
  expect_gt(nrow(h$species), 0L)
  expect_gt(length(h$products), 0L)
})

definitions_only <- function() {
  pre <- .hpr_prefix(fixture("hpr_example_synthetic.hpr"))
  expect_true(pre$found)
  f <- tempfile(fileext = ".hpr")
  writeBin(.hpr_close_tags(pre$bytes), f)
  f
}

test_that("a file with definitions but no stems says so", {
  f <- definitions_only(); on.exit(unlink(f))
  h <- read_hpr(f, "defs.hpr"); expect_true(h$ok)
  st <- read_hpr_stems(f, "defs.hpr", defs = h)
  expect_false(st$ok)
  expect_equal(st$errors, "defs.hpr contains no stems (definitions only).")
  expect_equal(hpr_file_failure_reason(st$errors, "defs.hpr"), "no stems")
  tr <- tr_msg(st$errors, "no")
  expect_equal(tr, "defs.hpr inneholder ingen stammer (bare definisjoner).")
})

test_that("the bucking check of a definitions-only file reports that, not a product error", {
  f <- definitions_only(); on.exit(unlink(f))
  r <- bucking_check(f, filename = "defs.hpr")
  expect_false(r$ok)
  expect_equal(r$errors, "defs.hpr contains no stems (definitions only).")
})

test_that("a failure inside the bucking check comes back as ok = FALSE with a message", {
  testthat::local_mocked_bindings(read_hpr_logs = function(...) stop("boom"), .package = "optBuck")
  r <- bucking_check(fixture("hpr_check_synthetic.hpr"), filename = "x.hpr")
  expect_false(r$ok)
  expect_match(r$errors, "x.hpr could not be checked: boom", fixed = TRUE)
})

test_that("stems that all fail the cleaning give the reason, not a bare validation error", {
  testthat::local_mocked_bindings(
    hpr_stem_checks = function(st, species_map) list(ok = FALSE, errors = "No stems are left to buck: reason.",
                                                     warnings = "1 stem dropped: profile falls too steeply."),
    .package = "optBuck")
  r <- bucking_check(fixture("hpr_check_synthetic.hpr"), filename = "x.hpr")
  expect_false(r$ok)
  expect_equal(r$errors, "No stems are left to buck: reason.")
  expect_match(paste(r$warnings, collapse = " "), "dropped")
  expect_error(prepare_hpr_job(list(ok = FALSE, errors = "No stems are left to buck: reason.")),
               "did not pass validation. No stems are left to buck: reason.", fixed = TRUE)
})
