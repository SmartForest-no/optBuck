test_that("a file with logs but no StemDiameters gets estimated, marked stem profiles", {
  skip_if_not_installed("taperNOR")
  a <- read_hpr_stems(fixture("hpr_check_synthetic.hpr"))
  b <- read_hpr_stems(fixture("hpr_logsonly_synthetic.hpr"))
  expect_true(b$ok)
  expect_true(all(a$stems$n_points > 0L) && !any(a$stems$estimated))        # measured profiles are never marked
  expect_equal(sum(b$stems$estimated), 8L)                                    # the multi-tree stem has no estimate
  expect_false(b$stems$estimated[b$stems$kind == "multi"])
  expect_true(any(grepl("^Stem profiles are estimated", b$warnings)))
  expect_false(any(grepl("^Stem profiles are estimated", a$warnings)))
  expect_true(all(b$stems$category[b$stems$estimated] == "Over bark"))
  # close to the profile the invented file was made from (same stems, 1 m steps)
  for (i in which(b$stems$estimated)) {
    ia <- match(b$stems$stem_key[i], a$stems$stem_key)
    pa <- a$pos[a$idx == ia]; da <- a$d[a$idx == ia]; pb <- b$pos[b$idx == i]; db <- b$d[b$idx == i]
    g <- seq(0, min(max(pa), max(pb)), 100)
    expect_lt(max(abs(stats::approx(pb, db, g)$y / stats::approx(pa, da, g)$y - 1)), 0.2)
  }
  # estimation can be switched off: the old error comes back
  off <- read_hpr_stems(fixture("hpr_logsonly_synthetic.hpr"), estimate_profiles = FALSE)
  expect_false(off$ok)
  expect_match(off$errors, "no stem profile", fixed = TRUE)
})

test_that("estimated stems are counted and labelled apart from measured ones", {
  skip_if_not_installed("taperNOR")
  a <- read_hpr_stems(fixture("hpr_check_synthetic.hpr"))
  b <- read_hpr_stems(fixture("hpr_logsonly_synthetic.hpr"))
  smap <- function(st) setNames(guess_hpr_species(st$species$name, st$species$user_id), st$species$key)
  ck <- hpr_stem_checks(b, smap(b))
  expect_true(ck$ok)
  expect_equal(c(ck$n_estimated, ck$n_measured), c(sum(ck$species_used %in% c("spruce", "pine")) , 0L), ignore_attr = TRUE)
  # two files in one upload: the counts split, and tree_data names the source of each profile
  b2 <- b; b2$stems$stem_key <- paste0("b", b2$stems$stem_key)         # keys must differ between files
  both <- combine_hpr_stems(list(a, b2), c("a.hpr", "b.hpr"))
  sm <- setNames(rep(c("spruce", "pine"), length.out = nrow(both$species)), both$species$key)
  ckb <- hpr_stem_checks(both, sm)
  expect_gt(ckb$n_measured, 0L); expect_gt(ckb$n_estimated, 0L)
  pr <- hpr_stem_profiles(both, sm)
  expect_setequal(unique(pr$tree_data$height_source), c("Measured profile", "Estimated profile"))
  expect_equal(sum(pr$tree_data$height_source == "Estimated profile"), ckb$n_estimated)
})

test_that("the bucking check runs on estimated profiles and says so", {
  skip_if_not_installed("taperNOR")
  x <- bucking_check(fixture("hpr_logsonly_synthetic.hpr"))
  expect_true(x$ok)
  expect_false(isTRUE(x$no_profile))
  expect_gt(x$n_estimated, 0L); expect_equal(x$n_measured, 0L)
  expect_true(any(grepl("^Stem profiles are estimated", x$warnings)))
  expect_true(any(grepl("stems have estimated stem profiles", x$warnings)))
  expect_true(is.finite(x$key_figures$yield[1]))
  # a file with measured profiles has no such note
  y <- bucking_check(fixture("hpr_check_synthetic.hpr"))
  expect_false(any(grepl("estimated", y$warnings)))
})

test_that("a file without quality codes gets the note, a file with them does not", {
  st <- list(gidx = c(1L, 1L, 2L))
  expect_true(hpr_no_grades(list(gidx = integer(0)), list(use = 1:20)))
  expect_false(hpr_no_grades(st, list(use = 1:2)))
  expect_false(hpr_no_grades(st, list(use = integer(0))))
  expect_true(hpr_no_grades(list(gidx = 1L), list(use = 1:100)))        # 1 % of the stems have grades
})

test_that("the new messages are translated", {
  for (m in c("Stem profiles are estimated from the log diameters and DBH (taperNOR/TapeR) because the file has no StemDiameters. The yield index is typically about 1 percentage point lower than with measured profiles and suits comparison between files; the top of the stem is uncertain.",
              "The file has no quality codes; the best possible bucking assumes defect-free stems.",
              "5 of 8 stems have estimated stem profiles."))
    expect_false(identical(tr_msg(m, "no"), m))
  expect_match(tr_msg("The file has no quality codes; the best possible bucking assumes defect-free stems.", "no"), "kvalitetskoder")
  expect_match(tr("hs_profiles_count", "3", "5", lang = "no"), "3 målt, 5 estimert")
})

test_that("estimated profiles follow a later change of the species choice", {
  skip_if_not_installed("taperNOR")
  b <- read_hpr_stems(fixture("hpr_logsonly_synthetic.hpr"))
  keys <- b$species$key
  as_sp <- function(x) setNames(rep(x, length(keys)), keys)
  p_sp <- hpr_stem_profiles(b, as_sp("spruce")); p_pi <- hpr_stem_profiles(b, as_sp("pine"))
  expect_true(p_sp$ok && p_pi$ok)
  # the height fit uses the other taper curve: the profiles differ
  expect_false(isTRUE(all.equal(p_sp$profiles$DiameterValue, p_pi$profiles$DiameterValue)))
  # a guessed-species read gives the same profiles as the explicit map of the guess
  g <- setNames(guess_hpr_species(b$species$name, b$species$user_id), keys)
  expect_equal(hpr_stem_profiles(b, g)$profiles$DiameterValue, hpr_stem_profiles(b, ifelse(g == "pine", "pine", "spruce"))$profiles$DiameterValue)
  # switching back and forth returns the original profile; the input is never changed
  expect_equal(hpr_stem_profiles(b, as_sp("spruce"))$profiles$DiameterValue, p_sp$profiles$DiameterValue)
  expect_equal(hpr_stem_profiles(b, as_sp("pine"))$profiles$DiameterValue, p_pi$profiles$DiameterValue)
  expect_true(all(b$stems$est_species[b$stems$estimated] %in% c("spruce", "pine")))
  # same as reading with the other species up front
  b2 <- b; b2$est_cache <- NULL
  e <- hpr_estimate_profiles(b2, read_hpr_logs(fixture("hpr_logsonly_synthetic.hpr"))$logs,
                             read_hpr_dbh(fixture("hpr_logsonly_synthetic.hpr")), species_map = as_sp("pine"))
  expect_equal(sort(e$st$d), sort(hpr_reestimate_profiles(b2, as_sp("pine"))$d))
  # measured profiles are untouched
  a <- read_hpr_stems(fixture("hpr_check_synthetic.hpr"))
  expect_identical(hpr_reestimate_profiles(a, setNames(rep("pine", nrow(a$species)), a$species$key)), a)
})
