# .hpr stems use the harvester's own bark function where the file has a supported one (2026-11-11).
test_that("the Swedish Zacco bark function of the file reproduces the recorded under-bark diameters", {
  f <- system.file("extdata", "example.hpr", package = "optBuck"); skip_if(!nzchar(f))
  h <- read_hpr(f); expect_true(isTRUE(h$ok))
  expect_true(all(h$species$bark_category == "Swedish Zacco"))
  expect_true(all(is.finite(h$species$bark_a) & is.finite(h$species$bark_b)))
  rl <- read_hpr_logs(f, defs = h); lg <- rl$logs
  sp <- h$species[match(lg$species_key, h$species$key), ]
  ok <- is.finite(lg$TopOb) & is.finite(lg$TopUb)
  pred <- lg$TopOb[ok] - (sp$bark_a[ok] + sp$bark_b[ok] * lg$TopOb[ok])
  expect_lt(max(abs(pred - lg$TopUb[ok])), 1.01)          # the file's diameters are rounded to whole mm
})

test_that("a stem's under-bark profile follows the file's bark function, else barkNOR (with one message)", {
  f <- system.file("extdata", "example.hpr", package = "optBuck"); skip_if(!nzchar(f))
  st <- read_hpr_stems(f); expect_true(isTRUE(st$ok))
  smap <- setNames(guess_hpr_species(st$species$name, st$species$user_id), st$species$key)
  pr <- hpr_stem_profiles(st, smap)
  pv <- add_under_bark_profile(profiles_to_profile_vectors(pr$profiles)[[1L]])
  a <- pr$profiles$bark_a[1L]; b <- pr$profiles$bark_b[1L]
  expect_equal(pv$DiameterValueUB, pmax(pv$DiameterValue - (a + b * pv$DiameterValue), 0))
  expect_false(any(grepl("^Bark:", pr$warnings)))
  # an unsupported category: barkNOR for those stems, and a message
  st2 <- st; st2$species$bark_category[] <- "Skogforsk 2004, Norway spruce"; st2$species$bark_a[] <- NA_real_; st2$species$bark_b[] <- NA_real_
  pr2 <- hpr_stem_profiles(st2, smap)
  pv2 <- add_under_bark_profile(profiles_to_profile_vectors(pr2$profiles)[[1L]])
  expect_equal(pv2$DiameterValueUB, add_under_bark_profile({ q <- profiles_to_profile_vectors(pr2$profiles)[[1L]]; q$bark_ab <- NULL; q })$DiameterValueUB)
  expect_false(isTRUE(all.equal(pv$DiameterValueUB, pv2$DiameterValueUB)))
  expect_equal(sum(grepl("^Bark: the file's bark function", pr2$warnings)), 1L)
  expect_match(tr_msg(grep("^Bark:", pr2$warnings, value = TRUE), "no"), "støttes ikke")
  # the category "None" deducts nothing
  st3 <- st; st3$species$bark_category[] <- "None"; st3$species$bark_a[] <- 0; st3$species$bark_b[] <- 0
  pv3 <- add_under_bark_profile(profiles_to_profile_vectors(hpr_stem_profiles(st3, smap)$profiles)[[1L]])
  expect_equal(pv3$DiameterValueUB, pv3$DiameterValue)
  # NFI stems and tree lists never carry it
  expect_null(add_under_bark_profile(list(DiameterValue = c(200, 100), DBH = 200, diameterPosition = c(0L, 130L), species_group = "spruce"))$bark_ab)
})
