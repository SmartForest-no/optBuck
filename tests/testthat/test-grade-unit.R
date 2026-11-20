test_that("grade start positions written in dm are read as cm x 10 (decided from the positions)", {
  cm <- read_hpr_stems(fixture("hpr_gradeunit_cm.hpr"))
  dm <- read_hpr_stems(fixture("hpr_gradeunit_dm.hpr"))
  expect_true(cm$ok && dm$ok)
  expect_identical(c(cm$grade_unit, cm$grade_unit_how), c("cm", "positions"))
  expect_identical(c(dm$grade_unit, dm$grade_unit_how), c("dm", "positions"))
  expect_false(any(grepl("unit (cm or dm)", dm$warnings, fixed = TRUE)))
  # the same stems: after the fix both files give grade changes within 10 cm of the first log's end
  g2 <- function(s) s$gpos[s$gpos > 0]
  expect_lt(max(abs(g2(dm) - g2(cm))), 5 + 1e-9)               # dm rounds to whole dm
  expect_equal(sort(unique(cm$gpos[cm$gpos > 0] %% 1)), 0)
  # the grid grade of a stem: grade 5 in the butt log, 2 above it, in both files
  smap <- setNames(rep("spruce", nrow(dm$species)), dm$species$key)
  for (r in list(cm, dm)) {
    s <- r$stems; i <- 1L
    gp <- r$gpos[r$gidx == i]; gv <- r$gval[r$gidx == i]
    gr <- hpr_grade_grid(seq(0, 600, 10), gp, gv, 2)
    expect_identical(gr[1L], 5); expect_identical(gr[length(gr)], 2)
    expect_gt(sum(gr == 5), 25)                                 # about 3 m of grade 5, not 40 cm
  }
})

test_that("a control-system version known to write dm decides, even with too few stems for the positions", {
  v <- read_hpr_stems(fixture("hpr_gradeunit_dm_version.hpr"))
  expect_identical(c(v$grade_unit, v$grade_unit_how), c("dm", "version"))
  expect_gt(min(v$gpos[v$gpos > 0]), 250)
})

test_that("positions that cannot be decided stay cm with one note", {
  u <- read_hpr_stems(fixture("hpr_grades_synthetic.hpr"))        # 5 stems, no recorded logs
  expect_identical(c(u$grade_unit, u$grade_unit_how), c("cm", "undecided"))
  expect_equal(sum(grepl("unit (cm or dm)", u$warnings, fixed = TRUE)), 1L)
  expect_true(all(u$gpos %in% c(0, 300, 500, 800, 295, 0)))        # unchanged
  # a file without grade changes has nothing to decide and no note
  a <- read_hpr_stems(fixture("hpr_check_synthetic.hpr"))
  expect_false(any(grepl("unit (cm or dm)", a$warnings, fixed = TRUE)))
  expect_match(tr_msg("Stem grade positions: the unit (cm or dm) cannot be determined from this file; centimetres are assumed.", "no"), "enheten")
})

test_that("the shares used to decide the unit", {
  lg <- data.frame(stem_key = c("a", "a"), StopPos = c(309, 800))
  s <- hpr_grade_unit_shares(c(1L, 1L), c(0, 31), "a", lg)
  expect_equal(c(s$n, s$cm, s$dm), c(1, 0, 1))
})

test_that("the owner's cm files are unchanged by the unit check", {
  skip_if(!file.exists("C:/optbuck_private/hpr_files.csv"), "private files not available")
  m <- read.csv("C:/optbuck_private/hpr_files.csv", stringsAsFactors = FALSE)
  for (id in paste0("file", 1:5)) {
    p <- file.path("C:/optbuck_private/hpr", m$file[m$id == id]); skip_if(!file.exists(p))
    r <- read_hpr_stems(p, estimate_profiles = FALSE)
    expect_identical(r$grade_unit, "cm", info = id)
    expect_true(max(c(0, r$gpos)) > 300 || length(r$gpos) == 0L, info = id)
  }
})
