test_that("data files are found in the package", {
  expect_true(file.exists(path_input("ProductData_spruce_pine_default.csv")))
  expect_true(file.exists(extdata_path("nfi", "nfi_plots_github.parquet")))
  expect_true(dir.exists(app_dir()))
})
