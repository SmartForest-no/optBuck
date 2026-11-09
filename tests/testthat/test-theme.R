# The one green of the app (inst/app/theme.R: OPTBUCK_GREEN) and the one static copy of it, the icon file.
test_that("the icon file holds the app's green and nothing else copies it", {
  app <- system.file("app", package = "optBuck")
  skip_if(!nzchar(app), "no app folder")
  theme <- readLines(file.path(app, "theme.R"), warn = FALSE)
  line <- grep('^OPTBUCK_GREEN <- "', theme, value = TRUE)
  expect_length(line, 1L)
  green <- sub('^OPTBUCK_GREEN <- "(#[0-9A-Fa-f]{6})".*$', "\\1", line)
  icon <- paste(readLines(file.path(app, "www", "optbuck-icon.svg"), warn = FALSE), collapse = "\n")
  expect_true(grepl(tolower(green), tolower(icon), fixed = TRUE))
  # the CSS takes the green from the variables written by optbuck_css_vars(): no copy of it there
  css <- paste(readLines(file.path(app, "www", "optbuck.css"), warn = FALSE), collapse = "\n")
  expect_false(grepl(tolower(green), tolower(css), fixed = TRUE))
  # in theme.R the value is written once (comments do not count)
  code <- theme[!grepl("^\\s*#", theme)]
  expect_equal(sum(grepl(tolower(green), tolower(code), fixed = TRUE)), 1L)
})
