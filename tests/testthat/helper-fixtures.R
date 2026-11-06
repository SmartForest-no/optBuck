# The invented fixtures of the package (no real harvester data).
fixture <- function(name) system.file("extdata", "inputs", "test_fixtures", name, package = "optBuck")
options(optbuck.workers = 1L)          # tests run without worker processes
