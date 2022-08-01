# load packages
library(testthat)
library(aoh)

# enable parallel testing
Sys.unsetenv("R_TESTS")

# determine reporter
if (identical(Sys.getenv("CI"), "true")) {
  reporter = "progress"
} else {
  reporter = testthat::check_reporter()
}

# run tests
if (isTRUE(is_gdal_version_met()) && isTRUE(is_proj_version_met())) {
  test_check("aoh", reporter = reporter)
} else {
  message("skipping all tests due to out-dated system dependencies")
}
