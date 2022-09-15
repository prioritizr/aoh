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

# print grass info
print("link2GI::findGRASS()")
print(link2GI::findGRASS())

# run tests
if (
  isTRUE(aoh:::is_gdal_version_met()) &&
  isTRUE(aoh:::is_proj_version_met())
) {
  test_check("aoh", reporter = reporter)
} else {
  message("skipping all tests due to out-dated system dependencies")
}
