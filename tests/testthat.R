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
test_check("aoh", reporter = reporter)
