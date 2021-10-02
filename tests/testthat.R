# load packages
library(testthat)
library(aoh)

# enable parallel testing
Sys.unsetenv("R_TESTS")

# run tests
test_check("aoh")
