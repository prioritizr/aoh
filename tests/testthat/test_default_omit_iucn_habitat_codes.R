context("default_omit_iucn_habitat_codes()")

test_that("expected results", {
  expect_is(x <<- default_omit_iucn_habitat_codes(), "character")
  expect_gt(length(x), 0)
}
