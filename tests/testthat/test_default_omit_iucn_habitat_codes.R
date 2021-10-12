context("default_omit_iucn_habitat_codes()")

test_that("expected results", {
  x <- default_omit_iucn_habitat_codes()
  expect_is(x, "character")
  expect_gt(length(x), 0)
})
