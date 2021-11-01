context("is_gdal_available")

test_that("expected result", {
  expect_is(is_gdal_available(), "logical")
})
