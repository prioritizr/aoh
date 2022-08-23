context("gdal functions")

test_that("is_gdal_calc_available()", {
  expect_is(is_gdal_calc_available(), "logical")
})
