context("gdal functions")

test_that("is_gdal_python_available()", {
  expect_is(is_gdal_python_available(), "logical")
})
