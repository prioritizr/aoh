context("gdal functions")

test_that("is_gdal_available()", {
  expect_is(is_gdal_available(), "logical")
})

test_that("gdal_version()", {
  expect_is(gdal_version(), "character")
})
