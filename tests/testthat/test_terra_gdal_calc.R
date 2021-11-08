context("terra_gdal_calc()")

test_that("works", {
  skip_on_cran()
  skip_if_gdal_not_available()
  skip_if_not(
    is_gdal_calc_available(),
    message = "gdal_calc.py not available"
  )
  # create data
  x <- terra::rast(
   ncols = 40, nrows = 40, xmin = -110, xmax = -90, ymin = 40, ymax = 60,
   crs = "+proj=longlat +datum=WGS84"
  )
  terra::values(x) <- seq_len(terra::ncell(x))
  # create object
  z1 <- terra_gdal_calc(x, "(X<=20)*1", verbose = FALSE)
  z2 <- (x <= 20 + 1)
  # tests
  expect_is(z1, "SpatRaster")
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1)[[1]], terra::values(z2)[[1]])
})
