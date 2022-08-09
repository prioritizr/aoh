context("terra_gdal_calc()")

test_that("single layer", {
  skip_on_cran()
  skip_if_not_installed("gdalUtilities")
  skip_if_gdal_python_not_available()
  # create data
  x <- terra::rast(
   ncols = 40, nrows = 40, xmin = -110, xmax = -90, ymin = 40, ymax = 60,
   crs = "+proj=longlat +datum=WGS84"
  )
  terra::values(x) <- seq_len(terra::ncell(x))
  # create object
  z1 <- terra_gdal_calc(x, "(X<=20)*1", verbose = interactive())
  z2 <- (x <= 20) * 1
  # tests
  expect_is(z1, "SpatRaster")
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})

test_that("two layers", {
  skip_on_cran()
  skip_if_not_installed("gdalUtilities")
  skip_if_not(
    is_gdal_python_available(),
    message = "gdal_calc.py not available"
  )
  # create data
  x <- terra::rast(
   ncols = 40, nrows = 40, xmin = -110, xmax = -90, ymin = 40, ymax = 60,
   crs = "+proj=longlat +datum=WGS84"
  )
  terra::values(x) <- seq_len(terra::ncell(x))
  y <- x * 3
  # create object
  z1 <- terra_gdal_calc(x, "(X*2)+(Y*3)", y = y, verbose = interactive())
  z2 <- (x * 2) + (y * 3)
  # tests
  expect_is(z1, "SpatRaster")
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})

test_that("three layers", {
  skip_on_cran()
  skip_if_not_installed("gdalUtilities")
  skip_if_not(
    is_gdal_python_available(),
    message = "gdal_calc.py not available"
  )
  # create data
  x <- terra::rast(
   ncols = 40, nrows = 40, xmin = -110, xmax = -90, ymin = 40, ymax = 60,
   crs = "+proj=longlat +datum=WGS84"
  )
  terra::values(x) <- seq_len(terra::ncell(x))
  y <- x * 3
  z <- x * 5
  # create object
  z1 <- terra_gdal_calc(
    x, "(X*2)+(Y*3)+(Z*5)", y = y, z = z, verbose = interactive()
  )
  z2 <- (x * 2) + (y * 3) + (z * 5)
  # tests
  expect_is(z1, "SpatRaster")
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})
