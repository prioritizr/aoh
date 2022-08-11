context("terra_gdal_calc()")

test_that("single layer (in memory)", {
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
  z1 <- suppressMessages(terra_gdal_calc(x, "(X<=20)*1", verbose = TRUE))
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

test_that("three layers (in memory)", {
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

test_that("three layers (on disk)", {
  skip_on_cran()
  skip_if_not_installed("gdalUtilities")
  skip_if_not(
    is_gdal_python_available(),
    message = "gdal_calc.py not available"
  )
  # create paths
  f1 <- tempfile(fileext = ".tif")
  f2 <- tempfile(fileext = ".tif")
  f3 <- tempfile(fileext = ".tif")
  # create data
  x <- terra::rast(
   ncols = 40, nrows = 40, xmin = -110, xmax = -90, ymin = 40, ymax = 60,
   crs = "+proj=longlat +datum=WGS84"
  )
  terra::values(x) <- seq_len(terra::ncell(x))
  x <- terra_force_disk(x, f1)
  y <- terra_force_disk(x * 3, f2)
  z <- terra_force_disk(x * 5, f3)
  # create object
  z1 <- terra_gdal_calc(
    x, "(X*2)+(Y*3)+(Z*5)", y = y, z = z, verbose = interactive()
  )
  z2 <- (x * 2) + (y * 3) + (z * 5)
  # tests
  expect_is(z1, "SpatRaster")
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1), terra::values(z2), tolerance = 1e-5)
  unlink(f1, force = TRUE)
  unlink(f2, force = TRUE)
  unlink(f3, force = TRUE)
})
