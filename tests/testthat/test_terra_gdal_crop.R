context("terra_gdal_crop()")

test_that("single core (in memory)", {
  skip_on_cran()
  skip_if_not_installed("gdalUtilities")
  # create data
  x <- terra::rast(
   ncols = 40, nrows = 40, xmin = -110, xmax = -90, ymin = 40, ymax = 60,
   crs = "+proj=longlat +datum=WGS84"
  )
  terra::values(x) <- seq_len(terra::ncell(x))
  y <- terra::ext(x) - c(5, 2.5, 1, 1.5)
  # create object
  z1 <- terra_gdal_crop(x, y, verbose = interactive(), NAflag = -9999)
  z2 <- terra::crop(x, y, snap = "out")
  # tests
  expect_is(z1, "SpatRaster")
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(as.list(terra::ext(z1)), as.list(y))
  expect_equal(terra::values(z1), terra::values(z2))
})

test_that("single core (filename)", {
  skip_on_cran()
  skip_if_not_installed("gdalUtilities")
  # create data
  f <- tempfile(fileext = ".tif")
  x <- terra::rast(
   ncols = 40, nrows = 40, xmin = -110, xmax = -90, ymin = 40, ymax = 60,
   crs = "+proj=longlat +datum=WGS84"
  )
  terra::values(x) <- seq_len(terra::ncell(x))
  terra_force_disk(x, f)
  y <- terra::ext(x) - c(5, 2.5, 1, 1.5)
  # create object
  z1 <- terra_gdal_crop(f, y, verbose = interactive(), NAflag = -9999)
  z2 <- terra::crop(x, y, snap = "out")
  # tests
  expect_is(z1, "SpatRaster")
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(as.list(terra::ext(z1)), as.list(y))
  expect_equal(terra::values(z1), terra::values(z2))
  unlink(f, force = TRUE)
})

test_that("parallel processing", {
  skip_on_cran()
  skip_if_not_installed("gdalUtilities")
  # create data
  x <- terra::rast(
   ncols = 40, nrows = 40, xmin = -110, xmax = -90, ymin = 40, ymax = 60,
   crs = "+proj=longlat +datum=WGS84"
  )
  terra::values(x) <- seq_len(terra::ncell(x))
  y <- terra::ext(x) - c(5, 2.5, 1, 1.5)
  # create object
  z1 <- terra_gdal_crop(x, y, verbose = interactive(), n_threads = 2)
  z2 <- terra::crop(x, y, snap = "out")
  # tests
  expect_is(z1, "SpatRaster")
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(as.list(terra::ext(z1)), as.list(y))
  expect_equal(terra::values(z1), terra::values(z2))
})
