context("terra_gdal_crop()")

test_that("single core", {
  skip_on_cran()
  skip_if_gdal_not_available()
  # create data
  x <- terra::rast(
   ncols = 40, nrows = 40, xmin = -110, xmax = -90, ymin = 40, ymax = 60,
   crs = "+proj=longlat +datum=WGS84"
  )
  terra::values(x) <- seq_len(terra::ncell(x))
  y <- terra::ext(x) - c(5, 2.5, 1, 1.5)
  # create object
  z1 <- terra_gdal_crop(x, y, verbose = interactive())
  z2 <- terra::crop(x, y, snap = "out")
  # tests
  expect_is(z1, "SpatRaster")
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(as.list(terra::ext(z1)), as.list(y))
  expect_equal(terra::values(z1), terra::values(z2))
})

test_that("parallel processing", {
  skip_on_cran()
  skip_if_gdal_not_available()
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