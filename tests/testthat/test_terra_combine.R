context("terra_combine()")

test_that("terra_combine", {
  # create data
  x <- rast(
    ncols = 10, nrows = 10,
    xmin = 0, xmax = 10, ymin = 0, ymax = 10,
    crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
  )
  terra::values(x) <- runif(terra::ncell(x))
  names(x) <- "l1"
  y <- rast(
    ncols = 5, nrows = 5,
    xmin = 90, xmax = 95, ymin = 80, ymax = 85,
    crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
  )
  terra::values(y) <- runif(terra::ncell(y))
  names(y) <- "l2"
  # combine data
  r <- terra_combine(list(x, y))
  # tests
  expect_is(r, "SpatRaster")
  expect_equal(terra::xmin(r), 0)
  expect_equal(terra::xmax(r), 95)
  expect_equal(terra::ymin(r), 0)
  expect_equal(terra::ymax(r), 85)
  expect_equal(terra::nlyr(r), 2)
  expect_equal(names(r), c(names(x), names(y)))
  expect_equal(terra::res(r), terra::res(x))
  expect_equal(terra::origin(r), terra::origin(x))
  expect_equal(terra::values(crop(r[[1]], x)), terra::values(x))
  expect_equal(terra::values(crop(r[[2]], y)), terra::values(y))
})
