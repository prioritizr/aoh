context("terra_gdal_rasterize")

test_that("normal", {
  skip_on_cran()
  skip_if_not_installed("gdalUtilities")
  # import data
  f <- system.file("ex/lux.shp", package = "terra")
  sf <- sf::read_sf(f)
  v <- terra::vect(f)
  x <- terra::rast(v, ncols = 75, nrows = 100)
  x <- terra::setValues(x, runif(terra::ncell(x)))
  # create object
  z1 <- terra_gdal_rasterize(x, sf, burn = 5, verbose = interactive())
  z2 <- rasterize(v, x, field = 5, background = 0)
  # tests
  expect_is(z1, "SpatRaster")
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(as.list(terra::ext(z1)), as.list(terra::ext(z2)))
  expect_equivalent(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})

test_that("invert", {
  skip_on_cran()
  skip_if_not_installed("gdalUtilities")
  # import data
  f <- system.file("ex/lux.shp", package = "terra")
  sf <- sf::read_sf(f)
  v <- terra::vect(f)
  x <- terra::rast(v, ncols = 75, nrows = 100)
  x <- terra::setValues(x, runif(terra::ncell(x)))
  # create object
  z1 <- terra_gdal_rasterize(
    x, sf, invert = TRUE, burn = 200, verbose = interactive()
  )
  z2 <- terra::rasterize(v, x, field = 0, background = 200)
  # tests
  expect_is(z1, "SpatRaster")
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(as.list(terra::ext(z1)), as.list(terra::ext(z2)))
  expect_equivalent(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})

test_that("update", {
  skip_on_cran()
  skip_if_not_installed("gdalUtilities")
  # import data
  f <- system.file("ex/lux.shp", package = "terra")
  sf <- sf::read_sf(f)
  v <- terra::vect(f)
  x <- terra::rast(v, ncols = 75, nrows = 100)
  x <- terra::setValues(x, runif(terra::ncell(x)))
  # create object
  z1 <- terra_gdal_rasterize(
    x, sf, burn = 5, update = TRUE, verbose = interactive()
  )
  z2 <- rasterize(v, x, field = 5, update = TRUE)
  # tests
  expect_is(z1, "SpatRaster")
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(as.list(terra::ext(z1)), as.list(terra::ext(z2)))
  expect_equivalent(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})
