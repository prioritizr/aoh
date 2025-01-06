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
  z1 <- terra_gdal_rasterize(
    x, sf, burn = 5, NAflag = 1
  )
  z2 <- rasterize(v, x, field = 5, background = 0)
  # tests
  expect_is(z1, "SpatRaster")
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(as.list(terra::ext(z1)), as.list(terra::ext(z2)))
  expect_equivalent(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})

test_that("filename", {
  skip_on_cran()
  skip_if_not_installed("gdalUtilities")
  # import data
  f <- system.file("ex/lux.shp", package = "terra")
  sf <- sf::read_sf(f)
  v <- terra::vect(f)
  x <- terra::rast(v, ncols = 75, nrows = 100)
  x <- terra::setValues(x, runif(terra::ncell(x)))
  f1 <- tempfile(fileext = ".tif")
  terra_force_disk(x, f1)
  # create object
  z1 <- terra_gdal_rasterize(
    f1, sf, burn = 5, NAflag = 1
  )
  z2 <- rasterize(v, x, field = 5, background = 0)
  # tests
  expect_is(z1, "SpatRaster")
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(as.list(terra::ext(z1)), as.list(terra::ext(z2)))
  expect_equivalent(terra::values(z1), terra::values(z2), tolerance = 1e-5)
  unlink(f1, force = TRUE)
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
    x, sf, invert = TRUE, burn = 200
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
    x, sf, burn = 5, update = TRUE
  )
  z2 <- rasterize(v, x, field = 5, update = TRUE)
  # tests
  expect_is(z1, "SpatRaster")
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(as.list(terra::ext(z1)), as.list(terra::ext(z2)))
  expect_equivalent(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})

test_that("touches", {
  skip_on_cran()
  skip_if_not_installed("gdalUtilities")
  # create data
  sf <- sf::st_sf(
    tibble::tibble(geom = sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857))
  )
  sf <- sf::st_buffer(sf, 10)
  x <- terra::rast(matrix(c(1, 2)))
  terra::ext(x) <- c(xmin = -200, xmax = 200, ymin = -300, ymax = 800)
  terra::crs(x) <- terra::crs(sf)
  # create object
  z1 <- terra_gdal_rasterize(
    x, sf, burn = 5, touches = TRUE
  )
  z2 <- terra::deepcopy(x)
  terra::values(z2) <- c(0, 5)
  z3 <- terra_gdal_rasterize(
    x, sf, burn = 5, touches = FALSE
  )
  z4 <- terra::deepcopy(x)
  terra::values(z4) <- c(0, 0)
  # tests
  expect_is(z1, "SpatRaster")
  expect_is(z2, "SpatRaster")
  expect_is(z3, "SpatRaster")
  expect_is(z4, "SpatRaster")
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(as.list(terra::ext(z1)), as.list(terra::ext(z2)))
  expect_equivalent(terra::values(z1), terra::values(z2), tolerance = 1e-5)
  expect_true(terra::compareGeom(z3, z4, stopOnError = FALSE, res = TRUE))
  expect_equal(as.list(terra::ext(z3)), as.list(terra::ext(z4)))
  expect_equivalent(terra::values(z3), terra::values(z4), tolerance = 1e-5)
})
