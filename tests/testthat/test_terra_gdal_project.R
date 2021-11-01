context("terra_gdal_project()")

test_that("single layer (single core)", {
  skip_on_cran()
  skip_if_gdal_not_available()
  # create data
  x <- terra::rast(
    ncols = 40, nrows = 40,
    xmin = -110, xmax = -90, ymin = 40, ymax = 60,
    crs = "+proj=longlat +datum=WGS84"
  )
  x <- terra::init(x, runif)
  names(x) <- "random"
  y <- terra::rast(
    ncols = 10, nrows = 10,
    xmin = -110, xmax = -90, ymin = 40, ymax = 60,
    crs = "+proj=longlat +datum=WGS84"
  )
  y <- terra::init(y, runif)
  # create correct result
  z1 <- terra::project(x, y, method = "near")
  # create results using function
  z2 <- terra_gdal_project(
    x, y, method = "near", verbose = FALSE, parallel_n_threads = 1
  )
  # tests
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1), terra::values(z2), tolerance = 0.1)
})

test_that("single layer (parallel processing)", {
  skip_on_cran()
  skip_if_gdal_not_available()
  # create data
  x <- terra::rast(
    ncols = 40, nrows = 40,
    xmin = -110, xmax = -90, ymin = 40, ymax = 60,
    crs = "+proj=longlat +datum=WGS84"
  )
  x <- terra::init(x, runif)
  names(x) <- "random"
  y <- terra::rast(
    ncols = 10, nrows = 10,
    xmin = -110, xmax = -90, ymin = 40, ymax = 60,
    crs = "+proj=longlat +datum=WGS84"
  )
  y <- terra::init(y, runif)
  # create correct result
  z1 <- terra::project(x, y, method = "near")
  # create results using function
  z2 <- terra_gdal_project(
    x, y, method = "near", verbose = FALSE, parallel_n_threads = 2
  )
  # tests
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})

test_that("multiple layers (single core)", {
  skip_on_cran()
  skip_if_gdal_not_available()
  # create data
  x <- terra::rast(
    ncols = 40, nrows = 40,
    xmin = -110, xmax = -90, ymin = 40, ymax = 60,
    crs = "+proj=longlat +datum=WGS84"
  )
  x <- terra::rast(
    lapply(seq_len(10), function(i) {
      terra::init(x, runif)
    })
  )
  names(x) <- paste0("random", seq_len(terra::nlyr(x)))
  y <- terra::rast(
    ncols = 10, nrows = 10,
    xmin = -110, xmax = -90, ymin = 40, ymax = 60,
    crs = "+proj=longlat +datum=WGS84"
  )
  y <- terra::init(y, runif)
  # create correct result
  z1 <- terra::project(x, y, method = "near")
  # create results using function
  z2 <- terra_gdal_project(
    x, y, method = "near", verbose = TRUE,
    parallel_n_threads = 1, NAflag = -9999
  )
  # tests
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})

test_that("multiple layers (parallel processing)", {
  skip_on_cran()
  skip_if_gdal_not_available()
  # create data
  x <- terra::rast(
    ncols = 40, nrows = 40,
    xmin = -110, xmax = -90, ymin = 40, ymax = 60,
    crs = "+proj=longlat +datum=WGS84"
  )
  x <- terra::rast(
    lapply(seq_len(10), function(i) {
      terra::init(x, runif)
    })
  )
  names(x) <- paste0("random", seq_len(terra::nlyr(x)))
  y <- terra::rast(
    ncols = 10, nrows = 10,
    xmin = -110, xmax = -90, ymin = 40, ymax = 60,
    crs = "+proj=longlat +datum=WGS84"
  )
  y <- terra::init(y, runif)
  # create correct result
  z1 <- terra::project(x, y, method = "near")
  # create results using function
  z2 <- terra_gdal_project(
    x, y, method = "near", verbose = FALSE,
    parallel_n_threads = 2, NAflag = -9999
  )
  # tests
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})
