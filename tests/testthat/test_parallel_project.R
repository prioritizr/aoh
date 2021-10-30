context("parallel_project()")

test_that("single layer (single core)", {
  skip_on_cran()
  # create data
  x <- terra::rast(
    ncols = 40, nrows = 40,
    xmin = -110, xmax = -90, ymin = 40, ymax = 60,
    crs = "+proj=longlat +datum=WGS84"
  )
  x <- terra::init(x, runif)
  names(x) <- "random"
  y <- terra::rast(
    ncols = 94, nrows = 124,
    xmin = -944881, xmax = 935118, ymin = 4664377, ymax = 7144377,
    crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
  )
  y <- terra::init(y, runif)
  # create correct result
  z1 <- terra::project(x, y)
  # create results using function
  z2 <- parallel_project(x, y, verbose = FALSE, parallel_n_threads = 1)
  # tests
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})

test_that("single layer (PSOCK cluster)", {
  skip_on_cran()
  # create data
  x <- terra::rast(
    ncols = 40, nrows = 40,
    xmin = -110, xmax = -90, ymin = 40, ymax = 60,
    crs = "+proj=longlat +datum=WGS84"
  )
  x <- terra::init(x, runif)
  names(x) <- "random"
  y <- terra::rast(
    ncols = 94, nrows = 124,
    xmin = -944881, xmax = 935118, ymin = 4664377, ymax = 7144377,
    crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
  )
  y <- terra::init(y, runif)
  # create correct result
  z1 <- terra::project(x, y)
  # create results using function
  z2 <- parallel_project(
    x, y, parallel_n_threads = 2, verbose = FALSE, parallel_cluster = "PSOCK"
  )
  # tests
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})

test_that("single layer (FORK cluster)", {
  skip_on_cran()
  skip_on_os("windows")
  # create data
  x <- terra::rast(
    ncols = 40, nrows = 40,
    xmin = -110, xmax = -90, ymin = 40, ymax = 60,
    crs = "+proj=longlat +datum=WGS84"
  )
  x <- terra::init(x, runif)
  names(x) <- "random"
  y <- terra::rast(
    ncols = 94, nrows = 124,
    xmin = -944881, xmax = 935118, ymin = 4664377, ymax = 7144377,
    crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
  )
  y <- terra::init(y, runif)
  # create correct result
  z1 <- terra::project(x, y)
  # create results using function
  z2 <- parallel_project(
    x, y, parallel_n_threads = 2, verbose = FALSE, parallel_cluster = "FORK"
  )
  # tests
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})

test_that("multiple layers (single core)", {
  skip_on_cran()
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
    ncols = 94, nrows = 124,
    xmin = -944881, xmax = 935118, ymin = 4664377, ymax = 7144377,
    crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
  )
  y <- terra::init(y, runif)
  # create correct result
  z1 <- terra::project(x, y)
  # create results using function
  z2 <- parallel_project(x, y, verbose = FALSE, parallel_n_threads = 1)
  # tests
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})

test_that("multiple layers (PSOCK cluster)", {
  skip_on_cran()
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
    ncols = 94, nrows = 124,
    xmin = -944881, xmax = 935118, ymin = 4664377, ymax = 7144377,
    crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
  )
  y <- terra::init(y, runif)
  # create correct result
  z1 <- terra::project(x, y)
  # create results using function
  z2 <- parallel_project(
    x, y, parallel_n_threads = 2, verbose = FALSE, parallel_cluster = "PSOCK"
  )
  # tests
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})

test_that("multiple layers (FORK cluster)", {
  skip_on_cran()
  skip_on_os("windows")
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
    ncols = 94, nrows = 124,
    xmin = -944881, xmax = 935118, ymin = 4664377, ymax = 7144377,
    crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
  )
  y <- terra::init(y, runif)
  # create correct result
  z1 <- terra::project(x, y)
  # create results using function
  z2 <- parallel_project(
    x, y, parallel_n_threads = 2, verbose = FALSE, parallel_cluster = "FORK"
  )
  # tests
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})
