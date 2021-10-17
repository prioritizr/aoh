context("parallel_project()")

test_that("single layer", {
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
  z <- terra::project(x, y)
  # create results using function
  z1 <- parallel_project(x, y, verbose = FALSE, parallel_n_threads = 1)
  expect_equal(terra::values(z), terra::values(z1), tolerance = 1e-5)
  z2 <- parallel_project(
    x, y, parallel_n_threads = 2, verbose = FALSE, parallel_cluster = "PSOCK"
  )
  expect_equal(terra::values(z), terra::values(z2), tolerance = 1e-5)
  skip_on_os("windows")
  z3 <- parallel_project(
    x, y, parallel_n_threads = 2, verbose = FALSE, parallel_cluster = "FORK"
  )
  expect_equal(terra::values(z), terra::values(z3), tolerance = 1e-5)
})

test_that("multiple layers", {
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
  z <- terra::project(x, y)
  # create results using function
  z1 <- parallel_project(x, y, verbose = FALSE, parallel_n_threads = 1)
  expect_equal(terra::values(z), terra::values(z1), tolerance = 1e-5)
  z2 <- parallel_project(
    x, y, parallel_n_threads = 2, verbose = FALSE, parallel_cluster = "PSOCK"
  )
  expect_equal(terra::values(z), terra::values(z2), tolerance = 1e-5)
  skip_on_os("windows")
  z3 <- parallel_project(
    x, y, parallel_n_threads = 2, verbose = FALSE, parallel_cluster = "FORK"
  )
  expect_equal(terra::values(z), terra::values(z3), tolerance = 1e-5)
})
