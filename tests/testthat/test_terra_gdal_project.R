context("terra_gdal_project()")

test_that("single layer (single core)", {
  skip_on_cran()
  skip_if_not_installed("gdalUtilities")
  # create data
  f <- tempfile(fileext = ".tif")
  x <- terra::rast(
    ncols = 40, nrows = 40,
    xmin = -110, xmax = -90, ymin = 40, ymax = 60,
    crs = "+proj=longlat +datum=WGS84"
  )
  x <- terra::init(x, runif, filename = f)
  names(x) <- "random"
  y <- terra::project(
    x, sf::st_crs("ESRI:54017")[[2]], res = 100000, method = "near"
  )
  # create correct result
  z1 <- terra::project(x, y, method = "near")
  # create results using function
  z2 <- terra::rast(terra_gdal_project(
    x, y, method = "near", verbose = interactive(), n_threads = 1,
    tiled = TRUE, bigtiff = TRUE, output_raster = FALSE
  ))
  # tests
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equivalent(terra::values(z1), terra::values(z2), tolerance = 0.1)
  unlink(f)
})

test_that("single layer (filename)", {
  skip_on_cran()
  skip_if_not_installed("gdalUtilities")
  # create data
  f1 <- tempfile(fileext = ".tif")
  f2 <- tempfile(fileext = ".tif")
  x <- terra::rast(
    ncols = 40, nrows = 40,
    xmin = -110, xmax = -90, ymin = 40, ymax = 60,
    crs = "+proj=longlat +datum=WGS84"
  )
  x <- terra::init(x, runif, filename = f1)
  terra_force_disk(x, f1)
  names(x) <- "random"
  y <- terra::project(
    x, sf::st_crs("ESRI:54017")[[2]], res = 100000, method = "near",
    filename = f2
  )
  # create correct result
  z1 <- terra::project(x, y, method = "near")
  # create results using function
  z2 <- terra::rast(terra_gdal_project(
    f1, f2, method = "near", verbose = interactive(), n_threads = 1,
    tiled = TRUE, bigtiff = TRUE, output_raster = FALSE
  ))
  # tests
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equivalent(terra::values(z1), terra::values(z2), tolerance = 0.1)
  unlink(f1)
  unlink(f2)
})

test_that("single layer (parallel processing)", {
  skip_on_cran()
  skip_if_not_installed("gdalUtilities")
  # create data
  x <- terra::rast(
    ncols = 40, nrows = 40,
    xmin = -110, xmax = -90, ymin = 40, ymax = 60,
    crs = "+proj=longlat +datum=WGS84"
  )
  x <- terra::init(x, runif)
  names(x) <- "random"
  y <- terra::project(
    x, sf::st_crs("ESRI:54017")[[2]], res = 100000, method = "near"
  )
  # create correct result
  z1 <- terra::project(x, y, method = "near")
  # create results using function
  z2 <- terra_gdal_project(
    x, y, method = "near", verbose = interactive(), n_threads = 2
  )
  # tests
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})

test_that("multiple layers (single core)", {
  skip_on_cran()
  skip_if_not_installed("gdalUtilities")
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
  y <- terra::project(
    x, sf::st_crs("ESRI:54017")[[2]], res = 100000, method = "near"
  )
  # create correct result
  z1 <- terra::project(x, y, method = "near")
  # create results using function
  z2 <- terra_gdal_project(
    x, y, method = "near", verbose = TRUE,
    n_threads = 1, NAflag = -9999
  )
  # tests
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})

test_that("multiple layers (parallel processing)", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("gdalUtilities")
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
  y <- terra::project(
    x, sf::st_crs("ESRI:54017")[[2]], res = 100000, method = "near"
  )
  # create correct result
  z1 <- terra::project(x, y, method = "near")
  # create results using function
  z2 <- terra_gdal_project(
    x, y, method = "near", verbose = interactive(),
    n_threads = 2, NAflag = "none"
  )
  # tests
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(terra::values(z1), terra::values(z2), tolerance = 1e-5)
})
