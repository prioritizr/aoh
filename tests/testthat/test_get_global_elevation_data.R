context("get_global_elevation_data()")

test_that("raw data (from online)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  # create object
  x <- get_global_elevation_data(
    preprocessed = FALSE, force = TRUE, verbose = FALSE
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(sf::st_crs(terra::crs(x)) == sf::st_crs(4326))
  expect_lte(terra::xmin(x), -180)
  expect_gte(terra::xmax(x), 180)
  expect_lte(terra::xmin(x), -55)
  expect_gte(terra::ymax(x), 83)
})

test_that("raw data (from cache)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  # create object
  x <- get_global_elevation_data(
    preprocessed = FALSE, force = FALSE, verbose = FALSE
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(sf::st_crs(terra::crs(x)) == sf::st_crs(4326))
  expect_lte(terra::xmin(x), -180)
  expect_gte(terra::xmax(x), 180)
  expect_lte(terra::xmin(x), -55)
  expect_gte(terra::ymax(x), 83)
})

test_that("preprocessed data (from online)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  # get data
  d <- get_world_berhman_1km_rast()
  # create object
  x <- get_global_elevation_data(
    preprocessed = TRUE, force = TRUE, verbose = FALSE
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(terra::compareGeom(x, d, res = TRUE, stopOnError = FALSE))

})

test_that("preprocessed data (from cache)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  # get data
  d <- get_world_berhman_1km_rast()
  # create object
  x <- get_global_elevation_data(
    preprocessed = TRUE, force = FALSE, verbose = FALSE
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(terra::compareGeom(x, d, res = TRUE, stopOnError = FALSE))
})
