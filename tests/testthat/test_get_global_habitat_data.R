context("get_global_habitat_data()")

test_that("latest version", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  # create object
  x <- get_global_habitat_data(
    version = "latest", force = TRUE, verbose = FALSE
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(sf::st_crs(terra::crs(x)) == sf::st_crs(4326))
  expect_lte(terra::xmin(d), -180)
  expect_gte(terra::xmax(d), 180)
  expect_lte(terra::xmin(d), -89)
  expect_gte(terra::ymax(d), 89)
})

test_that("manually specified version", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  # create object
  x <- get_global_habitat_data(
    version = "10.5281/zenodo.3816946", force = TRUE, verbose = FALSE
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(sf::st_crs(terra::crs(x)) == sf::st_crs(4326))
  expect_lte(terra::xmin(d), -180)
  expect_gte(terra::xmax(d), 180)
  expect_lte(terra::xmin(d), -89)
  expect_gte(terra::ymax(d), 89)
})
