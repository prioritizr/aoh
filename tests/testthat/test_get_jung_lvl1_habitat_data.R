context("get_jung_lvl1_habitat_data()")

test_that("latest version (from online)", {
  # skip if needed
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  skip_if_zenodo_api_not_available()
  # create object
  d <- new_temp_dir()
  x <- get_jung_lvl1_habitat_data(
    dir = d,
    version = "latest",
    force = TRUE,
    verbose = interactive()
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(sf::st_crs(terra::crs(x)) == sf::st_crs("ESRI:54017"))
  expect_lte(terra::xmin(x), -17367531)
  expect_gte(terra::xmax(x), 17367569)
  expect_lte(terra::xmin(x), -6005523)
  expect_gte(terra::ymax(x), 7287077)
  expect_equal(terra::nlyr(x), 1)
  # clean up
  unlink(d, recursive = TRUE, force = TRUE)
})

test_that("latest version (from cache)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("rappdirs")
  skip_if_zenodo_api_not_available()
  skip_if_cached_data_not_available()
  skip_if_zenodo_data_not_available(latest_jung_version)
  # create object
  x <- get_jung_lvl1_habitat_data(
    dir = rappdirs::user_data_dir("aoh"),
    version = "latest",
    force = FALSE,
    verbose = interactive()
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(sf::st_crs(terra::crs(x)) == sf::st_crs("ESRI:54017"))
  expect_lte(terra::xmin(x), -17367531)
  expect_gte(terra::xmax(x), 17367569)
  expect_lte(terra::xmin(x), -6005523)
  expect_gte(terra::ymax(x), 7287077)
  expect_equal(terra::nlyr(x), 1)
})

test_that("specified version (from online)", {
  # skip if needed
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  skip_if_zenodo_api_not_available()
  skip_if_zenodo_data_not_available(latest_jung_version)
  # create object
  d <- new_temp_dir()
  x <- get_jung_lvl1_habitat_data(
    dir = d,
    version = latest_jung_version,
    force = FALSE,
    verbose = interactive()
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(sf::st_crs(terra::crs(x)) == sf::st_crs("ESRI:54017"))
  expect_lte(terra::xmin(x), -17367531)
  expect_gte(terra::xmax(x), 17367569)
  expect_lte(terra::xmin(x), -6005523)
  expect_gte(terra::ymax(x), 7287077)
  expect_equal(terra::nlyr(x), 1)
  # clean up
  unlink(d, recursive = TRUE, force = TRUE)
})

test_that("specified version (from cache)", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("rappdirs")
  skip_if_cached_data_not_available()
  skip_if_zenodo_data_not_available(latest_jung_version)
  # create object
  x <- get_jung_lvl1_habitat_data(
    dir = rappdirs::user_data_dir("aoh"),
    version = latest_jung_version,
    force = FALSE,
    verbose = interactive()
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(sf::st_crs(terra::crs(x)) == sf::st_crs("ESRI:54017"))
  expect_lte(terra::xmin(x), -17367531)
  expect_gte(terra::xmax(x), 17367569)
  expect_lte(terra::xmin(x), -6005523)
  expect_gte(terra::ymax(x), 7287077)
  expect_equal(terra::nlyr(x), 1)
})
