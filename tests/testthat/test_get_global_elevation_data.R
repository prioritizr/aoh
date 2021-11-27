context("get_global_elevation_data()")

test_that("latest version (from online)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  # create object
  x <- get_global_elevation_data(
    version = "latest", force = TRUE, verbose = interactive()
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

test_that("latest version (from cache)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("rappdirs")
  # create object
  x <- get_global_elevation_data(
    dir = rappdirs::user_data_dir("aoh"),
    version = "latest", force = FALSE, verbose = interactive()
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
  skip_if_offline()
  skip_if_local_and_slow_internet()
  # create object
  x <- get_global_elevation_data(
    version = "10.5281/zenodo.5719984", force = TRUE, verbose = interactive()
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

test_that("specified version (from cache)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("rappdirs")
  # create object
  x <- get_global_elevation_data(
    dir = rappdirs::user_data_dir("aoh"),
    version = "10.5281/zenodo.5719984", force = FALSE, verbose = interactive()
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
