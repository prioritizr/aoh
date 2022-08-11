context("simulate_spp_data()")

test_that("single species", {
  # skip if needed
  skip_if_not_installed("fields")
  skip_if_not_installed("smoothr")
  # set parameters
  n <- 1
  set.seed(500)
  # load data
  boundary_data <- sf::read_sf(
    system.file("testdata", "sim_boundary_data.gpkg", package = "aoh")
  )
  habitat_data <- terra::rast(
    system.file("testdata", "sim_habitat_data.tif", package = "aoh")
  )
  elevation_data <- terra::rast(
    system.file("testdata", "sim_elevation_data.tif", package = "aoh")
  )
  # create object
  x <- simulate_spp_data(
    n = n,
    boundary_data = boundary_data,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data
  )
  # tests
  expect_is(x, "list")
  expect_named(x, c("spp_range_data", "spp_habitat_data", "spp_summary_data"))
  validate_range_data(x$spp_range_data, n = n)
  validate_habitat_data(x$spp_habitat_data, n = n)
  validate_summary_data(x$spp_summary_data, n = n)
})

test_that("multiple species", {
  # skip if needed
  skip_if_not_installed("fields")
  skip_if_not_installed("smoothr")
  # set parameters
  n <- 3
  set.seed(500)
  # load data
  boundary_data <- sf::read_sf(
    system.file("testdata", "sim_boundary_data.gpkg", package = "aoh")
  )
  habitat_data <- terra::rast(
    system.file("testdata", "sim_habitat_data.tif", package = "aoh")
  )
  elevation_data <- terra::rast(
    system.file("testdata", "sim_elevation_data.tif", package = "aoh")
  )
  # create object
  x <- simulate_spp_data(
    n = n,
    boundary_data = boundary_data,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data
  )
  # tests
  expect_is(x, "list")
  expect_named(x, c("spp_range_data", "spp_habitat_data", "spp_summary_data"))
  validate_range_data(x$spp_range_data, n = n)
  validate_habitat_data(x$spp_habitat_data, n = n)
  validate_summary_data(x$spp_summary_data, n = n)
})

test_that("built-in data", {
  # skip if needed
  skip_if_not_installed("fields")
  skip_if_not_installed("smoothr")
  skip_if_zenodo_api_not_available()
  skip_if_cached_data_not_available()
  skip_if_zenodo_data_not_available(latest_lumb_cgls_version)
  skip_if_zenodo_data_not_available(latest_elevation_version)
  # set parameters
  n <- 1
  set.seed(500)
  cd <- rappdirs::user_data_dir("aoh")
  # load data
  boundary_data <- sf::read_sf(
    system.file("testdata", "sim_boundary_data.gpkg", package = "aoh")
  )
  # create object
  x <- simulate_spp_data(
    n = n,
    boundary_data = boundary_data,
    cache_dir = cd
  )
  # tests
  expect_is(x, "list")
  expect_named(x, c("spp_range_data", "spp_habitat_data", "spp_summary_data"))
  validate_range_data(x$spp_range_data, n = n)
  validate_habitat_data(x$spp_habitat_data, n = n)
  validate_summary_data(x$spp_summary_data, n = n)
})
