context("simulate_spp_data()")

test_that("single species", {
  # skip if needed
  skip_if_not_installed("smoothr")
  skip_if_not_installed("RandomFields")
  # set parameters
  n <- 1
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
  x <- simulate_spp_data(n, boundary_data, habitat_data, elevation_data)
  # tests
  expect_is(x, "list")
  expect_named(x, c("spp_range_data", "spp_habitat_data", "spp_summary_data"))
  validate_range_data(x$spp_range_data, n = n)
  validate_habitat_data(x$spp_habitat_data, n = n)
  validate_summary_data(x$spp_summary_data, n = n)
})

test_that("multiple species", {
  # skip if needed
  skip_if_not_installed("smoothr")
  skip_if_not_installed("RandomFields")
  # set parameters
  n <- 5
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
  x <- simulate_spp_data(n, boundary_data, habitat_data, elevation_data)
  # tests
  expect_is(x, "list")
  expect_named(x, c("spp_range_data", "spp_habitat_data", "spp_summary_data"))
  validate_range_data(x$spp_range_data, n = n)
  validate_habitat_data(x$spp_habitat_data, n = n)
  validate_summary_data(x$spp_summary_data, n = n)
})

test_that("global elevation and habitat data", {
  # skip if needed
  skip_if_not_installed("smoothr")
  skip_if_not_installed("RandomFields")
  # set parameters
  set.seed(500)
  RandomFields::RFoptions(seed = 500)
  n <- 5
  hv <- "10.5281/zenodo.3816946"
  cd <- rappdirs::user_data_dir("aoh")
  # create data
  boundary_data <- sf::read_sf(
    system.file("testdata", "sim_boundary_data.gpkg", package = "aoh")
  )
  # create object
  x <- simulate_spp_data(n, boundary_data, habitat_version = hv, cache_dir = cd)
  # tests
  expect_is(x, "list")
  expect_named(x, c("spp_range_data", "spp_habitat_data", "spp_summary_data"))
  validate_range_data(x$spp_range_data, n = n)
  validate_habitat_data(x$spp_habitat_data, n = n)
  validate_summary_data(x$spp_summary_data, n = n)
})
