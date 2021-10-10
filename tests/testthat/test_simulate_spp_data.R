context("simulate_spp_data()")

test_that("single species [manually specified data]", {
  # create data
  boundary_data <- sf::read_sf(
    system.file("extdata", "sim_boundary_data.gpkg", package = "aoh")
  )
  habitat_data <- terra::rast(
    system.file("extdata", "sim_habitat_data.tif", package = "aoh")
  )
  elevation_data <- terra::rast(
    system.file("extdata", "sim_elevation_data.tif", package = "aoh")
  )
  # tests
  expect_is(
    x <<- simulate_spp_data(1, boundary_data, habitat_data, elevation_data),
    "list"
  )
  expect_is(x$range_data, "sf")
  expect_is(x$habitat_data, "tbl_df")
  expect_is(x$summary_data, "tbl_df")
})

test_that("multiple species [manually specified data]", {
  # create data
  boundary_data <- sf::read_sf(
    system.file("extdata", "sim_boundary_data.gpkg", package = "aoh")
  )
  habitat_data <- terra::rast(
    system.file("extdata", "sim_habitat_data.tif", package = "aoh")
  )
  elevation_data <- terra::rast(
    system.file("extdata", "sim_elevation_data.tif", package = "aoh")
  )
  # tests
  expect_is(
    x <<- simulate_spp_data(5, boundary_data, habitat_data, elevation_data),
    "list"
  )
  expect_is(x$range_data, "sf")
  expect_is(x$habitat_data, "tbl_df")
  expect_is(x$summary_data, "tbl_df")
})

test_that("multiple species [automatic specified data]", {
  # create data
  cd <- rappdirs::user_data_dir("aoh")
  boundary_data <- sf::read_sf(
    system.file("extdata", "sim_boundary_data.gpkg", package = "aoh")
  )
  # tests
  expect_is(
    x <<- simulate_spp_data(5, boundary_data, cache_dir = cd),
    "list"
  )
  expect_is(x$range_data, "sf")
  expect_is(x$habitat_data, "tbl_df")
  expect_is(x$summary_data, "tbl_df")
})
