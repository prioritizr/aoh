context("get_global_elevation_data()")

test_that("expected results", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  # tests
  expect_is(
    d <<- get_global_elevation_data(force = TRUE, verbose = FALSE),
    "SpatRaster"
  )
  expect_true(sf::st_crs(terra::crs(d)) == sf::st_crs(4326))
  expect_lte(terra::xmin(d), -180)
  expect_gte(terra::xmax(d), 180)
  expect_lte(terra::xmin(d), -55)
  expect_gte(terra::ymax(d), 83)
})
