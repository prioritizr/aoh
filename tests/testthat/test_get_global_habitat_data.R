context("get_global_habitat_data()")

test_that("expected results", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  # tests
  expect_is(
    d <<- get_global_habitat_data(
      version = "10.5281/zenodo.4058356", force = TRUE, verbose = FALSE
    ),
    "SpatRaster"
  )
  expect_is(
    d <<- get_global_habitat_data(
      version = "latest", force = TRUE, verbose = FALSE
    ),
    "SpatRaster"
  )
  expect_true(sf::st_crs(terra::crs(d)) == sf::st_crs(4326))
  expect_lte(terra::xmin(d), -180)
  expect_gte(terra::xmax(d), 180)
  expect_lte(terra::xmin(d), -89)
  expect_gte(terra::ymax(d), 89)
})
