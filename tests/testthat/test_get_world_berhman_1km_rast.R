context("world_behrmann_1km_rast()")

test_that("expected results", {
  # create object
  x <- get_world_behrmann_1km_rast()
  # tests
  expect_is(x, "SpatRaster")
  expect_equal(terra::xres(x), 1000)
  expect_equal(terra::yres(x), 1000)
  expect_true(sf::st_crs(terra::crs(x)) == sf::st_crs("ESRI:54017"))
})
