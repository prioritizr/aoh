context("world_berhman_1km_rast()")

test_that("expected results", {
  expect_is((r <<- get_world_berhman_1km_rast()), "SpatRaster")
  expect_equal(terra::xres(r), 1000)
  expect_equal(terra::yres(r), 1000)
  expect_true(sf::st_crs(terra::crs(r)), sf::st_crs("ESRI:54017"))
})
