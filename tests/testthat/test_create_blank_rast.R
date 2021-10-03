context("create_blank_rast()")

test_that("expected results", {
  # load data
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, sf::st_crs("ESRI:54017"))
  # tests
  expect_is(
    r <<- create_blank_rast(1000, 2000, sf::st_crs(nc), sf::st_bbox(nc)),
    "SpatRaster"
  )
  expect_equal(terra::xres(r), 1000)
  expect_equal(terra::yres(r), 2000)
  expect_true(sf::st_crs(terra::crs(r)) == sf::st_crs(nc))
  expect_lte(terra::xmin(r), sf::st_bbox(nc)$xmin)
  expect_lte(terra::ymin(r), sf::st_bbox(nc)$ymin)
  expect_gte(terra::xmax(r), sf::st_bbox(nc)$xmax)
  expect_gte(terra::ymax(r), sf::st_bbox(nc)$ymax)
})
