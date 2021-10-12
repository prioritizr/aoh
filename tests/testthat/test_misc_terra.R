context("misc_terra")

test_that("create_template_rast()", {
  # load data
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, sf::st_crs("ESRI:54017"))
  # create object
  x <- create_template_rast(1000, 2000, sf::st_crs(nc), sf::st_bbox(nc))
  # tests
  expect_is(x, "SpatRaster")
  expect_equal(terra::xres(x), 1000)
  expect_equal(terra::yres(x), 2000)
  expect_true(sf::st_crs(terra::crs(x)) == sf::st_crs(nc))
  expect_lte(terra::xmin(x), sf::st_bbox(nc)$xmin)
  expect_lte(terra::ymin(x), sf::st_bbox(nc)$ymin)
  expect_gte(terra::xmax(x), sf::st_bbox(nc)$xmax)
  expect_gte(terra::ymax(x), sf::st_bbox(nc)$ymax)
})

test_that("terra_st_crs()", {
  # create object
  x <- terra::rast(
    ncols = 40, nrows = 40, xmin = -110, xmax = -90, ymin = 40, ymax = 60,
    crs = "+proj=longlat +datum=WGS84"
  )
  # tests
  expect_true(sf::st_crs("+proj=longlat +datum=WGS84") == terra_st_crs(x))
})

test_that("terra_st_bbox()", {
  # create objects
  x <- terra::rast(
     ncols = 40, nrows = 40, xmin = -110, xmax = -90, ymin = 40, ymax = 60,
     crs = "+proj=longlat +datum=WGS84"
  )
  bb <- sf::st_bbox(
    c(
      xmin = terra::xmin(x),
      xmax = terra::xmax(x),
      ymin = terra::ymin(x),
      ymax = terra::ymax(x)
    )
  )
  # tests
  expect_equal(as.list(bb), as.list(terra_st_bbox(x)))
})
