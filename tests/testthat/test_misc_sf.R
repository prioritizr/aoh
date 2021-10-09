context("misc_sf")

test_that("read_sf_n()", {
  f <- system.file("shape/nc.shp", package = "sf")
  expect_equal(sf::read_sf(f), read_sf_n(f))
  expect_equal(sf::read_sf(f)[1:5, ], read_sf_n(f, n = 5))
})

test_that("sf_terra_ext()", {
  # create data
  x <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  bb <- sf::st_bbox(x)
  # tests
  expect_equal(
    as.list(sf_terra_ext(x)),
    as.list(terra::ext(c(bb$xmin, bb$xmax, bb$ymin, bb$ymax)))
  )
})
