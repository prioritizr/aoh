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

test_that("terra_fasterize() (small dataset)", {
  skip_on_cran()
  # load data
  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc <- sf::st_transform(nc, sf::st_crs("ESRI:54017"))
  # create template
  r <- terra::rast(
    xmin = -17367531, xmax = 17367569,
    ymin = -6005523, ymax = 7287077,
    res = c(10000, 10000),
    crs = as.character(sf::st_crs("ESRI:54017"))[[2]]
  )
  # create objects
  x <- terra_fasterize(nc, r)
  y <- terra::rasterize(terra::vect(nc), r)
  names(x) <- names(y)
  # compare results
  expect_true(terra::global(abs(x - y), "max", na.rm = TRUE) <= 1e-5)
})

test_that("terra_fasterize() (large dataset)", {
  skip_on_cran()
  skip_if_not_installed("rnaturalearth")
  # load data
  d <- rnaturalearth::ne_countries(type = "countries", returnclass = "sf")
  d <- d[d$region_wb %in% c("Sub-Saharan Africa", "North America"), ]
  d <- sf::st_make_valid(d)
  d <- sf::st_wrap_dateline(d, options = c("WRAPDATELINE=YES"))
  d <- sf::st_make_valid(d)
  d <- sf::st_transform(d, sf::st_crs("ESRI:54017"))
  d <- sf::st_make_valid(d)
  d <- sf::st_union(d)
  d <- sf::st_make_valid(d)
  d <- sf::st_as_sf(d, idx = 1)
  # create template
  r <- terra::rast(
    xmin = -17367531, xmax = 17367569,
    ymin = -6005523, ymax = 7287077,
    res = c(10000, 10000),
    crs = as.character(sf::st_crs("ESRI:54017"))[[2]]
  )
  # create objects
  x <- terra_fasterize(d, r)
  y <- terra::rasterize(terra::vect(d), r)
  names(x) <- names(y)
  # compare results
  expect_true(terra::global(abs(x - y), "max", na.rm = TRUE) <= 1e-5)
})

test_that("terra_fasterize() (touches)", {
  # create data
  sf <- sf::st_sf(
    tibble::tibble(geom = sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857))
  )
  sf <- sf::st_buffer(sf, 10)
  x <- terra::rast(matrix(c(1, 2)))
  terra::ext(x) <- c(xmin = -200, xmax = 200, ymin = -300, ymax = 800)
  terra::crs(x) <- terra::crs(sf)
  # create object
  z1 <- terra_fasterize(sf, x, touches = TRUE)
  z2 <- terra::deepcopy(x)
  terra::values(z2) <- c(NA, 1)
  z3 <- terra_fasterize(sf, x, touches = FALSE)
  z4 <- terra::deepcopy(x)
  terra::values(z4) <- c(NA, NA)
  # tests
  expect_is(z1, "SpatRaster")
  expect_is(z2, "SpatRaster")
  expect_is(z3, "SpatRaster")
  expect_is(z4, "SpatRaster")
  expect_true(terra::compareGeom(z1, z2, stopOnError = FALSE, res = TRUE))
  expect_equal(as.list(terra::ext(z1)), as.list(terra::ext(z2)))
  expect_equivalent(terra::values(z1), terra::values(z2), tolerance = 1e-5)
  expect_true(terra::compareGeom(z3, z4, stopOnError = FALSE, res = TRUE))
  expect_equal(as.list(terra::ext(z3)), as.list(terra::ext(z4)))
  expect_equivalent(terra::values(z3), terra::values(z4), tolerance = 1e-5)
})
