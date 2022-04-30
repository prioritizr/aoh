context("collate_spp_extent_data")

test_that("expected result", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f1 <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  f2 <- system.file("testdata", "sim_spp_summary_data.csv", package = "aoh")
  f3 <- system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh")
  # load data
  d <- read_spp_range_data(f1)
  d <- d[, "id_no", drop = FALSE]
  d <- sf::st_transform(d, sf::st_crs("ESRI:54017"))
  # create template raster
  template_data <- terra::rast(
    xmin = -17367531, xmax = 17367569,
    ymin = -6005523, ymax = 7287077,
    res = c(100, 100),
    crs = as.character(sf::st_crs("ESRI:54017"))[[2]]
  )
  # create object
  x <- collate_spp_extent_data(
    x = d,
    template_data = template_data
  )
  # tests
  expect_is(x, "sf")
  expect_named(x, c("id_no", "xmin", "xmax", "ymin", "ymax", "geometry"))
  expect_equal(d$id_no, x$id_no)
  expect_is(x$xmin, "numeric")
  expect_is(x$xmax, "numeric")
  expect_is(x$ymin, "numeric")
  expect_is(x$ymax, "numeric")
  expect_true(all(is.finite(x$xmin)))
  expect_true(all(is.finite(x$xmax)))
  expect_true(all(is.finite(x$ymin)))
  expect_true(all(is.finite(x$ymax)))
})
