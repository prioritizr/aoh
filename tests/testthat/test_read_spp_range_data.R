context("read_spp_range_data()")

test_that("simulated data", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  # create data
  x <- read_spp_range_data(f)
  # tests
  expect_is(x, "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs(4326))
})
