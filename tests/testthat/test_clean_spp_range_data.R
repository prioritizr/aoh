context("clean_spp_range_data()")

test_that("simulated data", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f <- system.file("extdata", "SIMULATED_SPECIES.zip", package = "aoh")
  # tests
  expect_is(x <<- clean_spp_range_data(read_spp_range_data(f)), "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs("ESRI:54017"))
  expect_equal(anyDuplicated(x$aoh_id), 0L)
})
