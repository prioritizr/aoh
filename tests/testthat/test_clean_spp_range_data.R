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

test_that("IUCN Red List amphibian data", {
  # skip if needed
  skip_on_cran()
  skip_if_iucn_red_list_data_not_available("AMPHIBIANS.zip")
  # find data
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "AMPHIBIANS.zip"
  )
  # tests
  expect_is(x <<- clean_spp_range_data(read_spp_range_data(f, n = 20)), "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs("ESRI:54017"))
  expect_equal(anyDuplicated(x$aoh_id), 0L)
})

test_that("IUCN Red List reptile data", {
  # skip if needed
  skip_on_cran()
  skip_if_iucn_red_list_data_not_available("REPTILES.zip")
  # find data
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "REPTILES.zip"
  )
  # tests
  expect_is(x <<- clean_spp_range_data(read_spp_range_data(f, n = 20)), "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs("ESRI:54017"))
  expect_equal(anyDuplicated(x$aoh_id), 0L)
})

test_that("IUCN Red List terrestrial mammal data", {
  # skip if needed
  skip_on_cran()
  skip_if_iucn_red_list_data_not_available("MAMMALS_TERRESTRIAL_ONLY.zip")
  # find data
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "MAMMALS_TERRESTRIAL_ONLY.zip"
  )
  # tests
  expect_is(x <<- clean_spp_range_data(read_spp_range_data(f, n = 20)), "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs("ESRI:54017"))
  expect_equal(anyDuplicated(x$aoh_id), 0L)
})
