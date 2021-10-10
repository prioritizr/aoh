context("read_spp_range_data()")

test_that("simulated data", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f <- system.file("extdata", "SIMULATED_SPECIES.zip", package = "aoh")
  # tests
  expect_is((x <<- read_spp_range_data(f)), "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs(4326))
})

test_that("amphibian data", {
  # skip if needed
  skip_on_cran()
  skip_if_iucn_red_list_data_not_available("AMPHIBIANS.zip")
  # specify file path
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "AMPHIBIANS.zip"
  )
  # tests
  expect_is((x <<- read_spp_range_data(f, n = 20)), "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs(4326))
})

test_that("reptile data", {
  # skip if needed
  skip_on_cran()
  skip_if_iucn_red_list_data_not_available("REPTILES.zip")
  # specify file path
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "REPTILES.zip"
  )
  expect_is(x <<- read_spp_range_data(f, n = 20), "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs(4326))
})

test_that("terrestrial mammal data", {
  # skip if needed
  skip_on_cran()
  skip_if_iucn_red_list_data_not_available("MAMMALS_TERRESTRIAL_ONLY.zip")
  # specify file path
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "MAMMALS_TERRESTRIAL_ONLY.zip"
  )
  # tests
  expect_is(x <<- read_spp_range_data(f, n = 20), "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs(4326))
})
