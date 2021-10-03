context("read_spp_range_data()")

# NOTICE
#
# The IUCN Red List data are downloaded from a private data repository
# that requires valid GitHub user credentials for access.
# Thus these datasets are NOT distributed with the package.
# They are also NOT publicly accessible either.
# If you wish to run these tests locally, you need to download the relevant IUCN
# datasets manually (from https://www.iucnredlist.org/) and copy the
# files into the folder at the following location:
# rappdirs::user_data_dir("iucn-red-list-data")

test_that("simulated data", {
  expect_true(TRUE) # TODO
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
