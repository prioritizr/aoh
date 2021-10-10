context("clean_spp_range_data()")

test_that("simulated data", {
  expect_true(TRUE) # TODO
})

# The following tests use IUCN Red List data that are downloaded from a private
# data repository. To ensure compliance with IUCN Red List data licenses,
# this data repository requires valid user credentials for access.
# Thus the IUCN Red List data are NOT distributed with the package and
# they are NOT publicly accessible.
# If you wish to run these tests locally, you need to download the relevant IUCN
# datasets manually (from https://www.iucnredlist.org/) and copy the
# files into the folder at the following location:
# rappdirs::user_data_dir("iucn-red-list-data")

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
