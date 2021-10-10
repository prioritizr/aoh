context("create_spp_aoh_data()")

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
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "AMPHIBIANS.zip"
  )
  cd <- rappdirs::user_data_dir("aoh")
  hv <- "10.5281/zenodo.3816946"
  # tests
  expect_is((d <<- read_spp_range_data(f, n = 20)), "sf")
  expect_is(
    x <<- create_spp_aoh_data(
      d, tempdir(), habitat_version = hv, cache_dir = cd, verbose = FALSE
    ),
    "tbl_df"
  )
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_is(x$path, "character")
  expect_equal(sum(is.na(x$path)), 0)
})

test_that("reptile data", {
  # skip if needed
  skip_on_cran()
  skip_if_iucn_red_list_data_not_available("REPTILES.zip")
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "REPTILES.zip"
  )
  cd <- rappdirs::user_data_dir("aoh")
  hv <- "10.5281/zenodo.3816946"
  # tests
  expect_is((d <<- read_spp_range_data(f, n = 20)), "sf")
  expect_is(
    x <<- create_spp_aoh_data(
      d, tempdir(), habitat_version = hv, cache_dir = cd, verbose = FALSE
    ),
    "tbl_df"
  )
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_is(x$path, "character")
  expect_equal(sum(is.na(x$path)), 0)
})

test_that("terrestrial mammal data", {
  # skip if needed
  skip_on_cran()
  skip_if_iucn_red_list_data_not_available("MAMMALS_TERRESTRIAL_ONLY.zip")
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "MAMMALS_TERRESTRIAL_ONLY.zip"
  )
  cd <- rappdirs::user_data_dir("aoh")
  hv <- "10.5281/zenodo.3816946"
  # tests
  expect_is((d <<- read_spp_range_data(f, n = 20)), "sf")
  expect_is(
    x <<- create_spp_aoh_data(
      d, tempdir(), habitat_version = hv, cache_dir = cd, verbose = FALSE
    ),
    "tbl_df"
  )
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_is(x$path, "character")
  expect_equal(sum(is.na(x$path)), 0)
})
