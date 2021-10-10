context("create_spp_aoh_data()")

test_that("simulated data", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f <- system.file("extdata", "SIMULATED_SPECIES.zip", package = "aoh")
  elevation_data <- terra::rast(
    system.file("extdata", "sim_elevation_data.tif", package = "aoh")
  )
  habitat_data <- terra::rast(
    system.file("extdata", "sim_habitat_data.tif", package = "aoh")
  )
  spp_habitat_data <- read.csv(
    system.file("extdata", "sim_spp_habitat_data.csv", package = "aoh"),
    sep = ",", header = TRUE
  )
  spp_summary_data <- read.csv(
    system.file("extdata", "sim_spp_summary_data.csv", package = "aoh"),
    sep = ",", header = TRUE
  )
  # tests
  expect_is(d <<- read_spp_range_data(f), "sf")
  expect_is(
    x <<- create_spp_aoh_data(
      x = d,
      output_dir = tempdir(),
      habitat_data = habitat_data,
      elevation_data = elevation_data,
      spp_habitat_data = spp_habitat_data,
      spp_summary_data = spp_summary_data,
      verbose = FALSE
    ),
    "tbl_df"
  )
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_is(x$path, "character")
  expect_equal(sum(is.na(x$path)), 0)
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
  expect_is((d <<- read_spp_range_data(f, n = 5)), "sf")
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
  expect_is((d <<- read_spp_range_data(f, n = 5)), "sf")
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
  expect_is((d <<- read_spp_range_data(f, n = 5)), "sf")
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
