context("clean_spp_range_data()")

test_that("simulated data (IUCN format)", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  # create object
  d <- read_spp_range_data(f)
  x <- clean_spp_range_data(d)
  # tests
  expect_is(x, "sf")
  expect_gt(nrow(x), 1)
  expect_equal(unique(d$id_no), unique(x$id_no))
  expect_true(sf::st_crs(x) == st_crs("ESRI:54017"))
  expect_equal(anyDuplicated(x$aoh_id), 0L)
  expect_named(x, cleaned_names)
})

test_that("simulated data (BirdLife format)", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  # import data
  d <- read_spp_range_data(f)
  # rename columns
  d <- dplyr::rename(d, SISID = "id_no")
  d <- dplyr::rename(d, Order_ = "order_")
  d <- dplyr::rename(d, marine_system = "marine")
  d <- dplyr::rename(d, freshwater_system = "freshwater")
  d <- dplyr::rename(d, terrestrial_system = "terrestial")
  d <- dplyr::rename(d, RedListCategory_2021 = "category")
  d <- dplyr::rename(d, FamilyName = "family")
  # remove columns
  d <- dplyr::select(d, -subspecies, -genus, -kingdom, -phylum)
  # reformat columns
  d <- dplyr::mutate(d, marine_system = marine_system == "true")
  d <- dplyr::mutate(d, terrestrial_system = terrestrial_system == "true")
  d <- dplyr::mutate(d, freshwater_system = freshwater_system == "true")
  # create object
  x <- clean_spp_range_data(d)
  # tests
  expect_is(x, "sf")
  expect_named(x, cleaned_names)
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs("ESRI:54017"))
  expect_equal(anyDuplicated(x$aoh_id), 0L)
})

test_that("amphibian data (IUCN format)", {
  # skip if needed
  skip_on_cran()
  skip_if_iucn_red_list_data_not_available("AMPHIBIANS.zip")
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "AMPHIBIANS.zip"
  )
  # create data
  x <- clean_spp_range_data(read_spp_range_data(f, n = 10))
  # tests
  expect_is(x, "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs("ESRI:54017"))
  expect_named(x, cleaned_names)
})

test_that("reptile data (IUCN format)", {
  # skip if needed
  skip_on_cran()
  skip_if_iucn_red_list_data_not_available("REPTILES.zip")
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "REPTILES.zip"
  )
  # create data
  x <- clean_spp_range_data(read_spp_range_data(f, n = 10))
  # tests
  expect_is(x, "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs("ESRI:54017"))
  expect_named(x, cleaned_names)
})

test_that("terrestrial mammal data (IUCN format)", {
  # skip if needed
  skip_on_cran()
  skip_if_iucn_red_list_data_not_available("MAMMALS_TERRESTRIAL_ONLY.zip")
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "MAMMALS_TERRESTRIAL_ONLY.zip"
  )
  # create data
  x <- clean_spp_range_data(read_spp_range_data(f, n = 10))
  # tests
  expect_is(x, "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs("ESRI:54017"))
  expect_named(x, cleaned_names)
})

test_that("bird data (BirdLife format)", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("archive")
  skip_if_iucn_red_list_data_not_available("BOTW.7z")
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "BOTW.7z"
  )
  # create data
  x <- clean_spp_range_data(suppressWarnings(read_spp_range_data(f, n = 10)))
  # tests
  expect_is(x, "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs("ESRI:54017"))
  expect_named(x, cleaned_names)
})
