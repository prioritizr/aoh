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

test_that("simulated data (current BirdLife format)", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  # import data
  d <- read_spp_range_data(f)
  # rename columns
  d <- dplyr::rename(d, SISID = "id_no")
  d <- dplyr::rename(d, SCINAME = "binomial")
  d <- dplyr::rename(d, PRESENC = "presence")
  d <- dplyr::rename(d, ORIGIN = "origin")
  d <- dplyr::rename(d, SEASONA = "seasonal")
  # remove columns
  d <- dplyr::select(d, SISID, SCINAME, PRESENC, ORIGIN, SEASONA)
  # create object
  x <- clean_spp_range_data(d)
  # tests
  expect_is(x, "sf")
  expect_named(x, cleaned_names)
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs("ESRI:54017"))
  expect_equal(anyDuplicated(x$aoh_id), 0L)
})

test_that("simulated data (old BirdLife format)", {
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

test_that("custom presence, origin, and seasonal codes", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  presence <- c(1, 3)
  origin <- c(1, 5)
  season <- c(1, 5)
  # create object
  d <- read_spp_range_data(f)
  x <- clean_spp_range_data(
    x = d,
    keep_iucn_rl_presence = presence,
    keep_iucn_rl_origin = origin,
    keep_iucn_rl_season = season
  )
  # tests
  expect_is(x, "sf")
  expect_gt(nrow(x), 1)
  expect_equal(unique(d$id_no), unique(x$id_no))
  expect_true(sf::st_crs(x) == st_crs("ESRI:54017"))
  expect_equal(anyDuplicated(x$aoh_id), 0L)
  expect_named(x, cleaned_names)
  expect_true(all(x$seasonal %in% season))
  for (i in unique(d$id_no)) {
    di <- d[d$id_no == i, drop = FALSE]
    xi <- x[x$id_no == i, drop = FALSE]
    j <- which(!(di$presence %in% presence) & (di$origin %in% origin))
    di <- sf::st_transform(d[j, drop = FALSE], sf::st_crs(xi))
    int <- sf::st_overlaps(di, xi, sparse = FALSE)
    expect_false(any(c(int)))
  }
})

test_that("amphibian data (IUCN format)", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("prepr")
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
  skip_if_not_installed("prepr")
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
  skip_if_not_installed("prepr")
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
  skip_if_not_installed("prepr")
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

test_that("mammal dateline wrapping issues (IUCN format)", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("prepr")
  skip_if_iucn_red_list_data_not_available("MAMMALS_TERRESTRIAL_ONLY.zip")
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "MAMMALS_TERRESTRIAL_ONLY.zip"
  )
  # import data
  ids <- c(951, 4975, 6568, 6569, 13451)
  d <- read_spp_range_data(f)
  d <- d[which(d$id_no %in% ids), , drop = FALSE]
  # create data
  x <- clean_spp_range_data(d)
  # create polygon to see if species ranges cross the oceans due to
  # dateline wrapping issues
  pl <- structure(c(
      -35.718741, -35.718741, -19.406233, -19.406233, -35.718741,
      -53.789148, 85, 85, -53.789148, -53.789148),
      .Dim = c(5L, 2L))
  pl <- sf::st_sfc(st_polygon(list(pl)), crs = 4326)
  pl <- sf::st_transform(pl, st_crs("ESRI:54017"))
  # tests
  expect_is(x, "sf")
  expect_gte(nrow(x), 1)
  expect_equal(nrow(x), nrow(d))
  expect_true(sf::st_crs(x) == st_crs("ESRI:54017"))
  expect_named(x, cleaned_names)
  expect_false(
    any(c(sf::st_intersects(x, pl, sparse = FALSE)))
  )
})
