context("read_spp_range_data()")

test_that("simulated data (shapefile zip format)", {
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
  expect_true(all(assertthat::has_name(x, iucn_names)))
  expect_equal(dplyr::last(names(x)), "geometry")
})

test_that("simulated data (shapefile zip format, multiple files)", {
  # unzip data
  td1 <- tempfile()
  dir.create(td1, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(
    system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh"),
    exdir = td1
  )
  o <- sf::read_sf(dir(td1, pattern = "^.*\\.shp$", full.names = TRUE))
  # create zip with 2 shapefiles
  td2 <- tempfile()
  dir.create(td2, showWarnings = FALSE, recursive = TRUE)
  n <- nrow(o)
  m <- median(seq_len(n))
  sf::write_sf(
    o[seq(1, m - 1), , drop = FALSE],
    paste0(td2, "/SIMULATED_SPECIES1.shp")
  )
  sf::write_sf(
    o[seq(m, n), , drop = FALSE],
    paste0(td2, "/SIMULATED_SPECIES2.shp")
  )
  f <- tempfile(fileext = ".zip")
  withr::with_dir(td2, {
    utils::zip(zipfile = f, files = dir(td2))
  })
  # create data
  x <- suppressWarnings(read_spp_range_data(f))
  # tests
  expect_is(x, "sf")
  expect_equal(nrow(x), nrow(o))
  expect_equal(x$id_no, o$id_no)
  expect_true(sf::st_crs(x) == st_crs(4326))
  expect_true(all(assertthat::has_name(x, iucn_names)))
  expect_equal(dplyr::last(names(x)), "geometry")
  # clean up
  unlink(td1, force = TRUE, recursive = TRUE)
  unlink(td2, force = TRUE, recursive = TRUE)
  unlink(f, force = TRUE, recursive = TRUE)
})

test_that("simulated data (shapefile 7z format)", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("archive")
  # create 7z archive
  td <- tempfile()
  dir.create(td, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(
    system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh"),
    exdir = td
  )
  f <- tempfile(fileext = ".7z")
  withr::with_dir(td, {
    archive::archive_write_files(archive = f, files = dir(td), format = "7zip")
  })
  # create data
  x <- suppressWarnings(read_spp_range_data(f))
  # tests
  expect_is(x, "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs(4326))
  expect_true(all(assertthat::has_name(x, iucn_names)))
  expect_equal(dplyr::last(names(x)), "geometry")
  # clean up
  unlink(td, force = TRUE, recursive = TRUE)
  unlink(f, force = TRUE, recursive = TRUE)
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
  x <- read_spp_range_data(f, n = 10)
  # tests
  expect_is(x, "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs(4326))
  expect_true(all(assertthat::has_name(x, iucn_names)))
  expect_equal(dplyr::last(names(x)), "geometry")
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
  x <- read_spp_range_data(f, n = 10)
  # tests
  expect_is(x, "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs(4326))
  expect_true(all(assertthat::has_name(x, iucn_names)))
  expect_equal(dplyr::last(names(x)), "geometry")
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
  x <- read_spp_range_data(f, n = 10)
  # tests
  expect_is(x, "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs(4326))
  expect_true(all(assertthat::has_name(x, iucn_names)))
  expect_equal(dplyr::last(names(x)), "geometry")
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
  x <- suppressWarnings(read_spp_range_data(f, n = 10))
  # tests
  expect_is(x, "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs(4326))
  expect_true(all(assertthat::has_name(x, birdlife_names)))
  expect_equal(dplyr::last(names(x)), "geometry")
})

test_that("bird data (alternate BirdLife format)", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("archive")
  skip_if_iucn_red_list_data_not_available("BOTW_2021.7z")
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "BOTW_2021.7z"
  )
  # create data
  x <- suppressWarnings(read_spp_range_data(f, n = 10))
  # tests
  expect_is(x, "sf")
  expect_gt(nrow(x), 1)
  expect_true(sf::st_crs(x) == st_crs(4326))
  expect_true(all(assertthat::has_name(x, alt_birdlife_names)))
  expect_equal(dplyr::last(names(x)), "geometry")
})
