context("create_spp_aoh_data()")

test_that("simulated data", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  elevation_data <- terra::rast(
    system.file("testdata", "sim_elevation_data.tif", package = "aoh")
  )
  habitat_data <- terra::rast(
    system.file("testdata", "sim_habitat_data.tif", package = "aoh")
  )
  spp_habitat_data <- read.csv(
    system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh"),
    sep = ",", header = TRUE
  )
  spp_summary_data <- read.csv(
    system.file("testdata", "sim_spp_summary_data.csv", package = "aoh"),
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
    "sf"
  )
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_true(assertthat::has_name(x, "id_no"))
  expect_is(x$id_no, "integer")
  expect_true(assertthat::has_name(x, "binomial"))
  expect_is(x$binomial, "character")
  expect_true(assertthat::has_name(x, "seasonal"))
  expect_is(x$seasonal, "integer")
  expect_true(assertthat::has_name(x, "habitat_code"))
  expect_is(x$habitat_code, "character")
  expect_true(assertthat::has_name(x, "elevation_lower"))
  expect_is(x$elevation_lower, "numeric")
  expect_true(assertthat::has_name(x, "elevation_upper"))
  expect_is(x$elevation_upper, "numeric")
  expect_true(assertthat::has_name(x, "xmin"))
  expect_is(x$elevation_upper, "numeric")
  expect_true(assertthat::has_name(x, "xmax"))
  expect_is(x$elevation_upper, "numeric")
  expect_true(assertthat::has_name(x, "ymin"))
  expect_is(x$elevation_upper, "numeric")
  expect_true(assertthat::has_name(x, "ymax"))
  expect_is(x$elevation_upper, "numeric")
  expect_true(assertthat::has_name(x, "path"))
  expect_is(x$path, "character")
  expect_equal(sum(is.na(x$path)), 0)
})

test_that("example data", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  # specify file path
  f <- system.file("extdata", "EXAMPLE_SPECIES.zip", package = "aoh")
  cd <- rappdirs::user_data_dir("aoh")
  hv <- "10.5281/zenodo.3816946"
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

test_that("PSOCK parallel processing", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  elevation_data <- terra::rast(
    system.file("testdata", "sim_elevation_data.tif", package = "aoh")
  )
  habitat_data <- terra::rast(
    system.file("testdata", "sim_habitat_data.tif", package = "aoh")
  )
  spp_habitat_data <- read.csv(
    system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh"),
    sep = ",", header = TRUE
  )
  spp_summary_data <- read.csv(
    system.file("testdata", "sim_spp_summary_data.csv", package = "aoh"),
    sep = ",", header = TRUE
  )
  # tests
  expect_is(d <<- read_spp_range_data(f), "sf")
  expect_is(
    x1 <<- create_spp_aoh_data(
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
  expect_is(
    x2 <<- create_spp_aoh_data(
      x = d,
      output_dir = tempdir(),
      habitat_data = habitat_data,
      elevation_data = elevation_data,
      spp_habitat_data = spp_habitat_data,
      spp_summary_data = spp_summary_data,
      parallel_n_threads = 2,
      parallel_cluster = "PSOCK",
      verbose = FALSE
    ),
    "tbl_df"
  )
  expect_equal(x1, x2)
  expect_true(all(sapply(seq_len(nrow(x1)), function(i) {
    all(
      terra::values(terra::rast(x1$path[[i]])) ==
      terra::values(terra::rast(x2$path[[i]])),
      na.rm = TRUE
    )
  })))
})

test_that("FORK parallel processing", {
  # skip if needed
  skip_on_cran()
  skip_on_os("windows")
  # specify file path
  f <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  elevation_data <- terra::rast(
    system.file("testdata", "sim_elevation_data.tif", package = "aoh")
  )
  habitat_data <- terra::rast(
    system.file("testdata", "sim_habitat_data.tif", package = "aoh")
  )
  spp_habitat_data <- read.csv(
    system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh"),
    sep = ",", header = TRUE
  )
  spp_summary_data <- read.csv(
    system.file("testdata", "sim_spp_summary_data.csv", package = "aoh"),
    sep = ",", header = TRUE
  )
  # tests
  expect_is(d <<- read_spp_range_data(f), "sf")
  expect_is(
    x1 <<- create_spp_aoh_data(
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
  expect_is(
    x2 <<- create_spp_aoh_data(
      x = d,
      output_dir = tempdir(),
      habitat_data = habitat_data,
      elevation_data = elevation_data,
      spp_habitat_data = spp_habitat_data,
      spp_summary_data = spp_summary_data,
      parallel_n_threads = 2,
      parallel_cluster = "FORK",
      verbose = FALSE
    ),
    "tbl_df"
  )
  expect_equal(x1, x2)
  expect_true(all(sapply(seq_len(nrow(x1)), function(i) {
    all(
      terra::values(terra::rast(x1$path[[i]])) ==
      terra::values(terra::rast(x2$path[[i]])),
      na.rm = TRUE
    )
  })))
})

test_that("amphibian data", {
  # skip if needed
  skip_on_cran()
  skip_on_local()
  skip_if_iucn_red_list_data_not_available("AMPHIBIANS.zip")
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "AMPHIBIANS.zip"
  )
  cd <- rappdirs::user_data_dir("aoh")
  hv <- "10.5281/zenodo.3816946"
  # tests
  expect_is((d <<- read_spp_range_data(f, n = 10)), "sf")
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
  skip_on_local()
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
  skip_on_local()
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
