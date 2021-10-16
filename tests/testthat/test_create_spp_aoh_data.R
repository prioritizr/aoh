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
  # load data
  d <- read_spp_range_data(f)
  # create objects
  x <- create_spp_aoh_data(
    x = d,
    output_dir = tempdir(),
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    verbose = FALSE
  )
  # tests
  expect_is(x, "sf")
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_named(
    x,
    c(
      "id_no", "binomial", "seasonal", "full_habitat_code", "habitat_code",
      "elevation_lower", "elevation_upper", "xmin", "xmax", "ymin", "ymax",
      "path", "geometry"
     )
  )
  expect_is(x$id_no, "integer")
  expect_is(x$binomial, "character")
  expect_is(x$seasonal, "integer")
  expect_is(x$habitat_code, "character")
  expect_is(x$elevation_lower, "numeric")
  expect_is(x$elevation_upper, "numeric")
  expect_is(x$elevation_upper, "numeric")
  expect_is(x$elevation_upper, "numeric")
  expect_is(x$elevation_upper, "numeric")
  expect_is(x$elevation_upper, "numeric")
  expect_is(x$path, "character")
  expect_equal(sum(is.na(x$path)), 0)
})

test_that("some species missing habitat data", {
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
  # load data
  d <- read_spp_range_data(f)
  # create copy of spp_habitat_data with missing habitat data for one species
  spp_id <- unique(spp_habitat_data$id_no)[2]
  spp_habitat_data_alt <- dplyr::bind_rows(
    dplyr::filter(spp_habitat_data, !id_no %in% spp_id),
    dplyr::mutate(
      head(dplyr::filter(spp_habitat_data, id_no %in% spp_id), 1),
      code = NA_integer_
    )
  )
  # create output dirs
  output_dir1 <- tempfile()
  output_dir2 <- tempfile()
  dir.create(output_dir1, showWarnings = FALSE, recursive = TRUE)
  dir.create(output_dir2, showWarnings = FALSE, recursive = TRUE)
  # create objects
  x1 <- create_spp_aoh_data(
    x = d,
    output_dir = output_dir1,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    force = TRUE,
    verbose = FALSE
  )
  x2 <- create_spp_aoh_data(
    x = d,
    output_dir = output_dir2,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    spp_habitat_data = spp_habitat_data_alt,
    spp_summary_data = spp_summary_data,
    force = TRUE,
    verbose = FALSE
  )
  # tests
  expect_equal(
    dplyr::select(x1[x1$id_no != spp_id, ], -path),
    dplyr::select(x2[x2$id_no != spp_id, ], -path)
  )
  expect_equal(
    lapply(
      x1$path[x1$id_no != spp_id],
      function(x) terra::values(terra::rast(x))
    ),
    lapply(
      x2$path[x2$id_no != spp_id],
      function(x) terra::values(terra::rast(x))
    )
  )
  expect_equal(x2$habitat_code[x2$id_no == spp_id], "")
  expect_equal(x2$path[x2$id_no == spp_id], NA_character_)
  # clean up
  unlink(output_dir1, recursive = TRUE)
  unlink(output_dir2, recursive = TRUE)
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
  # load data
  d <- read_spp_range_data(f)
  # create objects
  x1 <- create_spp_aoh_data(
    x = d,
    output_dir = tempdir(),
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    verbose = FALSE
  )
  x2 <- create_spp_aoh_data(
    x = d,
    output_dir = tempdir(),
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    parallel_n_threads = 2,
    parallel_cluster = "PSOCK",
    verbose = FALSE
  )
  # tests
  expect_is(x1, "sf")
  expect_is(x2, "sf")
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
  # load data
  d <- read_spp_range_data(f)
  # create objects
  x1 <- create_spp_aoh_data(
    x = d,
    output_dir = tempdir(),
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    verbose = FALSE
  )
  x2 <- create_spp_aoh_data(
    x = d,
    output_dir = tempdir(),
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    parallel_n_threads = 2,
    parallel_cluster = "PSOCK",
    verbose = FALSE
  )
  # tests
  expect_is(x1, "sf")
  expect_is(x2, "sf")
  expect_equal(x1, x2)
  expect_true(all(sapply(seq_len(nrow(x1)), function(i) {
    all(
      terra::values(terra::rast(x1$path[[i]])) ==
      terra::values(terra::rast(x2$path[[i]])),
      na.rm = TRUE
    )
  })))
})

test_that("example data", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f <- system.file("extdata", "EXAMPLE_SPECIES.zip", package = "aoh")
  cd <- rappdirs::user_data_dir("aoh")
  hv <- "10.5281/zenodo.3816946"
  # load data
  d <- read_spp_range_data(f)
  # create objects
  x <- suppressWarnings(
    create_spp_aoh_data(
      x = d,
      output_dir = tempdir(),
      cache_dir = cd,
      habitat_version = hv,
      parallel_n_threads = 2,
      verbose = FALSE
    )
  )
  # tests
  expect_is(x, "sf")
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_is(x$path, "character")
  expect_equal(sum(is.na(x$path)), 0)
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
  # load data
  d <- read_spp_range_data(f, n = 10)
  # create objects
  x <- create_spp_aoh_data(
    d, tempdir(), habitat_version = hv, cache_dir = cd, verbose = FALSE
  )
  # tests
  expect_is(x, "sf")
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
  # load data
  d <- read_spp_range_data(f, n = 10)
  # create objects
  x <- create_spp_aoh_data(
    d, tempdir(), habitat_version = hv, cache_dir = cd, verbose = FALSE
  )
  # tests
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
  # load data
  d <- read_spp_range_data(f, n = 10)
  # create objects
  x <- create_spp_aoh_data(
    d, tempdir(), habitat_version = hv, cache_dir = cd, verbose = FALSE
  )
  # tests
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_is(x$path, "character")
  expect_equal(sum(is.na(x$path)), 0)
})
