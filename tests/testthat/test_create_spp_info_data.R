context("create_spp_info_data()")

test_that("simulated data (default arguments)", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
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
  x <- create_spp_info_data(
    x = d,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    verbose = TRUE
  )
  # tests
  expect_is(x, "sf")
  expect_named(x, info_names)
  validate_info_data(x, spp_habitat_data, spp_summary_data)
})

test_that("simulated data (crs = EPSG:4326)", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
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
  x <- create_spp_info_data(
    x = d,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    crs = sf::st_crs(4326),
    verbose = FALSE
  )
  # tests
  expect_is(x, "sf")
  expect_named(x, info_names)
  validate_info_data(x, spp_habitat_data, spp_summary_data)
})

test_that("example data", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("vcr")
  skip_if_cached_data_not_available()
  skip_if_zenodo_data_not_available(latest_lumb_cgls_version)
  skip_if_zenodo_data_not_available(latest_elevation_version)
  # specify file path
  f <- system.file("extdata", "EXAMPLE_SPECIES.zip", package = "aoh")
  cd <- tempfile()
  dir.create(cd, showWarnings = FALSE, recursive = TRUE)
  # load data
  d <- read_spp_range_data(f)
  # create objects
  vcr::local_cassette("example-info")
  version <- rredlist::rl_version()
  x <- suppressWarnings(
    create_spp_info_data(
      x = d,
      cache_dir = cd,
      verbose = interactive()
    )
  )
  # tests
  expect_is(x, "sf")
  expect_named(x, info_names)
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_equal(d$id_no, x$id_no)
  validate_info_data(
    x = x,
    spp_habitat_data = get_spp_habitat_data(
      x$id_no, dir = cd, version = version, verbose = interactive()
    ),
    spp_summary_data = get_spp_summary_data(
      x$id_no, dir = cd, version = version, verbose = interactive()
    )
  )
  unlink(cd, force = TRUE, recursive = TRUE)
})
