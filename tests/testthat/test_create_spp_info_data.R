context("create_spp_info_data()")

test_that("simulated data", {
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

test_that("example data", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_iucn_key_missing()
  # specify file path
  f <- system.file("extdata", "EXAMPLE_SPECIES.zip", package = "aoh")
  cd <- rappdirs::user_data_dir("aoh")
  # load data
  d <- read_spp_range_data(f)
  # create objects
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
      x$id_no, dir = cd, verbose = interactive()
    ),
    spp_summary_data = get_spp_summary_data(
      x$id_no, dir = cd, verbose = interactive()
    )
  )
})

test_that("some species missing habitat data", {
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
  # create copy of spp_habitat_data with missing habitat data for one species
  spp_id <- unique(spp_habitat_data$id_no)[2]
  spp_habitat_data_alt <- dplyr::bind_rows(
    dplyr::filter(spp_habitat_data, !id_no %in% spp_id),
    dplyr::mutate(
      head(dplyr::filter(spp_habitat_data, id_no %in% spp_id), 1),
      code = NA_integer_
    )
  )
  # create objects
  x1 <- create_spp_info_data(
    x = d,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    verbose = interactive()
  )
  x2 <- create_spp_info_data(
    x = d,
    spp_habitat_data = spp_habitat_data_alt,
    spp_summary_data = spp_summary_data,
    verbose = interactive()
  )
  # tests
  expect_is(x1, "sf")
  expect_is(x2, "sf")
  expect_named(x1, info_names)
  expect_named(x2, info_names)
  expect_equal(
    x1[x1$id_no != spp_id, ],
    x2[x2$id_no != spp_id, ]
  )
  validate_info_data(x1, spp_habitat_data, spp_summary_data)
  validate_info_data(x2, spp_habitat_data_alt, spp_summary_data)
})

test_that("species with reversed elevation limits", {
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
  # create copy of spp_summary_data with reversed elevation data
  spp_summary_data_alt <- spp_summary_data
  spp_summary_data_alt$elevation_lower <- spp_summary_data$elevation_upper
  spp_summary_data_alt$elevation_upper <- spp_summary_data$elevation_lower
  # create objects
  x1 <- create_spp_info_data(
    x = d,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    verbose = interactive()
  )
  x2 <- create_spp_info_data(
    x = d,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data_alt,
    verbose = interactive()
  )
  # tests
  expect_is(x1, "sf")
  expect_is(x2, "sf")
  expect_named(x1, info_names)
  expect_named(x2, info_names)
  expect_equal(x1, x2)
  validate_info_data(x1, spp_habitat_data, spp_summary_data)
  validate_info_data(x2, spp_habitat_data, spp_summary_data_alt)
})
