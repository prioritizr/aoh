context("format_spp_data")

test_that("correct format", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f1 <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  f2 <- system.file("testdata", "sim_spp_summary_data.csv", package = "aoh")
  f3 <- system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh")
  # load data
  d1 <- read_spp_range_data(f1)
  d2 <- utils::read.table(
    f2, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  d3 <- utils::read.table(
    f3, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  # create objects
  x <- format_spp_data(
    x = d1,
    spp_summary_data = d2,
    spp_habitat_data = d3,
    verbose = FALSE
  )
  # tests
  expect_is(x, "sf")
  expect_true(assertthat::has_name(x, "aoh_id"))
  expect_is(x$aoh_id, "character")
  expect_equal(anyDuplicated(x$aoh_id), 0L)
  expect_true(assertthat::has_name(x, "id_no"))
  expect_is(x$id_no, "integer")
  expect_true(all(x$id_no %in% d1$id_no))
  expect_true(assertthat::has_name(x, "seasonal"))
  expect_is(x$seasonal, "integer")
  expect_true(all(x$seasonal %in% seq_len(5)))
  expect_true(assertthat::has_name(x, "habitat_code"))
  expect_is(x$habitat_code, "list")
  expect_true(assertthat::has_name(x, "elevation_lower"))
  expect_is(x$elevation_lower, "numeric")
  expect_true(assertthat::noNA(x$elevation_lower))
  expect_true(assertthat::has_name(x, "elevation_upper"))
  expect_is(x$elevation_upper, "numeric")
  expect_true(assertthat::noNA(x$elevation_upper))
})

test_that("correct handling of NAs in in spp_habitat_data", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f1 <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  f2 <- system.file("testdata", "sim_spp_summary_data.csv", package = "aoh")
  f3 <- system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh")
  # load data
  d1 <- read_spp_range_data(f1)
  d2 <- utils::read.table(
    f2, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  d3 <- utils::read.table(
    f3, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  # prepare data
  d1 <- d1[1, , drop = FALSE]
  d2 <- d2[d2$id_no %in% d1$id_no, , drop = FALSE]
  d3 <- d3[d3$id_no %in% d1$id_no, , drop = FALSE]
  d3_alt <- d3
  d3$season <- NA_character_
  # create objects
  x1 <- format_spp_data(
    x = d1,
    spp_summary_data = d2,
    spp_habitat_data = d3,
    verbose = FALSE
  )
  x2 <- format_spp_data(
    x = d1,
    spp_summary_data = d2,
    spp_habitat_data = d3_alt,
    verbose = FALSE
  )
  # tests
  expect_is(x1, "sf")
  expect_is(x2, "sf")
  expect_equal(x1, x2)
  expect_equal(x1$habitat_code, list(as.character(d3$code)))
})
