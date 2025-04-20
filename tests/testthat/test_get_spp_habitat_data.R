context("get_spp_habitat_data()")

test_that("single taxon identifier", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("vcr")
  # set parameters
  data(iucn_habitat_data)
  id_no <- c(18)
  # create object from API
  vcr::use_cassette("habitat-single", {
    x <- get_spp_habitat_data(id_no, force = FALSE, verbose = interactive())
  })
  # create object from cache
  x2 <- get_spp_habitat_data(
    id_no, force = FALSE, verbose = interactive(), key = "asdf",
    version = "2025-1" # matches vcr cassette from previous call
  )
  # tests
  expect_is(x, "data.frame")
  expect_gte(nrow(x), 1)
  expect_named(
    x,
    c("id_no", "code", "habitat", "suitability", "season", "majorimportance")
  )
  expect_true(all(id_no %in% x$id_no))
  expect_true(all(x$code %in% iucn_habitat_data$code))
  expect_equal(x, x2)
})

test_that("multiple taxon identifiers", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("vcr")
  # set parameters
  data(iucn_habitat_data)
  id_no <- c(18, 137, 138, 139)
  # create objects
  vcr::use_cassette("habitat-multiple", {
    x <- get_spp_habitat_data(id_no, force = FALSE, verbose = interactive())
  })
  # tests
  expect_is(x, "data.frame")
  expect_gte(nrow(x), 1)
  expect_named(
    x,
    c("id_no", "code", "habitat", "suitability", "season", "majorimportance")
  )
  expect_true(all(id_no %in% x$id_no))
  expect_true(all(x$code %in% iucn_habitat_data$code))
})

test_that("some taxon missing habitat information", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("vcr")
  # set parameters
  data(iucn_habitat_data)
  id_no <- c(-100, 135913)
  # create objects
  vcr::use_cassette("habitat-missing", {
    x <- get_spp_habitat_data(id_no, force = TRUE, verbose = interactive())
  })
  # tests
  expect_is(x, "data.frame")
  expect_gte(nrow(x), 1)
  expect_named(
    x,
    c("id_no", "code", "habitat", "suitability", "season", "majorimportance")
  )
  expect_true(all(id_no %in% x$id_no))
  ## tests for non-missing taxa
  x2 <- x[x$id_no == 135913, ]
  expect_gte(nrow(x2), 1)
  expect_true(all(x2$code %in% iucn_habitat_data$code))
  ## tests for missing taxa
  x3 <- x[x$id_no == -100, ]
  expect_equal(x3$id_no, -100)
  expect_equal(x3$code, rep(NA_character_, length(id_no) - 1))
  expect_equal(x3$habitat, rep(NA_character_, length(id_no) - 1))
  expect_equal(x3$suitability, rep(NA_character_, length(id_no) - 1))
  expect_equal(x3$season, rep(NA_character_, length(id_no) - 1))
  expect_equal(x3$majorimportance, rep(NA_character_, length(id_no) - 1))
})

test_that("all taxon missing habitat information", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("vcr")
  # set parameters
  data(iucn_habitat_data)
  id_no <- c(-100)
  # create objects
  vcr::use_cassette("habitat-missing", {
    x <- get_spp_habitat_data(id_no, force = TRUE, verbose = interactive())
  })
  # tests
  expect_is(x, "data.frame")
  expect_gte(nrow(x), 1)
  expect_named(
    x,
    c("id_no", "code", "habitat", "suitability", "season", "majorimportance")
  )
  expect_equal(x$id_no, -100)
  expect_equal(x$code, rep(NA_character_, length(id_no)))
  expect_equal(x$habitat, rep(NA_character_, length(id_no)))
  expect_equal(x$suitability, rep(NA_character_, length(id_no)))
  expect_equal(x$season, rep(NA_character_, length(id_no)))
  expect_equal(x$majorimportance, rep(NA_character_, length(id_no)))
})

test_that("accessing API V3 cache", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("vcr")
  # import data
  data(iucn_habitat_data)
  # specify parameters
  id_no <- c(
    670L, 2072L, 2374L, 3667L, 4421L, 4650L, 5808L, 6701L, 8110L, 8644L
  )
  cache_dir <- system.file("testdata", package = "aoh")
  # create objects
  x <- get_spp_habitat_data(
    id_no, dir = cache_dir, version = "1990-1", verbose = interactive()
  )
  # tests
  expect_is(x, "data.frame")
  expect_gte(nrow(x), 1)
  expect_named(
    x,
    c("id_no", "code", "habitat", "suitability", "season", "majorimportance")
  )
  expect_true(all(id_no %in% x$id_no))
  expect_true(all(x$code %in% iucn_habitat_data$code))
})
