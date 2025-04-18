context("get_spp_threat_data()")

test_that("single taxon identifier", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("vcr")
  # set parameters
  data(iucn_threat_data)
  id_no <- c(18)
  # create objects
  vcr::use_cassette("threat-single", {
    x <- get_spp_threat_data(id_no, force = TRUE, verbose = interactive())
  })
  # tests
  expect_is(x, "data.frame")
  expect_gte(nrow(x), 1)
  expect_named(
    x,
    c(
      "id_no", "code", "title", "timing", "scope", "severity", "score",
      "internationaltrade", "ancestry", "virus", "ias", "text"
    )
  )
  expect_true(all(id_no %in% x$id_no))
  expect_true(all(x$code %in% iucn_threat_data$code))
})

test_that("multiple taxon identifiers", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("vcr")
  # set parameters
  data(iucn_threat_data)
  id_no <- c(18, 137, 138, 139)
  # create objects
  vcr::use_cassette("threat-multiple", {
    x <- suppressMessages(
      get_spp_threat_data(id_no, force = TRUE, verbose = TRUE)
    )
  })
  # tests
  expect_is(x, "data.frame")
  expect_gte(nrow(x), 1)
  expect_named(
    x,
    c(
      "id_no", "code", "title", "timing", "scope", "severity", "score",
      "internationaltrade", "ancestry", "virus", "ias", "text"
    )
  )
  expect_true(all(id_no %in% x$id_no))
  expect_true(all(x$code %in% iucn_threat_data$code))
})

test_that("some taxon missing summary data", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("vcr")
  # set parameters
  data(iucn_threat_data)
  id_no <- c(18, -100)
  # create objects
  vcr::use_cassette("threat-missing-x1", {
    x1 <- get_spp_threat_data(id_no[1], force = TRUE, verbose = interactive())
  })
  vcr::use_cassette("threat-missing-x2", {
    x2 <- get_spp_threat_data(id_no, force = TRUE, verbose = interactive())
  })
  # tests
  ## x1
  expect_is(x1, "data.frame")
  expect_true(all(id_no[1] %in% x1$id_no))
  expect_gte(nrow(x1), 1)
  expect_named(
    x1,
    c(
      "id_no", "code", "title", "timing", "scope", "severity", "score",
      "internationaltrade", "ancestry", "virus", "ias", "text"
    )
  )
  ## x2
  expect_is(x2, "data.frame")
  expect_true(all(id_no %in% x2$id_no))
  expect_gte(nrow(x2), 1)
  expect_equal(x1, x2[-nrow(x2), ])
  expect_true(all(vapply(x2[nrow(x2), -1], FUN.VALUE = logical(1), is.na)))
})

test_that("taxon with multiple records", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("vcr")
  # set parameters
  data(iucn_threat_data)
  id_no <- c(178652, 177906)
  # create objects
  vcr::use_cassette("threat-multiple-records", {
    x <- get_spp_threat_data(id_no, force = TRUE, verbose = interactive())
  })
  # tests
  expect_is(x, "data.frame")
  expect_gte(nrow(x), 1)
  expect_named(
    x,
    c(
      "id_no", "code", "title", "timing", "scope", "severity", "score",
      "internationaltrade", "ancestry", "virus", "ias", "text"
    )
  )
  expect_true(all(id_no %in% x$id_no))
  expect_true(all(x$code %in% iucn_threat_data$code))
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
  x <- get_spp_threat_data(
    id_no, dir = cache_dir, version = "1990-1", verbose = interactive()
  )
  # tests
  expect_is(x, "data.frame")
  expect_gte(nrow(x), 1)
  expect_named(
    x,
    c(
      "id_no", "code", "title", "timing", "scope", "severity", "score",
      "invasive"
    )
  )
  expect_true(all(id_no %in% x$id_no))
  expect_true(all(x$code %in% iucn_threat_data$code))
})
