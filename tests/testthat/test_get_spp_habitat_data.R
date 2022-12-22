context("get_spp_habitat_data()")

test_that("single taxon identifier", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("vcr")
  # set parameters
  data(iucn_habitat_data)
  id_no <- c(18)
  # create objects
  vcr::use_cassette("habitat-single", {
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
  id_no <- c(-100, 41129, 135913, 135758)
  # create objects
  vcr::use_cassette("habitat-missing", {
    x <- get_spp_habitat_data(id_no, force = TRUE, verbose = interactive())
  })
  # tests
  # tests
  expect_is(x, "data.frame")
  expect_gte(nrow(x), 1)
  expect_named(
    x,
    c("id_no", "code", "habitat", "suitability", "season", "majorimportance")
  )
  expect_equal(x$id_no, id_no)
  expect_equal(x$code, rep(NA_character_, length(id_no)))
  expect_equal(x$habitat, rep(NA_character_, length(id_no)))
  expect_equal(x$suitability, rep(NA_character_, length(id_no)))
  expect_equal(x$season, rep(NA_character_, length(id_no)))
  expect_equal(x$majorimportance, rep(NA_character_, length(id_no)))
})
