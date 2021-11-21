context("get_spp_habitat_data()")

test_that("single taxon identifier", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_iucn_key_missing()
  # set parameters
  id_no <- c(18)
  # create objects
  x1 <- get_spp_habitat_data(id_no, force = TRUE, verbose = interactive())
  Sys.sleep(2)
  x2 <- get_spp_habitat_data(id_no, force = FALSE, verbose = interactive())
  # tests
  expect_is(x1, "data.frame")
  expect_is(x2, "data.frame")
  expect_true(all(id_no %in% x1$id_no))
  expect_gte(nrow(x1), 1)
  expect_named(
    x1,
    c("id_no", "code", "habitat", "suitability", "season", "majorimportance")
  )
  expect_identical(x1, x2)
})

test_that("multiple taxon identifiers", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_iucn_key_missing()
  # set parameters
  id_no <- c(18, 137, 138, 139)
  # create objects
  x1 <- get_spp_habitat_data(id_no, force = TRUE, verbose = interactive())
  Sys.sleep(2)
  x2 <- get_spp_habitat_data(id_no, force = FALSE, verbose = interactive())
  # tests
  expect_is(x1, "data.frame")
  expect_is(x2, "data.frame")
  expect_true(all(id_no %in% x1$id_no))
  expect_gte(nrow(x1), 1)
  expect_named(
    x1,
    c("id_no", "code", "habitat", "suitability", "season", "majorimportance")
  )
  expect_identical(x1, x2)
})

test_that("some taxon missing habitat information", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_iucn_key_missing()
  # set parameters
  id_no <- c(-100, 41129, 135913, 135758)
  # create objects
  x1 <- get_spp_habitat_data(id_no, force = TRUE, verbose = interactive())
  Sys.sleep(2)
  x2 <- get_spp_habitat_data(id_no, force = FALSE, verbose = interactive())
  # tests
  # tests
  expect_is(x1, "data.frame")
  expect_is(x2, "data.frame")
  expect_true(all(id_no %in% x1$id_no))
  expect_gte(nrow(x1), 1)
  expect_named(
    x1,
    c("id_no", "code", "habitat", "suitability", "season", "majorimportance")
  )
  expect_identical(x1, x2)
  expect_equal(x1$id_no, id_no)
  expect_equal(x1$code, rep(NA_character_, length(id_no)))
  expect_equal(x1$habitat, rep(NA_character_, length(id_no)))
  expect_equal(x1$suitability, rep(NA_character_, length(id_no)))
  expect_equal(x1$season, rep(NA_character_, length(id_no)))
  expect_equal(x1$majorimportance, rep(NA_character_, length(id_no)))
})
