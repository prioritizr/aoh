context("get_spp_habitat_data()")

test_that("single taxon identifier", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  # set parameters
  id_no <- c(18)
  # create objects
  x1 <- get_spp_habitat_data(id_no, force = TRUE, verbose = FALSE)
  Sys.sleep(2)
  x2 <- get_spp_habitat_data(id_no, force = FALSE, verbose = FALSE)
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
  # set parameters
  id_no <- c(18, 137, 138, 139)
  # create objects
  x1 <- get_spp_habitat_data(id_no, force = TRUE, verbose = FALSE)
  Sys.sleep(2)
  x2 <- get_spp_habitat_data(id_no, force = FALSE, verbose = FALSE)
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

test_that("invalid taxon identifier", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  # set parameters
  id_no <- c(18, -100)
  # tests
  expect_error(
    get_spp_habitat_data(id_no, force = TRUE, verbose = FALSE)
  )
})
