context("get_spp_habitat_data()")

test_that("single taxon identifier", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  # tests
  id_no <- c(18)
  expect_is(
    x <<- get_spp_habitat_data(id_no, force = TRUE, verbose = FALSE),
    "data.frame"
  )
  Sys.sleep(2)
  expect_is(
    x2 <<- get_spp_habitat_data(id_no, force = FALSE, verbose = FALSE),
    "data.frame"
  )
  expect_true(all(id_no %in% x$id_no))
  expect_gte(nrow(x), 1)
  expect_true(assertthat::has_name(x, "id_no"))
  expect_true(assertthat::has_name(x, "code"))
  expect_true(assertthat::has_name(x, "habitat"))
  expect_true(assertthat::has_name(x, "suitability"))
  expect_true(assertthat::has_name(x, "season"))
  expect_true(assertthat::has_name(x, "majorimportance"))
  expect_identical(x, x2)
})

test_that("multiple taxon identifiers", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  # tests
  id_no <- c(18, 137, 138, 139)
  expect_is(
    x <<- get_spp_habitat_data(id_no, force = TRUE, verbose = FALSE),
    "data.frame"
  )
  Sys.sleep(2)
  expect_is(
    x2 <<- get_spp_habitat_data(id_no, force = FALSE, verbose = FALSE),
    "data.frame"
  )
  expect_true(all(id_no %in% x$id_no))
  expect_gte(nrow(x), 1)
  expect_true(assertthat::has_name(x, "id_no"))
  expect_true(assertthat::has_name(x, "code"))
  expect_true(assertthat::has_name(x, "habitat"))
  expect_true(assertthat::has_name(x, "suitability"))
  expect_true(assertthat::has_name(x, "season"))
  expect_true(assertthat::has_name(x, "majorimportance"))
  expect_identical(x, x2)
})

test_that("invalid taxon identifier", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  # tests
  id_no <- c(18, -100)
  expect_error(
    get_spp_habitat_data(id_no, force = TRUE, verbose = FALSE)
  )
})
