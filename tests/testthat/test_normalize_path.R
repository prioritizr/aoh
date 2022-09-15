context("normalize_path")

test_that("mustWork = NA", {
  skip_on_cran()
  expect_is(normalize_path(tempdir(), mustWork = NA), "character")
  expect_is(
    suppressWarnings(normalize_path(tempfile(), mustWork = NA)),
    "character"
  )
  expect_warning(normalize_path(tempfile(), mustWork = NA))
})

test_that("mustWork = TRUE", {
  skip_on_cran()
  expect_is(normalize_path(tempdir(), mustWork = TRUE), "character")
  expect_error(normalize_path(tempfile(), mustWork = TRUE))
})

test_that("mustWork = FALSE", {
  skip_on_cran()
  expect_is(normalize_path(tempdir(), mustWork = FALSE), "character")
  expect_is(normalize_path(tempfile(), mustWork = FALSE), "character")
})
