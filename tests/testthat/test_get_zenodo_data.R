context("get_zenodo_data()")

test_that("latest version", {
  skip_on_cran()
  # download file
  d <- new_temp_dir()
  f <- "prioritizr/dem-v1.0.1.zip"
  v <- "latest"
  p <- get_zenodo_data(x = "10.5281/zenodo.5719984", version = v, file = f)
  # tests
  expect_is(p, "character")
  expect_true(file.exists(p))
  # clean up
  unlink(d, recursive = TRUE, force = TRUE)
})

test_that("manually specified version", {
  skip_on_cran()
  # download file
  d <- new_temp_dir()
  f <- "prioritizr/dem-v1.0.1.zip"
  v <-  "10.5281/zenodo.6622149"
  p <- get_zenodo_data(x = "10.5281/zenodo.5719984", version = v, file = f)
  # tests
  expect_is(p, "character")
  expect_true(file.exists(p))
  # clean up
  unlink(d, recursive = TRUE, force = TRUE)
})
