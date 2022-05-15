context("get_spp_threat_data()")

test_that("single taxon identifier", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_iucn_key_missing()
  skip_if_iucn_api_not_available()
  # set parameters
  id_no <- c(18)
  # create objects
  x1 <- get_spp_threat_data(id_no, force = TRUE, verbose = interactive())
  Sys.sleep(2)
  x2 <- get_spp_threat_data(id_no, force = FALSE, verbose = interactive())
  # tests
  expect_is(x1, "data.frame")
  expect_is(x2, "data.frame")
  expect_gte(nrow(x1), 1)
  expect_named(
    x1,
    c(
      "id_no", "code", "title", "timing", "scope", "severity",
      "score", "invasive"
    )
  )
  expect_true(all(id_no %in% x1$id_no))
  expect_identical(x1, x2)
})

test_that("multiple taxon identifiers", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_iucn_key_missing()
  skip_if_iucn_api_not_available()
  # tests
  id_no <- c(18, 137, 138, 139)
  # create objects
  x1 <- get_spp_threat_data(id_no, force = TRUE, verbose = interactive())
  Sys.sleep(2)
  x2 <- get_spp_threat_data(id_no, force = FALSE, verbose = interactive())
  # tests
  expect_is(x1, "data.frame")
  expect_is(x2, "data.frame")
  expect_gte(nrow(x1), 1)
  expect_named(
    x1,
    c(
      "id_no", "code", "title", "timing", "scope", "severity",
      "score", "invasive"
    )
  )
  expect_true(all(id_no %in% x1$id_no))
  expect_identical(x1, x2)
})

test_that("some taxon missing summary data", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_iucn_key_missing()
  skip_if_iucn_api_not_available()
  # set parameters
  id_no <- c(18, -100)
  # create objects
  x1 <- get_spp_threat_data(id_no[1], force = TRUE, verbose = interactive())
  x2 <- get_spp_threat_data(id_no, force = TRUE, verbose = interactive())
  # tests
  ## x1
  expect_is(x1, "data.frame")
  expect_true(all(id_no[1] %in% x1$id_no))
  expect_gte(nrow(x1), 1)
  expect_named(
    x1,
    c(
      "id_no", "code", "title", "timing", "scope", "severity",
      "score", "invasive"
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
  skip_if_offline()
  skip_if_iucn_key_missing()
  skip_if_iucn_api_not_available()
  # set parameters
  id_no <- c(178652, 177906)
  # create objects
  x <- get_spp_threat_data(id_no, force = TRUE, verbose = interactive())
  # tests
  expect_is(x, "data.frame")
  expect_gte(nrow(x), 1)
  expect_named(
    x,
    c(
      "id_no", "code", "title", "timing", "scope", "severity",
      "score", "invasive"
    )
  )
  expect_true(all(id_no %in% x$id_no))
})
