context("zenodo misc functions")

test_that("get_doi_files", {
  skip_on_cran()
  skip_if_zenodo_website_not_available()
  x <- get_doi_files("https://doi.org/10.5281/zenodo.6622038")
  y <- tibble::tibble(
    filename = c(
      "jung-lvl1-10-5281_zenodo-4058819.tif",
      "jung-lvl2-10-5281_zenodo-4058819.tif",
      "prioritizr/jung-habitat-data-v1.0.0.zip"
    ),
    download = c(
      "https://zenodo.org/record/6622038/files/jung-lvl1-10-5281_zenodo-4058819.tif?download=1",
      "https://zenodo.org/record/6622038/files/jung-lvl2-10-5281_zenodo-4058819.tif?download=1",
      "https://zenodo.org/record/6622038/files/prioritizr/jung-habitat-data-v1.0.0.zip?download=1"
    )
  )
  expect_equal(x, y,)
})

test_that("get_doi_versions (multiple versions)", {
  skip_on_cran()
  skip_if_zenodo_website_not_available()
  x <- get_doi_versions("https://doi.org/10.5281/zenodo.6622038")
  y <- tibble::tibble(
    version = c("v0.0.1", "v1.0.0", "v1.0.0"),
    created = structure(
      c(1654574400, 1654574400, 1654574400),
      tzone = "", class = c("POSIXct", "POSIXt")
    ),
    doi = c(
      "10.5281/zenodo.6622030",
      "10.5281/zenodo.6622037",
      "10.5281/zenodo.6622038")
  )
  # subset x to include same versions as y,
  # to future proof in case new versions are uploaded
  # N.B. we don't test "created" due to time zone conversion differences
  expect_is(x, "tbl_df")
  x <- x[seq_len(3), , drop = FALSE]
  expect_equal(names(x), names(y))
  expect_equal(x$version, y$version)
  expect_equal(x$doi, y$doi)
})

test_that("get_doi_versions (single version)", {
  skip_on_cran()
  skip_if_zenodo_website_not_available()
  x <- get_doi_versions("https://doi.org/10.5281/zenodo.46757")
  y <- tibble::tibble(
    version = NA_character_,
    created = as.POSIXct(NA_real_),
    doi = c("10.5281/zenodo.46757")
  )
  expect_equal(x, y)
})
