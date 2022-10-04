context("setup")

# initialize data
test_that("cache directory", {
  skip_on_cran()
  # create data directory
  dir.create(
    rappdirs::user_data_dir("aoh"),
    showWarnings = FALSE,
    recursive = TRUE
  )
  # tests
  expect_true(file.exists(rappdirs::user_data_dir("aoh")))
})

test_that("Jung level 1 habitat data", {
  skip_on_cran()
  skip_if_zenodo_website_not_available()
  # download data
  x <- try(
    get_jung_lvl1_habitat_data(
      version = "latest", force = FALSE, verbose = interactive(),
      dir = rappdirs::user_data_dir("aoh")
    ),
    silent = TRUE
  )
  # extract doi
  latest_jung_version <<- try(
    extract_cache_doi("jung-lvl1"),
    silent = TRUE
  )
  # skip if failed
  skip_if_zenodo_data_not_available(latest_jung_version)
  # re-import data
  x <- get_jung_lvl1_habitat_data(
    version = latest_jung_version, force = FALSE, verbose = interactive(),
    dir = rappdirs::user_data_dir("aoh")
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_is(latest_jung_version, "character")
})

test_that("Jung level 2 habitat data", {
  skip_on_cran()
  skip_if_zenodo_website_not_available()
  # download data
  x <- try(
    get_jung_lvl2_habitat_data(
      version = "latest", force = FALSE, verbose = interactive(),
      dir = rappdirs::user_data_dir("aoh")
    ),
    silent = TRUE
  )
  # extract doi
  latest_jung_version <<- try(
    extract_cache_doi("jung-lvl2"),
    silent = TRUE
  )
  # skip if failed
  skip_if_zenodo_data_not_available(latest_jung_version)
  # re-import data
  x <- get_jung_lvl2_habitat_data(
    version = latest_jung_version, force = FALSE, verbose = interactive(),
    dir = rappdirs::user_data_dir("aoh")
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_is(latest_jung_version, "character")
})

test_that("Jung level 1 potential habitat data", {
  skip_on_cran()
  skip_if_zenodo_website_not_available()
  # download data
  x <- try(
    get_jung_plvl1_habitat_data(
      version = "latest", force = FALSE, verbose = interactive(),
      dir = rappdirs::user_data_dir("aoh")
    ),
    silent = TRUE
  )
  # extract doi
  latest_jung_potential_version <<- try(
    extract_cache_doi("jung-plvl1"),
    silent = TRUE
  )
  # skip if failed
  skip_if_zenodo_data_not_available(latest_jung_potential_version)
  # re-import data
  x <- get_jung_plvl1_habitat_data(
    version = latest_jung_potential_version, force = FALSE,
    verbose = interactive(),
    dir = rappdirs::user_data_dir("aoh")
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_is(latest_jung_potential_version, "character")
})

test_that("Lumbierres CGLS habitat data", {
  skip_on_cran()
  skip_if_zenodo_website_not_available()
  # download data
  x <- try(
      get_lumb_cgls_habitat_data(
      version = "latest",
      force = FALSE,
      verbose = interactive(),
      dir = rappdirs::user_data_dir("aoh")
    ),
    silent = TRUE
  )
  # extract doi
  latest_lumb_cgls_version <<- try(
    extract_cache_doi("lumbierres-"),
    silent = TRUE
  )
  # skip if failed
  skip_if_zenodo_data_not_available(latest_lumb_cgls_version)
  # re-import data
  x <- get_lumb_cgls_habitat_data(
    version = latest_lumb_cgls_version, force = FALSE,
    verbose = interactive(),
    dir = rappdirs::user_data_dir("aoh")
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_is(latest_lumb_cgls_version, "character")
})

test_that("elevation data", {
  skip_on_cran()
  skip_if_zenodo_website_not_available()
  # download data
  x <- try(
    get_global_elevation_data(
      version = "latest",
      force = FALSE,
      verbose = interactive(),
      dir = rappdirs::user_data_dir("aoh")
    ),
    silent = TRUE
  )
  # extract doi
  latest_elevation_version <<- try(
    extract_cache_doi("dem-"),
    silent = TRUE
  )
  # skip if failed
  skip_if_zenodo_data_not_available(latest_elevation_version)
  # re-import data
  x <- get_global_elevation_data(
    version = latest_elevation_version,
    force = FALSE,
    verbose = interactive(),
    dir = rappdirs::user_data_dir("aoh")
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_is(latest_elevation_version, "character")
})
