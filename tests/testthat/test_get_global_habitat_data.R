context("get_global_habitat_data()")

test_that("latest version (raw)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  # create object
  x <- get_global_habitat_data(
    version = "latest", preprocessed = FALSE, force = TRUE, verbose = FALSE
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(sf::st_crs(terra::crs(x)) == sf::st_crs(4326))
  expect_lte(terra::xmin(x), -180)
  expect_gte(terra::xmax(x), 180)
  expect_lte(terra::xmin(x), -89)
  expect_gte(terra::ymax(x), 89)
  expect_equal(terra::nlyr(x), 79)
})

test_that("latest version (preprocessed)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  # get data
  d <- get_world_behrmann_1km_rast()
  # determine valid codes
  code_data <- habitat_code_data()
  iucn_codes <- names(
    get_global_habitat_data(
      version = "latest", preprocessed = FALSE, force = FALSE, verbose = FALSE
    )
  )
  iucn_codes <- intersect(
    iucn_codes, code_data$iucn_code[code_data$terrestrial]
  )
  # create object
  x <- get_global_habitat_data(
    version = "latest", preprocessed = TRUE, force = TRUE, verbose = FALSE
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(terra::compareGeom(x, d, res = TRUE, stopOnError = FALSE))
  expect_equal(terra::nlyr(x), 63)
  expect_equal(sort(iucn_codes), sort(names(x)))
})

test_that("manually specified version (raw from online)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  # create object
  x <- get_global_habitat_data(
    version = "10.5281/zenodo.4058819", preprocessed = FALSE,
    force = TRUE, verbose = FALSE
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(sf::st_crs(terra::crs(x)) == sf::st_crs(4326))
  expect_lte(terra::xmin(x), -180)
  expect_gte(terra::xmax(x), 180)
  expect_lte(terra::xmin(x), -89)
  expect_gte(terra::ymax(x), 89)
  expect_equal(terra::nlyr(x), 79)
})

test_that("manually specified version (preprocessed from online)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  # get data
  d <- get_world_behrmann_1km_rast()
  # determine valid codes
  code_data <- habitat_code_data()
  iucn_codes <- names(
    get_global_habitat_data(
      version = "latest", preprocessed = FALSE, force = FALSE, verbose = FALSE
    )
  )
  iucn_codes <- intersect(
    iucn_codes, code_data$iucn_code[code_data$terrestrial]
  )
  # create object
  x <- get_global_habitat_data(
    version = "10.5281/zenodo.4058819", preprocessed = TRUE,
    force = TRUE, verbose = FALSE
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(terra::compareGeom(x, d, res = TRUE, stopOnError = FALSE))
  expect_equal(terra::nlyr(x), 63)
  expect_equal(sort(iucn_codes), sort(names(x)))
})

test_that("manually specified version (raw from cache)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  # create object
  x <- get_global_habitat_data(
    version = "10.5281/zenodo.4058819", preprocessed = FALSE,
    force = FALSE, verbose = FALSE
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(sf::st_crs(terra::crs(x)) == sf::st_crs(4326))
  expect_lte(terra::xmin(x), -180)
  expect_gte(terra::xmax(x), 180)
  expect_lte(terra::xmin(x), -89)
  expect_gte(terra::ymax(x), 89)
  expect_equal(terra::nlyr(x), 79)
})

test_that("manually specified version (preprocessed from cache)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  # get data
  d <- get_world_behrmann_1km_rast()
  # determine valid codes
  code_data <- habitat_code_data()
  iucn_codes <- names(
    get_global_habitat_data(
      version = "10.5281/zenodo.4058819", preprocessed = FALSE, verbose = TRUE
    )
  )
  iucn_codes <- intersect(
    iucn_codes, code_data$iucn_code[code_data$terrestrial]
  )
  # create object
  x <- get_global_habitat_data(
    version = "10.5281/zenodo.4058819", preprocessed = TRUE,
    force = FALSE, verbose = FALSE
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(terra::compareGeom(x, d, res = TRUE, stopOnError = FALSE))
  expect_equal(terra::nlyr(x), 63)
  expect_equal(sort(iucn_codes), sort(names(x)))
})
