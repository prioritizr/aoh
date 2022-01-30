context("get_jung_plvl1_habitat_data()")

test_that("latest version (from online)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  # create object
  x <- get_jung_plvl1_habitat_data(
    version = "latest", force = TRUE, verbose = interactive()
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(sf::st_crs(terra::crs(x)) == sf::st_crs("ESRI:54017"))
  expect_lte(terra::xmin(x), -17367531)
  expect_gte(terra::xmax(x), 17367569)
  expect_lte(terra::xmin(x), -6005523)
  expect_gte(terra::ymax(x), 7287077)
  expect_equal(terra::nlyr(x), 1)
})

test_that("latest version (from cache)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("rappdirs")
  # create object
  x <- get_jung_plvl1_habitat_data(
    dir = rappdirs::user_data_dir("aoh"),
    version = "latest",
    force = FALSE,
    verbose = interactive()
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(sf::st_crs(terra::crs(x)) == sf::st_crs("ESRI:54017"))
  expect_lte(terra::xmin(x), -17367531)
  expect_gte(terra::xmax(x), 17367569)
  expect_lte(terra::xmin(x), -6005523)
  expect_gte(terra::ymax(x), 7287077)
  expect_equal(terra::nlyr(x), 1)
})

test_that("specified version (from online)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  # create object
  x <- get_jung_plvl1_habitat_data(
    version = latest_jung_potential_version,
    force = TRUE,
    verbose = interactive()
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(sf::st_crs(terra::crs(x)) == sf::st_crs("ESRI:54017"))
  expect_lte(terra::xmin(x), -17367531)
  expect_gte(terra::xmax(x), 17367569)
  expect_lte(terra::xmin(x), -6005523)
  expect_gte(terra::ymax(x), 7287077)
  expect_equal(terra::nlyr(x), 1)
})

test_that("specified version (from cache)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("rappdirs")
  # create object
  x <- get_jung_plvl1_habitat_data(
    dir = rappdirs::user_data_dir("aoh"),
    version = latest_jung_potential_version,
    force = FALSE,
    verbose = interactive()
  )
  # tests
  expect_is(x, "SpatRaster")
  expect_true(sf::st_crs(terra::crs(x)) == sf::st_crs("ESRI:54017"))
  expect_lte(terra::xmin(x), -17367531)
  expect_gte(terra::xmax(x), 17367569)
  expect_lte(terra::xmin(x), -6005523)
  expect_gte(terra::ymax(x), 7287077)
  expect_equal(terra::nlyr(x), 1)
})

# test_that("potential habitat is larger than current", {
#   # skip if needed
#   skip_on_cran()
#   skip_if_offline()
#   skip_if_iucn_key_missing()
#   skip_if_gdal_not_available()
#   skip_if_gdal_python_not_available()
#   # specify file path
#   f <- system.file("extdata", "EXAMPLE_SPECIES.zip", package = "aoh")
#   cd <- rappdirs::user_data_dir("aoh")
#   # load data
#   d <- read_spp_range_data(f)
#   # create output dirs
#   output_dir1 <- tempfile()
#   output_dir2 <- tempfile()
#   dir.create(output_dir1, showWarnings = FALSE, recursive = TRUE)
#   dir.create(output_dir2, showWarnings = FALSE, recursive = TRUE)
#   # create objects
#   x1 <- suppressWarnings(
#     create_spp_aoh_data(
#       x = d,
#       output_dir = output_dir1,
#       habitat_data = get_jung_lvl1_habitat_data(
#         dir = cd, version = latest_jung_version
#       ),
#       crosswalk_data = crosswalk_jung_lvl1_data,
#       cache_dir = cd,
#       elevation_version = latest_elevation_version,
#       verbose = interactive()
#     )
#   )
#   x2 <- suppressWarnings(
#     create_spp_aoh_data(
#       x = d,
#       output_dir = output_dir2,
#       habitat_data = get_jung_plvl1_habitat_data(
#         dir = cd, version = latest_jung_potential_version
#       ),
#       crosswalk_data = crosswalk_jung_lvl1_data,
#       cache_dir = cd,
#       elevation_version = latest_elevation_version,
#       verbose = interactive()
#     )
#   )
#   # tests
#   expect_is(x1, "sf")
#   expect_is(x2, "sf")
#   expect_named(x1, aoh_names)
#   expect_named(x2, aoh_names)
#   validate_aoh_data(
#     x = x1,
#     habitat_data = get_jung_lvl1_habitat_data(
#       dir = cd, version = latest_jung_version
#     ),
#     elevation_data = get_global_elevation_data(
#       dir = cd, version = latest_elevation_version
#     ),
#     crosswalk_data = crosswalk_jung_lvl1_data
#   )
#   validate_aoh_data(
#     x = x2,
#     habitat_data = get_jung_plvl1_habitat_data(
#       dir = cd, version = latest_jung_potential_version
#     ),
#     elevation_data = get_global_elevation_data(
#       dir = cd, version = latest_elevation_version
#     ),
#     crosswalk_data = crosswalk_jung_lvl1_data
#   )
#   expect_equal(
#     dplyr::select(x1, -path, -habitat_code),
#     dplyr::select(x2, -path, -habitat_code)
#   )
#   expect_gte(
#     vapply(seq_len(nrow(x2)), FUN.VALUE = numeric(1), function(i) {
#       terra::global(
#         x = terra::rast(x2$path[i]),
#         fun = "sum",
#         na.rm = TRUE
#       )[[1]]
#     }),
#     vapply(seq_len(nrow(x1)), FUN.VALUE = numeric(1), function(i) {
#       terra::global(
#         x = terra::rast(x1$path[i]),
#         fun = "sum",
#         na.rm = TRUE
#       )[[1]]
#     })
#   )
#   # clean up
#   unlink(output_dir1, recursive = TRUE)
#   unlink(output_dir2, recursive = TRUE)
# })
