context("create_spp_frc_data")

test_that("simulated data", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  elevation_data <- terra::rast(
    system.file("testdata", "sim_elevation_data.tif", package = "aoh")
  )
  habitat_data <- terra::rast(
    system.file("testdata", "sim_habitat_data.tif", package = "aoh")
  )
  spp_habitat_data <- read.csv(
    system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh"),
    sep = ",", header = TRUE
  )
  spp_summary_data <- read.csv(
    system.file("testdata", "sim_spp_summary_data.csv", package = "aoh"),
    sep = ",", header = TRUE
  )
  # load data
  d <- read_spp_range_data(f)
  # create object
  x <- create_spp_frc_data(
    x = create_spp_info_data(
      x = d,
      spp_habitat_data = spp_habitat_data,
      spp_summary_data = spp_summary_data,
      verbose = interactive()
    ),
    res = 5000,
    output_dir = tempdir(),
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data,
    verbose = interactive()
  )
  # tests
  expect_is(x, "sf")
  expect_is(x$path, "character")
  expect_equal(sum(is.na(x$path)), 0)
  expect_gte(
    min(vapply(x$path, FUN.VALUE = numeric(1), function(x) {
      terra::global(terra::rast(x), "min", na.rm = TRUE)[[1]]
    })),
    0
  )
  expect_lte(
    min(vapply(x$path, FUN.VALUE = numeric(1), function(x) {
      terra::global(terra::rast(x), "max", na.rm = TRUE)[[1]]
    })),
    1
  )
  expect_gt(
    min(vapply(x$path, FUN.VALUE = numeric(1), function(x) {
      terra::global(terra::rast(x), "sum", na.rm = TRUE)[[1]]
    })),
    0
  )
  # clean up
  unlink(x$path[!is.na(x$path)])
})

test_that("different engines produce same result", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("gdalUtilities")
  skip_if_gdal_calc_not_available()
  # specify file path
  f <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  elevation_data <- terra::rast(
    system.file("testdata", "sim_elevation_data.tif", package = "aoh")
  )
  habitat_data <- terra::rast(
    system.file("testdata", "sim_habitat_data.tif", package = "aoh")
  )
  spp_habitat_data <- read.csv(
    system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh"),
    sep = ",", header = TRUE
  )
  spp_summary_data <- read.csv(
    system.file("testdata", "sim_spp_summary_data.csv", package = "aoh"),
    sep = ",", header = TRUE
  )
  # create output dirs
  output_dir1 <- tempfile()
  output_dir2 <- tempfile()
  dir.create(output_dir1, showWarnings = FALSE, recursive = TRUE)
  dir.create(output_dir2, showWarnings = FALSE, recursive = TRUE)
  # prepare data
  d <- create_spp_info_data(
    x = read_spp_range_data(f),
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    verbose = interactive()
  )
  # compute fractional coverage
  x1 <- create_spp_frc_data(
    x = d,
    res = 5000,
    engine = "terra",
    output_dir = output_dir1,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data,
    verbose = interactive()
  )
  x2 <- create_spp_frc_data(
    x = d,
    res = 5000,
    engine = "gdal",
    output_dir = output_dir2,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data,
    verbose = interactive()
  )
  # tests
  expect_is(x1, "sf")
  expect_is(x2, "sf")
  expect_equal(nrow(x1), nrow(x2))
  expect_equal(
    dplyr::select(x1, -path),
    dplyr::select(x2, -path)
  )
  expect_equivalent(
    lapply(
      x1$path,
      function(x) terra::values(terra::rast(x))
    ),
    lapply(
      x2$path,
      function(x) terra::values(terra::rast(x))
    )
  )
  # clean up
  unlink(output_dir1, force = TRUE, recursive = TRUE)
  unlink(output_dir2, force = TRUE, recursive = TRUE)
})

test_that("example data", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_iucn_key_missing()
  skip_if_iucn_api_not_available()
  skip_if_cached_data_not_available()
  skip_if_zenodo_data_not_available(latest_lumb_cgls_version)
  skip_if_zenodo_data_not_available(latest_elevation_version)
  # specify file path
  f <- system.file("extdata", "EXAMPLE_SPECIES.zip", package = "aoh")
  cd <- rappdirs::user_data_dir("aoh")
  # prepare data
  d <- create_spp_info_data(
    x = read_spp_range_data(f),
    cache_dir = cd,
    verbose = interactive()
  )
  # create object
  x <- suppressWarnings(
    create_spp_frc_data(
      x = d,
      res = 5000,
      cache_dir = cd,
      output_dir = tempdir(),
      habitat_version = latest_lumb_cgls_version,
      elevation_version = latest_elevation_version,
      verbose = interactive()
    )
  )
  # tests
  expect_is(x, "sf")
  expect_named(x, aoh_names)
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_equal(d$id_no, x$id_no)
  expect_is(x$path, "character")
  expect_equal(sum(is.na(x$path)), 0)
  expect_gte(
    min(vapply(x$path, FUN.VALUE = numeric(1), function(x) {
      terra::global(terra::rast(x), "min", na.rm = TRUE)[[1]]
    })),
    0
  )
  expect_lte(
    min(vapply(x$path, FUN.VALUE = numeric(1), function(x) {
      terra::global(terra::rast(x), "max", na.rm = TRUE)[[1]]
    })),
    1
  )
  expect_gt(
    min(vapply(x$path, FUN.VALUE = numeric(1), function(x) {
      terra::global(terra::rast(x), "sum", na.rm = TRUE)[[1]]
    })),
    0
  )
  # clean up
  unlink(x$path[!is.na(x$path)])
})
