context("create_spp_aoh_data()")

test_that("simulated data (terra engine)", {
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
  d <- create_spp_info_data(
    x = read_spp_range_data(f),
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    verbose = interactive()
  )
  # create objects
  x <- create_spp_aoh_data(
    x = d,
    output_dir = tempdir(),
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data,
    verbose = TRUE
  )
  # tests
  expect_is(x, "sf")
  expect_named(x, aoh_names)
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_equal(
    d,
    dplyr::select(x, -habitat_code, -path, -xmin, -xmax, -ymin, -ymax)
  )
  expect_is(x$id_no, "integer")
  expect_is(x$binomial, "character")
  expect_is(x$seasonal, "integer")
  expect_is(x$migratory, "logical")
  expect_is(x$category, "character")
  expect_is(x$full_habitat_code, "character")
  expect_is(x$habitat_code, "character")
  expect_is(x$elevation_lower, "numeric")
  expect_is(x$elevation_upper, "numeric")
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
  validate_aoh_data(
    x = x,
    elevation_data = elevation_data,
    habitat_data = habitat_data,
    crosswalk_data = crosswalk_jung_lvl2_data
  )
  # clean up
  unlink(x$path[!is.na(x$path)])
})

test_that("simulated data (GDAL engine)", {
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
  # load data
  d <- create_spp_info_data(
    x = read_spp_range_data(f),
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    verbose = interactive()
  )
  # create output dirs
  output_dir <- tempfile()
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  # create objects
  x <- create_spp_aoh_data(
    x = d,
    output_dir = output_dir,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data,
    engine = "gdal",
    n_threads = 2,
    verbose = interactive()
  )
  # tests
  expect_is(x, "sf")
  expect_named(x, aoh_names)
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_equal(
    d,
    dplyr::select(x, -habitat_code, -path, -xmin, -xmax, -ymin, -ymax)
  )
  expect_is(x$id_no, "integer")
  expect_is(x$binomial, "character")
  expect_is(x$seasonal, "integer")
  expect_is(x$full_habitat_code, "character")
  expect_is(x$habitat_code, "character")
  expect_is(x$elevation_lower, "numeric")
  expect_is(x$elevation_upper, "numeric")
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
  validate_aoh_data(
    x = x,
    elevation_data = elevation_data,
    habitat_data = habitat_data,
    crosswalk_data = crosswalk_jung_lvl2_data
  )
  # clean up
  unlink(output_dir, recursive = TRUE)
})

test_that("simulated data (GRASS engine)", {
  # skip if needed
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("sp")
  skip_if_not_installed("gdalUtilities")
  skip_if_not_installed("rgrass")
  skip_if_grass_not_available()
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
  d <- create_spp_info_data(
    x = read_spp_range_data(f),
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    verbose = interactive()
  )
  # create output dirs
  output_dir <- tempfile()
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  # create objects
  x <- create_spp_aoh_data(
    x = d,
    output_dir = output_dir,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data,
    engine = "grass",
    n_threads = 2,
    verbose = interactive()
  )
  # tests
  expect_is(x, "sf")
  expect_named(x, aoh_names)
  expect_equal(
    d,
    dplyr::select(x, -habitat_code, -path, -xmin, -xmax, -ymin, -ymax)
  )
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_is(x$id_no, "integer")
  expect_is(x$binomial, "character")
  expect_is(x$seasonal, "integer")
  expect_is(x$habitat_code, "character")
  expect_is(x$elevation_lower, "numeric")
  expect_is(x$elevation_upper, "numeric")
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
  validate_aoh_data(
    x = x,
    elevation_data = elevation_data,
    habitat_data = habitat_data,
    crosswalk_data = crosswalk_jung_lvl2_data
  )
  # clean up
  unlink(output_dir, recursive = TRUE)
})


test_that("example data", {
  # skip if needed
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("vcr")
  skip_if_cached_data_not_available()
  skip_if_zenodo_data_not_available(latest_lumb_cgls_version)
  skip_if_zenodo_data_not_available(latest_elevation_version)
  # specify file path
  f <- system.file("extdata", "EXAMPLE_SPECIES.zip", package = "aoh")
  cd <- rappdirs::user_data_dir("aoh")
  cd2 <- tempfile()
  dir.create(cd2, showWarnings = FALSE, recursive = TRUE)
  # load data
  vcr::use_cassette("aoh-example-info", {
    d <-
    suppressMessages(
      create_spp_info_data(
        x = read_spp_range_data(f),
        cache = cd2,
        verbose = TRUE
      )
    )
  })
  # create objects
  x <- suppressMessages(
    suppressWarnings(
      create_spp_aoh_data(
        x = d,
        output_dir = tempdir(),
        cache_dir = cd,
        habitat_version = latest_lumb_cgls_version,
        elevation_version = latest_elevation_version,
        verbose = TRUE
      )
    )
  )
  # tests
  expect_is(x, "sf")
  expect_named(x, aoh_names)
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_equal(
    d,
    dplyr::select(x, -habitat_code, -path, -xmin, -xmax, -ymin, -ymax)
  )
  expect_equal(d$id_no, x$id_no)
  expect_is(x$path, "character")
  expect_equal(sum(is.na(x$path)), 0)
  validate_aoh_data(
    x = x,
    habitat_data = get_lumb_cgls_habitat_data(
      dir = cd, version = latest_lumb_cgls_version
    ),
    elevation_data = get_global_elevation_data(
      dir = cd, version = latest_elevation_version
    ),
    crosswalk_data = crosswalk_lumb_cgls_data
  )
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
  unlink(cd2, force = TRUE, recursive = TRUE)
})

test_that("some species missing habitat data", {
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
  # create copy of spp_habitat_data with missing habitat data for one species
  spp_id <- unique(spp_habitat_data$id_no)[2]
  spp_habitat_data_alt <- dplyr::bind_rows(
    dplyr::filter(spp_habitat_data, !id_no %in% spp_id),
    dplyr::mutate(
      head(dplyr::filter(spp_habitat_data, id_no %in% spp_id), 1),
      code = NA_integer_
    )
  )
  # create output dirs
  output_dir1 <- tempfile()
  output_dir2 <- tempfile()
  dir.create(output_dir1, showWarnings = FALSE, recursive = TRUE)
  dir.create(output_dir2, showWarnings = FALSE, recursive = TRUE)
  # create objects
  x1 <- create_spp_aoh_data(
    x = create_spp_info_data(
      x = read_spp_range_data(f),
      spp_habitat_data = spp_habitat_data,
      spp_summary_data = spp_summary_data,
      verbose = interactive()
    ),
    output_dir = output_dir1,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data,
    verbose = interactive()
  )
  x2 <- create_spp_aoh_data(
    x = create_spp_info_data(
      x = read_spp_range_data(f),
      spp_habitat_data = spp_habitat_data_alt,
      spp_summary_data = spp_summary_data,
      verbose = interactive()
    ),
    output_dir = output_dir2,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data,
    verbose = interactive()
  )
  # tests
  expect_is(x1, "sf")
  expect_is(x2, "sf")
  expect_named(x1, aoh_names)
  expect_named(x2, aoh_names)
  validate_aoh_data(
    x = x1,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data
  )
  validate_aoh_data(
    x = x2,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data
  )
  expect_equal(
    dplyr::select(x1[x1$id_no != spp_id, ], -path),
    dplyr::select(x2[x2$id_no != spp_id, ], -path)
  )
  expect_equal(
    lapply(
      x1$path[x1$id_no != spp_id],
      function(x) terra::values(terra::rast(x))
    ),
    lapply(
      x2$path[x2$id_no != spp_id],
      function(x) terra::values(terra::rast(x))
    )
  )
  expect_equal(x2$habitat_code[x2$id_no == spp_id], "")
  expect_equal(x2$path[x2$id_no == spp_id], NA_character_)
  expect_equal(x2$xmin[x2$id_no == spp_id], NA_real_)
  expect_equal(x2$xmax[x2$id_no == spp_id], NA_real_)
  expect_equal(x2$ymin[x2$id_no == spp_id], NA_real_)
  expect_equal(x2$ymax[x2$id_no == spp_id], NA_real_)
  # clean up
  unlink(output_dir1, recursive = TRUE)
  unlink(output_dir2, recursive = TRUE)
})

test_that("small range data (rasterize_touches, engine = terra)", {
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
  # create objects with rasterize_touches = FALSE
  td1 <- tempfile()
  td2 <- tempfile()
  dir.create(td1, showWarnings = FALSE)
  dir.create(td2, showWarnings = FALSE)
  # create fake species with tiny range
  spp_range <- read_spp_range_data(f)[1, ]
  spp_range <- sf::st_transform(spp_range, 3857)
  spp_range <- sf::st_set_geometry(
    spp_range,
    sf::st_buffer(sf::st_centroid(sf::st_geometry(spp_range)), 1)
  )
  # update habitat data to contain suitable habitat across all cells
  spp_suitable_codes <-
    spp_habitat_data$code[spp_habitat_data$id_no == spp_range$id_no[[1]]][[1]]
  spp_suitable_value <-
    crosswalk_jung_lvl2_data$value[
      crosswalk_jung_lvl2_data$code == spp_suitable_codes
    ][[1]]
  terra::values(habitat_data) <-
    rep(spp_suitable_value, terra::ncell(habitat_data))
  # update elevation data to contain suitable habitat across all cells
  idx <- which(spp_summary_data$id_no == spp_range$id_no)[[1]]
  spp_suitable_elevation <- mean(
    c(
      spp_summary_data$elevation_upper[idx],
      spp_summary_data$elevation_lower[idx]
    )
  )
  terra::values(elevation_data) <-
    rep(spp_suitable_elevation, terra::ncell(elevation_data))
  # load data
  d <- create_spp_info_data(
    x = spp_range,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    verbose = interactive()
  )
  # create objects with rasterize_touches = FALSE
  x1 <- create_spp_aoh_data(
    x = d,
    output_dir = td1,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data,
    rasterize_touches = FALSE,
    engine = "terra",
    verbose = TRUE
  )
  # create objects with rasterize_touches = TRUE, via terra
  x2 <- create_spp_aoh_data(
    x = d,
    output_dir = td2,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data,
    rasterize_touches = TRUE,
    engine = "terra",
    verbose = TRUE
  )
  # tests
  ## verify that x1 only contains NA values, since the species range
  ## does not overlap with any cell centroids
  expect_equal(
    terra::global(terra::rast(x1$path), "notNA", na.rm = TRUE)[[1]],
    0
  )
  ## verify that x2 only has 1 non-NA value, since the species range
  ## does not overlap with any cell centroids
  expect_equal(
    terra::global(terra::rast(x2$path), "notNA", na.rm = TRUE)[[1]],
    1
  )
  ## verify that x2 has a single cell that is not NA
  expect_equal(
    c(na.omit(terra::values(terra::rast(x2$path)))),
    1
  )
  # clean up
  unlink(x1$path[!is.na(x1$path)])
  unlink(x2$path[!is.na(x2$path)])
})

test_that("small range data (rasterize_touches, engine = gdal)", {
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
  # create objects with rasterize_touches = FALSE
  td1 <- tempfile()
  td2 <- tempfile()
  dir.create(td1, showWarnings = FALSE)
  dir.create(td2, showWarnings = FALSE)
  # create fake species with tiny range
  spp_range <- read_spp_range_data(f)[1, ]
  spp_range <- sf::st_transform(spp_range, 3857)
  spp_range <- sf::st_set_geometry(
    spp_range,
    sf::st_buffer(sf::st_centroid(sf::st_geometry(spp_range)), 1)
  )
  # update habitat data to contain suitable habitat across all cells
  spp_suitable_codes <-
    spp_habitat_data$code[spp_habitat_data$id_no == spp_range$id_no[[1]]][[1]]
  spp_suitable_value <-
    crosswalk_jung_lvl2_data$value[
      crosswalk_jung_lvl2_data$code == spp_suitable_codes
    ][[1]]
  terra::values(habitat_data) <-
    rep(spp_suitable_value, terra::ncell(habitat_data))
  # update elevation data to contain suitable habitat across all cells
  idx <- which(spp_summary_data$id_no == spp_range$id_no)[[1]]
  spp_suitable_elevation <- mean(
    c(
      spp_summary_data$elevation_upper[idx],
      spp_summary_data$elevation_lower[idx]
    )
  )
  terra::values(elevation_data) <-
    rep(spp_suitable_elevation, terra::ncell(elevation_data))
  # load data
  d <- create_spp_info_data(
    x = spp_range,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    verbose = interactive()
  )
  # create objects with rasterize_touches = FALSE
  x1 <- create_spp_aoh_data(
    x = d,
    output_dir = td1,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data,
    rasterize_touches = FALSE,
    engine = "gdal",
    verbose = TRUE
  )
  # create objects with rasterize_touches = TRUE, via terra
  x2 <- create_spp_aoh_data(
    x = d,
    output_dir = td2,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data,
    rasterize_touches = TRUE,
    engine = "gdal",
    verbose = TRUE
  )
  # tests
  ## verify that x1 only contains NA values, since the species range
  ## does not overlap with any cell centroids
  expect_equal(
    terra::global(terra::rast(x1$path), "notNA", na.rm = TRUE)[[1]],
    0
  )
  ## verify that x2 only has 1 non-NA value, since the species range
  ## does not overlap with any cell centroids
  expect_equal(
    terra::global(terra::rast(x2$path), "notNA", na.rm = TRUE)[[1]],
    1
  )
  ## verify that x2 has a single cell that is not NA
  expect_equal(
    c(na.omit(terra::values(terra::rast(x2$path)))),
    1
  )
  # clean up
  unlink(x1$path[!is.na(x1$path)])
  unlink(x2$path[!is.na(x2$path)])
})

test_that("amphibian data (IUCN format)", {
  # skip if needed
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  skip_if_gdal_calc_not_available()
  skip_if_iucn_key_missing()
  skip_if_iucn_api_not_available()
  skip_if_not_installed("prepr")
  skip_if_iucn_red_list_data_not_available("AMPHIBIANS.zip")
  skip_if_cached_data_not_available()
  skip_if_zenodo_data_not_available(latest_lumb_cgls_version)
  skip_if_zenodo_data_not_available(latest_elevation_version)
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "AMPHIBIANS.zip"
  )
  cd <- rappdirs::user_data_dir("aoh")
  # load data
  d <- read_spp_range_data(f, n = 50)
  # subset data (i.e. some range restricted species)
  ids <- c(58648, 57930)
  d <- d[which(d$id_no %in%ids), , drop = FALSE]
  assertthat::assert_that(all(ids %in% d$id_no))
  # create objects
  x <- create_spp_aoh_data(
    x = create_spp_info_data(
      x = d,
      cache_dir = cd,
      verbose = interactive()
    ),
    output_dir = tempdir(),
    habitat_version = latest_lumb_cgls_version,
    elevation_version = latest_elevation_version,
    cache_dir = cd,
    engine = "gdal",
    verbose = interactive()
  )
  # tests
  expect_is(x, "sf")
  expect_named(x, aoh_names)
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
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

test_that("reptile data (IUCN format)", {
  # skip if needed
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  skip_if_gdal_calc_not_available()
  skip_if_iucn_key_missing()
  skip_if_iucn_api_not_available()
  skip_if_not_installed("prepr")
  skip_if_iucn_red_list_data_not_available("REPTILES.zip")
  skip_if_cached_data_not_available()
  skip_if_zenodo_data_not_available(latest_lumb_cgls_version)
  skip_if_zenodo_data_not_available(latest_elevation_version)
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "REPTILES.zip"
  )
  cd <- rappdirs::user_data_dir("aoh")
  # load data
  d <- read_spp_range_data(f, n = 50)
  # subset data (i.e. some range restricted species)
  ids <- c(42495889, 10246, 102795062)
  d <- d[which(d$id_no %in%ids), , drop = FALSE]
  assertthat::assert_that(all(ids %in% d$id_no))
  # create objects
  x <- create_spp_aoh_data(
    x = create_spp_info_data(
      x = d,
      cache_dir = cd,
      verbose = interactive()
    ),
    output_dir = tempdir(),
    cache_dir = cd,
    habitat_version = latest_lumb_cgls_version,
    elevation_version = latest_elevation_version,
    engine = "gdal",
    verbose = interactive()
  )
  # tests
  expect_is(x, "sf")
  expect_named(x, aoh_names)
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
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

test_that("terrestrial mammal data (IUCN format)", {
  # skip if needed
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  skip_if_gdal_calc_not_available()
  skip_if_iucn_key_missing()
  skip_if_iucn_api_not_available()
  skip_if_not_installed("prepr")
  skip_if_iucn_red_list_data_not_available("MAMMALS_TERRESTRIAL_ONLY.zip")
  skip_if_cached_data_not_available()
  skip_if_zenodo_data_not_available(latest_lumb_cgls_version)
  skip_if_zenodo_data_not_available(latest_elevation_version)
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "MAMMALS_TERRESTRIAL_ONLY.zip"
  )
  cd <- rappdirs::user_data_dir("aoh")
  # load data
  d <- read_spp_range_data(f, n = 100)
  # subset data (i.e. some range restricted species)
  ids <- c(40555, 136264)
  d <- d[which(d$id_no %in%ids), , drop = FALSE]
  assertthat::assert_that(all(ids %in% d$id_no))
  # create objects
  x <- create_spp_aoh_data(
    x = create_spp_info_data(
      x = d,
      cache_dir = cd,
      verbose = interactive()
    ),
    output_dir = tempdir(),
    cache_dir = cd,
    habitat_version = latest_lumb_cgls_version,
    elevation_version = latest_elevation_version,
    engine = "gdal",
    verbose = interactive()
  )
  # tests
  expect_is(x, "sf")
  expect_named(x, aoh_names)
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
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

test_that("bird data (BirdLife format)", {
  # skip if needed
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  skip_if_gdal_calc_not_available()
  skip_if_iucn_key_missing()
  skip_if_iucn_api_not_available()
  skip_if_not_installed("prepr")
  skip_if_iucn_red_list_data_not_available("BOTW.7z")
  skip_if_cached_data_not_available()
  skip_if_zenodo_data_not_available(latest_lumb_cgls_version)
  skip_if_zenodo_data_not_available(latest_elevation_version)
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "BOTW.7z"
  )
  cd <- rappdirs::user_data_dir("aoh")
  # load data
  d <- suppressWarnings(read_spp_range_data(f, n = 200))
  # subset data (i.e. some range restricted  species)
  ids <- c(22691663, 22709717)
  d <- d[which(d$SISID %in% ids), drop = FALSE]
  d <- d[-which(d$SISID == 22709717 & d$seasonal == 3), , drop = FALSE]
  assertthat::assert_that(all(ids %in% d$SISID))
  # create objects
  x <- create_spp_aoh_data(
    x = create_spp_info_data(
      x = d,
      cache_dir = cd,
      verbose = interactive()
    ),
    output_dir = tempdir(),
    cache_dir = cd,
    habitat_version = latest_lumb_cgls_version,
    elevation_version = latest_elevation_version,
    engine = "gdal",
    verbose = interactive()
  )
  # tests
  expect_is(x, "sf")
  expect_named(x, aoh_names)
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
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

test_that("bird data (alternate BirdLife format)", {
  # skip if needed
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  skip_if_gdal_calc_not_available()
  skip_if_iucn_key_missing()
  skip_if_iucn_api_not_available()
  skip_if_not_installed("prepr")
  skip_if_iucn_red_list_data_not_available("BOTW_2021.7z")
  skip_if_cached_data_not_available()
  skip_if_zenodo_data_not_available(latest_lumb_cgls_version)
  skip_if_zenodo_data_not_available(latest_elevation_version)
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "BOTW_2021.7z"
  )
  cd <- rappdirs::user_data_dir("aoh")
  # load data
  d <- suppressWarnings(read_spp_range_data(f, n = 200))
  # subset data (i.e. some range restricted  species)
  ids <- c(22687417, 22679929)
  d <- d[which(d$id_no %in% ids), drop = FALSE]
  assertthat::assert_that(all(ids %in% d$id_no))
  # create objects
  x <- create_spp_aoh_data(
    x = create_spp_info_data(
      x = d,
      cache_dir = cd,
      verbose = interactive()
    ),
    output_dir = tempdir(),
    cache_dir = cd,
    habitat_version = latest_lumb_cgls_version,
    elevation_version = latest_elevation_version,
    engine = "gdal",
    verbose = interactive()
  )
  # tests
  expect_is(x, "sf")
  expect_named(x, aoh_names)
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
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

test_that("message thrown for missing crosswalk classes", {
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
  # create new crosswalk table
  cw_data <- crosswalk_jung_lvl2_data
  remove_class <- spp_habitat_data$code[[1]]
  cw_data <- cw_data[cw_data$code != remove_class, , drop = FALSE]
  # filter data
  r <- read_spp_range_data(f)[seq_len(10)]
  # load data
  d <- create_spp_info_data(
    x = r,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    verbose = interactive()
  )
  # create objects
  expect_message(
    x <- create_spp_aoh_data(
      x = d,
      output_dir = tempdir(),
      habitat_data = habitat_data,
      elevation_data = elevation_data,
      crosswalk_data = cw_data,
      verbose = FALSE
    ),
    "^.*crosswalk.*missing.*habitat.*classification.*$"
  )
  # tests
  expect_is(x, "sf")
  expect_named(x, aoh_names)
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_equal(
    d,
    dplyr::select(x, -habitat_code, -path, -xmin, -xmax, -ymin, -ymax)
  )
  expect_is(x$id_no, "integer")
  expect_is(x$binomial, "character")
  expect_is(x$seasonal, "integer")
  expect_is(x$migratory, "logical")
  expect_is(x$category, "character")
  expect_is(x$full_habitat_code, "character")
  expect_is(x$habitat_code, "character")
  expect_is(x$elevation_lower, "numeric")
  expect_is(x$elevation_upper, "numeric")
  expect_is(x$path, "character")
})

test_that("message thrown for small raster extent", {
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
  # prepare range data
  r <- read_spp_range_data(f)[seq_len(10)]
  r <- sf::st_buffer(r, 100000)
  # load data
  d <- create_spp_info_data(
    x = r,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    verbose = interactive()
  )
  # create objects
  expect_message(
    x <- create_spp_aoh_data(
      x = d,
      output_dir = tempdir(),
      habitat_data = habitat_data,
      elevation_data = elevation_data,
      crosswalk_data = crosswalk_jung_lvl2_data,
      verbose = FALSE
    ),
    "^.*habitat.*elevation.*fully.*ranges.*$"
  )
  # tests
  expect_is(x, "sf")
  expect_named(x, aoh_names)
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_equal(
    d,
    dplyr::select(x, -habitat_code, -path, -xmin, -xmax, -ymin, -ymax)
  )
  expect_is(x$id_no, "integer")
  expect_is(x$binomial, "character")
  expect_is(x$seasonal, "integer")
  expect_is(x$migratory, "logical")
  expect_is(x$category, "character")
  expect_is(x$full_habitat_code, "character")
  expect_is(x$habitat_code, "character")
  expect_is(x$elevation_lower, "numeric")
  expect_is(x$elevation_upper, "numeric")
  expect_is(x$path, "character")
})
