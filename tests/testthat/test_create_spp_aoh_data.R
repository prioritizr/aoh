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
  skip_if_gdal_not_available()
  skip_if_gdal_python_not_available()
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
  skip_if_gdal_not_available()
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
  skip_if_offline()
  skip_if_iucn_key_missing()
  # specify file path
  f <- system.file("extdata", "EXAMPLE_SPECIES.zip", package = "aoh")
  cd <- rappdirs::user_data_dir("aoh")
  # load data
  d <- create_spp_info_data(
    x = read_spp_range_data(f),
    cache_dir = cd,
    verbose = interactive()
  )
  # create objects
  x <- suppressWarnings(
    create_spp_aoh_data(
      x = d,
      output_dir = tempdir(),
      cache_dir = cd,
      habitat_version = latest_lumbierres_version,
      elevation_version = latest_elevation_version,
      verbose = interactive()
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
    habitat_data = get_lumbierres_habitat_data(
      dir = cd, version = latest_lumbierres_version
    ),
    elevation_data = get_global_elevation_data(
      dir = cd, version = latest_elevation_version
    ),
    crosswalk_data = crosswalk_lumbierres_data
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

test_that("different habitat data produce similar results", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_iucn_key_missing()
  skip_if_gdal_not_available()
  skip_if_gdal_python_not_available()
  skip_if_grass_not_available()
  # specify file path
  f <- system.file("extdata", "EXAMPLE_SPECIES.zip", package = "aoh")
  cd <- rappdirs::user_data_dir("aoh")
  # load data
  d <- create_spp_info_data(
    x = read_spp_range_data(f),
    cache_dir = cd,
    verbose = interactive()
  )
  # create output dirs
  output_dir1 <- tempfile()
  output_dir2 <- tempfile()
  output_dir3 <- tempfile()
  dir.create(output_dir1, showWarnings = FALSE, recursive = TRUE)
  dir.create(output_dir2, showWarnings = FALSE, recursive = TRUE)
  dir.create(output_dir3, showWarnings = FALSE, recursive = TRUE)
  # create objects
  x1 <- suppressWarnings(
    create_spp_aoh_data(
      x = d,
      output_dir = output_dir1,
      habitat_data = get_jung_lvl2_habitat_data(
        dir = cd, version = latest_jung_version
      ),
      crosswalk_data = crosswalk_jung_lvl2_data,
      cache_dir = cd,
      elevation_version = latest_elevation_version,
      engine = "terra",
      verbose = interactive()
    )
  )
  x2 <- suppressWarnings(
    create_spp_aoh_data(
      x = d,
      output_dir = output_dir2,
      habitat_data = get_lumbierres_habitat_data(
        dir = cd, version = latest_lumbierres_version
      ),
      crosswalk_data = crosswalk_lumbierres_data,
      cache_dir = cd,
      elevation_version = latest_elevation_version,
      engine = "gdal",
      n_threads = 2,
      verbose = interactive()
    )
  )
  x3 <- suppressWarnings(
    create_spp_aoh_data(
      x = d,
      output_dir = output_dir3,
      habitat_data = get_lumbierres_habitat_data(
        dir = cd, version = latest_lumbierres_version
      ),
      crosswalk_data = crosswalk_lumbierres_data,
      cache_dir = cd,
      elevation_version = latest_elevation_version,
      engine = "grass",
      n_threads = 2,
      verbose = interactive()
    )
  )
  # tests
  expect_is(x1, "sf")
  expect_is(x2, "sf")
  expect_is(x3, "sf")
  expect_named(x1, aoh_names)
  expect_named(x2, aoh_names)
  expect_named(x3, aoh_names)
  validate_aoh_data(
    x = x1,
    habitat_data = get_jung_lvl2_habitat_data(
      dir = cd, version = latest_jung_version
    ),
    elevation_data = get_global_elevation_data(
      dir = cd, version = latest_elevation_version
    ),
    crosswalk_data = crosswalk_jung_lvl2_data
  )
  validate_aoh_data(
    x = x2,
    habitat_data = get_lumbierres_habitat_data(
      dir = cd, version = latest_lumbierres_version
    ),
    elevation_data = get_global_elevation_data(
      dir = cd, version = latest_elevation_version
    ),
    crosswalk_data = crosswalk_lumbierres_data
  )
  validate_aoh_data(
    x = x3,
    habitat_data = get_lumbierres_habitat_data(
      dir = cd, version = latest_lumbierres_version
    ),
    elevation_data = get_global_elevation_data(
      dir = cd, version = latest_elevation_version
    ),
    crosswalk_data = crosswalk_lumbierres_data
  )
  expect_equal(
    dplyr::select(x1, -path, -habitat_code),
    dplyr::select(x2, -path, -habitat_code)
  )
  expect_equal(
    dplyr::select(x1, -path, -habitat_code),
    dplyr::select(x3, -path, -habitat_code)
  )
  expect_gt(
    min(vapply(seq_len(nrow(x1)), FUN.VALUE = numeric(1), function(i) {
      terra::global(
        x = terra::rast(x1$path[i]) == terra::rast(x2$path[i]),
        fun = "mean",
        na.rm = TRUE
      )[[1]]
    })),
    0.35
  )
  expect_gt(
    min(vapply(seq_len(nrow(x1)), FUN.VALUE = numeric(1), function(i) {
      terra::global(
        x = terra::rast(x1$path[i]) == terra::rast(x3$path[i]),
        fun = "mean",
        na.rm = TRUE
      )[[1]]
    })),
    0.35
  )
  # clean up
  unlink(output_dir1, recursive = TRUE)
  unlink(output_dir2, recursive = TRUE)
  unlink(output_dir3, recursive = TRUE)
})

test_that("amphibian data (IUCN format)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_gdal_python_not_available()
  skip_if_iucn_key_missing()
  skip_if_not_installed("prepr")
  skip_if_iucn_red_list_data_not_available("AMPHIBIANS.zip")
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
    habitat_version = latest_lumbierres_version,
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
  skip_if_offline()
  skip_if_gdal_python_not_available()
  skip_if_iucn_key_missing()
  skip_if_not_installed("prepr")
  skip_if_iucn_red_list_data_not_available("REPTILES.zip")
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
    habitat_version = latest_lumbierres_version,
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
  skip_if_offline()
  skip_if_gdal_python_not_available()
  skip_if_iucn_key_missing()
  skip_if_not_installed("prepr")
  skip_if_iucn_red_list_data_not_available("MAMMALS_TERRESTRIAL_ONLY.zip")
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
    habitat_version = latest_lumbierres_version,
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
  skip_if_offline()
  skip_if_gdal_python_not_available()
  skip_if_iucn_key_missing()
  skip_if_not_installed("prepr")
  skip_if_iucn_red_list_data_not_available("BOTW.7z")
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
    habitat_version = latest_lumbierres_version,
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
