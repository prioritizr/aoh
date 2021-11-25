context("create_spp_aoh_data()")

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
  # create objects
  x <- create_spp_aoh_data(
    x = d,
    output_dir = tempdir(),
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_data,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    use_gdal = FALSE,
    verbose = TRUE
  )
  # tests
  expect_is(x, "sf")
  expect_named(x, aoh_names)
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_is(x$id_no, "integer")
  expect_is(x$binomial, "character")
  expect_is(x$seasonal, "integer")
  expect_is(x$habitat_code, "character")
  expect_is(x$elevation_lower, "numeric")
  expect_is(x$elevation_upper, "numeric")
  expect_is(x$elevation_upper, "numeric")
  expect_is(x$elevation_upper, "numeric")
  expect_is(x$elevation_upper, "numeric")
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
  # load data
  d <- read_spp_range_data(f)
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
    x = d,
    output_dir = output_dir1,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_data,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    use_gdal = FALSE,
    force = TRUE,
    verbose = interactive()
  )
  x2 <- create_spp_aoh_data(
    x = d,
    output_dir = output_dir2,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_data,
    spp_habitat_data = spp_habitat_data_alt,
    spp_summary_data = spp_summary_data,
    use_gdal = FALSE,
    force = TRUE,
    verbose = interactive()
  )
  # tests
  expect_is(x1, "sf")
  expect_is(x2, "sf")
  expect_named(x1, aoh_names)
  expect_named(x2, aoh_names)
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
  # clean up
  unlink(output_dir1, recursive = TRUE)
  unlink(output_dir2, recursive = TRUE)
})

test_that("species with reversed elevation limits", {
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
  # create copy of spp_summary_data with reversed elevation data
  spp_summary_data_alt <- spp_summary_data
  spp_summary_data_alt$elevation_lower <- spp_summary_data$elevation_upper
  spp_summary_data_alt$elevation_upper <- spp_summary_data$elevation_lower
  # create output dirs
  output_dir1 <- tempfile()
  output_dir2 <- tempfile()
  dir.create(output_dir1, showWarnings = FALSE, recursive = TRUE)
  dir.create(output_dir2, showWarnings = FALSE, recursive = TRUE)
  # create objects
  x1 <- create_spp_aoh_data(
    x = d,
    output_dir = output_dir1,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_data,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    use_gdal = FALSE,
    force = TRUE,
    verbose = interactive()
  )
  x2 <- create_spp_aoh_data(
    x = d,
    output_dir = output_dir2,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_data,
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data_alt,
    use_gdal = FALSE,
    force = TRUE,
    verbose = interactive()
  )
  # tests
  expect_is(x1, "sf")
  expect_is(x2, "sf")
  expect_named(x1, aoh_names)
  expect_named(x2, aoh_names)
  expect_equal(
    dplyr::select(x1, -path),
    dplyr::select(x2, -path)
  )
  expect_equal(
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
  unlink(output_dir1, recursive = TRUE)
  unlink(output_dir2, recursive = TRUE)
})

test_that("example data", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_iucn_key_missing()
  # specify file path
  f <- system.file("extdata", "EXAMPLE_SPECIES.zip", package = "aoh")
  cd <- rappdirs::user_data_dir("aoh")
  hv <- "10.5281/zenodo.4058819"
  # load data
  d <- read_spp_range_data(f)
  # create objects
  x <- suppressWarnings(
    create_spp_aoh_data(
      x = d,
      output_dir = tempdir(),
      cache_dir = cd,
      habitat_version = hv,
      use_gdal = FALSE,
      verbose = interactive()
    )
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

test_that("amphibian data", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_iucn_key_missing()
  skip_if_iucn_red_list_data_not_available("AMPHIBIANS.zip")
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "AMPHIBIANS.zip"
  )
  cd <- rappdirs::user_data_dir("aoh")
  hv <- "10.5281/zenodo.4058819"
  # load data
  d <- read_spp_range_data(f, n = 100)
  # subset data for testing (i.e. some Asian taxa)
  d <- d[d$family == "RANIDAE" & d$genus != "Glandirana", , drop = FALSE]
  # create objects
  x <- create_spp_aoh_data(
    x = d,
    output_dir = tempdir(),
    habitat_version = hv,
    use_gdal = FALSE,
    cache_dir = cd,
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

test_that("reptile data", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_iucn_key_missing()
  skip_if_iucn_red_list_data_not_available("REPTILES.zip")
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "REPTILES.zip"
  )
  cd <- rappdirs::user_data_dir("aoh")
  hv <- "10.5281/zenodo.4058819"
  ev <- "10.5281/zenodo.5719984"
  # load data
  d <- read_spp_range_data(f, n = 50)
  # subset data for testing (i.e. some Australian taxa)
  d <- d[grepl("Tingley", d$compiler, fixed = TRUE), , drop = FALSE]
  # exclude species that that has zero AOH because insufficient data
  # available for rocky habitats in Australia
  d <- d[d$genus != "Pseudothecadactylus", , drop = FALSE]
  # create objects
  x <- create_spp_aoh_data(
    x = d,
    output_dir = tempdir(),
    cache_dir = cd,
    habitat_version = hv,
    elevation_version = ev,
    use_gdal = FALSE,
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

test_that("terrestrial mammal data", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_iucn_key_missing()
  skip_if_iucn_red_list_data_not_available("MAMMALS_TERRESTRIAL_ONLY.zip")
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "MAMMALS_TERRESTRIAL_ONLY.zip"
  )
  cd <- rappdirs::user_data_dir("aoh")
  hv <- "10.5281/zenodo.4058819"
  ev <- "10.5281/zenodo.5719984"
  # load data
  d <- read_spp_range_data(f, n = 50)
  # subset data for testing (i.e. some Oceanic taxa)
  d <- d[d$family == "PTEROPODIDAE", , drop = FALSE]
  # create objects
  x <- create_spp_aoh_data(
    x = d,
    output_dir = tempdir(),
    cache_dir = cd,
    habitat_version = hv,
    elevation_version = ev,
    use_gdal = FALSE,
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

test_that("bird data", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_iucn_key_missing()
  skip_if_iucn_red_list_data_not_available("BOTW.7z")
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "BOTW.7z"
  )
  cd <- rappdirs::user_data_dir("aoh")
  hv <- "10.5281/zenodo.4058819"
  ev <- "10.5281/zenodo.5719984"
  # load data
  d <- suppressWarnings(read_spp_range_data(f, n = 200))
  # subset data (i.e. some Oceanic species)
  d <- d[which(d$Subfamily == "Raphinae"), , drop = FALSE]
  # exclude species that occurs on islands that are missing from habitat data
  d <- d[d$SISID != 22691024, , drop = FALSE]
  # create objects
  x <- create_spp_aoh_data(
    x = d,
    output_dir = tempdir(),
    cache_dir = cd,
    habitat_version = hv,
    elevation_version = ev,
    use_gdal = FALSE,
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

test_that("bird data (migratory)", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_iucn_key_missing()
  skip_if_iucn_red_list_data_not_available("BOTW.7z")
  # specify parameters for processing
  f <- file.path(
    rappdirs::user_data_dir("iucn-red-list-data"),
    "BOTW.7z"
  )
  cd <- rappdirs::user_data_dir("aoh")
  hv <- "10.5281/zenodo.4058819"
  ev <- "10.5281/zenodo.5719984"
  # load data
  d <- read_spp_range_data(f, n = 50)
  # subset data (geographically restricted genus)
  d <- d[startsWith(d$binomial, "Gorsachius"), drop = FALSE]
  d <- d[d$seasonal != 4, drop = FALSE]
  # create objects
  x <- create_spp_aoh_data(
    x = d,
    output_dir = tempdir(),
    cache_dir = cd,
    habitat_version = hv,
    elevation_version = ev,
    use_gdal = FALSE,
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
