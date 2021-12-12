context("format_spp_data")

test_that("general case", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f1 <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  f2 <- system.file("testdata", "sim_spp_summary_data.csv", package = "aoh")
  f3 <- system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh")
  # load data
  d1 <- read_spp_range_data(f1)
  d2 <- utils::read.table(
    f2, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  d3 <- utils::read.table(
    f3, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  # create template raster
  template_data <- terra::rast(
    xmin = -17367531, xmax = 17367569,
    ymin = -6005523, ymax = 7287077,
    res = c(100, 100),
    crs = as.character(sf::st_crs("ESRI:54017"))[[2]]
  )
  # force summary data to have different iucn categories to make sure
  # that iucn categories are assigned correctly
  d1$category <- sample(letters[1:5], nrow(d1), replace = TRUE)
  # create object
  x <- format_spp_data(
    x = clean_spp_range_data(d1),
    spp_summary_data = d2,
    spp_habitat_data = d3,
    template_data = template_data,
    verbose = interactive()
  )
  # tests
  expect_is(x, "sf")
  expect_true(assertthat::has_name(x, "aoh_id"))
  expect_is(x$aoh_id, "character")
  expect_equal(anyDuplicated(x$aoh_id), 0L)
  expect_true(assertthat::has_name(x, "id_no"))
  expect_is(x$id_no, "integer")
  expect_true(all(x$id_no %in% d1$id_no))
  expect_true(assertthat::has_name(x, "seasonal"))
  expect_is(x$seasonal, "integer")
  expect_true(all(x$seasonal %in% seq_len(5)))
  expect_true(assertthat::has_name(x, "habitat_code"))
  expect_is(x$habitat_code, "list")
  expect_true(assertthat::has_name(x, "elevation_lower"))
  expect_is(x$elevation_lower, "numeric")
  expect_true(assertthat::noNA(x$elevation_lower))
  expect_true(assertthat::has_name(x, "elevation_upper"))
  expect_is(x$elevation_upper, "numeric")
  expect_true(assertthat::noNA(x$elevation_upper))
  expect_is(x$category, "character")
  expect_equal(x$category, d2$category[match(x$id_no, d2$id_no)])
})

test_that("NA season values in spp_habitat_data", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f1 <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  f2 <- system.file("testdata", "sim_spp_summary_data.csv", package = "aoh")
  f3 <- system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh")
  # load data
  d1 <- read_spp_range_data(f1)
  d2 <- utils::read.table(
    f2, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  d3 <- utils::read.table(
    f3, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  # create template raster
  template_data <- terra::rast(
    xmin = -17367531, xmax = 17367569,
    ymin = -6005523, ymax = 7287077,
    res = c(100, 100),
    crs = as.character(sf::st_crs("ESRI:54017"))[[2]]
  )
  # prepare data
  d1 <- d1[1, , drop = FALSE]
  d2 <- d2[d2$id_no %in% d1$id_no, , drop = FALSE]
  d3 <- d3[d3$id_no %in% d1$id_no, , drop = FALSE]
  d3_alt <- d3
  d3$season <- NA_character_
  # create objects
  x1 <- format_spp_data(
    x = clean_spp_range_data(d1),
    spp_summary_data = d2,
    spp_habitat_data = d3,
    template_data = template_data,
    verbose = interactive()
  )
  x2 <- format_spp_data(
    x = clean_spp_range_data(d1),
    spp_summary_data = d2,
    spp_habitat_data = d3_alt,
    template_data = template_data,
    verbose = interactive()
  )
  # tests
  expect_is(x1, "sf")
  expect_is(x2, "sf")
  expect_equal(x1, x2)
  expect_equal(
    x1$habitat_code,
    list(stringi::stri_sort(d3$code, numeric = TRUE))
  )
})

test_that("NA code values in spp_habitat_data", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f1 <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  f2 <- system.file("testdata", "sim_spp_summary_data.csv", package = "aoh")
  f3 <- system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh")
  # load data
  d1 <- read_spp_range_data(f1)
  d2 <- utils::read.table(
    f2, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  d3 <- utils::read.table(
    f3, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  # create template raster
  template_data <- terra::rast(
    xmin = -17367531, xmax = 17367569,
    ymin = -6005523, ymax = 7287077,
    res = c(100, 100),
    crs = as.character(sf::st_crs("ESRI:54017"))[[2]]
  )
  # prepare data by replacing NA to code column for one species
  spp_id <- unique(d3$id_no)[2]
  d3_alt <- dplyr::bind_rows(
    dplyr::filter(d3, !id_no %in% spp_id),
    dplyr::mutate(
      head(dplyr::filter(d3, id_no %in% spp_id), 1),
      code = NA_integer_
    )
  )
  # create objects
  x1 <- format_spp_data(
    x = clean_spp_range_data(d1),
    spp_summary_data = d2,
    spp_habitat_data = d3,
    template_data = template_data,
    verbose = interactive()
  )
  x2 <- format_spp_data(
    x = clean_spp_range_data(d1),
    spp_summary_data = d2,
    spp_habitat_data = d3_alt,
    template_data = template_data,
    verbose = interactive()
  )
  # tests
  expect_is(x1, "sf")
  expect_is(x2, "sf")
  expect_equal(
    x1[!x1$id_no %in% spp_id, , drop = FALSE],
    x2[!x2$id_no %in% spp_id, , drop = FALSE]
  )
  expect_identical(
    x2$habitat_code[x2$id_no %in% spp_id],
    list(character(0))
  )
})

test_that("resident distributions of migratory birds lacking habitats", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f1 <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  f2 <- system.file("testdata", "sim_spp_summary_data.csv", package = "aoh")
  f3 <- system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh")
  # load data
  d1 <- read_spp_range_data(f1)
  d2 <- utils::read.table(
    f2, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  d3 <- utils::read.table(
    f3, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  # create template raster
  template_data <- terra::rast(
    xmin = -17367531, xmax = 17367569,
    ymin = -6005523, ymax = 7287077,
    res = c(100, 100),
    crs = as.character(sf::st_crs("ESRI:54017"))[[2]]
  )
  # coerce all species to birds
  d1_birds <- d1
  d1_birds$class <- "AVES"
  # select a migratory species
  mig_id <- which(
    d1$id_no %in% d1$id_no[which(d1$seasonal == 1)] &
    d1$id_no %in% d1$id_no[which(d1$seasonal == 2)] &
    d1$id_no %in% d1$id_no[which(d1$seasonal == 3)]
  )
  mig_id <- sample(d1$id_no[mig_id], 1)
  # remove habitat information for this species' resident distribution
  idx <- which(!(d3$id_no == mig_id & d3$season == "Resident"))
  d3 <- d3[idx, , drop = FALSE]
  # create objects
  x1 <- format_spp_data(
    x = clean_spp_range_data(d1_birds),
    spp_summary_data = d2,
    spp_habitat_data = d3,
    template_data = template_data,
    verbose = interactive()
  )
  x2 <- format_spp_data(
    x = clean_spp_range_data(d1),
    spp_summary_data = d2,
    spp_habitat_data = d3,
    template_data = template_data,
    verbose = interactive()
  )
  # tests
  expect_is(x1, "sf")
  expect_is(x2, "sf")
  expect_equal(
    x1[which(!(x1$id_no == mig_id & x1$seasonal == 1)), ],
    dplyr::mutate(
      x2[which(!(x2$id_no == mig_id & x2$seasonal == 1)), ],
      class = "AVES"
    )
  )
  expect_equal(
    x1$habitat_code[which(x1$id_no == mig_id & x1$seasonal == 1)],
    x1$habitat_code[which(x1$id_no == mig_id & x1$seasonal == 2)]
  )
  expect_equal(
    list(character(0)),
    x2$habitat_code[which(x2$id_no == mig_id & x2$seasonal == 1)]
  )
})
