context("collate_spp_info_data")

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
  # force summary data to have different iucn categories to make sure
  # that iucn categories are assigned correctly
  d1$category <- sample(letters[1:5], nrow(d1), replace = TRUE)
  # create object
  x <- collate_spp_info_data(
    x = clean_spp_range_data(d1),
    spp_summary_data = d2,
    spp_habitat_data = d3,
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
  expect_true(assertthat::has_name(x, "migratory"))
  expect_is(x$migratory, "logical")
  expect_true(assertthat::noNA(x$migratory))
  expect_true(assertthat::has_name(x, "category"))
  expect_is(x$category, "character")
  expect_true(assertthat::has_name(x, "full_habitat_code"))
  expect_is(x$full_habitat_code, "character")
  expect_true(assertthat::has_name(x, "elevation_lower"))
  expect_is(x$elevation_lower, "numeric")
  expect_true(assertthat::noNA(x$elevation_lower))
  expect_true(assertthat::has_name(x, "elevation_upper"))
  expect_is(x$elevation_upper, "numeric")
  expect_true(assertthat::noNA(x$elevation_upper))
  expect_is(x$category, "character")
  expect_equal(x$category, d2$category[match(x$id_no, d2$id_no)])
})

test_that("elevation limits (adjust_elevational_limits = TRUE)", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f1 <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  f2 <- system.file("testdata", "sim_spp_summary_data.csv", package = "aoh")
  f3 <- system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh")
  # load data
  d1 <- read_spp_range_data(f1)
  cd1 <- clean_spp_range_data(d1)
  d2 <- utils::read.table(
    f2, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  d3 <- utils::read.table(
    f3, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  # manually update elevational limits
  d2_1 <- d2
  ## case 1
  id_1 <- d2$id_no[[1]]
  d2_1$elevation_lower[[1]] <- -1000 # should be fixed to -500
  ## case 2
  id_2 <- d2$id_no[[2]]
  d2_1$elevation_upper[[2]] <- 1e6 # should be fixed to 9000
  ## case 3
  id_3 <- d2$id_no[[3]]
  d2_1$elevation_lower[[3]] <- NA_real_ # should be fixed to -500
  ## case 4
  id_4 <- d2$id_no[[4]]
  d2_1$elevation_upper[[4]] <- NA_real_ # should be fixed to 9000
  ## case 5
  id_5 <- d2$id_no[[5]]
  d2_1$elevation_lower[[5]] <- 5 # should be fixed to -19
  d2_1$elevation_upper[[5]] <- 7 # should be fixed to 31
  ## case 6
  id_6 <- d2$id_no[[6]]
  d2_1$elevation_lower[[6]] <- 500 # should be fixed to -500
  d2_1$elevation_upper[[6]] <- 100 # should be fixed to 9000
  # create objects
  x <- collate_spp_info_data(
    x = cd1,
    spp_summary_data = d2,
    spp_habitat_data = d3,
    verbose = interactive()
  )
  y <- collate_spp_info_data(
    x = cd1,
    spp_summary_data = d2_1,
    spp_habitat_data = d3,
    adjust_elevational_limits = TRUE,
    verbose = interactive()
  )
  # tests
  x_0 <- dplyr::filter(
    x, !.data$id_no %in% c(id_1, id_2, id_3, id_4, id_5, id_6)
  )
  y_0 <- dplyr::filter(
    y, !.data$id_no %in% c(id_1, id_2, id_3, id_4, id_5, id_6)
  )
  expect_equal(x_0, y_0)
  ## case 1
  x_1 <- dplyr::filter(x, .data$id_no == id_1)
  y_1 <- dplyr::filter(y, .data$id_no == id_1)
  expect_true(all(y_1$elevation_lower == -500))
  expect_equal(y_1$elevation_upper, x_1$elevation_upper)
  ## case 2
  x_2 <- dplyr::filter(x, .data$id_no == id_2)
  y_2 <- dplyr::filter(y, .data$id_no == id_2)
  expect_true(all(y_2$elevation_upper == 9000))
  expect_equal(y_2$elevation_lower, x_2$elevation_lower)
  ## case 3
  x_3 <- dplyr::filter(x, .data$id_no == id_3)
  y_3 <- dplyr::filter(y, .data$id_no == id_3)
  expect_true(all(y_3$elevation_lower == -500))
  expect_equal(y_3$elevation_upper, x_3$elevation_upper)
  ## case 4
  x_4 <- dplyr::filter(x, .data$id_no == id_4)
  y_4 <- dplyr::filter(y, .data$id_no == id_4)
  expect_true(all(y_4$elevation_upper == 9000))
  expect_equal(y_4$elevation_lower, x_4$elevation_lower)
  ## case 5
  y_5 <- dplyr::filter(y, .data$id_no == id_5)
  expect_true(all(y_5$elevation_lower == -19))
  expect_true(all(y_5$elevation_upper == 31))
  ## case 6
  y_6 <- dplyr::filter(y, .data$id_no == id_6)
  expect_true(all(y_6$elevation_lower == -500))
  expect_true(all(y_6$elevation_upper == 9000))
})

test_that("elevation limits (adjust_elevational_limits = FALSE)", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f1 <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  f2 <- system.file("testdata", "sim_spp_summary_data.csv", package = "aoh")
  f3 <- system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh")
  # load data
  d1 <- read_spp_range_data(f1)
  cd1 <- clean_spp_range_data(d1)
  d2 <- utils::read.table(
    f2, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  d3 <- utils::read.table(
    f3, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  # manually update elevational limits
  d2$elevation_lower[is.na(d2$elevation_lower)] <- -500
  d2$elevation_upper[is.na(d2$elevation_upper)] <- 9000
  ## case 1
  d2_1 <- d2
  d2_1$elevation_lower[[1]] <- -1000
  d2_1$elevation_upper[[2]] <- 1e6
  d2_1$elevation_lower[[3]] <- 5
  d2_1$elevation_upper[[3]] <- 7
  ## case 2
  d2_2 <- d2
  d2_2$elevation_lower <- NA_real_
  ## case 3
  d2_3 <- d2
  d2_3$elevation_upper <- NA_real_
  # create objects
  x <- collate_spp_info_data(
    x = cd1,
    spp_summary_data = d2_1,
    spp_habitat_data = d3,
    adjust_elevational_limits = FALSE,
    verbose = interactive()
  )
  # tests
  expect_equal(
    tibble::tibble(id_no = x$id_no, elevation_lower = x$elevation_lower),
    tibble::tibble(id_no = x$id_no) %>%
      dplyr::left_join(
        dplyr::select(d2_1, .data$id_no, .data$elevation_lower),
        by = "id_no"
      )
  )
  expect_equal(
    tibble::tibble(id_no = x$id_no, elevation_upper = x$elevation_upper),
    tibble::tibble(id_no = x$id_no) %>%
      dplyr::left_join(
        dplyr::select(d2_1, .data$id_no, .data$elevation_upper),
        by = "id_no"
      )
  )
  expect_error(
    collate_spp_info_data(
      x = cd1,
      spp_summary_data = d2_2,
      spp_habitat_data = d3,
      adjust_elevational_limits = FALSE,
      verbose = interactive()
    ),
    "must have finite"
  )
  expect_error(
    collate_spp_info_data(
      x = cd1,
      spp_summary_data = d2_3,
      spp_habitat_data = d3,
      adjust_elevational_limits = FALSE,
      verbose = interactive()
    ),
    "must have finite"
  )
})

test_that("habitat codes (adjust_habitat_codes = TRUE)", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f1 <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  f2 <- system.file("testdata", "sim_spp_summary_data.csv", package = "aoh")
  f3 <- system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh")
  # load data
  d1 <- read_spp_range_data(f1)
  cd1 <- clean_spp_range_data(d1)
  d2 <- utils::read.table(
    f2, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  d3 <- utils::read.table(
    f3, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  # create object
  x <- collate_spp_info_data(
    x = cd1,
    spp_summary_data = d2,
    spp_habitat_data = d3,
    adjust_habitat_codes = TRUE,
    verbose = interactive()
  )
  # tests
  ids <- unique(d1$id_no)
  for (i in seq_along(ids)) {
    s <- dplyr::filter(cd1, id_no == ids[i])
    h <- dplyr::filter(d3, id_no == ids[i])
    m <- any(c(2L, 3L, 4L) %in% s$seasonal)
    h$seasonal <- convert_to_seasonal_id(h$season)
    for (j in unique(s$seasonal)) {
      w <- dplyr::filter(x, id_no == ids[i], seasonal == j)
      if ((j == 1) && !isTRUE(m)) {
        h_ids <- h$code[(h$seasonal %in% seq_len(5)) | is.na(h$seasonal)]
      } else if ((j == 1) && isTRUE(m)) {
        h_ids <- h$code[(h$seasonal %in% c(1L, 2L, 3L, 5L)) | is.na(h$seasonal)]
      } else if (j == 2) {
        h_ids <- h$code[(h$seasonal %in% c(1L, 2L, 5L)) | is.na(h$seasonal)]
      } else if (j == 3) {
        h_ids <- h$code[(h$seasonal %in% c(1L, 3L, 5L)) | is.na(h$seasonal)]
      } else if (j == 4) {
        h_ids <- h$code[(h$seasonal %in% c(1L, 4L, 5L)) | is.na(h$seasonal)]
      } else {
        stop("invalid seasonal id")
      }
      h_code <- paste(sort(unique(h_ids)), collapse = "|")
      expect_equal(w$full_habitat_code, h_code)
    }
  }
})

test_that("habitat codes (adjust_habitat_codes = FALSE)", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f1 <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  f2 <- system.file("testdata", "sim_spp_summary_data.csv", package = "aoh")
  f3 <- system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh")
  # load data
  d1 <- read_spp_range_data(f1)
  cd1 <- clean_spp_range_data(d1)
  d2 <- utils::read.table(
    f2, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  d3 <- utils::read.table(
    f3, header = TRUE, sep = ",", stringsAsFactors = FALSE
  )
  # create object
  x <- collate_spp_info_data(
    x = cd1,
    spp_summary_data = d2,
    spp_habitat_data = d3,
    adjust_habitat_codes = FALSE,
    verbose = interactive()
  )
  # tests
  ids <- unique(d1$id_no)
  for (i in seq_along(ids)) {
    s <- dplyr::filter(cd1, id_no == ids[i])
    h <- dplyr::filter(d3, id_no == ids[i])
    h$seasonal <- convert_to_seasonal_id(h$season)
    for (j in unique(s$seasonal)) {
      w <- dplyr::filter(x, id_no == ids[i], seasonal == j)
      if (j == 1) {
        h_ids <- h$code[h$seasonal %in% 1L]
      } else if (j == 2) {
        h_ids <- h$code[h$seasonal %in% 2L]
      } else if (j == 3) {
        h_ids <- h$code[h$seasonal %in% 3L]
      } else if (j == 4) {
        h_ids <- h$code[h$seasonal %in% 4L]
      } else {
        stop("invalid seasonal id")
      }
      h_code <- paste(sort(unique(h_ids)), collapse = "|")
      expect_equal(w$full_habitat_code, h_code)
    }
  }
})
