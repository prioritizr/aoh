habitat_codes <- read.table(
    system.file("extdata", "habitat-codes.csv", package = "aoh"),
    header = TRUE, sep = ",", quote = "\"", colClasses = "character"
  )[["iucn_code"]]

habitat_names <- read.table(
    system.file("extdata", "habitat-codes.csv", package = "aoh"),
    header = TRUE, sep = ",", quote = "\"", colClasses = "character"
  )[["name"]]


validate_range_data <- function(x, n = 1) {
  expect_is(x, "sf")
  expect_true(sf::st_crs(x)  == sf::st_crs(4326))
  expect_true(assertthat::has_name(x, "id_no"))
  expect_equal(dplyr::n_distinct(x$id_no), n)
  expect_true(assertthat::noNA(x$id_no))
  expect_true(assertthat::has_name(x, "presence"))
  expect_true(assertthat::noNA(x$presence))
  expect_true(all(x$presence %in% seq_len(6)))
  expect_true(assertthat::has_name(x, "origin"))
  expect_true(assertthat::noNA(x$origin))
  expect_true(all(x$origin %in% seq_len(6)))
  expect_true(assertthat::has_name(x, "seasonal"))
  expect_true(assertthat::noNA(x$seasonal))
  expect_true(all(x$seasonal %in% seq_len(5)))
  expect_true(assertthat::has_name(x, "marine"))
  expect_true(assertthat::noNA(x$marine))
  expect_true(all(x$marine %in% c("true", "false")))
  expect_true(assertthat::has_name(x, "terrestial"))
  expect_true(assertthat::noNA(x$terrestial))
  expect_true(all(x$terrestial %in% c("true", "false")))
  expect_true(assertthat::has_name(x, "freshwater"))
  expect_true(assertthat::noNA(x$freshwater))
  expect_true(all(x$freshwater %in% c("true", "false")))
}

validate_habitat_data <- function(x, n = 1) {
  expect_is(x, "tbl_df")
  expect_true(assertthat::has_name(x, "id_no"))
  expect_equal(dplyr::n_distinct(x$id_no), n)
  expect_true(assertthat::noNA(x$id_no))
  expect_true(assertthat::has_name(x, "code"))
  expect_true(assertthat::noNA(x$code))
  expect_true(all(x$code %in% habitat_codes))
  expect_true(assertthat::has_name(x, "habitat"))
  expect_true(assertthat::noNA(x$habitat))
  expect_true(all(x$habitat %in% habitat_names))
  expect_true(assertthat::has_name(x, "suitability"))
  expect_true(assertthat::noNA(x$suitability))
  expect_true(all(x$suitability == "Suitable"))
  expect_true(assertthat::has_name(x, "season"))
  expect_true(assertthat::noNA(x$season))
  expect_true(
    all(x$season %in% convert_to_seasonal_name(seq_len(5)))
  )
}

validate_summary_data <- function(x, n = 1) {
  expect_is(x, "tbl_df")
  expect_true(assertthat::has_name(x, "id_no"))
  expect_equal(dplyr::n_distinct(x$id_no), n)
  expect_true(assertthat::has_name(x, "taxonid"))
  expect_equal(dplyr::n_distinct(x$taxonid), n)
  expect_true(assertthat::has_name(x, "scientific_name"))
  expect_true(assertthat::noNA(x$scientific_name))
  expect_true(assertthat::has_name(x, "genus"))
  expect_true(assertthat::noNA(x$genus))
  expect_true(assertthat::has_name(x, "elevation_upper"))
  expect_true(assertthat::noNA(x$elevation_upper))
  expect_true(all(is.finite((x$elevation_upper))))
  expect_gte(min(x$elevation_upper), 0)
  expect_true(assertthat::has_name(x, "elevation_lower"))
  expect_true(assertthat::noNA(x$elevation_lower))
  expect_true(all(is.finite((x$elevation_lower))))
  expect_gte(min(x$elevation_lower), 0)
  expect_true(all(x$elevation_lower <= x$elevation_upper))
}
