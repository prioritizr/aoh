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
  expect_true(all(x$code %in% iucn_habitat_data$code))
  expect_true(assertthat::has_name(x, "habitat"))
  expect_true(assertthat::noNA(x$habitat))
  expect_true(all(x$habitat %in% iucn_habitat_data$name))
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

validate_aoh_data <- function(x, elevation_data, habitat_data, crosswalk_data) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, "sf"),
    inherits(elevation_data, "SpatRaster"),
    inherits(habitat_data, "SpatRaster"),
    inherits(crosswalk_data, "data.frame")
  )
  # set terra options
  on.exit(terra::terraOptions(progress = 3, tempdir = tempdir()))
  # compute results
  result <- vapply(seq_len(nrow(x)), FUN.VALUE = logical(1), function(i) {
    ## skip check if NA
    if (is.na(x$path[[i]])) {
      expect_equal(x$habitat_code[[i]], "")
      return(TRUE)
    }
    ## create temporary directory for processing
    tmp_dir <- tempfile()
    dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
    terra::terraOptions(progress = 0, tempdir = tmp_dir)
    ## import aoh
    curr_aoh <- terra::rast(x$path[[i]])
    ## prepare data
    curr_elev <- terra::crop(elevation_data, curr_aoh)
    curr_habitat <- terra::crop(habitat_data, curr_aoh)
    ## extract raster values
    curr_spp_codes <- strsplit(x$habitat_code[[i]], "|", fixed = TRUE)[[1]]
    curr_spp_values <- crosswalk_data[
      crosswalk_data$code %in% curr_spp_codes, , drop = FALSE
    ]
    curr_spp_values <- unique(curr_spp_values$value)
    ## initialize aoh
    correct_aoh <- terra::app(curr_habitat, function(x) {
      as.numeric(x %in% curr_spp_values)
    })
    ## set places outside elevation limits to 0
    correct_aoh <-
      correct_aoh *
      ((curr_elev <= x$elevation_upper[[i]]) &
      (curr_elev >= x$elevation_lower[[i]]))
    ## create mask
    v <- x[i, , drop = FALSE]
    v$idx <- 1
    curr_mask <- terra_fasterize(
      sf = v[, "idx", drop = FALSE],
      raster = curr_aoh
    )
    ## mask by species range
    correct_aoh <- terra::mask(correct_aoh, curr_mask)
    ## tests
    expect_equivalent(terra::values(correct_aoh), terra::values(curr_aoh))
    ## clean up
    unlink(tmp_dir, recursive = TRUE, force = TRUE)
    ## return success
    TRUE
  })
  # return result
  all(result)
}
