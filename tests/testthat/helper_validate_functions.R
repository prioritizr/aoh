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
    correct_aoh[is.na(correct_aoh)] <- 0
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

validate_info_data <- function(x, spp_habitat_data, spp_summary_data) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, "sf"),
    inherits(spp_habitat_data, "data.frame"),
    inherits(spp_summary_data, "data.frame")
  )

  # initial checks
  expect_true(is.numeric(x$id_no))
  expect_equal(nrow(x), dplyr::n_distinct(x$id_no, x$seasonal))
  expect_is(x$binomial, "character")
  expect_true(is.numeric(x$seasonal))
  expect_is(x$full_habitat_code, "character")
  expect_true(is.numeric(x$elevation_lower))
  expect_true(is.numeric(x$elevation_upper))

  # main checks
  result <- lapply(seq_len(nrow(x)), function(i) {
    ## extract information
    curr_x <- x[i, , drop = FALSE]
    curr_id <- curr_x$id_no
    curr_seas <- curr_x$seasonal
    curr_sum <-
      spp_summary_data[which(spp_summary_data$id_no == curr_id), , drop = FALSE]
    curr_spp_hab <-
      spp_habitat_data[which(spp_habitat_data$id_no == curr_id), , drop = FALSE]
    ## prepare elevational limits
    curr_el <- curr_sum$elevation_lower
    curr_eu <- curr_sum$elevation_upper
    if (is.na(curr_el)) curr_el <- -500
    if (isTRUE(curr_el == 0)) curr_el <- -500
    if (is.na(curr_eu)) curr_eu <- 9000
    if (curr_el > curr_eu) {
      curr_el <- -500
      curr_eu <- 9000
    }
    if (curr_el < -500) curr_el <- -500
    if (curr_eu > 9000) curr_eu <- 9000
    if ((curr_eu - curr_el) < 50) {
      pad <- (50 - (curr_eu - curr_el)) / 2
      curr_el <- curr_el - pad
      curr_eu <- curr_eu + pad
    }
    ## prepare seasonal codes
    curr_spp_hab$seasonal <- aoh:::convert_to_seasonal_id(curr_spp_hab$season)
    if ((curr_seas == 1L) & !isTRUE(curr_x$migratory)) {
       curr_seas_code <- c(1L, 2L, 3L, 4L, 5L)
    } else if ((curr_seas == 1L) & isTRUE(curr_x$migratory)) {
      curr_seas_code <- c(1L, 2L, 3L, 5L)
    } else if (curr_seas == 2L) {
      curr_seas_code <- c(1L, 2L, 5L)
    } else if (curr_seas == 3L) {
      curr_seas_code <- c(1L, 3L, 5L)
    } else if (curr_seas == 4L) {
      curr_seas_code <- c(1L, 4L, 5L)
    }
    curr_spp_hab <- dplyr::filter(
      curr_spp_hab, (seasonal %in% curr_seas_code) | is.na(seasonal)
    )
    ## check expected values
    expect_equal(curr_x$elevation_lower, curr_el)
    expect_equal(curr_x$elevation_upper, curr_eu)
    expect_equal(
      curr_x$full_habitat_code,
      paste(
        stringi::stri_sort(unique(curr_spp_hab$code), numeric = TRUE),
        collapse = "|"
      )
    )
    expect_equal(
      curr_x$migratory,
      any(x$seasonal[x$id_no == curr_id] %in% c(2L, 3L, 4L))
    )
    ## return success
    TRUE
  })
  ## return success
  TRUE
}
