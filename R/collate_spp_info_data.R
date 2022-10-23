#' @include internal.R misc_terra.R misc_sf.R
NULL

#' Collate species information data
#'
#' Collate species data obtained from the
#' [International Union for Conservation of Nature (IUCN) Red List of
#' Threatened Species](https://www.iucnredlist.org/).
#' Specifically, it combines data delineating species geographic ranges,
#' habitat preferences, and elevational limits into a single dataset.
#'
#' @param x `sf::st_sf()` Spatial object containing species range data.
#'  Arguments should follow the output format from [clean_spp_range_data()].
#'
#' @param spp_summary_data `tibble:tibble()` Table containing species
#'   summary data.
#'   Arguments should follow the output format from [get_spp_summary_data()].
#'
#' @param spp_habitat_data `tibble:tibble()` Table containing species summary
#'   data.
#'   Arguments should follow the output format from [get_spp_habitat_data()].
#'
#' @inheritParams create_spp_info_data
#'
#' @return
#' A [sf::st_sf()] spatial object containing an updated version of the
#' argument to `x` that has been cleaned and contains the following additional
#' columns:
#' `"category"`, `"migratory", `"full_habitat_code"`, `"elevation_lower"`, and
#' `"elevation_upper"`.
#'
#' @noRd
collate_spp_info_data <- function(x,
                                  spp_summary_data,
                                  spp_habitat_data,
                                  omit_habitat_codes =
                                    iucn_habitat_codes_marine(),
                                  adjust_elevational_limits = TRUE,
                                  adjust_habitat_codes = TRUE,
                                  verbose = TRUE) {
  # assert arguments are valid
  ## initial checks
  assertthat::assert_that(
    ## x
    inherits(x, "sf"),
    nrow(x) > 0,
    assertthat::has_name(x, "id_no"),
    assertthat::has_name(x, "seasonal"),
    assertthat::has_name(x, "class"),
    ## adjust_habitat_codes
    assertthat::is.flag(adjust_habitat_codes),
    assertthat::noNA(adjust_habitat_codes),
    ## adjust_elevational_limits
    assertthat::is.flag(adjust_elevational_limits),
    assertthat::noNA(adjust_elevational_limits),
    ## spp_summary_data
    inherits(spp_summary_data, "data.frame"),
    assertthat::has_name(spp_summary_data, "id_no"),
    assertthat::has_name(spp_summary_data, "elevation_upper"),
    assertthat::has_name(spp_summary_data, "elevation_lower"),
    assertthat::has_name(spp_summary_data, "category"),
    ## spp_habitat_data
    inherits(spp_habitat_data, "data.frame"),
    assertthat::has_name(spp_habitat_data, "id_no"),
    assertthat::has_name(spp_habitat_data, "code"),
    assertthat::has_name(spp_habitat_data, "habitat"),
    assertthat::has_name(spp_habitat_data, "suitability"),
    assertthat::has_name(spp_habitat_data, "season")
  )
  ## omit_habitat_codes
  if (length(omit_habitat_codes) > 0) {
    assertthat::assert_that(
      is.character(omit_habitat_codes),
      assertthat::noNA(omit_habitat_codes)
    )
  }
  assertthat::assert_that(
    all(x$id_no %in% spp_summary_data$id_no),
    msg = paste0(
      "argument to x contains species not present in summary data ",
      "(i.e., based on matching values in the \"id_no\" columns)"
    )
  )
  if (any(!x$id_no %in% spp_habitat_data$id_no)) {
    # nocov start
    warning(
      paste(
        "argument to \"x\" contains",
        sum(!x$id_no %in% spp_habitat_data$id_no),
        "species lacking habitat classification data"
      ),
      immediate. = TRUE
    )
    # nocov end
  }
  assertthat::assert_that(
    identical(anyDuplicated(spp_summary_data$id_no), 0L),
    msg = paste(
      "argument to \"spp_summary_data\" must not contain duplicate",
      "\"id_no\" values"
    )
  )

  ## identify which species are migratory
  migratory_ids <- na.omit(unique(x$id_no[x$seasonal %in% c(2L, 3L, 4L)]))
  x$migratory <- x$id_no %in% migratory_ids

  # add IUCN Red List category information from summary data
  ## although the range data do contain IUCN Red List categories,
  ## we will overwrite the values with those from the summary data.
  ## this is because the BirdLife dataset does not necessarily provide the
  ## latest IUCN threat categories (e.g., 2020-1 version of BirdLife data
  ## provides IUCN threat categories based on 2020 version of Red List)
  nms <- c("id_no", "category")
  x <- dplyr::left_join(
    x = dplyr::select(x, -.data$category),
    y = spp_summary_data[, nms, drop = FALSE],
    by = "id_no"
  )

  # add elevation columns
  if (isTRUE(adjust_elevational_limits)) {
    ## convert NA values to -500 and 9000 for lower and upper thresholds
    idx <- is.na(spp_summary_data$elevation_lower)
    spp_summary_data$elevation_lower[idx] <- -500
    idx <- is.na(spp_summary_data$elevation_upper)
    spp_summary_data$elevation_upper[idx] <- 9000
    ## overwrite values if lower elevation limit is > upper elevation limit
    idx <- which(
      is.finite(spp_summary_data$elevation_lower) &
      is.finite(spp_summary_data$elevation_upper)
    )
    if (length(idx) > 0) {
      l <- spp_summary_data$elevation_lower[idx]
      u <- spp_summary_data$elevation_upper[idx]
      idx2 <- idx[which(l > u)]
      spp_summary_data$elevation_lower[idx2] <- -500
      spp_summary_data$elevation_upper[idx2] <- 9000
      rm(l, u, idx2)
    }
    ## replace 0 m values with -500 m values
    idx <- which(spp_summary_data$elevation_lower == 0)
    spp_summary_data$elevation_lower[idx] <- -500
    ## fix values that are outside limits
    idx <- which(
      is.finite(spp_summary_data$elevation_lower) &
      is.finite(spp_summary_data$elevation_upper)
    )
    spp_summary_data$elevation_lower[idx] <-
      pmax(spp_summary_data$elevation_lower[idx], -500)
    spp_summary_data$elevation_upper[idx] <-
      pmin(spp_summary_data$elevation_upper[idx], 9000)
    rm(idx)
    ## fix values that are within 50 m of each other
    elev_diff <-
      spp_summary_data$elevation_upper - spp_summary_data$elevation_lower
    elev_pad <- (50 - elev_diff) / 2
    idx <- which(elev_diff < 50)
    if (length(idx) > 0) {
      spp_summary_data$elevation_lower[idx] <-
        spp_summary_data$elevation_lower[idx] - elev_pad[idx]
      spp_summary_data$elevation_upper[idx] <-
        spp_summary_data$elevation_upper[idx] + elev_pad[idx]
    }
    rm(idx, elev_diff, elev_pad)
  } else {
    assertthat::assert_that(
      all(is.finite(spp_summary_data$elevation_lower)) &&
        all(is.finite(spp_summary_data$elevation_upper)),
      msg = paste(
        "spp_summary_data must have finite (non-NA) values in the",
        "\"elevation_lower\" and \"elevation_upper\" columns when",
        "setting \"adjust_elevational_limits = FALSE\""
      )
    )
  }

  ## append columns
  nms <- c("id_no", "elevation_lower", "elevation_upper")
  x <- dplyr::left_join(
    x = x,
    y = spp_summary_data[, nms, drop = FALSE],
    by = "id_no"
  )

  # add habitat preference columns
  ## remove rows for data with no habitat code
  idx <- !is.na(spp_habitat_data$code)
  spp_habitat_data <- spp_habitat_data[idx, , drop = FALSE]
  ## remove rows for taxa not present in x
  idx <- spp_habitat_data$id_no %in% x$id_no
  spp_habitat_data <- spp_habitat_data[idx, , drop = FALSE]
  ## convert season descriptions to codes
  spp_habitat_data$seasonal <- convert_to_seasonal_id(spp_habitat_data$season)
  ## coerce code column to character for subsequent joins with habitat_data
  spp_habitat_data$habitat_code <- as.character(spp_habitat_data$code)
  ## remove omit habitat codes
  idx <- which(!spp_habitat_data$habitat_code %in% omit_habitat_codes)
  spp_habitat_data <- spp_habitat_data[idx, , drop = FALSE]
  ## if exact matches for habitat codes should not be used then...
  if (isTRUE(adjust_habitat_codes)) {
    ### create new data by merging habitat codes following KBA guidelines
    spp_habitat_data <- dplyr::bind_rows(
      list(
        ### resident (non-migratory species)
        prepare_habitat_codes(
          dplyr::filter(spp_habitat_data, .data$seasonal %in% migratory_ids),
          focal_seasonal = 1L,
          extra_seasonal = c(2L, 3L, 4L, 5L, NA_integer_)
        ),
        ### resident (migratory species)
        prepare_habitat_codes(
          dplyr::filter(spp_habitat_data, !.data$seasonal %in% migratory_ids),
          focal_seasonal = 1L,
          extra_seasonal = c(2L, 3L, 5L, NA_integer_)
        ),
        ### breeding
        prepare_habitat_codes(
          spp_habitat_data,
          focal_seasonal = 2L,
          extra_seasonal = c(1L, 5L, NA_integer_)
        ),
        ### non-breeding
        prepare_habitat_codes(
          spp_habitat_data,
          focal_seasonal = 3L,
          extra_seasonal = c(1L, 5L, NA_integer_)
        ),
        ### passage
        prepare_habitat_codes(
          spp_habitat_data,
          focal_seasonal = 4L,
          extra_seasonal = c(1L, 5L, NA_integer_)
        )
      )
    )
  }
  ## combine habitat codes into a single list-column
  spp_habitat_data <- dplyr::group_by(
    spp_habitat_data, .data$id_no, .data$seasonal
  )
  spp_habitat_data <- dplyr::summarize(
    spp_habitat_data,
    full_habitat_code = list(unique(.data$habitat_code))
  )
  spp_habitat_data <- dplyr::ungroup(spp_habitat_data)

  ## add habitat codes to x
  nms <- c("id_no", "seasonal", "full_habitat_code")
  x <- dplyr::left_join(
    x = x,
    y = spp_habitat_data[, nms, drop = FALSE],
    by = c("id_no", "seasonal")
  )

  ## standardize values in habitat code column
  ## i.e., replace NULLs with an empty character vector, and
  ## lexicographically sort habitat codes
  x$full_habitat_code <- lapply(x$full_habitat_code, function(x) {
    if (is.character(x)) {
      return(stringi::stri_sort(x, numeric = TRUE))
    }
    character(0)
  })

  ## convert list-column to "|" delimited character-column
  x$full_habitat_code <- vapply(
    x$full_habitat_code, paste, character(1), collapse = "|"
  )

  # return result
  x
}

prepare_habitat_codes <- function(x, focal_seasonal, extra_seasonal) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, "data.frame"),
    assertthat::has_name(x, "id_no"),
    assertthat::has_name(x, "seasonal"),
    assertthat::has_name(x, "habitat_code"),
    is.integer(focal_seasonal),
    assertthat::is.count(focal_seasonal),
    assertthat::noNA(focal_seasonal),
    is.integer(extra_seasonal),
    length(extra_seasonal) >= 1,
    isTRUE(!focal_seasonal %in% extra_seasonal)
  )

  # if x has zero rows, then return empty data frame
  if (nrow(x) == 0) return(x)

  # convert NA values to an integer code to ensure correct handling
  if (any(is.na(extra_seasonal))) {
    extra_seasonal[is.na(extra_seasonal)] <- -9999L
    x$seasonal[is.na(x$seasonal)] <- -9999L
  }

  # extract data
  x_focal <- dplyr::filter(x, .data$seasonal == focal_seasonal)
  x_extra <- dplyr::filter(x, .data$seasonal %in% extra_seasonal)

  # if there are no seasonal codes which need updating, then return focal data
  if (nrow(x_extra) == 0) return(x_focal)

  # replace extra seasonal codes with focal seasonal code
  x_extra$seasonal <- focal_seasonal

  # merge results
  out <- dplyr::bind_rows(x_focal, x_extra)

  # remove duplicates
  out <- dplyr::distinct(
    out, .data$id_no, .data$seasonal, .data$habitat_code, .keep_all = TRUE
  )

  # return results
  out
}
