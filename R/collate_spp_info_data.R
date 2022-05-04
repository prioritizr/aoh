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
#' `"category"`, `"full_habitat_code"`, `"elevation_lower"`, and
#' `"elevation_upper"`.
#'
#' @noRd
collate_spp_info_data <- function(x,
                                  spp_summary_data,
                                  spp_habitat_data,
                                  omit_habitat_codes =
                                    iucn_habitat_codes_marine(),
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
    warning(
      paste(
        "argument to \"x\" contains",
        sum(!x$id_no %in% spp_habitat_data$id_no),
        "species lacking habitat classification data"
      ),
      immediate. = TRUE
    )
  }
  assertthat::assert_that(
    identical(anyDuplicated(spp_summary_data$id_no), 0L),
    msg = paste(
      "argument to \"spp_summary_data\" must not contain duplicate",
      "\"id_no\" values"
    )
  )

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
  ## convert NA values to -Inf and Inf for lower and upper thresholds
  idx <- is.na(spp_summary_data$elevation_lower)
  spp_summary_data$elevation_lower[idx] <- 0
  idx <- is.na(spp_summary_data$elevation_upper)
  spp_summary_data$elevation_upper[idx] <- 9000
  ## ensure that lower elevation limit is <= upper elevation limit
  idx <- which(
    is.finite(spp_summary_data$elevation_lower) &
    is.finite(spp_summary_data$elevation_upper)
  )
  if (length(idx) > 0) {
    l <- spp_summary_data$elevation_lower[idx]
    u <- spp_summary_data$elevation_upper[idx]
    spp_summary_data$elevation_lower[idx] <- pmin(l, u)
    spp_summary_data$elevation_upper[idx] <- pmax(l, u)
    rm(l, u)
  }
  ## append columns
  nms <- c("id_no", "elevation_lower", "elevation_upper")
  x <- dplyr::left_join(
    x = x,
    y = spp_summary_data[, nms, drop = FALSE],
    by = "id_no"
  )

  # add habitat preference columns
  ## remove rows for taxa missing habitat information
  idx <- !is.na(spp_habitat_data$code)
  spp_habitat_data <- spp_habitat_data[idx, , drop = FALSE]
  ## remove rows for taxa not present in x
  idx <- spp_habitat_data$id_no %in% x$id_no
  spp_habitat_data <- spp_habitat_data[idx, , drop = FALSE]
  ## convert season descriptions to codes
  spp_habitat_data$seasonal <- convert_to_seasonal_id(
    spp_habitat_data$season
  )
  ## for bird species that are missing data for "resident" distributions,
  ## we will assume that the habitat codes for their "resident"
  ## distributions are the same as their "breeding" distributions
  spp_habitat_data <- add_missing_habitat_codes(
    x = x,
    habitat_data = spp_habitat_data,
    source_code = 2,
    update_code = 1,
    class = "AVES"
  )
  ## for bird species that are missing data for "breeding" distributions,
  ## we will assume that the habitat codes for their "breeding"
  ## distributions are the same as their "resident" distributions
  spp_habitat_data <- add_missing_habitat_codes(
    x = x,
    habitat_data = spp_habitat_data,
    source_code = 1,
    update_code = 2,
    class = "AVES"
  )
  ## for bird species that are missing data for "non-breeding" distributions,
  ## we will assume that the habitat codes for their "non-breeding"
  ## distributions are the same as their "resident" distributions
  spp_habitat_data <- add_missing_habitat_codes(
    x = x,
    habitat_data = spp_habitat_data,
    source_code = 1,
    update_code = 3,
    class = "AVES"
  )
  ## remove rows for habitat preferences that are not suitable
  idx <- which(
    tolower(spp_habitat_data$suitability) %in% c("major", "suitable")
  )
  spp_habitat_data <- spp_habitat_data[idx, , drop = FALSE]
  ## coerce code column to character for subsequent joins with habitat_data
  spp_habitat_data$habitat_code <- as.character(spp_habitat_data$code)
  ## remove omit habitat codes
  idx <- which(!spp_habitat_data$habitat_code %in% omit_habitat_codes)
  spp_habitat_data <- spp_habitat_data[idx, , drop = FALSE]
  ## replace rows with NA values "season" in the season column with
  ## duplicates for every possible seasonal code (i.e., [1, 2, 3, 4, 5])
  idx <- is.na(spp_habitat_data$seasonal)
  spp_habitat_data <- dplyr::bind_rows(
    spp_habitat_data[!idx, , drop = FALSE],
    plyr::ldply(
      which(idx),
      function(i) {
        x <- spp_habitat_data[rep(i, 5), , drop = FALSE]
        x$seasonal <- seq_len(5)
        x
      }
    )
  )

  ## combine habitat codes into a single list-column
  spp_habitat_data <- dplyr::group_by(
    spp_habitat_data, .data$id_no, .data$seasonal
  )
  spp_habitat_data <- dplyr::summarize(
    spp_habitat_data,
    full_habitat_code = list(.data$habitat_code)
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

add_missing_habitat_codes <- function(x, habitat_data, source_code,
                                      update_code, class) {
  idx <- which(
    (!x$id_no %in%
      habitat_data$id_no[which(habitat_data$seasonal == update_code)]
    ) &
    (x$id_no %in% x$id_no[which(x$seasonal == update_code)]) &
    (x$class == class)
  )
  missing_ids <- unique(x$id_no[idx])
  if (length(missing_ids > 0)) {
    idx <- which(
      (habitat_data$id_no %in% missing_ids) &
      (habitat_data$seasonal == source_code)
    )
    missing_spp_habitat <- habitat_data[idx, , drop = FALSE]
    missing_spp_habitat$seasonal <- update_code
    habitat_data <- dplyr::bind_rows(
      habitat_data,
      missing_spp_habitat
    )
  }
  habitat_data
}
