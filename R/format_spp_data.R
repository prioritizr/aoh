#' @include internal.R clean_spp_range_data.R misc_terra.R misc_sf.R
NULL

#' Format species data
#'
#' Format species data obtained from the
#' [International Union for Conservation of Nature (IUCN) Red List of
#' Threatened Species](https://www.iucnredlist.org/).
#' Specifically, it combines data delineating species geographic ranges,
#' habitat preferences, and altitudinal limits into a single dataset.
#'
#' @inheritParams create spp_aoh_data
#' @inheritParams clean_spp_range_data
#'
#' @return
#' A [sf::st_sf()] spatial object containing an updated version of the
#' argument to `x` that has been cleaned for subsequent analysis
#' (see [clean_spp_range_data()]) and contains the additional columns:
#' `"habitat_codes"`, `"elevation_lower"`, `"elevation_upper"`,
#' `"xmin"`, `"xmax"`, `"ymin"`, and `"ymax"`.
#'
#' @noRd
format_spp_data <- function(x,
                            template_data = get_world_berhman_1km_rast(),
                            spp_summary_data = NULL,
                            spp_habitat_data = NULL,
                            cache_dir = tempdir(),
                            iucn_version = "latest",
                            key = NULL,
                            force = FALSE,
                            omit_habitat_codes =
                              default_omit_iucn_habitat_codes(),
                            verbose = TRUE) {
  # assert arguments are valid
  ## initial checks
  assertthat::assert_that(
    ## x
    inherits(x, "sf"),
    nrow(x) > 0,
    assertthat::has_name(x, "id_no"),
    assertthat::has_name(x, "seasonal"),
    ## omit_habitat_codes
    is.character(omit_habitat_codes),
    assertthat::noNA(omit_habitat_codes)
  )
  ## spp_summary_data
  ### import data
  if (is.null(spp_summary_data)) {
    #### display message
    if (verbose) {
      cli::cli_alert_info("importing species summary data")
    }
    #### processing
    spp_summary_data <- get_spp_summary_data(
      x$id_no, dir = cache_dir, version = iucn_version, key = key,
      force = force, verbose = verbose
    )
  }
  ### validate data
  assertthat::assert_that(
    inherits(spp_summary_data, "data.frame"),
    assertthat::has_name(spp_summary_data, "id_no"),
    assertthat::has_name(spp_summary_data, "elevation_upper"),
    assertthat::has_name(spp_summary_data, "elevation_lower")
  )
  ## spp_habitat_data
  ### import data
  if (is.null(spp_habitat_data)) {
    #### display message
    if (verbose) {
      cli::cli_alert_info("importing species habitat data")
    }
    #### processing
    spp_habitat_data <- get_spp_habitat_data(
      unique(x$id_no), dir = cache_dir, version = iucn_version, key = key,
      force = force, verbose = verbose
    )
  }
  ### validate data
  assertthat::assert_that(
    inherits(spp_habitat_data, "data.frame"),
    assertthat::has_name(spp_habitat_data, "id_no"),
    assertthat::has_name(spp_habitat_data, "code"),
    assertthat::has_name(spp_habitat_data, "habitat"),
    assertthat::has_name(spp_habitat_data, "suitability"),
    assertthat::has_name(spp_habitat_data, "season")
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

  # clean species range data
  ## display message
  if (verbose) {
    cli::cli_progress_step("cleaning species range data")
  }
  ## processing
  x <- clean_spp_range_data(x = x, crs = terra_st_crs(template_data))
  ## addition data validation
  assertthat::assert_that(
    nrow(x) > 0,
    msg = "argument to x does not contain any terrestrial species"
  )
  assertthat::assert_that(
    identical(anyDuplicated(paste0(x$id_no, x$seasonal)), 0L),
    msg = paste(
      "failed to combine multiple geometries for a species'",
      "seasonal distribution"
    )
  )
  assertthat::assert_that(
    all(x$id_no %in% spp_summary_data$id_no),
    msg = paste0(
      "argument to x contains species not present in summary data ",
      "(i.e. based on matching values in the \"id_no\" columns)"
    )
  )

  # add elevation columns
  ## display message
  if (verbose) {
    cli::cli_progress_step("extracting species elevation data")
  }
  ## convert NA values to -Inf and Inf for lower and upper thresholds
  idx <- is.na(spp_summary_data$elevation_lower)
  spp_summary_data$elevation_lower[idx] <- -Inf
  idx <- is.na(spp_summary_data$elevation_upper)
  spp_summary_data$elevation_upper[idx] <- Inf
  ## extract relevant columns
  nms <- c("id_no", "elevation_lower", "elevation_upper")
  spp_summary_data <- spp_summary_data[, nms, drop = FALSE]
  ## append columns
  x <- dplyr::left_join(x, spp_summary_data, by = "id_no")

  # add habitat affiliation columns
  ## display message
  if (verbose) {
    cli::cli_progress_step("extracting species habitat data")
  }
  ## remove rows for taxa missing habitat information
  idx <- !is.na(spp_habitat_data$code)
  spp_habitat_data <- spp_habitat_data[idx, , drop = FALSE]
  ## remove rows for taxa not present in x
  idx <- spp_habitat_data$id_no %in% x$id_no
  spp_habitat_data <- spp_habitat_data[idx, , drop = FALSE]
  ## remove rows for habitat preferences that are not Suitable
  idx <- which(tolower(spp_habitat_data$suitability) == "suitable")
  spp_habitat_data <- spp_habitat_data[idx, , drop = FALSE]
  ## convert season descriptions to codes
  spp_habitat_data$seasonal <- convert_to_seasonal_id(
    spp_habitat_data$season
  )
  ## coerce code column to character for subsequent joins with habitat_data
  spp_habitat_data$habitat_code <- as.character(spp_habitat_data$code)
  ## remove omit habitat codes
  idx <- which(!spp_habitat_data$habitat_code %in% omit_habitat_codes)
  spp_habitat_data <- spp_habitat_data[idx, , drop = FALSE]
  ## replace rows with NA values "season" in the season column with
  ## duplicates for every possible seasonal code (i.e. [1, 2, 3, 4, 5])
  idx <- is.na(spp_habitat_data$seasonal)
  spp_habitat_data <- dplyr::bind_rows(
    spp_habitat_data[!idx, , drop = FALSE],
    purrr::map_dfr(
      which(idx),
      function(i) {
        x <- spp_habitat_data[rep(i, 5), , drop = FALSE]
        x$seasonal <- seq_len(5)
        x
      }
    )
  )

  ## combine habitat codes into a single column using "|" delimiters
  spp_habitat_data <- dplyr::group_by(
    spp_habitat_data, .data$id_no, .data$seasonal
  )
  spp_habitat_data <- dplyr::summarize(
    spp_habitat_data,
    habitat_code = list(.data$habitat_code)
  )
  spp_habitat_data <- dplyr::ungroup(spp_habitat_data)

  ## add habitat codes to x
  nms <- c("id_no", "seasonal", "habitat_code")
  x <- dplyr::left_join(
    x = x,
    y = spp_habitat_data[, nms, drop = FALSE],
    by = c("id_no", "seasonal")
  )

  ## replace NULLs with empty character vectors
  x$habitat_code <- lapply(x$habitat_code, function(x) {
    if (is.character(x)) return(x)
    character(0)
  })

  # add spatial extent columns
  ## display message
  if (verbose) {
    cli::cli_progress_step("extracting spatial extent metadata")
  }
  ## create empty version of data template
  empty_template <- terra::rast(
    xmin = terra::xmin(template_data),
    xmax = terra::xmax(template_data),
    ymin = terra::ymin(template_data),
    ymax = terra::ymax(template_data),
    resolution = terra::res(template_data),
    crs =  terra::crs(template_data),
  )
  ## add columns for xmin, xmax, ymin, and ymax
  ### note that we return NAs if the species doesn't overlap with the template
  x <- dplyr::bind_cols(
    x,
    purrr::map_dfr(seq_len(nrow(x)), function(i) {
      if (
        sf::st_intersects(
          x = sf::st_as_sfc(terra_st_bbox(empty_template)),
          y = sf::st_as_sfc(sf::st_bbox(x[i, ])),
          sparse = FALSE
        )[[1]]
      ) {
        ex <- terra::ext(
          terra::crop(
            x = empty_template, y = sf_terra_ext(x[i, ]), snap = "out")
          )
      } else {
        ex <- list(
          xmin = NA_real_, xmax = NA_real_, ymin = NA_real_, ymax = NA_real_
        )
      }
      data.frame(
        xmin = ex$xmin,
        xmax = ex$xmax,
        ymin = ex$ymin,
        ymax = ex$ymax
      )
    })
  )

  # return result
  x
}
