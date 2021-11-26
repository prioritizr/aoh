#' @include internal.R
NULL

#' Process species' Area of Habitat data using GRASS
#'
#' Generate Area of Habitat data for a single species' distribution using the
#' GRASS software for processing.
#'
#' @inheritParams engine_spp_aoh_terra
#'
#' @inherit engine_spp_aoh_terra return
#'
#' @noRd
engine_spp_aoh_grass <- function(range_data,
                                 habitat_data,
                                 elevation_data,
                                 habitat_values,
                                 lower_elevation,
                                 upper_elevation,
                                 extent,
                                 path,
                                 wopt = list()) {
  # validate arguments
  assertthat::assert_that(
    inherits(range_data, "sf"),
    inherits(habitat_data, "SpatRaster"),
    inherits(elevation_data, "SpatRaster"),
    is.numeric(habitat_values),
    length(habitat_values) > 0,
    assertthat::noNA(habitat_values),
    assertthat::is.number(lower_elevation),
    assertthat::noNA(lower_elevation),
    assertthat::is.number(upper_elevation),
    assertthat::noNA(upper_elevation),
    inherits(extent, "SpatExtent"),
    assertthat::is.string(path),
    assertthat::noNA(path),
    is.list(wopt)
  )

  # create temporary directory for processing
  tmp_dir <- gsub("\\", "/", tempfile(), fixed = TRUE)
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)

  # TODO

  # return result
  invisible(TRUE)
}
