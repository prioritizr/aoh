#' Process species Area of Habitat data using Google Earth Engine
#'
#' Process species Area of Habitat (AOH) data using Google Earth Engine
#' (via the \pkg{rgree} package).
#'
#' @inheritParams process_spp_aoh_data_on_local
#'
#' @noRd
process_spp_aoh_data_on_ee <- function(x,
                                          habitat_data,
                                          elevation_data,
                                          force = FALSE,
                                          verbose = TRUE) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, "sf"),
    assertthat::noNA(x$elevation_lower),
    assertthat::noNA(x$elevation_upper),
    inherits(habitat_data, "SpatRaster"),
    inherits(elevation_data, "SpatRaster"),
    assertthat::is.string(cache_dir),
    assertthat::noNA(cache_dir),
    assertthat::is.writeable(cache_dir),
    assertthat::is.flag(force),
    assertthat::noNA(force),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose),
    terra::compareGeom(habitat_data[[1]], elevation_data, stopOnError = FALSE)
  )
  stop("not implemented yet")
}
