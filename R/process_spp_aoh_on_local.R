#' @include internal.R misc_terra.R misc_sf.R
NULL

#' Process species Area of Habitat data locally
#'
#' Process species Area of Habitat (AOH) data using computational
#' resources that are available locally.
#'
#' @inheritParams engine_spp_aoh_terra
#' @inheritParams engine_spp_aoh_gdal
#' @inheritParams create_spp_aoh_data
#'
#' @return Invisible `TRUE` indicating success.
#'
#' @noRd
process_spp_aoh_on_local <- function(x,
                                     habitat_data,
                                     elevation_data,
                                     habitat_values,
                                     lower_elevation,
                                     upper_elevation,
                                     extent,
                                     path,
                                     engine = "terra",
                                     n_threads = 1,
                                     cache_limit = 200,
                                     rasterize_touches = FALSE) {
  # normalize the file path
  path <- normalize_path(path, mustWork = FALSE)

  # main processing
  if (identical(engine, "terra")) {
    engine_spp_aoh_terra(
      range_data = x,
      habitat_data = habitat_data,
      elevation_data = elevation_data,
      habitat_values = habitat_values,
      lower_elevation = lower_elevation,
      upper_elevation = upper_elevation,
      extent = extent,
      rasterize_touches = rasterize_touches,
      path = path
    )
  } else if (identical(engine, "gdal")) {
    engine_spp_aoh_gdal(
      range_data = x,
      habitat_data = habitat_data,
      elevation_data = elevation_data,
      habitat_values = habitat_values,
      lower_elevation = lower_elevation,
      upper_elevation = upper_elevation,
      extent = extent,
      path = path,
      n_threads = n_threads,
      rasterize_touches = rasterize_touches,
      cache_limit = cache_limit
    )
  } else if (identical(engine, "grass")) {
    engine_spp_aoh_grass(
      range_data = x,
      habitat_data = habitat_data,
      elevation_data = elevation_data,
      habitat_values = habitat_values,
      lower_elevation = lower_elevation,
      upper_elevation = upper_elevation,
      extent = extent,
      path = path,
      memory = cache_limit,
      verbose = FALSE
    )
  }

  # verify success
  assertthat::assert_that(
    file.exists(path),
    msg = paste("failed to save AOH data:", path)
  )

  # return result
  invisible(TRUE)
}
