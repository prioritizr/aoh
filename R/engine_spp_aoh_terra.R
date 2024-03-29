#' @include internal.R
NULL

#' Process species' Area of Habitat data using 'terra'
#'
#' Generate Area of Habitat data for a single species' distribution using the
#' \pkg{terra} package for processing.
#'
#' @param range_data `sf::st_sf()` Object containing the species' range data.
#'
#' @param habitat_data `terra::rast()` Raster dataset with habitat data.
#'
#' @param elevation_data `sf::st_sf()` Raster dataset with elevation data.
#'
#' @param habitat_values `numeric` Vector of values in the argument
#'   to `habitat_data` with suitable habitat.
#'
#' @param lower_elevation `numeric` Value denoting lower elevational
#'   limit for the species.
#'
#' @param upper_elevation `numeric` Value denoting lower elevational
#'   limit for the species.
#'
#' @param extent `terra::ext()` Object denoting the spatial extent for
#'  data processing.
#'
#' @param touches `logical` Should raster cells that are overlap with any
#'   part of a species' range be treated as covered?
#'   Defaults to `FALSE`, such that only cells that have their centroid
#'   covered by a species' range are treated as covered.
#'
#' @param path `character` File path to save resulting Area of Habitat data.
#'
#' @return An invisible `TRUE` indicating success.
#'
#' @noRd
engine_spp_aoh_terra <- function(range_data,
                                 habitat_data,
                                 elevation_data,
                                 habitat_values,
                                 lower_elevation,
                                 upper_elevation,
                                 extent,
                                 rasterize_touches,
                                 path) {
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
    assertthat::is.flag(rasterize_touches),
    assertthat::noNA(rasterize_touches),
    assertthat::is.string(path),
    assertthat::noNA(path)
  )

  # create temporary directory for processing
  tmp_dir <- normalize_path(tempfile(), mustWork = FALSE)
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  terra::terraOptions(progress = 0, tempdir = tmp_dir)

  # reset terra options on exit
  on.exit(terra::terraOptions(progress = 3, tempdir = tempdir()))

  # combine habitat and elevation data into a single object
  raster_data <- terra::rast(
    list(
      terra::crop(
        x = habitat_data,
        y = extent,
        wopt = list(datatype = "INT2U", gdal = c("COMPRESS=LZW", "BIGTIFF=YES"))
      ),
      terra::crop(
        x = elevation_data,
        y = extent,
        wopt = list(datatype = "INT2S", gdal = c("COMPRESS=LZW", "BIGTIFF=YES"))
      )
    )
  )

  # crop data to extent and convert to presence/absence of suitable habitat
  spp_habitat_data <- terra::lapp(
    raster_data,
    function(x, y) {
      1 * (
        ((x %in% habitat_values) &
        (y >= lower_elevation) &
        (y <= upper_elevation)) &
        (!is.na(x)) &
        (!is.na(y))
      )
    },
    wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW", "BIGTIFF=YES"))
  )

  # mask data by species range
  spp_habitat_data <- terra::mask(
    x = spp_habitat_data,
    mask = terra_fasterize(
      sf = range_data,
      raster = spp_habitat_data,
      touches = rasterize_touches
    ),
    updatevalue = NA_integer_,
    filename = path,
    wopt = list(datatype = "INT1U", gdal = c("COMPRESS=DEFLATE", "BIGTIFF=YES"))
  )

  # clean up
  unlink(tmp_dir, force = TRUE, recursive = TRUE)

  # return result
  invisible(TRUE)
}
