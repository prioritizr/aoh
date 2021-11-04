#' @include internal.R
NULL

#' World Behrmann 1 km raster
#'
#' Import a [terra::rast()] template raster dataset delineating the
#' global extent at 1 km resolution using the World Behrmann
#' coordinate reference system (ESRI:54017).
#'
#' @details
#' This raster datast is intended to be used as a template for
#' spatial data processing. As such, it does not contain any
#' meaningful data. Specifically, all grid cells contain a value of 1.
#'
#' @return A [terra::rast()] object.
#'
#' @examples
#' # import data
#' x <- get_world_behrmann_1km_rast()
#'
#' # preview data
#' print(x)
#'
#' # plot data
#' plot(x)
#' @export
get_world_behrmann_1km_rast <- function() {
  terra::rast(
    system.file("extdata", "world_behrmann_1km_rast.tif", package = "aoh")
  )
}
