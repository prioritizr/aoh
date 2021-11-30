#' @include internal.R
NULL

#' Create a template raster
#'
#' Create a template [terra::rast()] raster object based on specified
#' spatial properties.
#'
#' @param xres `numeric` Resolution for the x-axis.
#'
#' @param yres `numeric` Resolution for the y-axis.
#'
#' @param crs [sf::st_crs()] Spatial reference coordinate system object.
#'
#' @param bbox [sf::st_bbox()] Spatial bounding box object.
#'
#' @details
#' Please not that all grid cells contain missing (`NA`) values.
#'
#' @return A [terra::rast()] object with desired spatial properties.
#'
#' @examples
#' # load data
#' nc <- read_sf(system.file("shape/nc.shp", package = "sf"))
#'
#' # re-project to projected coordinate system
#' nc <- st_transform(nc, st_crs("ESRI:54017"))
#'
#' # create blank raster from nc
#' r <-  create_template_rast(1000, 1000, st_crs(nc), st_bbox(nc))
#'
#' # preview raster
#' print(r)
#'
#' @family geoprocessing
#'
#' @noRd
create_template_rast <- function(xres, yres, crs, bbox) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.count(xres),
    assertthat::noNA(xres),
    assertthat::is.count(yres),
    assertthat::noNA(yres),
    inherits(crs, "crs"),
    inherits(bbox, "bbox")
  )
  # preliminary processing
  xmin <- floor(bbox$xmin[[1]])
  xmax <- xmin + (xres * ceiling((ceiling(bbox$xmax[[1]]) - xmin) / xres))
  ymin <- floor(bbox$ymin[[1]])
  ymax <- ymin + (yres * ceiling((ceiling(bbox$ymax[[1]]) - ymin) / yres))
  # create raster
  r <- terra::rast(
    xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
    nlyrs = 1,
    crs = as.character(crs)[[2]],
    resolution = c(xres, yres)
  )
  # return raster
  r
}
