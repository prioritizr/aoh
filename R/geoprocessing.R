#' @include internal.R
NULL

#' Read spatial data
#'
#' Import spatial data. If desired, only a subset of the available data
#' are imported.
#'
#' @param dsn `character` data source name.
#'
#' @param layer `character` layer name. Defaults to `NULL`.
#'
#' @param n `integer` number of records to import.
#'   Defaults to `NULL` such that all data are imported.
#'
#' @return [sf::sf()] object.
#'
#' @noRd
read_sf_n <- function(dsn, layer = NULL, n = NULL) {
  # validate arguments
  assertthat::assert_that(assertthat::is.string(dsn),
                          inherits(layer, c("character", "NULL")),
                          inherits(n, c("numeric", "NULL")))
  if (!is.null(n)) {
   assertthat::assert_that(assertthat::is.count(n),
                           assertthat::noNA(n))
  }
  if (is.null(layer)) {
    layer <- sf::st_layers(dsn)$name[[1]]
  }
  assertthat::assert_that(assertthat::is.string(layer),
                          assertthat::noNA(layer))
  # construct query
  if (!is.null(n)) {
    query <- paste0("SELECT * FROM \"", layer, "\" WHERE FID <= ", n)
  } else {
    query <- NA
  }
  # import data
  out <- sf::read_sf(dsn = dsn, query = query)
  if (!is.null(n)) {
    if (nrow(out) > n) {
      out <- out[seq_len(n), ]
    }
  }
  # force sf_geometry column to be called "geometry"
  if (!"geometry" %in% names(out)) {
    old_name <- attr(out, "sf_column")
    names(out)[names(out) == old_name] <- "geometry"
    attr(out, "sf_column") <- "geometry"
  }
  # return result
  out
}

#' Create a blank SpatRast raster
#'
#' Create an empty [terra::rast()] raster object based on specified
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
#' r <-  create_blank_rast(1000, 1000, st_crs(nc), st_bbox(nc))
#'
#' # preview raster
#' print(r)
#' @noRd
create_blank_rast <- function(xres, yres, crs, bbox) {
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
  # return raster
  terra::rast(
    xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
    nlyrs = 1,
    crs = as.character(crs)[[2]],
    resolution = c(xres, yres)
  )
}
