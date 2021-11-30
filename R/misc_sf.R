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
#' @examples
#' # set data path
#' path <- system.file("shape/nc.shp", package = "sf")
#'
#' # read all data
#' nc <- read_sf(path)
#' print(nc)
#'
#' # read only first five geometries
#' nc2 <- read_sf_n(path, n = 5)
#' print(nc2)
#'
#' @family geoprocessing
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
    query <- paste0("SELECT * FROM \"", layer, "\" LIMIT ", n)
  } else {
    query <- paste0("SELECT * FROM \"", layer, "\"")
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

#' Spatial extent
#'
#' Create a [terra::ext()] object to describe the spatial extent of
#' a [sf::st_sf()] object following the \pkg{terra} package conventions.
#'
#' @param x [sf::st_sf()] Spatial object.
#'
#' @return A [terra::ext()] extent object.
#'
#' @examples
#' # import sf object
#' nc <- read_sf(system.file("shape/nc.shp", package = "sf"))
#'
#' # create terra extent object (i.e. SpatExtent)
#' sf_terra_ext(nc)
#'
#' @family geoprocessing
#'
#' @noRd
sf_terra_ext <- function(x) {
  assertthat::assert_that(inherits(x, c("sf", "sfc", "bbox")))
  if (inherits(x, c("sfc", "sf"))) {
    x <- sf::st_bbox(x)
  }
  terra::ext(c(x$xmin, x$xmax, x$ymin, x$ymax))
}

#' Coordinate reference system
#'
#' Create a `character` object to describe the coordinate reference system
#' of a [sf::st_sf()] object following the \pkg{terra} package conventions.
#'
#' @param x [sf::st_sf()] Object.
#'
#' @return A `character` value.
#'
#' @examples
#' # import sf object
#' nc <- read_sf(system.file("shape/nc.shp", package = "sf"))
#'
#' # create terra CRS value
#' sf_terra_crs(nc)
#'
#' @family geoprocessing
#'
#' @noRd
sf_terra_crs <- function(x) {
  assertthat::assert_that(inherits(x, c("sf", "crs")))
  if (!inherits(x, "crs")) {
    x <- sf::st_crs(x)
  }
  x$wkt
}
