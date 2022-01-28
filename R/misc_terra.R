#' @include internal.R
NULL

#' Coordinate reference system
#'
#' Create a [sf::st_crs()] object to describe the spatial coordinate
#' reference system of a [terra::rast()] object following the \pkg{sf} package
#' conventions.
#'
#' @param x [terra::rast()] Raster object.
#'
#' @return A [sf::st_crs()] object.
#'
#' @examples
#' # create raster object
#' x <- rast(
#'   ncols = 40, nrows = 40,
#'   xmin = -110, xmax = -90, ymin = 40, ymax = 60,
#'   crs = "+proj=longlat +datum=WGS84"
#' )
#'
#' # extract coordinate reference system
#' terra_st_crs(x)
#'
#' @family geoprocessing
#'
#' @noRd
terra_st_crs <- function(x) {
  assertthat::assert_that(inherits(x, "SpatRaster"))
  sf::st_crs(terra::crs(x))
}

#' Bounding box
#'
#' Create a [sf::st_bbox()] object to describe the spatial extent
#' of a [terra::rast()] object following the \pkg{sf} package
#' conventions.
#'
#' @param x [terra::rast()] Raster object.
#'
#' @return A sf::st_crs()] object.
#'
#' @examples
#' # create raster object
#' x <- rast(
#'   ncols = 40, nrows = 40,
#'   xmin = -110, xmax = -90, ymin = 40, ymax = 60,
#'   crs = "+proj=longlat +datum=WGS84"
#' )
#'
#' # extract bounding box
#' terra_st_bbox(x)
#'
#' @family geoprocessing
#'
#' @noRd
terra_st_bbox <- function(x) {
  assertthat::assert_that(inherits(x, "SpatRaster"))
  sf::st_bbox(
    c(xmin = terra::xmin(x),
      ymin = terra::ymin(x),
      xmax = terra::xmax(x),
      ymax = terra::ymax(x)
    ),
    crs = sf::st_crs(terra::crs(x))
  )
}

#' Quickly rasterize vector data
#'
#' This function converts a [sf::st_as_sf()] object to a [terra::rast()]
#' object using [fasterize::fasterize()]. It is similar to
#' [terra::rasterize()], except that it has greater performance.
#'
#' @param sf [sf::st_sf()] Object.
#'
#' @param raster [terra::rast()] Object.
#'
#' @param ... Additional arguments passed to [fasterize::fasterize()].
#'
#' @return A [terra::rast()] object.
#'
#' @examples
#' \dontrun{
#' # load data
#' nc <- read_sf(system.file("shape/nc.shp", package = "sf"))
#' nc <- st_transform(nc, st_crs("ESRI:54017"))
#' r <- get_world_behrmann_1km_rast()
#'
#' # convert to raster
#' nc_rast <- terra_fasterize(nc, r)
#'
#' # plot result
#' plot(nc_rast)
#' }
#' @noRd
terra_fasterize <- function(sf, raster, ...) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(sf, "sf"),
    inherits(raster, "SpatRaster")
  )

  # convert raster to RasterLayer
  raster <- methods::as(raster[[1]], "Raster")

  # store raster filename
  raster_filename <- raster::filename(raster)

  # create result
  out <- fasterize::fasterize(
    sf = sf,
    raster = raster,
    ...
  )

  # ensure that result isn't affected by
  # https://github.com/ecohealthalliance/fasterize/issues/41
  if (identical(raster::filename(out), raster_filename)) {
    out <- raster::brick(out)
  }

  # return result as terra object
  methods::as(out, "SpatRaster")
}

#' On disk?
#'
#' Check if a raster ([terra::rast()]) object is stored on disk?
#'
#' @param x [terra::rast()] Raster object.
#'
#' @details
#' The data is only considered available on disk if the data
#' is not stored in memory and is only obtained from a single file source.
#'
#' @return A `logical` indicating if the data is stored on disk.
#'
#' @noRd
terra_on_disk <- function(x) {
  assertthat::assert_that(inherits(x, "SpatRaster"))
  s <- terra::sources(x, nlyr = TRUE)
  out <-
    (nrow(s) == 1) &&
    all(nchar(s$source) > 0) &&
    all(file.exists(s$source))
  if (!out) return(out)
  out && (terra::nlyr(x) == terra::nlyr(terra::rast(s$source[[1]])))
}

#' Force file-backed raster
#'
#' Force a raster ([terra::rast()]) object to have data stored on disk.
#'
#' @param x [terra::rast()] Raster object.
#'
#' @param filename `character` File path to store data.
#'  Defaults to a temporarily (GeoTIFF) file.
#'
#' @param ... Arguments passed to [terra::writeRaster()] for saving data.
#'
#' @return A `[terra::rast()] raster object.
#'
#' @noRd
terra_force_disk <- function(x, filename = tempfile(fileext = ".tif"), ...) {
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    assertthat::is.string(filename),
    assertthat::noNA(filename)
  )
  if (!terra_on_disk(x)) {
    terra::writeRaster(x = x, filename = filename, ...)
    x <- terra::rast(filename)
  }
  x
}
