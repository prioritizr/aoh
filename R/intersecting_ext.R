#' @include internal.R
NULL

#' Intersecting extent
#'
#' Generate a [terra::ext()] object containing the spatial extent of one
#' [terra::rast()] raster object inside another [terra::rast()] object.
#'
#' @param x [terra::rast()] Raster object. This object specifies the
#'   [terra::crs()] coordinate reference system used for the output object.
#'
#' @param y [terra::rast()] Raster object. This object specifies the
#'   spatial extent for the output object.
#'
#' @param buffer `numeric` buffer applied to the spatial extent of `y` before
#'   reprojecting to the coordinate reference system of `x`. Defaults to 0.
#'
#' @return A [terra::ext()] object.
#'
#' @examples
#' # define raster object
#' x <- rast(
#'   ncols = 40, nrows = 40,
#'   xmin = -110, xmax = -90, ymin = 40, ymax = 60,
#'   crs = "+proj=longlat +datum=WGS84"
#' )
#' print(x)
#'
#' # define raster object with different coordinate reference system
#' y <- rast(
#'   ncols = 94, nrows = 124,
#'   xmin = -944881, xmax = 935118, ymin = 4664377, ymax = 7144377,
#'   crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
#' )
#' print(y)
#'
#' # find spatial extent of x that intersects with the spatial extent of y
#' print(intersecting_ext(x, y))
#'
#' @family geoprocessing
#'
#' @export
intersecting_ext <- function(x, y, buffer = 0) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    inherits(y, "SpatRaster"),
    assertthat::is.number(buffer)
  )

  # processing
  ## extract extent for y
  y_ext <- sf::st_as_sfc(terra_st_bbox(y))
  ## reproject to x coordinate system without buffer
  y_ext_no_buffer <- sf::st_bbox(
    sf::st_transform(y_ext, terra_st_crs(x))
  )
  ## reproject to x coordinate system with buffer
  y_ext_buffer <- sf::st_bbox(
    sf::st_transform(sf::st_buffer(y_ext, buffer), terra_st_crs(x))
  )
  ## handle NAs when reprojecting y to x
  if (any(is.na(c(y_ext_buffer)))) {
    ## create new extent
    y_ext_buffer2 <- c(xmin = 0, xmax = 0, ymin = 0, ymax = 0)
    y_ext_buffer2[["xmin"]] <- ifelse(
      is.na(y_ext_buffer$xmin), y_ext_no_buffer$xmin, y_ext_buffer$xmin
    )
    y_ext_buffer2[["xmax"]] <- ifelse(
      is.na(y_ext_buffer$xmax), y_ext_no_buffer$xmax, y_ext_buffer$xmax
    )
    y_ext_buffer2[["ymin"]] <- ifelse(
      is.na(y_ext_buffer$ymin), y_ext_no_buffer$ymin, y_ext_buffer$ymin
    )
    y_ext_buffer2[["ymax"]] <- ifelse(
      is.na(y_ext_buffer$ymax), y_ext_no_buffer$ymax, y_ext_buffer$ymax
    )
    ## handle if any remaining NAs
    if (any(is.na(y_ext_buffer2))) {
      ### if x is in lon/lat, and we still get NAs for reprojecting extent
      ## of y into x, then this is due to precision issues with
      ## data in x occurring at the global scale, so we can manually
      ## assume a global extent for x
      if (terra::is.lonlat(x)) {
        y_ext_buffer2 <-
          sf::st_bbox(
            c(xmin = -180, xmax = 180, ymin = -90, ymax = 90),
            crs = terra_st_crs(x)
          )
      } else {
        stop("failed to find intersecting extents")
      }
    }
  } else {
    y_ext_buffer2 <- y_ext_buffer
  }
  ## handle case where x is lon/lat and has a spatial extent that is
  ## larger than the world
  if (suppressWarnings(sf::st_is_longlat(x))) {
    x_ext <- as.list(terra_st_bbox(x))
    x_ext$xmin <- max(x_ext$xmin, -180)
    x_ext$xmax <- min(x_ext$xmax, 180)
    x_ext$ymin <- max(x_ext$ymin, -90)
    x_ext$ymax <- min(x_ext$ymax, 90)
    x_ext <- sf::st_bbox(unlist(x_ext), crs = sf::st_crs(x))
  } else {
    x_ext <- terra_st_bbox(x)
  }
  ## clip to x coordinate system
  if (
    isTRUE(y_ext_buffer2$xmin >= x_ext$xmin) &&
    isTRUE(y_ext_buffer2$ymin >= x_ext$ymin) &&
    isTRUE(y_ext_buffer2$xmax <= x_ext$xmax) &&
    isTRUE(y_ext_buffer2$ymax <= x_ext$ymax)
  ) {
    ### manually calculate intersecting extent
    z_ext <- y_ext_buffer2
  } else {
    ### use geoprocessing to calculate intersecting extent
    z_ext <- sf::st_bbox(
      sf::st_intersection(
        sf::st_as_sfc(
          sf::st_bbox(
            y_ext_buffer2,
            crs = terra_st_crs(x)
          )
        ),
        sf::st_as_sfc(x_ext)
      )
    )
  }

  # check that all coordinates are valid
  assertthat::assert_that(
    all(is.finite(c(z_ext$xmin, z_ext$xmax, z_ext$ymin, z_ext$ymax))),
    msg = "failed to find intersecting extents"
  )

  # return extent of intersecting areas
  terra::ext(c(z_ext$xmin, z_ext$xmax, z_ext$ymin, z_ext$ymax))
}
