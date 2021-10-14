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
#' @export
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
#' @export
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
#' @export
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

#' Fasterize
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
#' r <- get_world_berhman_1km_rast()
#'
#' # convert to raster
#' nc_rast <- terra_fasterize(nc, r)
#'
#' # plot result
#' plot(nc_rast)
#' }
#' @export
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
