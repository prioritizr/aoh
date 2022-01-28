#' @include internal.R misc_terra.R
NULL

#' @include internal.R misc_terra.R
NULL

#' Convert vector data into a raster using GDAL
#'
#' This function is a wrapper for [gdalUtils::gdal_rasterize()].
#'
#' @inheritParams terra_gdal_calc
#'
#' @param sf [sf::st_sf()] Spatial object to rasterize.
#'
#' @param burn `numeric` Value for encoding the vector data.
#'   Defaults to 1.
#'
#' @param invert `logical` Should the burn process be inverted?
#'   Defaults to `FALSE`.
#'
#' @param update `logical` Should the result by producing by updating
#'   the argument to `x`?
#'   If `FALSE` then the argument to `x` is only used to specify the
#'   spatial properties of the resulting raster
#'   (i.e. values have on the result),
#'   Defaults to `FALSE`.
#'
#' @param sf_filename `character` File name to temporarily save argument
#'   to `sf`.
#'   Defaults to a temporary (geopackage) file.
#'
#' @inherit terra_gdal_project return
#'
#' @examples
#' # please ensure that the gdalUtils package is installed and
#' # GDAL system binaries are installed to run this example
#'
#' @examplesIf is_gdal_available()
#' # import vector data
#' f <- system.file("ex/lux.shp", package = "terra")
#' sf <- read_sf(f)
#'
#' # create template raster
#' x <- rast(vect(sf), ncols = 75, nrows = 100)
#' x <- terra::setValues(x, runif(terra::ncell(x)))
#'
#' # rasterize vector data
#' z <- terra_gdal_rasterize(x, sf, burn = 5)
#'
#' # plot result
#' plot(z)
#' @noRd
terra_gdal_rasterize <- function(x, sf, burn = 1,
                                 invert = FALSE,
                                 update = FALSE,
                                 n_threads = 1,
                                 filename = tempfile(fileext = ".tif"),
                                 sf_filename = tempfile(fileext = ".gpkg"),
                                 datatype = "FLT4S",
                                 tiled = FALSE,
                                 bigtiff = FALSE,
                                 nbits = NULL,
                                 compress = "LZW",
                                 verbose = TRUE,
                                 output_raster = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, c("character", "SpatRaster")),
    inherits(sf, "sf"),
    assertthat::is.number(burn),
    assertthat::noNA(burn),
    assertthat::is.flag(invert),
    assertthat::noNA(invert),
    assertthat::is.string(filename),
    assertthat::noNA(filename),
    assertthat::is.string(sf_filename),
    assertthat::noNA(sf_filename),
    assertthat::is.string(compress),
    assertthat::noNA(compress),
    compress %in% c("LZW", "DEFLATE"),
    assertthat::is.string(datatype),
    assertthat::noNA(datatype),
    assertthat::is.flag(tiled),
    assertthat::noNA(tiled),
    assertthat::is.flag(bigtiff),
    assertthat::noNA(bigtiff),
    assertthat::is.count(n_threads),
    assertthat::noNA(n_threads),
    assertthat::is.flag(output_raster),
    assertthat::noNA(output_raster),
    is_gdal_available()
  )

  # compress options
  co <- paste0("NUM_THREADS=", n_threads)
  if (endsWith(filename, ".tif")) {
    co <- c(co, paste0("COMPRESS=", compress))
    if (tiled) {
      co <- c(co, "TILED=YES")
    }
    if (isTRUE(bigtiff)) {
      co <- c(co, "BIGTIFF=YES")
    }
    if (!is.null(nbits)) {
      co <- c(co, paste0("NBITS=", nbits))
    }
  }

  # save raster if needed
  if (inherits(x, "SpatRaster")) {
    x_on_disk <- terra_on_disk(x)
    x <- terra_force_disk(x, overwrite = TRUE, datatype = datatype, gdal = co)
    f1 <- terra::sources(x)[[1]]
  } else {
    x_on_disk <- TRUE
    f1 <- x
  }

  # copy raster if needed
  nms <- names(x)
  if (isTRUE(update)) {
    filename <- f1
    rm(x)
  }

  # save sf data
  sf::write_sf(sf, sf_filename, overwrite = TRUE)

  # main processing
  args <- list(
    src_datasource = sf_filename,
    dst_filename = filename,
    i = isTRUE(invert),
    burn = burn,
    output_Raster = FALSE,
    verbose = isTRUE(verbose),
    q = !isTRUE(verbose)
  )
  if (!isTRUE(update)) {
    f3 <- tempfile(fileext = ".wkt")
    writeLines(terra::crs(x), f3)
    args <- append(
      args,
      list(
        of = ifelse(endsWith(filename, ".vrt"), "VRT", "GTiff"),
        ot = gdal_datatype(datatype),
        co = co,
        tr = c(terra::xres(x), terra::yres(x)),
        a_srs = f3,
        te = c(terra::xmin(x), terra::ymin(x), terra::xmax(x), terra::ymax(x))
      )
    )
  }
  do.call(gdalUtils::gdal_rasterize, args)

  # clean up
  if ((!x_on_disk) && (!update)) {
    rm(x)
    unlink(f1, force = TRUE)
  }
  unlink(sf_filename, force = TRUE)
  if (!isTRUE(update)) {
    unlink(f3, force = TRUE)
  }

  # return result
  if (output_raster) {
    return(stats::setNames(terra::rast(filename), nms))
  } else {
    return(filename)
  }
}
