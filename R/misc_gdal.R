#' @include internal.R
NULL

#' Is GDAL available?
#'
#' Check if GDAL is available for processing data.
#'
#' @details
#' The function checks if GDAL is available by (1) verifying that the
#' \pkg{gdalUtils} package in installed and (2) verifying that
#' attempting to query the installed version of GDAL does not produce
#' an error (i.e. `system("gdalinfo --version")`).
#
#' @return A `logical` indicating if GDAL is available or not.
#'
#' @examples
#' # check if GDAL is available?
#' print(is_gdal_available())
#'
#' @export
is_gdal_available <- function() {
  requireNamespace("gdalUtils") &&
  !inherits(system("gdalinfo --version", intern = TRUE), "try-error")
}

#' Project a raster using GDAL
#'
#' This function is a wrapper for [gdalUtils::gdalwarp()].
#'
#' @param x [terra::rast()] Raster object with source data.
#'
#' @param y [terra::rast()] Raster object specifying spatial properties for
#'  output data.
#'
#' @param method `character` Name of interpolation method.
#'
#' @param filename `character` Filename for output raster.
#'  Defaults to `tempfile(fileext = ".tif")`.
#'
#' @param parallel_n_threads `integer` Number of computational threads to use
#'   for data processing.
#'   To reduce run time, it is strongly recommended to set this
#'   parameter based on available resources (see Examples section below).
#'   Defaults to 1.
#'
#' @param ... Arguments passed to [terra::writeRaster()].
#'
#' @return A [terra::rast()] raster object.
#'
#' @examples
#' # please ensure that the gdalUtils package is installed and
#' # GDAL system binaries are installed to run this example
#'
#' @examplesIf is_gdal_available()
#' # create raster with data
#' x <- rast(
#'   ncols = 40, nrows = 40, xmin = -110, xmax = -90, ymin = 40, ymax=60,
#'   crs = "+proj=longlat +datum=WGS84"
#' )
#' values(x) <- 1:ncell(x)
#'
#' # create raster to define spatial properties for projection
#' y <- rast(
#'   ncols = 94, nrows = 124, xmin = -944881, xmax = 935118, ymin = 4664377,
#'   ymax = 7144377,
#'   crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
#' )
#'
#' # project data
#' z <- terra_gdal_project(x, y)
#'
#' # preview result
#' print(z)
#' @export
terra_gdal_project <- function(x, y,
                            method = "bilinear",
                            parallel_n_threads = 1,
                            filename = tempfile(fileext = ".tif"),
                            verbose = TRUE,
                            ...) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    inherits(y, "SpatRaster"),
    assertthat::is.string(method),
    assertthat::is.string(filename),
    assertthat::is.count(parallel_n_threads),
    assertthat::noNA(parallel_n_threads),
    is_gdal_available()
  )
  # store options
  opts <- list(...)
  # create temporary files
  f1 <- tempfile(fileext = ".tif")
  f2 <- tempfile(fileext = ".wkt")
  f3 <- tempfile(fileext = ".wkt")
  # save raster
  terra::writeRaster(x, f1, overwrite = TRUE, ...)
  # save wkt data
  writeLines(terra::crs(x), f2)
  writeLines(terra::crs(y), f3)
  # main processing
  args <- list(
    srcfile = f1,
    dstfile = filename,
    s_srs = f2,
    t_srs = f3,
    te = c(terra::xmin(y), terra::ymin(y), terra::xmax(y), terra::ymax(y)),
    te_srs = f3,
    tr = terra::res(y),
    r = method,
    of = "GTiff",
    co = "COMPRESS=LZW",
    multi = isTRUE(parallel_n_threads >= 2),
    wo = paste0("NUM_THREADS=", parallel_n_threads),
    overwrite = TRUE,
    output_Raster = FALSE,
    verbose = isTRUE(verbose),
    q = !isTRUE(verbose)
  )
  if (!is.null(opts$NAflag)) {
    args$dstnodata <- opts$NAflag
  }
  do.call(gdalUtils::gdalwarp, args)
  # clean up
  unlink(f1, force = TRUE)
  unlink(f2, force = TRUE)
  # return result
  terra::rast(filename)
}
