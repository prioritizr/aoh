#' @include internal.R misc_terra.R
NULL

#' Project a raster using GDAL
#'
#' This function is a wrapper for [gdalUtilities::gdalwarp()] for use with
#' \pkg{terra} objects.
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
#' @param n_threads `integer` Number of computational threads to use
#'   for data processing.
#'   To reduce run time, it is strongly recommended to set this
#'   parameter based on available resources (see Examples section below).
#'   Defaults to 1.
#'
#' @param datatype `character` Value indicating the data type for saving data.
#'  Defaults to `"FLT4S"`.
#'
#' @param cache_limit `integer` Number of MB to use for GDAL caching.
#'  Defaults to 200.
#'
#' @param tiled `logical` Value indicating if GeoTIFF files should be tiled.
#'  Defaults to `FALSE`.
#'
#' @param bigtiff `logical` Value indicating the data should be stored in
#'  BIGTIFF format.
#'  Defaults to `FALSE`.
#'
#' @param compress `character` Value indicating compression format.
#'  Available options include `"LZW"` and `"DEFLATE"`.
#'  Defaults to `"LZW"`.
#'
#' @param NAflag `numeric` Value for representing missing (`NA`) values.
#'  A `"none"` value can also be used to indicate that no flag should be set.
#'  Defaults to `NULL` such that the value is determined automatically.
#'
#' @param verbose `logical` Should information be displayed during processing?
#'  Defaults to `TRUE`.
#'
#' @param output_raster `logical` Should a raster ([terra::rast()])
#'  object be returned?
#'  If `FALSE` then the file path for the resulting file is returned.
#'  Defaults to `TRUE`.
#'
#' @return A [terra::rast()] raster object.
#'
#' @examples
#' # please ensure that the gdalUtilities package is installed
#' # to run this example
#'
#' @examplesIf requireNamespace("gdalUtilities", quietly = TRUE)
#' # create raster with data
#' x <- rast(
#'   ncols = 40, nrows = 40, xmin = -110, xmax = -90, ymin = 40, ymax=60,
#'   crs = "+proj=longlat +datum=WGS84"
#' )
#' values(x) <- seq_len(ncell(x))
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
                               n_threads = 1,
                               filename = tempfile(fileext = ".tif"),
                               datatype = "FLT4S",
                               cache_limit = 200,
                               tiled = FALSE,
                               bigtiff = FALSE,
                               compress = "LZW",
                               verbose = TRUE,
                               NAflag = NULL,
                               output_raster = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    requireNamespace("gdalUtilities", quietly = TRUE),
    msg = paste(
      "the \"gdalUtilities\" package needs to be installed, use",
      "install.packages(\"gdalUtilities\")"
    )
  )
  assertthat::assert_that(
    inherits(x, c("character", "SpatRaster")),
    inherits(y, c("character", "SpatRaster")),
    assertthat::is.string(method),
    assertthat::noNA(method),
    assertthat::is.string(filename),
    assertthat::noNA(filename),
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
    assertthat::is.count(cache_limit),
    assertthat::noNA(cache_limit),
    any(endsWith(filename, c(".tif", ".vrt")))
  )

  # create temporary files
  f2 <- tempfile(fileext = ".wkt")
  f3 <- tempfile(fileext = ".wkt")

  # compress options
  if (endsWith(filename, ".tif")) {
    co <- paste0("NUM_THREADS=", n_threads)
    co <- c(co, paste0("COMPRESS=", compress))
    if (tiled) {
      co <- c(co, "TILED=YES")
    }
    if (isTRUE(bigtiff)) {
      co <- c(co, "BIGTIFF=YES")
    }
  } else {
    co <- NULL
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
    of = ifelse(endsWith(filename, ".vrt"), "VRT", "GTiff"),
    wm = as.character(cache_limit),
    multi = isTRUE(n_threads >= 2),
    ot = gdal_datatype(datatype),
    q = !isTRUE(verbose)
  )
  if (!is.null(co)) {
    args$co <- co
  }
  if (!is.null(NAflag)) {
    if (!identical(NAflag, "none")) {
      assertthat::assert_that(
        assertthat::is.number(NAflag),
        assertthat::noNA(NAflag)
      )
    } else {
      NAflag <- "None"
    }
    args$dstnodata <- NAflag
  }
  if (endsWith(filename, ".tif")) {
    args$wo <- paste0("NUM_THREADS=", n_threads)
    args$oo <- paste0("NUM_THREADS=", n_threads)
    args$doo <- paste0("NUM_THREADS=", n_threads)
  }
  withr::with_envvar(
    c(
      "NUM_THREADS" = n_threads,
      "GDAL_CACHEMAX" = as.integer(cache_limit),
      "GDAL_DISABLE_READDIR_ON_OPEN" = "TRUE"
    ),
    do.call(gdalUtilities::gdalwarp, args)
  )
  # clean up
  nms <- names(x)
  if (!x_on_disk) {
    rm(x)
    unlink(f1, force = TRUE)
  }
  unlink(f2, force = TRUE)
  unlink(f3, force = TRUE)

  # return result
  if (output_raster) {
    return(stats::setNames(terra::rast(filename), nms))
  } else {
    return(filename)
  }
}
