#' @include internal.R misc_terra.R
NULL

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
#' @param ... Arguments passed to [terra::writeRaster()].
#'
#' @param verbose `logical` should information be displayed during processing?
#'  Defaults to `TRUE`.
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
                               verbose = TRUE,
                               ...) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    inherits(y, "SpatRaster"),
    assertthat::is.string(method),
    assertthat::is.string(filename),
    assertthat::noNA(filename),
    assertthat::is.string(datatype),
    assertthat::noNA(datatype),
    assertthat::is.flag(tiled),
    assertthat::noNA(tiled),
    assertthat::is.count(n_threads),
    assertthat::noNA(n_threads),
    assertthat::is.count(cache_limit),
    assertthat::noNA(cache_limit),
    is_gdal_available()
  )
  # store options
  opts <- list(...)
  # create temporary files
  f2 <- tempfile(fileext = ".wkt")
  f3 <- tempfile(fileext = ".wkt")
  # save raster if needed
  x_on_disk <- terra_on_disk(x)
  if (!x_on_disk) {
    f1 <- tempfile(fileext = ".tif")
    terra::writeRaster(x, f1, overwrite = TRUE, datatype = datatype, ...)
  } else {
    f1 <- terra::sources(x)$source[[1]]
  }
  # save wkt data
  writeLines(terra::crs(x), f2)
  writeLines(terra::crs(y), f3)
  # compile co
  co <- c(
    "COMPRESS=LZW",
    paste0("NUM_THREADS=", n_threads)
  )
  if (tiled) {
    co <- c(co, "TILED=YES")
  }
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
    co = co,
    wm = as.character(cache_limit),
    multi = isTRUE(n_threads >= 2),
    wo = paste0("NUM_THREADS=", n_threads),
    oo = paste0("NUM_THREADS=", n_threads),
    doo = paste0("NUM_THREADS=", n_threads),
    ot = gdal_datatype(datatype),
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
  if (!x_on_disk) {
    unlink(f1, force = TRUE)
  }
  unlink(f2, force = TRUE)
  # return result
  stats::setNames(terra::rast(filename), names(x))
}

#' Disaggregate a raster using GDAL
#'
#' This function is a wrapper for [gdalUtils::gdal_translate()].
#'
#' @param fact `integer` factor to disaggregate data.
#'
#' @param ... Arguments passed to [terra::writeRaster()].
#'
#' @inheritParams terra_gdal_project
#'
#' @inherit terra_gdal_project return
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
#' values(x) <- seq_len(ncell(x))
#'
#' # disaggregate data
#' z <- terra_gdal_disaggregate(x, fact = 10)
#'
#' # preview result
#' print(z)
#' @export
terra_gdal_disaggregate <- function(x, fact,
                                   n_threads = 1,
                                   filename = tempfile(fileext = ".tif"),
                                   datatype = "FLT4S",
                                   tiled = FALSE,
                                   verbose = TRUE,
                                   ...) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    assertthat::is.count(fact),
    assertthat::noNA(fact),
    assertthat::is.string(filename),
    assertthat::noNA(filename),
    assertthat::is.string(datatype),
    assertthat::noNA(datatype),
    assertthat::is.flag(tiled),
    assertthat::noNA(tiled),
    assertthat::is.count(n_threads),
    assertthat::noNA(n_threads),
    terra::nlyr(x) == 1,
    is_gdal_available()
  )
  # store options
  opts <- list(...)
  # save raster if needed
  x_on_disk <- terra_on_disk(x)
  if (!x_on_disk) {
    f1 <- tempfile(fileext = ".tif")
    terra::writeRaster(x, f1, overwrite = TRUE, datatype = datatype, ...)
  } else {
    f1 <- terra::sources(x)$source[[1]]
  }
  # compile co
  co <- c(
    "COMPRESS=LZW",
    paste0("NUM_THREADS=", n_threads)
  )
  if (tiled) {
    co <- c(co, "TILED=YES")
  }
  # main processing
  args <- list(
    src_dataset = f1,
    dst_dataset = filename,
    tr = terra::res(x) / fact,
    of = "GTiff",
    co = co,
    ot = gdal_datatype(datatype),
    wo = paste0("NUM_THREADS=", n_threads),
    oo = paste0("NUM_THREADS=", n_threads),
    overwrite = TRUE,
    output_Raster = FALSE,
    verbose = isTRUE(verbose),
    q = !isTRUE(verbose)
  )
  if (!is.null(opts$NAflag)) {
    args$dstnodata <- opts$NAflag
  }
  do.call(gdalUtils::gdal_translate, args)
  # clean up
  if (!x_on_disk) {
    unlink(f1, force = TRUE)
  }
  # return result
  stats::setNames(terra::rast(filename), names(x))
}

#' Crop a raster using GDAL
#'
#' This function is a wrapper for [gdalUtils::gdal_translate()].
#'
#' @param ext [terra::ext()] Raster extent object.
#'
#' @param ... Arguments passed to [terra::writeRaster()].
#'
#' @inheritParams terra_gdal_project
#'
#' @inherit terra_gdal_project return
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
#' values(x) <- seq_len(ncell(x))
#'
#' # create extent for cropping
#' y <- ext(x) - c(5, 2.5, 1, 1.5)
#'
#' # crop data
#' z <- terra_gdal_crop(x, y)
#'
#' # preview result
#' print(z)
#' @export
terra_gdal_crop <- function(x, ext,
                            n_threads = 1,
                            filename = tempfile(fileext = ".tif"),
                            datatype = "FLT4S",
                            tiled = FALSE,
                            verbose = TRUE,
                            ...) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    inherits(ext, "SpatExtent"),
    assertthat::is.string(filename),
    assertthat::noNA(filename),
    assertthat::is.string(datatype),
    assertthat::noNA(datatype),
    assertthat::is.flag(tiled),
    assertthat::noNA(tiled),
    assertthat::is.count(n_threads),
    assertthat::noNA(n_threads),
    is_gdal_available()
  )
  # store options
  opts <- list(...)
  # save raster if needed
  x_on_disk <- terra_on_disk(x)
  if (!x_on_disk) {
    f1 <- tempfile(fileext = ".tif")
    terra::writeRaster(x, f1, overwrite = TRUE, datatype = datatype, ...)
  } else {
    f1 <- terra::sources(x)$source[[1]]
  }
  # save wkt data
  f2 <- tempfile(fileext = ".wkt")
  writeLines(terra::crs(x), f2)
  # compile co
  co <- c(
    "COMPRESS=LZW",
    paste0("NUM_THREADS=", n_threads)
  )
  if (tiled) {
    co <- c(co, "TILED=YES")
  }
  # main processing
  args <- list(
    src_dataset = f1,
    dst_dataset = filename,
    tr = terra::res(x),
    projwin = c(
      terra::xmin(ext), terra::ymax(ext), terra::xmax(ext), terra::ymin(ext)
    ),
    projwin_srs = f2,
    of = "GTiff",
    co = co,
    ot = gdal_datatype(datatype),
    wo = paste0("NUM_THREADS=", n_threads),
    oo = paste0("NUM_THREADS=", n_threads),
    overwrite = TRUE,
    output_Raster = FALSE,
    verbose = isTRUE(verbose),
    q = !isTRUE(verbose)
  )
  if (!is.null(opts$NAflag)) {
    args$dstnodata <- opts$NAflag
  }
  do.call(gdalUtils::gdal_translate, args)
  # clean up
  if (!x_on_disk) {
    unlink(f1, force = TRUE)
  }
  # import result
  out <- stats::setNames(terra::rast(filename), names(x))
  # verify cropped succesfull
  assertthat::assert_that(
    terra::ext(out) == ext,
    msg = "failed to crop data"
  )
  # return result
  out
}

#' GDAL calculate
#'
#' This function is a wrapper for the `gdal_calc.py` script.
#'
#' @inheritParams terra_gdal_project
#'
#' @param expr `character` Value containing expression.
#'
#' @param datatype `character` Data type for output raster.
#'
#' @inherit terra_gdal_project return
#'
#' @examples
#' # please ensure that the gdalUtils package is installed and
#' # GDAL system binaries are installed to run this example
#'
#' @examplesIf is_gdal_available() && is_gdal_calc_available()
#' # create raster with data
#' x <- rast(
#'   ncols = 40, nrows = 40, xmin = -110, xmax = -90, ymin = 40, ymax=60,
#'   crs = "+proj=longlat +datum=WGS84"
#' )
#' values(x) <- seq_len(ncell(x))
#'
#' # run calculation
#' y <- terra_gdal_calc(x, "(X < 20) * 1")
#'
#' # preview result
#' print(y)
#' @export
terra_gdal_calc <- function(x, expr,
                            filename = tempfile(fileext = ".tif"),
                            datatype = "FLT4S",
                            n_threads = 1,
                            tiled = FALSE,
                            verbose = TRUE,
                            ...) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    assertthat::is.string(expr),
    assertthat::is.string(filename),
    assertthat::noNA(filename),
    assertthat::is.flag(tiled),
    assertthat::noNA(tiled),
    assertthat::is.string(datatype),
    assertthat::noNA(datatype),
    terra::nlyr(x) == 1,
    is_gdal_available(),
    is_gdal_calc_available()
  )
  # save raster if needed
  x_on_disk <- terra_on_disk(x)
  if (!x_on_disk) {
    f1 <- tempfile(fileext = ".tif")
    terra::writeRaster(x, f1, overwrite = TRUE, datatype = datatype, ...)
  } else {
    f1 <- terra::sources(x)$source[[1]]
  }
  # build cmd processing
  cmd <- paste0(
    "gdal_calc.py ",
    "-X \"", f1, "\" ",
    "--outfile=\"", filename, "\" ",
    "--calc=\"", expr, "\" ",
    "--type=\"", gdal_datatype(datatype), "\" ",
    "--format=\"GTiff\" ",
    "--co=\"COMPRESS=LZW\" ",
    "--co=\"NUM_THREADS=", n_threads, "\""
  )
  # compile co
  if (tiled) {
    cmd <- paste0(cmd, " --co=\"TILED=YES\"")
  }
  if (!verbose) {
    cli::cli_alert_info(paste("System command:", cmd))
    cmd <- paste(cmd, "--quiet")
  }
  system(cmd, intern = !verbose)
  # clean up
  if (!x_on_disk) {
    unlink(f1, force = TRUE)
  }
  # return result
  stats::setNames(terra::rast(filename), names(x))
}

#' GDAL datatype
#'
#' Convert a raster data type to the format used by GDAL.
#'
#' @param x `character` Value containing data type.
#'
#' @return A `character` containing the converted data type.
#'
#' @export
gdal_datatype <- function(x) {
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x),
    x %in% c("INT1U", "INT2S", "INT4S", "FLT8S", "INT2U", "INT4U", "FLT4S")
  )
  switch(
    x,
    "INT1U" = "Byte",
    "INT2S" = "Int32",
    "INT4S" = "Int16",
    "FLT8S" = "Float64",
    "INT2U" = "UInt16",
    "INT4U" = "UInt32",
    "FLT4S" = "Float32"
  )
}
