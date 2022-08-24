#' @include internal.R misc_terra.R
NULL

#' GDAL calculate
#'
#' This function is a wrapper for the `gdal_calc.py` script for use with
#' \pkg{terra} objects.
#'
#' @inheritParams terra_gdal_project
#'
#' @param expr `character` Value containing expression.
#'
#' @param y `terra::rast()` Optional raster for calculations.
#'
#' @param z `terra::rast()` Optional raster for calculations.
#'
#' @param nbits `integer` Number of bits for output data.
#'  Defaults to `NULL` such that the number of bits is automatically
#'  determined.
#'
#' @param predictor `integer` Predictor for GeoTIFF compression
#'  (see [GDAL documentation](https://gdal.org/drivers/raster/gtiff.html)).
#'  Defaults to 1 such that no predictor is used for compression.
#'
#' @inherit terra_gdal_project return
#'
#' @seealso
#' See the package README for instructions to install the GDAL dependencies
#' for this function. The [is_osgeo4w_available()] and
#' [is_gdal_calc_available()] can be used to check if the installation
#' was successful.
#'
#' @examples
#' # please ensure that the Python and the GDAL system binaries are
#' # installed to run the example,
#' # see ?is_gdal_calc_available for more details
#'
#' \dontrun{
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
#' }
#' @export
terra_gdal_calc <- function(x, expr,
                            y = NULL,
                            z = NULL,
                            n_threads = 1,
                            filename = temp_raster_path(fileext = ".tif"),
                            datatype = "FLT4S",
                            tiled = FALSE,
                            bigtiff = FALSE,
                            compress = "LZW",
                            predictor = 1,
                            nbits = NULL,
                            verbose = TRUE,
                            NAflag = NULL,
                            output_raster = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, c("character", "SpatRaster")),
    inherits(y, c("NULL", "character", "SpatRaster")),
    inherits(z, c("NULL", "character", "SpatRaster")),
    assertthat::is.string(expr),
    assertthat::is.string(filename),
    assertthat::noNA(filename),
    assertthat::is.flag(tiled),
    assertthat::noNA(tiled),
    assertthat::is.flag(bigtiff),
    assertthat::noNA(bigtiff),
    assertthat::is.count(predictor),
    assertthat::noNA(predictor),
    isTRUE(predictor <= 3),
    assertthat::is.string(compress),
    assertthat::noNA(compress),
    compress %in% c("LZW", "DEFLATE"),
    assertthat::is.flag(output_raster),
    assertthat::noNA(output_raster),
    assertthat::is.string(datatype),
    assertthat::noNA(datatype),
    is_gdal_calc_available()
  )
  if (inherits(x, "SpatRaster")) {
    assertthat::assert_that(
      terra::nlyr(x) == 1
    )
  }
  if (inherits(y, "SpatRaster")) {
    assertthat::assert_that(
      terra::nlyr(y) == 1
    )
  }
  if (inherits(z, "SpatRaster")) {
    assertthat::assert_that(
      terra::nlyr(z) == 1
    )
  }

  # compress options
  co <- paste0("NUM_THREADS=", n_threads)
  if (endsWith(filename, ".tif")) {
    co <- c(co,
      paste0("COMPRESS=", compress),
      paste0("PREDICTOR=", predictor)
    )
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
  if (inherits(y, "SpatRaster")) {
    y_on_disk <- terra_on_disk(y)
    y <- terra_force_disk(y, overwrite = TRUE, datatype = datatype, gdal = co)
    f2 <- terra::sources(y)[[1]]
  } else {
    y_on_disk <- TRUE
    f2 <- y
  }
  if (inherits(z, "SpatRaster")) {
    z_on_disk <- terra_on_disk(y)
    z <- terra_force_disk(z, overwrite = TRUE, datatype = datatype, gdal = co)
    f3 <- terra::sources(z)[[1]]
  } else {
    z_on_disk <- TRUE
    f3 <- z
  }

  # build cmd processing
  cmd <- paste0("-X \"", f1, "\" ")
  if (!is.null(y)) {
    cmd <- paste0(cmd, "-Y \"", f2, "\" ")
  }
  if (!is.null(z)) {
    cmd <- paste0(cmd, "-Z \"", f3, "\" ")
  }
  cmd <- paste0(
    cmd,
    "--outfile=\"", filename, "\" ",
    "--calc=\"", expr, "\" ",
    "--type=\"", gdal_datatype(datatype), "\" ",
    paste(paste0("--co=\"", co, "\""), collapse = " ")
  )
  if (!is.null(NAflag)) {
    if (!identical(NAflag, "none")) {
      assertthat::assert_that(
        assertthat::is.number(NAflag),
        assertthat::noNA(NAflag)
      )
    }
    cmd <- paste0(cmd, " --NoDataValue=\"", NAflag, "\"")
  }
  if (!verbose) {
    cmd <- paste(cmd, "--quiet")
  }
  cmd <- gdal_calc_command(cmd)
  if (isTRUE(verbose)) {
    cli::cli_alert_info(paste("System command:", cmd))
  }
  system(cmd, intern = !verbose)

  # clean up
  nms <- names(x)
  if (!x_on_disk) {
    rm(x)
    unlink(f1, force = TRUE)
  }
  if (!y_on_disk) {
    rm(y)
    unlink(f2, force = TRUE)
  }
  if (!z_on_disk) {
    rm(z)
    unlink(f3, force = TRUE)
  }

  # return result
  if (output_raster) {
    return(stats::setNames(terra::rast(filename), nms))
  } else {
    return(filename)
  }
}

gdal_calc_command <- function(x) {
  if (is_osgeo4w_available()) {
    out <- osgeo4w_gdal_calc(x)
  } else {
    out <- python_gdal_calc(x)
  }
  out
}

# nocov start
osgeo4w_gdal_calc <- function(x) {
  # assert valid arguments
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x)
  )
  # find OSGeo4W root
  r <- Sys.getenv("OSGEO4W_ROOT") %||% "C:/OSGeo4W"
  # build bat file path
  bat <- normalizePath(file.path(r, "OSGeo4W.bat"), mustWork = FALSE)
  if (!file.exists(bat)) {
    stop(paste("OSGeo4W not available at", dirname(bat)))
  }
  # find gdal_calc.py
  p <- Sys.glob(
    normalizePath(
      paste0(r, "/apps/Pyth*/Scripts/gdal_calc.py"),
      mustWork = FALSE
    )
  )
  assertthat::assert_that(
    identical(length(p), 1L),
    msg = "could not find \"gdal_calc.py\" in OSGeo4W installation"
  )
  # build command
  paste0("\"", bat, "\" \"", normalizePath(p), "\" ", x)
}
# nocov end

python_gdal_calc <- function(x) {
  # assert valid arguments
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x)
  )
  # run command
  paste("gdal_calc.py", x)
}
