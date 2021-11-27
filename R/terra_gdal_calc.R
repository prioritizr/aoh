#' @include internal.R misc_terra.R
NULL

#' GDAL calculate
#'
#' This function is a wrapper for the `gdal_calc.py` script.
#'
#' @inheritParams terra_gdal_project
#'
#' @param expr `character` Value containing expression.
#'
#' @param y `terra::rast()` Optional raster for calculations.
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
                            y = NULL,
                            n_threads = 1,
                            filename = tempfile(fileext = ".tif"),
                            datatype = "FLT4S",
                            tiled = FALSE,
                            bigtiff = FALSE,
                            output_raster = TRUE,
                            verbose = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, c("character", "SpatRaster")),
    inherits(y, c("NULL", "character", "SpatRaster")),
    assertthat::is.string(expr),
    assertthat::is.string(filename),
    assertthat::noNA(filename),
    assertthat::is.flag(tiled),
    assertthat::noNA(tiled),
    assertthat::is.flag(bigtiff),
    assertthat::noNA(bigtiff),
    assertthat::is.flag(output_raster),
    assertthat::noNA(output_raster),
    assertthat::is.string(datatype),
    assertthat::noNA(datatype),
    is_gdal_available(),
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

  # compress options
  co <- paste0("NUM_THREADS=", n_threads)
  if (endsWith(filename, ".tif")) {
    co <- c(co, "COMPRESS=LZW")
    if (tiled) {
      co <- c(co, "TILED=YES")
    }
    if (isTRUE(bigtiff)) {
      co <- c(co, "BIGTIFF=YES")
    }
  }

  # save raster if needed
  if (inherits(x, "SpatRaster")) {
    x_on_disk <- terra_on_disk(x)
    x <- terra_force_disk(x, overwrite = TRUE, datatype = datatype, gdal = co)
    f1 <- terra::sources(x)$source[[1]]
  } else {
    x_on_disk <- TRUE
    f1 <- x
  }
  if (inherits(y, "SpatRaster")) {
    y_on_disk <- terra_on_disk(y)
    y <- terra_force_disk(y, overwrite = TRUE, datatype = datatype, gdal = co)
    f2 <- terra::sources(y)$source[[1]]
  } else {
    y_on_disk <- TRUE
    f2 <- y
  }

  # build cmd processing
  cmd <- "gdal_calc.py "
  if (is.null(y)) {
    cmd <- paste0(cmd, "-X \"", f1, "\" ")
  } else {
    cmd <- paste0(cmd, "-X \"", f1, "\" -Y \"", f2, "\" ")
  }
  cmd <- paste0(
    cmd,
    "--outfile=\"", filename, "\" ",
    "--calc=\"", expr, "\" ",
    "--type=\"", gdal_datatype(datatype), "\" ",
    paste(paste0("--co=\"", co, "\""), collapse = " ")
  )
  if (!verbose) {
    cmd <- paste(cmd, "--quiet")
  }
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

  # return result
  if (output_raster) {
    return(stats::setNames(terra::rast(filename), nms))
  } else {
    return(filename)
  }
}
