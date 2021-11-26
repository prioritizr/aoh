#' @include internal.R misc_terra.R
NULL

#' Crop a raster using GDAL
#'
#' This function is a wrapper for [gdalUtils::gdal_translate()].
#'
#' @param ext [terra::ext()] Raster extent object.
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
                            bigtiff = FALSE,
                            output_raster = TRUE,
                            verbose = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, c("character", "SpatRaster")),
    inherits(ext, "SpatExtent"),
    assertthat::is.string(filename),
    assertthat::noNA(filename),
    assertthat::is.string(datatype),
    assertthat::noNA(datatype),
    assertthat::is.flag(tiled),
    assertthat::noNA(tiled),
    assertthat::is.flag(bigtiff),
    assertthat::noNA(bigtiff),
    assertthat::is.flag(output_raster),
    assertthat::noNA(output_raster),
    assertthat::is.count(n_threads),
    assertthat::noNA(n_threads),
    is_gdal_available()
  )
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
    if (!x_on_disk) {
      f1 <- tempfile(fileext = ".tif")
      terra::writeRaster(
        x, f1, overwrite = TRUE, datatype = datatype, gdal = co
      )
    } else {
      f1 <- terra::sources(x)$source[[1]]
    }
  } else {
    x_on_disk <- TRUE
    f1 <- x
  }

  # save wkt data
  f2 <- tempfile(fileext = ".wkt")
  writeLines(terra::crs(x), f2)

  # main processing
  args <- list(
    src_dataset = f1,
    dst_dataset = filename,
    tr = terra::res(x),
    projwin = c(
      terra::xmin(ext), terra::ymax(ext), terra::xmax(ext), terra::ymin(ext)
    ),
    projwin_srs = f2,
    co = co,
    ot = gdal_datatype(datatype),
    wo = paste0("NUM_THREADS=", n_threads),
    oo = paste0("NUM_THREADS=", n_threads),
    overwrite = TRUE,
    output_Raster = FALSE,
    verbose = isTRUE(verbose),
    q = !isTRUE(verbose)
  )
  do.call(gdalUtils::gdal_translate, args)

  # clean up
  if (!x_on_disk) {
    unlink(f1, force = TRUE)
  }

  # return result
  if (output_raster) {
    return(stats::setNames(terra::rast(filename), names(x)))
  } else {
    return(filename)
  }
}
