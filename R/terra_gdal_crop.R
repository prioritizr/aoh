#' @include internal.R misc_terra.R
NULL

#' Crop a raster using GDAL
#'
#' This function is a wrapper for [gdalUtilities::gdal_translate()].
#'
#' @param ext [terra::ext()] Raster extent object.
#'
#' @inheritParams terra_gdal_project
#'
#' @inherit terra_gdal_project return
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
#' # create extent for cropping
#' y <- ext(x) - c(5, 2.5, 1, 1.5)
#'
#' # crop data
#' z <- terra_gdal_crop(x, y)
#'
#' # preview result
#' print(z)
#' @noRd
terra_gdal_crop <- function(x, ext,
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
    inherits(ext, "SpatExtent"),
    assertthat::is.string(filename),
    assertthat::noNA(filename),
    assertthat::is.string(datatype),
    assertthat::noNA(datatype),
    assertthat::is.flag(tiled),
    assertthat::noNA(tiled),
    assertthat::is.flag(bigtiff),
    assertthat::noNA(bigtiff),
    assertthat::is.string(compress),
    assertthat::noNA(compress),
    compress %in% c("LZW", "DEFLATE"),
    assertthat::is.flag(output_raster),
    assertthat::noNA(output_raster),
    assertthat::is.count(n_threads),
    assertthat::noNA(n_threads),
    any(endsWith(filename, c(".tif", ".vrt")))
  )
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
    of = ifelse(endsWith(filename, ".vrt"), "VRT", "GTiff"),
    ot = gdal_datatype(datatype),
    oo = paste0("NUM_THREADS=", n_threads),
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
    }
    args$a_nodata <- NAflag
  }
  withr::with_envvar(
    c(
      "NUM_THREADS" = n_threads,
      "GDAL_CACHEMAX" = as.integer(cache_limit),
      "GDAL_DISABLE_READDIR_ON_OPEN" = "TRUE"
    ),
    do.call(gdalUtilities::gdal_translate, args)
  )

  # clean up
  nms <- names(x)
  if (!x_on_disk) {
    rm(x)
    unlink(f1, force = TRUE)
  }
  unlink(f2, force = TRUE)

  # return result
  if (output_raster) {
    return(stats::setNames(terra::rast(filename), nms))
  } else {
    return(filename)
  }
}
