# alias for roxygen2:::`%||%`
`%||%` <- function(a, b) {
  if (length(a) > 0) {
    return(a)
  } else {
    return(b)
  }
}

#' Temporary raster file path
#'
#' Create a temporary raster file path.
#'
#' @param fileext `character` Defaults to ".tif".
#'
#' @details
#' This function is used to create temporary raster files that works
#' cross-platform. This is especially important because the [tempfile()]
#' function will sometimes concatenate the full path if it is too long.
#'
#' @return A `character` file path.
#'
#' @noRd
temp_raster_path <- function(fileext = ".tif") {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(fileext),
    assertthat::noNA(fileext)
  )
  # create temporary file
  f <- tempfile(fileext = fileext)
  # create a raster since terra::writeRaster auto-magically fixes
  # concatenation issues
  r <- terra::rast(
      vals = 1, nrows = 1, ncols = 1,
      xmin = 0, xmax = 1, ymin = 0, ymax = 1
  )
  # save the raster and then extract the file name
  ## N.B. this will
  f <- terra::sources(terra::writeRaster(r, f, overwrite = TRUE))
  # delete the raster to avoid polluting system
  unlink(f, force = TRUE, recursive = TRUE)
  # return the filename
  f
}
