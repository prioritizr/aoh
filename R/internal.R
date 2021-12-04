#' Missing data value for GeoTIFF file
#'
#' Find the missing data value for a GeoTIFF file/
#'
#' @param x `character` File path.
#'
#' @return A `numeric` value.
#'
#' @noRd
geotiff_NAflag <- function(x) {
  v <- sf::gdal_utils(x, util = "info", quiet = TRUE)
  v <- grep("NoData", strsplit(v, "\n")[[1]], value = TRUE)
  if (length(v) != 1) {
    return(NA_real_)
  }
  as.numeric(strsplit(v, "=")[[1]][[2]])
}
