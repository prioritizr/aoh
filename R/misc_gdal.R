#' @include internal.R misc_terra.R
NULL

#' GDAL datatype
#'
#' Convert a raster data type to the format used by GDAL.
#'
#' @param x `character` Value containing data type.
#'
#' @return A `character` containing the converted data type.
#'
#' @noRd
gdal_datatype <- function(x) {
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x),
    x %in% c("INT1U", "INT2S", "INT4S", "FLT8S", "INT2U", "INT4U", "FLT4S")
  )
  switch(
    x,
    "INT1U" = "Byte",
    "INT2S" = "Int16",
    "INT4S" = "Int32",
    "FLT8S" = "Float64",
    "INT2U" = "UInt16",
    "INT4U" = "UInt32",
    "FLT4S" = "Float32"
  )
}
