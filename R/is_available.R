#' @include internal.R
NULL

#' Is IUCN Red List API available?
#'
#' The International Union for Conservation of Nature (IUCN)
#' provides an [API to access data from Red List of Threatened
#' Species](https://apiv3.iucnredlist.org/).
#' This function checks whether data can be accessed from the API.
#' Please note that a token is required to access the API
#' (see below for instructions to obtain a token).
#'
#' @inheritParams get_spp_habitat_data
#'
#' @inheritSection aoh Accessing the IUCN Red List API
#'
#' @return A `logical` indicating if the IUCN Red List API can be accessed.
#'
#' @examples
#' # check if IUCN Red List API is available?
#' is_iucn_rl_api_available()
#'
#' @export
is_iucn_rl_api_available <- function(key = NULL) {
  x <- try(rredlist::rl_regions(key = key), silent = TRUE)
  !inherits(x, "try-error")
}

#' Is GDAL available?
#'
#' Check if the Geospatial Data Abstraction Library (GDAL) is available for
#' processing data.
#'
#' @details
#' The function verifies that (1) the \pkg{gdalUtils} package in installed,
#' (2) GDAL is installed (i.e. via `system("gdalinfo --version")`), and
#' (3) the version of GDAL installed is at least 3.0.2.
#' If any of these checks fail, then GDAL is not considered available.
#
#' @return A `logical` indicating if GDAL is available or not.
#'
#' @examples
#' # check if GDAL is available?
#' print(is_gdal_available())
#'
#' @export
is_gdal_available <- function() {
  if (!requireNamespace("gdalUtils", quietly = TRUE)) return(FALSE)
  v <- gdal_version()
  if (is.na(v)) return(FALSE)
  isTRUE(as.package_version(v) >= as.package_version("3.0.2"))
}

#' GDAL version
#'
#' Find the version of the Geospatial Data Abstraction Library (GDAL)
#' that is currently installed.
#'
#' @return A `character` value describing the version of GDAL installed.
#' If GDAL is not installed, then a missing (`NA`) value is returned.
#'
#' @examples
#' # show version of GDAL installed
#' print(gdal_version())
#'
#' @export
gdal_version <- function() {
  v <- try(
    silent = TRUE, {
      x <- system("gdalinfo --version", intern = TRUE)
      x <- strsplit(x, " ")[[1]][[2]]
      x <- gsub(",", "", x, fixed = TRUE)
      x
    }
  )
  if (inherits(v, "try-error")) return(NA_character_)
  v
}

#' Is gdal_calc available?
#'
#' Check if `gdal_calc.py` is available.
#'
#' @return A `logical` value indicating if it is available.
#'
#' @examples
#' # see if gdal_calc is available
#' print(is_gdal_calc_available())
#'
#' @export
is_gdal_calc_available <- function() {
  v <- try(
    system("gdal_calc.py --help", intern = TRUE),
    silent = TRUE
  )
  if (inherits(v, "try-error")) return(FALSE)
  TRUE
}

#' Is GRASS available?
#'
#' Check if the Geographic Resources Analysis Support System (GRASS)
#' is available for processing data.
#'
#' @details
#' The function verifies that
#' (1) the \pkg{rgrass7} package in installed,
#' (2) the \pkg{link2GI} package in installed,
#' (3) GRASS is installed (i.e. via [link2GI::findGRASS]), and
#' (4) the version of GDAL installed is at least 7.8.5.
#' If any of these checks fail, then GRASS is not considered available.
#
#' @return A `logical` indicating if GRASS is available or not.
#'
#' @examples
#' # check if GRASS is available?
#' print(is_grass_available())
#'
#' @export
is_grass_available <- function() {
  if (!requireNamespace("rgrass7", quietly = TRUE)) return(FALSE)
  if (!requireNamespace("link2GI", quietly = TRUE)) return(FALSE)
  x <- link2GI::findGRASS()
  if (!inherits(x, "data.frame")) return(FALSE)
  isTRUE(as.package_version(x$version) >= as.package_version("7.8.5"))
}