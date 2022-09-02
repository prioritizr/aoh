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
#' @param n `integer` Number of times to attempt to access the API.
#'
#' @return A `logical` indicating if the IUCN Red List API can be accessed.
#'
#' @examples
#' \dontrun{
#' # check if IUCN Red List API is available?
#' is_iucn_rl_api_available()
#' }
#'
#' @export
is_iucn_rl_api_available <- function(key = NULL, n = 5) {
  assertthat::assert_that(assertthat::is.count(n), assertthat::noNA(n))
  for (i in seq_len(n)) {
    x <- try(rredlist::rl_regions(key = key), silent = TRUE)
    if (inherits(x, "try-error")) {
      Sys.sleep(3)
    } else {
      break()
    }
  }
  !inherits(x, "try-error")
}

#' System GDAL version
#'
#' Find the version of the Geospatial Data Abstraction Library (GDAL)
#' that is currently installed on the system.
#'
#' @return A `character` value describing the version of GDAL installed.
#' If GDAL is not installed, then a missing (`NA`) value is returned.
#'
#' @examples
#' # show version of GDAL installed
#' print(system_gdal_version())
#'
#' @export
system_gdal_version <- function() {
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

#' Is OSGeo4W available?
#'
#' Check if OSGeo4W software is available.
#'
#' @details
#' This software is used to provide GDAL Python scripts on Windows systems
#' that are used to generate Area of Habitat data via the GDAL engine.
#' It can be installed from <https://trac.osgeo.org/osgeo4w/>.
#' Note that macOS and Linux systems do not require this software.
#' By default, it is assumed that the software is installed in the
#' `"C:/OSGeo4W"` directory. If the software is installed at a different
#' location, then the `"OSGEO4W_ROOT"` environmental variable can be
#' used to specify a different location.
#'
#' @inherit is_gdal_calc_available return
#'
#' @examples
#' # see if OSGeo4W is available at the default location
#' print(is_osgeo4w_available())
#'
#' \dontrun{
#' # specify a different location for OSGeo4W, and
#' # then see if OSGeo4W is available at this location
#' Sys.setenv("OSGEO4W_ROOT" = "C:/software/OSGeo4W")
#' print(is_osgeo4w_available())
#' }
#'
#' @export
is_osgeo4w_available <- function() {
  if (!identical(.Platform$OS.type, "windows")) return(FALSE)
  r <- getOption("OSGEO4W_ROOT") %||% "C:/OSGeo4W"
  file.exists(normalize_path(r, mustWork = FALSE)) &&
    file.exists(normalize_path(file.path(r, "OSGeo4W.bat"), mustWork = FALSE))
}

#' Is gdal_calc.py available?
#'
#' Check if `gdal_calc.py` Python script is available.
#'
#' @details
#' The `gdal_calc.py` Python script is used to process Area of Habitat data
#' when using the GDAL engine (within [create_spp_aoh_data()]) .
#' This function determines if this script is available on the system.
#' On Windows systems, it first tries to access this script using the
#' OSGeo4W software (available at <https://trac.osgeo.org/osgeo4w/>).
#' If that fails, or if you are not using a Windows system: it then tries to
#' access this script using default system variables.
#'
#' @return A `logical` value indicating if it is available.
#'
#' @examples
#' # see if gdal_calc python script is available
#' print(is_gdal_calc_available())
#'
#' @export
is_gdal_calc_available <- function() {
  # see if OSGeo4W installation available
  if (is_osgeo4w_available()) return(TRUE)
  # otherwise, let's try the system library
  v <- system_gdal_version()
  if (is.na(v)) {
    warning(
      "system installation of GDAL is unknown",
      immediate. = TRUE
    )
  } else {
    if (as.package_version(v) < as.package_version("3.0.2")) {
      warning(
        "system installation of GDAL is too old, version must be >= 3.0.2",
        immediate. = TRUE
      )
      return(FALSE)
    }
  }
  v <- try(
    system(python_gdal_calc("--help"), intern = TRUE),
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
#' (3) GRASS is installed (i.e., via [link2GI::findGRASS]), and
#' (4) the version of GRASS installed is at least 7.8.7.
#' If any of these checks fail, then GRASS is not considered available.
#
#' @inherit is_gdal_calc_available return
#'
#' @examples
#' \dontrun{
#' # check if GRASS is available?
#' print(is_grass_available())
#' }
#' @export
is_grass_available <- function() {
  if (!requireNamespace("rgrass7", quietly = TRUE)) return(FALSE)
  if (!requireNamespace("link2GI", quietly = TRUE)) return(FALSE)
  x <- try(
      with_timeout(
      suppressWarnings(link2GI::findGRASS()),
      cpu = 10,
      elapsed = 10
    ),
    silent = TRUE
  )
  if (!inherits(x, "data.frame")) return(FALSE)
  isTRUE(as.package_version(x$version) >= as.package_version("7.8.7"))
}

# obtained from https://stackoverflow.com/a/53018594/3483791
with_timeout <- function(expr, cpu, elapsed){
  expr <- substitute(expr)
  envir <- parent.frame()
  setTimeLimit(cpu = cpu, elapsed = elapsed, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))
  eval(expr, envir = envir)
}
