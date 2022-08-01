.onLoad <- function(libname, pkgname) {
  # import data for internal usage
  utils::data(
    "crosswalk_jung_lvl1_data",
    "crosswalk_jung_lvl2_data",
    "crosswalk_jung_plvl1_data",
    "crosswalk_lumb_cgls_data",
    "iucn_habitat_data",
    package = pkgname,
    envir = parent.env(environment())
  )
}


.onAttach <- function(libname, pkgname) {
  # initialize message
  m <- paste0(
    "alert: out-dated dependencies detected -",
    "please update them.\n"
  )
  show <- FALSE
  # check valid GDAL version installed
  if (!isTRUE(is_gdal_version_met())) {
    show <- TRUE
    m <- paste0(
      m, "  GDAL version ", get_gdal_min_version(), " is required, and ",
      get_gdal_installed_version(), " is installed.\n"
    )
  }
  # check valid PROJ version installed
  if (!isTRUE(is_proj_version_met())) {
    show <- TRUE
    m <- paste0(
      m, "  PROJ version ", get_proj_min_version(), " is required, and ",
      get_proj_installed_version(), " is installed.\n"
    )
  }
  # display message
  if (isTRUE(show)) {
    packageStartupMessage(
      paste0(
        m, "  N.B., after updating dependencies, please re-install the ",
        "sf, terra, and prepr R packages."
      )
    )
  }
  # return invisible
  invisible()
}

get_gdal_installed_version <- function() {
  v <- as.list(sf::sf_extSoftVersion())[["GDAL"]]
  if (is.null(v)) v <- "0.0.0" # nocov
  v
}

get_gdal_min_version <- function() {
  "3.0.2"
}

is_gdal_version_met <- function() {
  package_version(get_gdal_installed_version()) >=
  package_version(get_gdal_min_version())
}

get_proj_installed_version <- function() {
  v <- as.list(sf::sf_extSoftVersion())[["PROJ"]]
  if (is.null(v)) v <- "0.0.0" # nocov
  v
}

get_proj_min_version <- function() {
  "7.2.0"
}

is_proj_version_met <- function() {
  package_version(get_proj_installed_version()) >=
  package_version(get_proj_min_version())
}
