.onLoad <- function(libname, pkgname) {
  # import data for interal usage
  utils::data(
    "crosswalk_jung_data",
    "iucn_habitat_data",
    package = pkgname,
    envir = parent.env(environment())
  )
}
