.onLoad <- function(libname, pkgname) {
  # import data for internal usage
  utils::data(
    "crosswalk_jung_lvl1_data",
    "crosswalk_jung_lvl2_data",
    "crosswalk_lumbierres_data",
    "iucn_habitat_data",
    package = pkgname,
    envir = parent.env(environment())
  )
}
