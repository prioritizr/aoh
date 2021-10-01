#' @include internal.R
NULL

#' Read species geographic range data
#'
#' Import species geographic range data obtained from the International Union
#' for Conservation of Nature (IUCN) Red List of Threatened
#' Species (<https://www.iucnredlist.org/>).
#'
#' @param x `character` file path to the data (zip archive) containing the
#'   geographic range data.
#'
#' @details
#' Data for amphibians, reptiles, and mammals can be obtained directly from
#' the International Union for Conservation of Nature (IUCN) Red List website
#' (see https://www.iucnredlist.org/resources/spatial-data-download).
#' Data for birds can be obtained by requesting data from
#' [BirdLife International](http://www.birdlife.org/)
#' (see http://datazone.birdlife.org/species/requestdis).
#'
#' @return A [sf::sf()] object containing the dataset.
#'
#' @seealso
#' See [aoh_clean_spp_range_data()] to clean data for subsequent analysis.
#'
#' @examples
#' # find file path for simulated data following the IUCN Red List format
#' path <- system.file("XENOMORPHS_TERRESTRIAL_ONLY.zip")
#'
#' # import data
#' sim_spp_range_data <- read_eoo_data(path)
#'
#' # preview data
#' print(sim_spp_range_data)
#'
#' # plot data
#' plot(sim_spp_range_data)
#' @exports
read_eoo_data <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x),
    file.exists(x),
    endsWith(x, ".zip")
  )
  # unzip data to temporary directory
  temp_dir <- tempfile()
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(x, temp_dir)
  # find shapefile with data
  path <- dir(temp_dir, "^.*\\.shp$", full.names = TRUE)
  if (length(path) != 1L) {
    stop("argument to \"x\" does not contain spatial data")
  }
  # import data
  sf::read_sf(path)
}
