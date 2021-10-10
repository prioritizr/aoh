#' @include internal.R misc_sf.R
NULL

#' Read species range data
#'
#' Import species geographic range (i.e. extent of occurrence) data obtained
#' from the
#' [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/).
#'
#' @param path `character` File path to the data (zip archive) file.
#'
#' @param n `numeric` Number of features in the dataset to import.
#'  Defaults to `NULL` such that all available data is imported.
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
#' @examples
#' # find file path for simulated data following the IUCN Red List format
#' path <- system.file("extdata", "SIMULATED_SPECIES.zip", package = "aoh")
#'
#' # import data
#' sim_spp_range_data <- read_spp_range_data(path)
#'
#' # preview data (only if running R in an interactive session)
#' if (interactive()) {
#'   print(sim_spp_range_data)
#' }
#'
#' # plot data (only if running R in an interactive session)
#' if (interactive()) {
#'   print(sim_spp_range_data)
#' }
#' @export
read_spp_range_data <- function(path, n = NULL) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(path),
    assertthat::noNA(path),
    assertthat::is.readable(path),
    assertthat::has_extension(path, "zip")
  )
  # unzip data to temporary directory
  temp_dir <- tempfile()
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(path, exdir = temp_dir)
  # find shapefile with data
  input_path <- dir(temp_dir, "^.*\\.shp$", full.names = TRUE, recursive = TRUE)
  if (length(input_path) != 1L) {
    stop("argument to \"path\" does not contain spatial data")
  }
  # import data
  out <- read_sf_n(input_path, n = n)
  # clean up
  unlink(temp_dir, recursive = TRUE, force = TRUE)
  # return result
  out
}
