#' @include internal.R
NULL

#' Get global elevation data
#'
#' Import elevation data produced by Amatulli *et al.* (2018).
#' If data are not available locally, they are downloaded from the
#' [EarthEnv project](http://www.earthenv.org/).
#'
#' @inheritParams get_global_habitat_data
#'
#' @return A [raster::raster()] object containing the median elevation data.
#'  data. These data are available at the 1 km \eqn{\times} 1 km resolution.
#'
#' @references
#' Amatulli G, Domisch S, Tuanmu M-N, Parmentier B, Ranipeta A, Malczyk J, and
#' Jetz W (2018) A suite of global, cross-scale topographic variables for
#' environmental and biodiversity modeling. *Scientific Data*, 5:180040.
#' Available at <https://doi.org/10.1038/sdata.2018.40>.
#'
#' @examples
#' \dontrun{
#' # define persistent storage location
#' download_dir <- rappdirs::user_data_dir("aoh")
#'
#' # create download directory if needed
#' if (!file.exists(download_dir)) {
#'   dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
#' }
#'
#' # download and import elevation data
#' elev_data <- get_global_elevation_data(download_dir)
#'
#' # preview data
#' print(elev_data)
#'
#' # plot data
#' plot(elev_data)
#' }
#' @export
get_global_elevation_data <- function(dir = tempdir(), force = FALSE,
                                      verbose = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(dir),
    assertthat::noNA(dir),
    file.exists(dir),
    assertthat::is.flag(force),
    assertthat::noNA(force),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )
  # define data URL
  url <- "https://data.earthenv.org/topography/elevation_1KMmd_GMTEDmd.tif"
  # define download location
  path <- file.path(dir, "elevation_1KMmd_GMTED.tif")
  # check if need to download data data already available
  if (!file.exists(path) || isTRUE(force)) {
    if (!curl::has_internet()) {
      stop("no internet connection detected.")
    }
    curl::curl_download(url = url, destfile = path, quiet = !isTRUE(verbose))
  }
  # import data
  terra::rast(path)
}
