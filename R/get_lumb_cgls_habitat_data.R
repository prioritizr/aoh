#' @include internal.R
NULL

#' Get Lumbierres *et al.* (2021) CGLS habitat classification data
#'
#' Import habitat classification data derived from the
#' Copernicus Global Land Service Land Cover
#' (CGLS-LC100) dataset (Buchhorn *et al.*, 2019; Buchhorn *et al.*, 20200)
#' following Lumbierres *et al.* (2021).
#' If data are not available locally, they are downloaded from
#' a Zenodo repository (\doi{10.5281/zenodo.6622059}).
#'
#' @inheritParams get_jung_lvl2_habitat_data
#'
#' @details
#' The data were produced by obtaining the level 1 habitat
#' classification data (derived from Lumbierres *et al.* 2021),
#' and resampling the data (using nearest neighbor
#' interpolation) to the World Behrmannn coordinate reference
#' system (ESRI:54017).
#'
#' @inherit get_jung_lvl2_habitat_data return
#'
#' @inherit crosswalk_lumb_cgls_data references
#'
#' @seealso
#' See [crosswalk_lumb_cgls_data()] for details on which grid values correspond
#' to which habitat classification codes.
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
#' # download and import habitat data
#' habitat_data <- get_lumb_cgls_habitat_data(download_dir, version = "latest")
#'
#' # preview data
#' print(habitat_data)
#'
#' # plot data
#' plot(habitat_data)
#' }
#' @export
get_lumb_cgls_habitat_data <- function(dir = tempdir(),
                                       version = "latest",
                                       force = FALSE,
                                       verbose = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(dir),
    assertthat::noNA(dir),
    assertthat::is.string(version),
    assertthat::noNA(version),
    assertthat::is.flag(force),
    assertthat::noNA(force),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )

  # set variables
  file <- function(x) all(grepl("^lumbierres-.*\\.tif$", x))

  # normalize file paths
  dir <- normalize_path(dir, mustWork = FALSE)
  assertthat::assert_that(file.exists(dir))

  # find version if needed
  if (identical(version, "latest")) {
    ## verify if internet connection present
    if (!curl::has_internet()) {
      stop("no internet connection detected.") # nocov
    }

    ## find latest version
    version <- latest_zenodo_version(x = "10.5281/zenodo.6622060", file = file)
  }

  # get data
  path <- get_zenodo_data(
    x = version,
    dir = dir,
    file = file,
    force = force,
    verbose = verbose
  )

  # import data
  terra::rast(path)
}
