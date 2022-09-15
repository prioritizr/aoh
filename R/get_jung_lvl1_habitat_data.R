#' @include internal.R
NULL

#' Get Jung *et al.* (2020) habitat classification data (level 1)
#'
#' Import habitat classification data (level 1) derived from Jung *et al.*
#' (2020a).
#' If data are not available locally, they are downloaded from
#' a Zenodo repository (\doi{10.5281/zenodo.6622029}).
#'
#' @inheritParams get_jung_lvl2_habitat_data
#'
#' @details
#' The data were produced by obtaining the level 1 habitat
#' classification data from a
#' Zenodo repository
#' (Jung *et al.* 2020b), and resampling the data (using nearest neighbor
#' interpolation) to the World Behrmannn coordinate reference
#' systems (ESRI:54017).
#'
#' @seealso
#' See [crosswalk_jung_lvl1_data()] for details on which grid values correspond
#' to which habitat classification codes.
#'
#' @inherit get_jung_lvl2_habitat_data references return
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
#' habitat_data <- get_jung_lvl1_habitat_data(download_dir, version = "latest")
#'
#' # preview data
#' print(habitat_data)
#'
#' # plot data
#' plot(habitat_data)
#' }
#' @export
get_jung_lvl1_habitat_data <- function(dir = tempdir(),
                                       version = "latest",
                                       force = FALSE,
                                       verbose = TRUE) {
  # get data
  path <- get_zenodo_data(
    x = "10.5281/zenodo.6622038",
    file = function(x) all(grepl("^jung-lvl1-.*\\.tif$", x)),
    version = version,
    dir = dir,
    force = force,
    verbose = verbose
  )

  # return data
  terra::rast(path)
}
