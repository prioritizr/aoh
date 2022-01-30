#' @include internal.R
NULL

#' Get Jung *et al.* (2020) habitat classification data (level 1)
#'
#' Import habitat classification data (level 1) derived from Jung *et al.*
#' (2020a).
#' If data are not available locally, they are downloaded from
#' an online repository.
#'
#' @inheritParams get_jung_lvl2_habitat_data
#'
#' @details
#' The data were produced by obtaining the level 1 habitat
#' classification data from the
#' [Zenodo repository](https://zenodo.org/10.5281/zenodo.3666245)
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
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(dir),
    assertthat::noNA(dir),
    file.exists(dir),
    assertthat::is.string(version),
    assertthat::noNA(version),
    assertthat::is.flag(force),
    assertthat::noNA(force),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )

  # find DOI version if needed
  if (identical(version, "latest")) {
    ## verify if internet connection present
    if (!curl::has_internet()) {
      stop("no internet connection detected.")
    }

    ## find latest version
    version <- latest_zenodo_version(
      x = "10.5281/zenodo.4058356",
      file = function(x) {
        any(
          startsWith(x, "iucn_habitatclassification_composite_lvl1") &
          endsWith(x, ".zip")
        )
      }
    )
  }

  # process file path
  path <- gsub(".", "-", gsub("/", "_", version, fixed = TRUE), fixed = TRUE)
  path <- file.path(dir, paste0("jung-lvl1-", path, ".tif"))
  path <- gsub("\\", "/", path, fixed = TRUE)

  # fetch data if needed
  if (isTRUE(force) || !file.exists(path)) {
    piggyback::pb_download(
      file = basename(path),
      repo = "prioritizr/aoh",
      tag = "data",
      overwrite = TRUE,
      dest = dirname(path),
      show_progress = verbose
    )
  }

  # import data
  terra::rast(path)
}
