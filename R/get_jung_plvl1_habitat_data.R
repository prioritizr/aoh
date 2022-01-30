#' @include internal.R
NULL

#' Get Jung (2020) potential habitat classification data (level 1)
#'
#' Import potential habitat classification data derived from Jung (2020).
#' If data are not available locally, they are downloaded from
#' an online repository.
#'
#' @inheritParams get_jung_lvl2_habitat_data
#'
#' @details
#' The data predict the potential habitat types that would be available in a
#' given location if not for anthropogenic activities
#' (see Hengl *et al.* 2018 for details).
#' As such, they can be used to identify locations where
#' restoration activities could produce certain habitat types.
#' This means that they can, in turn, be used to predict the location of
#' suitable habitat for species following the restoration activities.
#' Since the data do not show the historic distribution of habitat types,
#' they cannot be used to examine patterns of habitat loss.
#'
#' The data were produced by obtaining the level 1 habitat
#' classification data from the
#' [Zenodo repository](https://doi.org/10.5281/zenodo.4038749)
#' (Jung 2020), and resampling the data (using nearest neighbor
#' interpolation) to the World Behrmannn coordinate reference
#' systems (ESRI:54017).
#'
#' @return
#' A [terra::rast()] object containing the potential habitat data
#' (100 m resolution). Pixel values indicate the potential habitat
#' classification codes.
#'
#' @seealso
#' See [crosswalk_jung_lvl1_data()] for details on which grid values correspond
#' to which habitat classification codes.
#'
#' @references
#' Jung M (2020) A layer of global potential habitats (insert version)
#' \[Data set\]. *Zenodo Digital Repository*.
#' Available at <https://doi.org/10.5281/zenodo.4038749>.
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
#' # download and import potential habitat data
#' ph_data <- get_jung_plvl1_habitat_data(download_dir, version = "latest")
#'
#' # preview data
#' print(ph_data)
#'
#' # plot data
#' plot(ph_data)
#' }
#' @export
get_jung_plvl1_habitat_data <- function(dir = tempdir(),
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
      x = "10.5281/zenodo.4038749",
      file = function(x) {
        any(
          startsWith(x, "pnv_lvl1") &
          endsWith(x, ".zip")
        )
      }
    )
  }

  # process file path
  path <- gsub(".", "-", gsub("/", "_", version, fixed = TRUE), fixed = TRUE)
  path <- file.path(dir, paste0("jung-plvl1-", path, ".tif"))
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
