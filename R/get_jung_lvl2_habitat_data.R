#' @include internal.R
NULL

#' Get Jung *et al.* (2020) habitat classification data (level 2)
#'
#' Import habitat classification data (level 2) derived from Jung *et al.*
#' (2020a).
#' If data are not available locally, they are downloaded from
#' a Zenodo repository (\doi{10.5281/zenodo.6622029}).
#'
#' @param dir `character` Folder path for downloading and caching data.
#'  By default, data are downloaded to a temporary directory (i.e.,
#'  `tempdir()`). **To avoid downloading the same data multiple times, it is
#'  strongly recommended to specify a persistent storage location (see Examples
#'  below).**
#'
#' @param version `character` Value indicating the specific version of the
#'  dataset
#'  that should be downloaded. The version should be indicated using the
#'  Digital Object Identifier of the specific version required (e.g.
#'  `"10.5281/zenodo.3673586"`).
#'  Defaults to `"latest"` such that the latest
#'  release of the dataset with available habitat data is used.
#'
#' @param force `logical` Should the data be downloaded even if the
#'  the data are already available?
#'  Defaults to `FALSE`.
#'
#' @param verbose `logical` Should progress be displayed while downloading
#'  data?
#'  Defaults to `TRUE`.
#'
#' @details
#' The data were produced by obtaining the level 2 habitat
#' classification data from a
#' Zenodo repository
#' (Jung *et al.* 2020b), and resampling the data (using nearest neighbor
#' interpolation) to the World Behrmannn coordinate reference
#' systems (ESRI:54017).
#'
#' @return A [terra::rast()] object containing the habitat data
#'  (100 m resolution). Pixel values indicate the habitat classification codes.
#'
#' @seealso
#' See [crosswalk_jung_lvl2_data()] for details on which grid values correspond
#' to which habitat classification codes.
#'
#' @references
#' Jung M, Dahal PR, Butchart SHM, Donald PF, De Lamo X, Lesiv M, Kapos V,
#' Rondinini C, and Visconti P (2020a) A global map of
#' terrestrial habitat types. *Scientific Data*, 7: 1--8.
#' \doi{10.1038/s41597-020-00599-8}
#'
#' Jung M, Dahal PR, Butchart SHM, Donald PF, De Lamo X, Lesiv M, Kapos V,
#' Rondinini C, and Visconti P (2020b) A global map of
#' terrestrial habitat types (insert version) \[Data set\].
#' *Zenodo*.
#' \doi{10.5281/zenodo.4058819}
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
#' habitat_data <- get_jung_lvl2_habitat_data(download_dir, version = "latest")
#'
#' # preview data
#' print(habitat_data)
#'
#' # plot data
#' plot(habitat_data)
#' }
#' @export
get_jung_lvl2_habitat_data <- function(dir = tempdir(),
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

  # set variables
  file <- function(x) all(grepl("^jung-lvl2-.*\\.tif$", x))
  dir <- gsub("\\", "/", dir, fixed = TRUE)

  # find version if needed
  if (identical(version, "latest")) {
    ## verify if internet connection present
    if (!curl::has_internet()) {
      stop("no internet connection detected.")
    }

    ## find latest version
    version <- latest_zenodo_version(x = "10.5281/zenodo.6622038", file = file)
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
