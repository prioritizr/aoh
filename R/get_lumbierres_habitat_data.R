#' @include internal.R
NULL

#' Get Lumbierres *et al.* (2021) habitat classification data
#'
#' Import habitat classification data derived Lumbierres *et al.* (2021).
#' If data are not available locally, they are downloaded from
#' an online repository.
#'
#' @inheritParams get_jung_habitat_data
#'
#' @details
#' The data were produced by obtaining the level 1 habitat
#' classification data from the
#' [Zenodo repository](https://doi.org/10.5281/zenodo.5146072)
#' (Lumbierres 2021), and resampling the data (using nearest neighbor
#' interpolation) to the World Behrmannn coordinate reference
#' systems (ESRI:54017).
#'
#' @inherit get_jung_habitat_data return
#'
#' @seealso
#' See [crosswalk_jung_data()] for details on which grid values correspond
#' to which habitat classification codes.
#'
#' @references
#' Lumbierres, M (2021). Map of habitat classes (Level 1) from the IUCN
#' Habitat.*Zenodo Digital Repository*.
#' Available at <https://doi.org/10.5281/zenodo.5146072>.
#'
#' Lumbierres M, Dahal PR, Di Marco M, Butchart SHM, Donald PF, and
#' Rondinini C (2021) Translating habitat class to land cover to map area of
#' habitat of terrestrial vertebrates. *Conservation Biology*, In press,
#' DOI:10.1111/cobi.13851.
#' Available at <https://doi.org/10.1111/cobi.13851>.
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
#' habitat_data <- get_lumbierres_habitat_data(download_dir, version = "latest")
#'
#' # preview data
#' print(habitat_data)
#'
#' # plot data
#' plot(habitat_data)
#' }
#' @export
get_lumbierres_habitat_data <- function(dir = tempdir(),
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
    ### note that latest_zenodo_version() fails for Zenodo archives that
    ### only have one version, so here we manually specify the first version if
    ### it fails
    version <- try(
      latest_zenodo_version(
        x = "10.5281/zenodo.5146072",
        file = "habitat_CGLS.tiff"
      ),
      silent = TRUE
    )
    if (inherits(version, "try-error")) {
     version <- "10.5281/zenodo.5146073"
    }
  }

  # process file path
  path <- gsub(".", "-", gsub("/", "_", version, fixed = TRUE), fixed = TRUE)
  path <- file.path(dir, paste0("lumbierres-", path, ".tif"))
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
