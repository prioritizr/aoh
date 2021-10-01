#' @include internal.R
NULL

#' Get habitat classification data
#'
#' Import habitat classification data produced by Jung *et al.* (2020).
#' If data are not available locally, they are downloaded from
#' the Zenodo Digital Archive (<doi:10.5281/zenodo.4058819>).
#'
#' @param x `character` folder path where data should be downloaded.
#'  By default, data are downloaded to a temporary directory (i.e. `tempdir()`).
#'  **To avoid downloading the same data multiple times, it is strongly
#'  recommended to specify a persistent storage location (see Examples below).**
#'
#' @param version `character` indicating the specific version of the dataset
#'  that should be downloaded. The version should be indicated using the
#'  Digital Object Identifier of the specific version required (e.g.
#'  10.5281/zenodo.3673586). Defaults to `"latest"` such that the latest
#'  release of the dataset with available habitat data is used.
#'
#' @param force `logical` should the data be downloaded even if the
#'  the data are already available?
#'  Defaults to `FALSE`.
#'
#' @param verbose `logical` should progress be displayed while downloading
#'  data?
#'  Defaults to `TRUE`.
#'
#' @return A [raster::brick()] object containing the level 2 habitat
#'  fractional coverage data.
#'  These data are available at the 1 km \eqn{\times} 1 km resolution.
#'  Each layer corresponds to a different habitat type, and each pixel
#'  denotes the fraction of the pixel that contains a given habitat type
#'  (i.e. a value of 0.5 indicates 50% coverage).
#'
#' @references
#' Jung M, Dahal PR, Butchart SH, Donald PF, De Lamo X, Lesiv M, Kapos V,
#' Rondinini C, and Visconti P (2020) A global map of
#' terrestrial habitat types. Scientific data, 7:1--8.
#' <https://doi.org/10.1038/s41597-020-00599-8>
#'
#' Jung M, Dahal PR, Butchart SH, Donald PF, De Lamo X, Lesiv M, Kapos V,
#' Rondinini C, and Visconti P (2020) A global map of
#' terrestrial habitat types ([insert version]) [Data set]. Zenodo.
#' <https://doi.org/10.5281/zenodo.4058819>
#'
#' @examples
#' \dontrun{
#' # define persistent storage location
#' download_dir <- rappdirs::app_dir("aoh")
#'
#' # download and import habitat data
#' habitat_data <- aoh_get_habitat_data(download_dir, version = "latest")
#'
#' # preview data
#' print(habitat_data)
#'
#' # plot data
#' plot(habitat_data)
#' }
#' @export
get_habitat_data <- function(x = tempdir(), version = "latest",
                             force = FALSE, verbose = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x),
    file.exists(x),
    assertthat::is.string(version),
    assertthat::noNA(version),
    assertthat::is.flag(force),
    assertthat::noNA(force),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )
  if (!curl::has_internet()) {
    stop("no internet connection detected.")
  }
  # define data DOI
  doi <- "10.5281/zenodo.4058356"
  # find available versions of dataset
  zc <- zen4R::ZenodoManager$new(logger = "INFO")
  all_versions <- zc$getRecordByDOI(doi)$getVersions()
  # find specified version of dataset
  if (identical(version, "latest")) {
    version <- NA_character_
    for (x in rev(all_versions$doi)) {
      if (version_has_habitat_data(x)) {
        version <- x
      }
    }
    if (is.na(version)) {
      stop(
        paste(
          "something went wrong, please submit an issue at",
          "https://github.com/jeffreyhanson/aoh"
        )
      )
    }
  } else {
    # verify that version valid
    if (!version %in% all_versions$doi) {
      message("available versions include:")
      message(paste(paste0("\"", all_versions$doi, "\"", collapse = ",")))
      stop("argument to \"version\" is not valid")
    }
    # verify that version has data
    if (!version_has_habitat_data(version, zc)) {
      stop(
        paste0(
          "argument to \"version\" specifies a version of the dataset that ",
          "only contains source code, please try a different version."
        )
      )
    }
  }
  # download data if needed
  record_dir <- file.path(x, version)
  if (!file.exists(record_path) || isTRUE(force)) {
    dir.create(record_dir, showWarnings = FALSE, recursive = TRUE)
    zc$getRecordByDOI(x)$downloadFiles(path = record_dir)
  }
  # find file with the data
  paths <- dir(record_data, full.names = TRUE)
  i <- which(
    startsWith(basename(paths), "lvl2_frac_1km_") &
    endsWith(basename(paths), ".zip")
  )
  path <- paths[i]
  if (length(path) != 1L) {
    stop(
      paste0(
        "something went wrong the importing data,",
        "try again with \"force = TRUE\" to force re-downloading the data."
      )
    )
  }
  # extract the data to temporary archive
  temp_dir <- tempfile()
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(path, temp_dir)
  # find file paths for layers
  layer_paths <- dir(temp_dir, "^.*\\.tif", full.names = TRUE)
  # import data
  r <- terra::rast(layer_paths)
  # assign names
  names(r) <- habitat_codes(layer_paths)
  # return result
  r
}


#' Has habitat data?
#'
#' Check if a specific version of the Jung *et al.* (2020) Zenodo repository
#' has valid habitat data.
#'
#' @param x `character` Digitial Object Identifier (DOI) for a specific
#'  version of the dataset.
#'
#' @param zc `ZenodoManager` object. This should be created by
#'   `ZenodoManager$new()`. Defaults to a new Zenodo Manager object.
#'
#' @details
#' This function is useful because some versions of the dataset on Zenodo only
#' contain the source code used to generate the data, and not contain
#' any data.
#'
#' @inherit get_habitat_data references
#'
#' @noRd
version_has_habitat_data <- function(x, zc = ZenodoManager$new()) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x),
    inherits(zc, "ZenodoManager")
  )
  # find all files in record
  f <- zc$getRecordByDOI(x)$listFiles()$filename
  # verify that has zip file following expected pattern
  any(startsWith(f, "lvl2_frac_1km_") & endsWith(f, ".zip"))
}

#' Habitat codes
#'
#' Extract habitat codes from the raster file names.
#'
#'
#' @param x `character` file name(s).
#'
#' @details
#' This function is designed to be used for processing habitat classification
#' data downloaded from Jung *et al.* (2020).
#'
#' @inherit get_habitat_data references
#'
#' @return `character` habitat classification codes.
#'
#' @nord
habitat_codes <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(x),
    assertthat::noNA(x),
    all(nchar(x) > 0),
    length(x) >= 0
  )
  # extract codes
  x <- gsub("iucn_habitatclassification_fraction_lvl2", "", x, fixed = TRUE)
  x <- strsplit(x, "_", x, fixed = TRUE)
  x <- vapply(x, FUN.VALUE = character(1), function(x) {
    x[which(nchar(x) > 0)[1]]
  })
  x
}
