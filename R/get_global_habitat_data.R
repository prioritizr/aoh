#' @include internal.R convert_filename_to_habitat_code.R
NULL

#' Get global habitat classification data
#'
#' Import habitat classification data produced by Jung *et al.* (2020a).
#' If data are not available locally, they are downloaded from
#' the Zenodo Digital Repository (Jung *et al.* 2020b).
#'
#' @param dir `character` folder path for downloading and caching data.
#'  By default, data are downloaded to a temporary directory (i.e. `tempdir()`).
#'  **To avoid downloading the same data multiple times, it is strongly
#'  recommended to specify a persistent storage location (see Examples below).**
#'
#' @param version `character` indicating the specific version of the dataset
#'  that should be downloaded. The version should be indicated using the
#'  Digital Object Identifier of the specific version required (e.g.
#'  `"10.5281/zenodo.3673586"`).
#'  Defaults to `"latest"` such that the latest
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
#' @return A [terra::rast()] object containing the level 2 habitat
#'  fractional coverage data.
#'  These data are available at the 1 km \eqn{\times} 1 km resolution.
#'  Each layer corresponds to a different habitat type, and each pixel
#'  denotes the fraction of the pixel that contains a given habitat type.
#'  **Note that pixel values are scaled to between 0 and 1000, such that
#'  a value of 0 indicates 0% coverage of a habitat type, and a value of
#'  1000 indicates 100% coverage of a habitat type.**
#'
#' @references
#' Jung M, Dahal PR, Butchart SHM, Donald PF, De Lamo X, Lesiv M, Kapos V,
#' Rondinini C, and Visconti P (2020a) A global map of
#' terrestrial habitat types. *Scientific Data*, 7:1--8.
#' Available at <https://doi.org/10.1038/s41597-020-00599-8>.
#'
#' Jung M, Dahal PR, Butchart SHM, Donald PF, De Lamo X, Lesiv M, Kapos V,
#' Rondinini C, and Visconti P (2020b) A global map of
#' terrestrial habitat types (insert version) \[Data set\].
#' *Zenodo Digital Repository*.
#' Available at <https://doi.org/10.5281/zenodo.4058819>.
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
#' habitat_data <- get_global_habitat_data(download_dir, version = "latest")
#'
#' # preview data
#' print(habitat_data)
#'
#' # plot data
#' plot(habitat_data)
#' }
#' @export
get_global_habitat_data <- function(dir = tempdir(), version = "latest",
                                    force = FALSE, verbose = TRUE) {
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
  if (!curl::has_internet()) {
    stop("no internet connection detected.")
  }

  # define data DOI
  doi <- jung_et_al_2020_habitat_data_doi

  # create file path for dataset based on version
  record_dir <- file.path(dir, gsub("/", "-", version, fixed = TRUE))

  # see if version is already available
  if (!file.exists(record_dir) || identical(version, "latest")) {
    ## find all available version
    z <- zen4R::ZenodoManager$new(logger = NULL)
    ## find specified version of dataset
    if (identical(version, "latest")) {
      ## find latest version DOI
      version <- latest_version_habitat_data(x = doi, z = z)
      ## update directory path for storing data
      record_dir <- file.path(dir, gsub("/", "-", version, fixed = TRUE))
    } else {
      ## verify that version valid
      all_versions <- suppressWarnings(z$getRecordByDOI(doi)$getVersions())
      if (!version %in% all_versions$doi) {
        message("available versions include:")
        message(paste(paste0("\"", all_versions$doi, "\"", collapse = ",")))
        stop("argument to \"version\" is not valid")
      }
    }
  }
  # download data if needed
  if (!file.exists(record_dir) || isTRUE(force)) {
    dir.create(record_dir, showWarnings = FALSE, recursive = TRUE)
    download_habitat_data(
      x = version, dir = record_dir, z = z, verbose = verbose
    )
  }
  # extract the data to temporary archive
  path <- dir(record_dir, "^.*\\.zip", full.names = TRUE)
  path <- gsub("\\", "/", path, fixed = TRUE)
  temp_dir <- tempfile()
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(path, exdir = temp_dir)
  # find file paths for layers
  raw_layer_paths <- dir(
    temp_dir, "^.*\\.tif", recursive = TRUE, full.names = TRUE
  )
  # ensure consistent file path separators
  raw_layer_paths <- gsub("\\", "/", raw_layer_paths, fixed = TRUE)
  # rename file paths to remove non-ascii characters
  layer_paths <- paste0(
    dirname(raw_layer_paths), "/",
    iconv(basename(raw_layer_paths), "latin1", "ASCII", sub = "")
  )
  file.rename(from = raw_layer_paths, to = layer_paths)
  # import data
  r <- terra::rast(layer_paths)
  # assign names
  names(r) <- convert_filename_to_habitat_code(basename(layer_paths))
  # return result
  r
}

#' Has habitat data?
#'
#' Check if a specific version of the Jung *et al.* (2020) Zenodo repository
#' has valid habitat data.
#'
#' @param x `character` Digital Object Identifier (DOI) for a specific
#'  version of the dataset.
#'
#' @param z `ZenodoManager` object. This should be created by
#'   `ZenodoManager$new()`. Defaults to a new Zenodo Manager object.
#'
#' @details
#' This function is useful because some versions of the dataset on Zenodo only
#' contain the source code used to generate the data, and not contain
#' any data.
#'
#' @inherit get_habitat_data references
#'
#' @return A `logical` value indicating if habitat data was detected.
#'
#' @noRd
version_has_habitat_data <- function(x, z = zen4R::ZenodoManager$new()) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x),
    inherits(z, "ZenodoManager")
  )
  # find all files in record
  f <- z$getRecordByDOI(x)$listFiles()$filename
  # verify that has zip file following expected pattern
  any(startsWith(f, "lvl2_frac_1km_") & endsWith(f, ".zip"))
}

#' Download habitat data
#'
#' Download habitat data from a specific version of the Jung *et al.* (2020)
#' Zenodo repository.
#'
#' @param x `character` Digitial Object Identifier (DOI) for a specific
#'  version of the dataset.
#'
#' @param dir `character` folder to store downloaded data.
#'
#' @param z `ZenodoManager` object. This should be created by
#'   `ZenodoManager$new()`. Defaults to a new Zenodo Manager object.
#'
#' @inheritParams get_global_habitat_data
#'
#' @inherit get_global_habitat_data references
#'
#' @return An invisible `TRUE` indicating success.
#'
#' @noRd
download_habitat_data <- function(x, dir, z = zen4R::ZenodoManager$new(),
                                  verbose = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x),
    assertthat::is.string(dir),
    assertthat::noNA(dir),
    inherits(z, "ZenodoManager")
  )
  # find all files in record
  f <- z$getRecordByDOI(x)$listFiles()
  # find file to download
  i <- which(
    startsWith(f$filename, "lvl2_frac_1km_") &
    endsWith(f$filename, ".zip")
  )
  assertthat::assert_that(
    length(i) == 1,
    msg = paste(
      "argument to \"version\" specifies a version of the dataset that",
      "only contains source code, please try a different version."
    )
  )
  # download data
  curl::curl_download(
    url = f$download[i],
    destfile = file.path(dir, f$filename[i]),
    quiet = !isTRUE(verbose)
  )
  # return succes
  invisible(TRUE)
}

#' Latest version of habitat data
#'
#' Find the Digital Object Identifier (DOI) for the latest version of
#' habitat data available from the Jung *et al.* (2020) Zenodo repository.
#'
#' @inheritParams download_habitat_data
#'
#' @inherit download_habitat_data references details
#'
#' @return A `character` value indicating the DOI.
#'
#' @noRd
latest_version_habitat_data <- function(x, z = zen4R::ZenodoManager$new()) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(z, "ZenodoManager")
  )
  # find version
  version <- NA_character_
  invisible(utils::capture.output({
    all_versions <- suppressWarnings(z$getRecordByDOI(x)$getVersions())
  }))
  for (x in rev(all_versions$doi)) {
    if (version_has_habitat_data(x = x, z = z)) {
      version <- x
    }
  }
  # verify that version was found
  assertthat::assert_that(
    !is.na(version),
    msg = "unable to find any versions with data available"
  )
  # return result
  version
}

# Digital object identifier for Jung et al 2020 Zenodo repository
jung_et_al_2020_habitat_data_doi <- "10.5281/zenodo.4058356"
