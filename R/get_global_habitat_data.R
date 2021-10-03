#' @include internal.R
NULL

#' Get global habitat classification data
#'
#' Import habitat classification data produced by Jung *et al.* (2020a).
#' If data are not available locally, they are downloaded from
#' the Zenodo Digital Archive (Jung *et al.* 2020b).
#'
#' @param dir `character` folder path for downloading and caching data.
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
#' Rondinini C, and Visconti P (2020a) A global map of
#' terrestrial habitat types. Scientific data, 7:1--8.
#' <https://doi.org/10.1038/s41597-020-00599-8>
#'
#' Jung M, Dahal PR, Butchart SH, Donald PF, De Lamo X, Lesiv M, Kapos V,
#' Rondinini C, and Visconti P (2020b) A global map of
#' terrestrial habitat types (insert version) \[Data set\]. Zenodo.
#' <https://doi.org/10.5281/zenodo.4058819>
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
#' habitat_data <- aoh_get_habitat_data(download_dir, version = "latest")
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
  doi <- "10.5281/zenodo.4058356"
  # find available versions of dataset
  zc <- zen4R::ZenodoManager$new(logger = NULL)
  all_versions <- suppressWarnings(zc$getRecordByDOI(doi)$getVersions())
  # find specified version of dataset
  if (identical(version, "latest")) {
    version <- NA_character_
    for (x in rev(all_versions$doi)) {
      if (version_has_habitat_data(x = x, zc = zc)) {
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
  }
  # download data if needed
  record_dir <- file.path(dir, gsub("/", "-", version, fixed = TRUE))
  if (!file.exists(record_dir) || isTRUE(force)) {
    dir.create(record_dir, showWarnings = FALSE, recursive = TRUE)
    download_habitat_data(
      x = version, dir = record_dir, zc = zc, verbose = verbose
    )
  }
  # extract the data to temporary archive
  path <- dir(record_dir, "^.*\\.zip", full.names = TRUE)
  temp_dir <- tempfile()
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(path, exdir = temp_dir)
  # find file paths for layers
  layer_paths <- dir(temp_dir, "^.*\\.tif", recursive = TRUE, full.names = TRUE)
  # import data
  r <- terra::rast(layer_paths)
  # assign names
  names(r) <- habitat_codes(basename(layer_paths))
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
#' @return A `logical` value indicating if habitat data was detected.
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
#' @param zc `ZenodoManager` object. This should be created by
#'   `ZenodoManager$new()`. Defaults to a new Zenodo Manager object.
#'
#' @inheritParams get_global_habitat_data
#'
#' @inherit get_global_habitat_data references
#'
#' @return An invisible `TRUE` indicating success.
#'
#' @noRd
download_habitat_data <- function(x, dir, zc = ZenodoManager$new(),
                                  verbose = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x),
    assertthat::is.string(dir),
    assertthat::noNA(dir),
    inherits(zc, "ZenodoManager")
  )
  # find all files in record
  f <- zc$getRecordByDOI(x)$listFiles()
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

#' Habitat codes
#'
#' Identify IUCN Red List habitat classification codes for data
#' files distributed with the Jung *et al.* (2020) dataset.
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
#' @noRd
habitat_codes <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(x),
    assertthat::noNA(x),
    all(nchar(x) > 0),
    length(x) >= 0
  )
  # extract Jung et al. codes
  assertthat::assert_that(
    all(startsWith(x, "iucn_habitatclassification_fraction_lvl2")),
    msg = "file name(s) is not recognized"
  )
  x <- gsub("iucn_habitatclassification_fraction_lvl2", "", x, fixed = TRUE)
  x <- strsplit(x, "_", x, fixed = TRUE)
  x <- vapply(x, FUN.VALUE = character(1), function(x) {
    x[which(nchar(x) > 0)[1]]
  })
  # import codes conversion table
  code_data <- system.file("extdata", "habitat-codes.csv", package = "aoh")
  code_data <- data.table::fread(
    code_data, data.table = FALSE, sep = ",", sep2 = ""
  )
  code_data <- tibble::as_tibble(code_data)
  # convert to IUCN codes
  assertthat::assert_that(
    all(x %in% code_data$code),
    msg = "habitat classification code(s) not recognized"
  )
  code_data$iucn_code[match(x, code_data$code)]
}
