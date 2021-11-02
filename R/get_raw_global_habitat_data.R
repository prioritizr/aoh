#' @include internal.R convert_filename_to_habitat_code.R
NULL

#' Get raw global habitat classification data
#'
#' Import habitat classification data produced by Jung *et al.* (2020a).
#' If data are not available locally, they are downloaded from
#' an online repository (Jung *et al.* 2020b).
#'
#' @inheritParams get_global_habitat_data
#'
#' @inherit get_global_habitat_data return references
#'
#' @noRd
get_raw_global_habitat_data <- function(dir = tempdir(),
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
  if (!curl::has_internet()) {
    stop("no internet connection detected.")
  }

  # define data DOI
  doi <- jung_et_al_2020_habitat_data_doi

  # create file path for dataset based on version
  record_dir <- file.path(
    dir, paste0("raw-", gsub("/", "-", version, fixed = TRUE))
  )

  # see if version is already available
  if (!file.exists(record_dir) || identical(version, "latest")) {
    ## find all available version
    z <- zen4R::ZenodoManager$new(logger = NULL)
    ## find specified version of dataset
    if (identical(version, "latest")) {
      ## find latest version DOI
      version <- latest_version_habitat_data(x = doi, z = z)
      ## update directory path for storing data
      record_dir <- file.path(
        dir, paste0("raw-", gsub("/", "-", version, fixed = TRUE))
      )
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
  if (!file.exists(record_dir) ||
      isTRUE(length(base::dir(record_dir, "^.*\\.zip$")) == 0) ||
      isTRUE(force)
    ) {
    dir.create(record_dir, showWarnings = FALSE, recursive = TRUE)
    download_raw_habitat_data(
      x = version, dir = record_dir, z = z, verbose = verbose
    )
  }

  # import data
  import_raw_habitat_data(record_dir)
}

#' Download raw habitat data
#'
#' Download raw habitat data from a specific version of the Jung *et al.* (2020)
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
download_raw_habitat_data <- function(x, dir, z = zen4R::ZenodoManager$new(),
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

#' Import raw habitat data
#'
#' Import raw habitat data from a specific version of the Jung *et al.* (2020)
#' Zenodo repository.
#'
#' @param dir `character` folder with data.
#'
#' @inheritParams get_global_habitat_data
#'
#' @inherit get_global_habitat_data references
#'
#' @return A [terra::rast()] raster dataset.
#'
#' @noRd
import_raw_habitat_data <- function(dir) {
  # assert valid arguments
  assertthat::assert_that(file.exists(dir))
  # extract the data to temporary archive
  path <- base::dir(dir, "^.*\\.zip", full.names = TRUE)
  # further validation arguments
  assertthat::assert_that(length(path) == 1)
  # unzip data
  path <- gsub("\\", "/", path, fixed = TRUE)
  temp_dir <- tempfile()
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(path, exdir = temp_dir)
  # find file paths for layers
  raw_layer_paths <- base::dir(
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
      break;
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
