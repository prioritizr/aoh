#' @include internal.R convert_filename_to_habitat_code.R
NULL

#' Get preprocessed global habitat classification data
#'
#' Import habitat classification data produced by Jung *et al.* (2020a)
#' that have been preprocessed to reduce processing time for
#' generating Area of Habitat data.
#' If data are not available locally, they are downloaded from
#' an online repository (<https://github.com/prioritizr/aoh/releases/tag/data>).
#'
#' @inheritParams get_global_habitat_data
#'
#' @inherit get_global_habitat_data return references
#'
#' @noRd
get_prep_global_habitat_data <- function(dir = tempdir(),
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
    dir, paste0("prep-", gsub("/", "-", version, fixed = TRUE))
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
        dir, paste0("prep-", gsub("/", "-", version, fixed = TRUE))
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
    download_prep_habitat_data(
      x = version, dir = record_dir, verbose = verbose
    )
  }

  # import data
  import_prep_habitat_data(record_dir)
}

#' Download preprocessed habitat data
#'
#' Download preprocessed habitat data for a specific version of the
#' Jung *et al.* (2020) Zenodo repository.
#'
#' @inheritParams download_raw_habitat_data
#'
#' @inherit get_global_habitat_data references
#'
#' @return An invisible `TRUE` indicating success.
#'
#' @noRd
download_prep_habitat_data <- function(x, dir, verbose = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x),
    assertthat::is.string(dir),
    assertthat::noNA(dir)
  )
  # generate filename from DOI
  f <- paste0(
    gsub(".", "-", gsub("/", "_", x, fixed = TRUE), fixed = TRUE), ".zip"
  )
  # download data
  piggyback::pb_download(
    file = f,
    repo = "prioritizr/aoh",
    tag = "data",
    overwrite = TRUE,
    dest = dir,
    show_progress = verbose
  )
  # return success
  invisible(TRUE)
}

#' Import preprocessed habitat data
#'
#' Import preprocessed habitat data for a specific version of the
#' Jung *et al.* (2020) Zenodo repository.
#'
#' @inheritParams download_raw_habitat_data
#'
#' @inherit get_global_habitat_data references
#'
#' @return An invisible `TRUE` indicating success.
#'
#' @noRd
import_prep_habitat_data <- function(dir) {
  # assert valid arguments
  assertthat::assert_that(file.exists(dir))
  # extract the data to temporary archive
  path <- base::dir(dir, "^.*\\.zip", full.names = TRUE)
  # further validation arguments
  assertthat::assert_that(length(path) == 1)
  # unzip data
  path <- gsub("\\", "/", path, fixed = TRUE)
  temp_dir <- gsub("\\", "/", tempfile(), fixed = TRUE)
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(path, exdir = temp_dir)
  # find file paths for layers
  layer_paths <- base::dir(
    temp_dir, "^.*\\.tif", recursive = TRUE, full.names = TRUE
  )
  # import data
  r <- terra::rast(layer_paths)
  # assign names
  names(r) <- convert_jung_codes_to_iucn_codes(
    gsub(".tif", "", basename(layer_paths), fixed = TRUE)
  )
  # return result
  r
}
