#' @include internal.R get_zenodo_file.R get_latest_zenodo_version.R
NULL

#' Download Zenodo data
#'
#' Download data from a specific version of a Zenodo repository.
#'
#' @param dir `character` folder path to store downloaded data.
#'
#' @inheritParams latest_zenodo_version
#'
#' @inheritParams get_jung_lvl2_habitat_data
#'
#' @return A `character` file path for the data.
#'
#' @noRd
get_zenodo_data <- function(x, file, version, dir = tempdir(),
                            force = FALSE, verbose = TRUE) {

  # assert valid argument
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x),
    assertthat::is.string(version),
    assertthat::noNA(version),
    assertthat::is.string(dir),
    assertthat::noNA(dir),
    assertthat::is.flag(force),
    assertthat::noNA(force),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )
  if (!is.function(file)) {
    assertthat::assert_that(
      assertthat::is.string(file),
      assertthat::noNA(file)
    )
  } else {
    assertthat::assert_that(is.function(file))
  }

  # normalize file paths
  dir <- normalize_path(dir, mustWork = FALSE)
  assertthat::assert_that(file.exists(dir))

  # find version if needed
  if (identical(version, "latest")) {
    ## verify if internet connection present
    if (!curl::has_internet()) {
      stop("no internet connection detected.") # nocov
    }
    ## find latest version
    version <- get_latest_zenodo_version(x = x, file = file)
  }

  # get data and return path
  get_zenodo_file(
    x = version,
    file = file,
    dir = dir,
    force = force,
    verbose = verbose
  )
}
