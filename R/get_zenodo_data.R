#' @include internal.R

#' Latest Zenodo version with data
#'
#' Find the Digital Object Identifier (DOI) for the latest version of
#' a Zenodo repository that contains a particular file.
#'
#' @param x `character` DOI value.
#'
#' @param file `character` file name for desired file.
#'  Alternatively, the argument can be a function that returns `TRUE` or
#'  `FALSE` if a `character` file name is detected.
#'
#' @inherit download_habitat_data references details
#'
#' @return A `character` value indicating the DOI.
#'
#' @noRd
latest_zenodo_version <- function(x, file) {
  # assert valid argument
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x)
  )
  if (!is.function(file)) {
    assertthat::assert_that(
      assertthat::is.string(file),
      assertthat::noNA(file)
    )
  } else {
    assertthat::assert_that(is.function(file))
  }

  # create Zenodo manager object
  z <- zen4R::ZenodoManager$new(logger = NULL)

  # find all versions
  invisible(utils::capture.output({
    all_versions <- suppressWarnings(z$getRecordByDOI(x)$getVersions())
  }))

  # find version with file present
  version <- NA_character_
  for (x in rev(all_versions$doi)) {
    ## find files present in repository
    f <- z$getRecordByDOI(x)$listFiles()$filename
    ## verify that has zip file following expected pattern
    if (inherits(file, "function")) {
      file_found <- any(vapply(f, file, logical(1)))
    } else {
      file_found <- any(f == file)
    }
    ## exit if file present
    if (file_found) {
      version <- x
      break;
    }
  }
  # verify that version was found
  assertthat::assert_that(
    !is.na(version),
    msg = "unable to find any versions with specified file"
  )

  # return result
  version
}

#' Download Zenodo data
#'
#' Download data from a specific version of a Zenodo repository.
#'
#' @param dir `character` folder to store downloaded data.
#'
#' @inheritParams latest_zenodo_version
#'
#' @inheritParams get_jung_lvl2_habitat_data
#'
#' @return A `character` file path for the downloaded data.
#'
#' @noRd
get_zenodo_data <- function(x, file, dir = tempdir(),
                            force = FALSE, verbose = TRUE) {
  # assert valid argument
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x),
    assertthat::is.string(dir),
    assertthat::noNA(dir),
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

  # process file path
  dir <- file.path(
    gsub("\\", "/", dir, fixed = TRUE),
    gsub(".", "-", gsub("/", "_", x, fixed = TRUE), fixed = TRUE)
  )
  if (!file.exists(dir)) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  }

  # exit if file already exists
  if (is.character(file)) {
    if (!isTRUE(force) && file.exists(file.path(dir, file))) {
      return(file.path(dir, file))
    }
  }

  # create Zenodo manager object
  z <- zen4R::ZenodoManager$new(logger = NULL)

  # find all files in record
  f <- z$getRecordByDOI(x)$listFiles()

  # find file to download
  if (inherits(file, "function")) {
    idx <- which(vapply(f$filename, file, logical(1)))
  } else {
    idx <- which(f$filename == file)
  }
  if (length(idx) == 0) {
    stop("argument to \"file\" not found in Zenodo repository")
  }

  # download data and return paths
  unname(vapply(idx, FUN.VALUE = character(1), function(i) {
    p <- file.path(dir, f$filename[i])
    if (isTRUE(force) || !file.exists(p)) {
      curl::curl_download(
        url = f$download[i],
        destfile = p,
        quiet = !isTRUE(verbose)
      )
    }
    p
  }))
}
