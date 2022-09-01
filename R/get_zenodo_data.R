#' @include internal.R

#' Latest Zenodo version with data
#'
#' Find the Digital Object Identifier (DOI) for the latest version of
#' a Zenodo repository that contains a particular file.
#'
#' @param x `character` DOI value.
#'
#' @param file `character` File name for desired file.
#'  Alternatively, the argument can be a function that returns `TRUE` or
#'  `FALSE` if a `character` file name is detected.
#'
#' @param n_attempts `integer` Number of times to attempt to query
#'  the Zenodo API. This is useful because the Zenodo API sometimes
#'  randomly returns internal server errors for valid requests.
#'  Note that the function will wait for 5 seconds between subsequent
#'  attempts.
#'  Defaults to 5.
#'
#' @inherit download_habitat_data references details
#'
#' @return A `character` value indicating the DOI.
#'
#' @noRd
latest_zenodo_version <- function(x, file, n_attempts = 5) {
  # assert valid argument
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x),
    assertthat::is.count(n_attempts),
    assertthat::noNA(n_attempts)
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
    all_versions <- suppressWarnings(
      get_zenodo_record(x, z, n_attempts = n_attempts)$getVersions()
    )
  }))

  # find version with file present
  version <- NA_character_
  for (x in rev(all_versions$doi)) {
    ## find files present in repository
    f <- get_zenodo_record(x, z, n_attempts = n_attempts)$listFiles()$filename
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
get_zenodo_data <- function(x, file, dir = tempdir(), n_attempts = 5,
                            force = FALSE, verbose = TRUE) {
  # assert valid argument
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x),
    assertthat::is.string(dir),
    assertthat::noNA(dir),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose),
    assertthat::is.count(n_attempts),
    assertthat::noNA(n_attempts)
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
  dir <- normalize_path(dir, mustWork = FALSE)
  if (!file.exists(dir)) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  }

  # exit if file already exists
  if (!isTRUE(force)) {
    if (
      is.character(file) &&
      file.exists(normalize_path(file.path(dir, file), mustWork = FALSE))
    ) {
      return(normalize_path(file.path(dir, file), mustWork = FALSE))
    } else if (is.function(file)) {
      f <- sort(dir(dir, full.names = TRUE), decreasing = TRUE)
      r <- vapply(f, file, FUN.VALUE = logical(1))
      if (any(r)) {
        return(
          normalize_path(file.path(dir, f[which(r)[[1]]]), mustWork = FALSE)
        )
      }
    }
  }

  # create Zenodo manager object
  z <- zen4R::ZenodoManager$new(logger = NULL)

  # find all files in record
  f <- get_zenodo_record(x, z, n_attempts = n_attempts)$listFiles()

  # find file to download
  if (inherits(file, "function")) {
    idx <- which(vapply(f$filename, file, logical(1)))
  } else {
    idx <- which(f$filename == file)
  }
  if (length(idx) == 0) {
    stop("argument to \"file\" not found in Zenodo repository") # nocov
  }

  # download data and return paths
  unname(vapply(idx, FUN.VALUE = character(1), function(i) {
    p <- normalize_path(file.path(dir, f$filename[i]), mustWork = FALSE)
    if (isTRUE(force) || !file.exists(p)) {
      curl::curl_download(
        url = f$download[i],
        destfile = p,
        quiet = !isTRUE(verbose)
      )
    }
    normalize_path(p, mustWork = FALSE)
  }))
}

#' Get Zenodo Record
#'
#' Get metadata for a Zenodo record using the Zenodo API.
#'
#' @param x `character` DOI.
#'
#' @param z `ZenodoManager` Zenodo manager object.
#'   Defaults to `zen4R::ZenodoManager$new(logger = NULL)`.
#'
#' @inheritParams latest_zenodo_version
#'
#' @return The result from `zen4R::ZenodoManager$getRecordByDOI()`.
#'
#' @noRd
get_zenodo_record <- function(x, z = zen4R::ZenodoManager$new(logger = NULL),
                  n_attempts = 5) {
  # assert valid arguments
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x),
    inherits(z, "ZenodoManager"),
    assertthat::is.count(n_attempts),
    assertthat::noNA(n_attempts)
  )

  # attempt to query API
  r <- NULL
  for (i in seq_len(n_attempts)) {
    ## try to query result
    r <- try(z$getRecordByDOI(x), silent = TRUE)
    ## if success, then exit now
    if (!inherits(r, "try-error")) {
      break
    }
    # nocov start
    ## if the error is not due to internal server error, then exit too
    if (
      any(
        grepl("$ operator is invalid for atomic vectors", r[[1]], fixed = TRUE)
      )
    ) {
      stop(r[[1]])
    }
    ## otherwise, wait for 5 seconds and try again
    if (i < n_attempts) {
      Sys.sleep(5)
    }
    # nocov end
  }

  # if the result is an error after trying multiple times, then throw error
  if (inherits(r, "try-error")) {
    stop("failed to access Zenodo API") # nocov
  }

  # return result
  r
}
