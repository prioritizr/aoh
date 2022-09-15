#' @include internal.R misc_zenodo.R
NULL

#' Download Zenodo file
#'
#' Download a file from a specific version of a Zenodo repository.
#'
#' @param dir `character` folder path to store downloaded file.
#'
#' @inheritParams latest_zenodo_version
#'
#' @inheritParams get_jung_lvl2_habitat_data
#'
#' @return A `character` file path for the download.
#'
#' @noRd
get_zenodo_file <- function(x, file, dir = tempdir(),
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
      r <- vapply(basename(f), file, FUN.VALUE = logical(1))
      if (any(r)) {
        return(
          normalize_path(f[which(r)[[1]]], mustWork = FALSE)
        )
      }
    }
  }

  # append url to x
  x <- paste0("https://doi.org/", x)

  # find all files in record
  f <- get_doi_files(x)

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
    n <- f$filename[i]
    if (endsWith(n, "?download=1")) {
      n <- gsub("?download=1", "", n, fixed = TRUE)
    }
    p <- normalize_path(file.path(dir, basename(n)), mustWork = FALSE)
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
