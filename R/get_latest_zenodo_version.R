#' @include internal.R  misc_zenodo.R
NULL

#' Get latest Zenodo version with data
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
#' @inherit download_habitat_data references details
#'
#' @return A `character` value indicating the DOI.
#'
#' @noRd
get_latest_zenodo_version <- function(x, file) {
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

  # append url to x
  x <- paste0("https://doi.org/", x)

  # find all versions
  ## note that versions are in order from [1] most recent to [n] most old
  all_versions <- get_doi_versions(x)

  # find version with file present
  version <- NA_character_
  for (x in rev(all_versions$doi)) {
    ## find files present in repository
    f <- get_doi_files(paste0("https://doi.org/", x))$filename

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
