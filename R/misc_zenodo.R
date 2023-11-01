#' @include internal.R
NULL

#' Get files associated with DOI
#'
#' Get information on the files associated with a DOI.
#'
#' @param x `character` Value containing a DOI.
#'
#' @return
#' A `tibble::tibble()` containing the filename and download URL
#' for each file associated with the DOI. Note that rows are ordered
#' such that the first row corresponds to the oldest record.
#'
#' @examples
#' get_doi_files("https://doi.org/10.5281/zenodo.6622038")
#'
#' @noRd
get_doi_files <- function(x) {
  # assert valid argument
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x)
  )
  assertthat::assert_that(
    startsWith(x, "https://doi.org/"),
    msg = "argument to x is not a DOI."
  )
  assertthat::assert_that(
    grepl("/zenodo.", x, fixed = TRUE),
    msg = "argument to x is not a Zenodo DOI."
  )

  # scrape html file
  d <- rvest::read_html(x)

  # extract metedata
  md <- rvest::html_elements(d, css = "#recordVersions")
  md <- jsonlite::fromJSON(rvest::html_attr(md, "data-record"))

  # extract file details
  file_names <- names(md$files$entries)
  file_urls <- paste0(md$links$files, "/", file_names, "?download=1")
  file_urls <- gsub("/api/", "/", file_urls, fixed = TRUE)

  # return result
  tibble::tibble(filename = file_names, download = file_urls)
}

#' Get DOI versions
#'
#' Get all the DOI versions for associated with a given DOI.
#'
#' @param x `character` Value containing a DOI.
#'
#' @return A `character` vector with DOIs.
#'
#' @examples
#' get_doi_versions("https://doi.org/10.5281/zenodo.6622038")
#'
#' @noRd
get_doi_versions <- function(x) {
  # assert valid argument
  assertthat::assert_that(
    assertthat::is.string(x),
    assertthat::noNA(x)
  )
  assertthat::assert_that(
    startsWith(x, "https://doi.org/"),
    msg = "argument to x is not a DOI."
  )
  assertthat::assert_that(
    grepl("/zenodo.", x, fixed = TRUE),
    msg = "argument to x is not a Zenodo DOI."
  )

  # scrape html file
  d <- rvest::read_html(x)

  # find metadata containers
  md <- rvest::html_elements(d, css = "#recordVersions")
  md <- jsonlite::fromJSON(rvest::html_attr(md, "data-record"))
  md <- rvest::read_html(md$links$versions)
  md <- jsonlite::fromJSON(rvest::html_text(md), "p")$hits[[1]]

  # extract metadata
  version <- vapply(md, FUN.VALUE = character(1), function(x) {
    if (is.null(x$metadata)) return(NA_character_)
    if (is.null(x$metadata$version)) return(NA_character_)
    trimws(x$metadata$version)
  })
  created <- vapply(md, FUN.VALUE = character(1), function(x) {
    if (is.null(x$metadata)) return(NA_character_)
    if (is.null(x$metadata$publication_date)) return(NA_character_)
    trimws(x$metadata$publication_date)
  })
  doi <- vapply(md, FUN.VALUE = character(1), function(x) {
    if (is.null(x$metadata)) return(NA_character_)
    if (is.null(x$metadata$doi)) return(NA_character_)
    trimws(x$metadata$doi)
  })

  # create table with metadata
  info <- tibble::tibble(
    version = version,
    created = as.POSIXct(created, format = "%Y-%m-%d"),
    doi = doi
  )

  # return result (reverse row ordering for compatibility with zen4R
  info[rev(seq_len(nrow(info))), , drop = FALSE]
}
