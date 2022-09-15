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

  # find file container
  file_div <- rvest::html_elements(d, css = ".files-box")

  # find table in file container
  file_table <- rvest::html_element(file_div, "table")
  file_rows <- rvest::html_children(rvest::html_element(file_table, "tbody"))
  file_info <- rvest::html_element(file_rows, "a")

  # extract file details
  file_names <- rvest::html_text(file_info)
  file_urls <- paste0(
    "https://zenodo.org", rvest::html_attr(file_info, "href")
  )

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
  metadata_divs <- rvest::html_elements(d, css = ".metadata")

  # extract div containing version numbers
  is_version_div <- vapply(metadata_divs, FUN.VALUE = logical(1), function(x) {
    h <- rvest::html_elements(x, css = "h4")
    if (length(h) == 0) return(FALSE)
    h <- h[[1]]
    identical(rvest::html_text(h), "Versions")
  })

  # return input doi if it's not associated with any versions
  if (!any(is_version_div)) return(x)

  # extract div containing version numbers
  version_div <- metadata_divs[[which(is_version_div)[[1]]]]

  # extract version table
  version_table <- rvest::html_element(version_div, "table")
  version_rows <- rvest::html_children(version_table)

  # parse information for each version
  info <- lapply(version_rows, function(x) {
    tibble::tibble(
      version = trimws(gsub(
        "Version ", "", fixed = TRUE,
        rvest::html_text(rvest::html_element(x, "a"))
      )),
      created = as.POSIXct(
        trimws(rvest::html_text(
          rvest::html_element(rvest::html_children(x)[[2]], "small")
        )),
        format = "%b %e, %Y"
      ),
      doi = trimws(rvest::html_text(rvest::html_element(x, "small"))),
    )
  })

  # compile table
  info <- dplyr::bind_rows(info)

  ## return result (reverse row ordering for compatibility with zen4R
  info[rev(seq_len(nrow(info))), , drop = FALSE]
}
