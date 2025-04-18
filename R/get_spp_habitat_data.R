#' @include internal.R
NULL

#' Get species habitat data
#'
#' Import species habitat preference data obtained from the [International Union
#' for Conservation of Nature (IUCN) Red List of Threatened
#' Species](https://www.iucnredlist.org/).
#' Please note that a token is required to download
#' data from the [IUCN Red List API](https://api.iucnredlist.org/)
#' (see instructions below to obtain a token).
#'
#' @param x `integer` Taxon identifier for the species on the International
#'   Union for Conservation of Nature (IUCN) Red List of Threatened
#'   Species. For example, the taxon identifier for the species
#'   *Loxodonta africana* is `181008073`.
#'
#' @param dir `character` Folder path where data should be downloaded.
#'  By default, data are downloaded to a temporary directory
#'  (i.e., `tempdir()`). **To avoid downloading the same data multiple times,
#'  it is strongly
#'  recommended to specify a persistent storage location (see Examples below).**
#'  If data are already available in folder for the specified version, then the
#'  data are imported and not re-downloaded from the IUCN Red List.
#'
#' @param version `character` Value indicating version of the IUCN Red List for
#'   obtaining data (e.g., `"2021-2"`).
#'   Although it is not possible to query past versions
#'   of the IUCN Red List, this functionality is useful for accessing
#'   data previously downloaded from the IUCN Red List.
#'   Defaults to `"latest"` such that data are downloaded from the most recent
#'   version of the IUCN Red List.
#'
#' @param key `character` Token for querying the IUCN Red List API.
#'   Defaults to `NULL` such that the token
#'   accessed from  the `"IUCN_REDLIST_KEY"` environmental variable
#'   (which can be specified in the .Renviron file).
#
#' @param delay `integer` Number of seconds to wait between subsequent calls
#'   to the IUCN Red List API. Defaults to 2 seconds (as recommended by the
#'   \pkg{rredlist} package;
#'   <https://docs.ropensci.org/rredlist/articles/rredlist.html>).
#'
#' @inheritParams get_global_elevation_data
#'
#' @details
#' Data are downloaded from the IUCN Red List using the
#' [rredlist::rl_habitats()]. This function is essentially a wrapper
#' designed to help download data for multiple species and provide
#' caching for previously downloaded data.
#'
#' @inheritSection aoh Accessing the IUCN Red List API
#'
#' @return A table ([tibble::tibble()]) object.
#'
#' @references
#' Please use the following citation for data obtained from the IUCN Red List:
#'
#' IUCN (insert year). IUCN Red List of Threatened Species. Version
#'  (insert version). Available at <www.iucnredlist.org>.
#'
#' To obtain the version number of the latest version, use
#' [rredlist::rl_version()].
#'
#' @examples
#' \dontrun{
#' # define species to download data for based on taxon identifiers
#' spp_ids <- c(18, 22694927)
#'
#' # define persistent storage location
#' download_dir <- rappdirs::user_data_dir("aoh")
#'
#' # create download directory if needed
#' if (!file.exists(download_dir)) {
#'   dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
#' }
#'
#' # download and import habitat preference data
#' result <- get_spp_habitat_data(spp_ids, download_dir)
#' }
#'
#' @examplesIf interactive()
#' \dontrun{
#' # preview data
#' print(result)
#' }
#' @export
get_spp_habitat_data <- function(x, dir = tempdir(), version = "latest",
                                 key = NULL, delay = 2, force = FALSE,
                                 verbose = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    is.numeric(x),
    assertthat::noNA(x),
    length(x) > 0,
    assertthat::is.string(dir),
    assertthat::is.string(version),
    assertthat::noNA(version),
    inherits(key, c("NULL", "character")),
    assertthat::is.number(delay),
    assertthat::noNA(delay),
    assertthat::is.flag(force),
    assertthat::noNA(force),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )

  # define template
  data_template <- tibble::tibble(
    code = character(0),
    habitat = character(0),
    suitability = character(0),
    season = character(0),
    majorimportance = character(0)
  )

  # if version is 2025-1, then use new data format
  if (
    identical(version, "latest") ||
    package_version(version) >= package_version("2025-1")
  ) {
    x <- get_spp_api_v4_data(
      x = x,
      data_format = format_habitat_data,
      data_template = data_template,
      dir = dir,
      version = version,
      force = force
    )
  } else {
    # otherwise use old data format
    x <- get_spp_api_v3_data(
      x = x,
      data_template = data_template,
      data_prefix = "habitat",
      dir = dir,
      version = version,
      force = force
    )
  }

  # return result
  x
}

#' Format habitat data
#'
#' Format habitat data from an IUCN Red List assessment.
#'
#' @param x `list` object generated from [rredlist::rl_assessment()].
#'
#' @param id_no `integer` value with taxon identifier.
#'
#' @return A `data.frame` object.
#'
#' @noRd
format_habitat_data <- function(x, id_no) {
  # assert valid arguments
  assertthat::assert_that(
    is.list(x),
    assertthat::is.number(id_no)
  )

  # process data
  if (is.data.frame(x$habitats)) {
    ## convert to tibble
    x <- tibble::as_tibble(x$habitats)
    ## fix column names
    names(x) <- tolower(names(x))
    ## extract habitat names
    if (
      assertthat::has_name(x, "description") &&
      is.data.frame(x$description) &&
      assertthat::has_name(x$description, "en")
    ) {
      x$habitat <- x$description$en
      x$description <- NULL
    }
    ## convert code to numeric values
    if (
      assertthat::has_name(x, "code") &&
      is.character(x$code)
    ) {
      x$code <- gsub("_", ".", x$code, fixed = TRUE)
    }
    ## add in id_no
    x$id_no <- id_no
  } else {
    x <- tibble::tibble(id_no = id_no)
  }

  # return result
  x
}
