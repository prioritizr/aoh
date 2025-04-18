#' @include internal.R
NULL

#' Get species threat data
#'
#' Import species threat data obtained from the [International Union
#' for Conservation of Nature (IUCN) Red List of Threatened
#' Species](https://www.iucnredlist.org/).
#' Please note that a token is required to download
#' data from the [IUCN Red List API](https://api.iucnredlist.org/)
#' (see instructions below to obtain a token).
#'
#' @inheritParams get_spp_habitat_data
#'
#' @details
#' Data are downloaded from the IUCN Red List using the
#' [rredlist::rl_search()]. This function is essentially a wrapper
#' designed to help download data for multiple species and provide
#' caching for previously downloaded data.
#'
#' @details
#' Data are downloaded from the IUCN Red List using the
#' [rredlist::rl_threats()]. This function is essentially a wrapper
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
#' # download and import threat data
#' result <- get_spp_threat_data(spp_ids, download_dir)
#' }
#'
#' @examplesIf interactive()
#' \dontrun{
#' # preview data
#' print(result)
#' }
#' @export
get_spp_threat_data <- function(x, dir = tempdir(), version = "latest",
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

  # if version is 2025-1, then use new data format
  if (
    identical(version, "latest") ||
    package_version(version) >= package_version("2025-1")
  ) {
    x <- get_spp_api_v4_data(
      x = x,
      data_format = format_threat_data,
      data_template = tibble::tibble(
        code = character(0),
        title = character(0),
        timing = character(0),
        scope = character(0),
        severity = character(0),
        score = character(0),
        internationaltrade = character(0),
        ancestry = character(0),
        virus = character(0),
        ias = character(0),
        text = character(0)
      ),
      dir = dir,
      version = version,
      force = force
    )
  } else {
    # otherwise use old data format
    x <- get_spp_api_v3_data(
      x = x,
      data_template = tibble::tibble(
        code = character(0),
        title = character(0),
        timing = character(0),
        scope = character(0),
        severity = character(0),
        score = character(0),
        invasive = character(0)
      ),
      data_prefix = "threat",
      dir = dir,
      version = version
    )
  }

  # return result
  x
}

#' Format threat data
#'
#' Format threat data from an IUCN Red List assessment.
#'
#' @param x `list` object generated from [rredlist::rl_assessment()].
#'
#' @param id_no `integer` value with taxon identifier.
#'
#' @return A `data.frame` object.
#'
#' @noRd
format_threat_data <- function(x, id_no) {
  # assert valid arguments
  assertthat::assert_that(
    is.list(x),
    assertthat::is.number(id_no)
  )

  # process data
  if (is.data.frame(x$threats)) {
    ## convert to tibble
    x <- tibble::as_tibble(x$threats)
    ## fix column names
    names(x) <- tolower(names(x))
    ## extract threat names
    if (
      assertthat::has_name(x, "description") &&
      assertthat::has_name(x$description, "en")
    ) {
      x$title <- x$description$en
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
