#' @include internal.R
NULL

#' Get species data from the IUCN Red List API V3
#'
#' Import species data obtained from the [International Union
#' for Conservation of Nature (IUCN) Red List of Threatened
#' Species](https://www.iucnredlist.org/).
#' Please note that this version of the API has been deprecated and
#' so this function can only be used to import previously downloaded data.
#'
#' @inheritParams get_spp_habitat_data
#'
#' @param data_prefix `character` Prefix for saving data to disk for caching.
#'
#' @param data_template `data.frame` Table containing the names and correct
#'   classes (e.g., `numeric`, `character`, `logical`) for each column
#'   expected from calling the argument to `api_function` (see Examples below).
#'
#' @param progress_name `character` Name for progress bar when downloading
#'   data from IUCN Red List API.
#'   Defaults to "`querying"`.
#'
#' @inherit get_spp_habitat_data references return
#'
#' @seealso get_spp_api_v4_data
#'
#' @examples
#' \dontrun{
#' # define species to download data for based on taxon identifiers
#' spp_ids <- c(12392, 22694927)
#'
#' # define persistent storage location
#' download_dir <- system.file("testdata/api-v3", package = "aoh")
#'
#' # download and import habitat data
#' result <- get_spp_api_data(
#'   x = spp_ids,
#'   data_prefix = "summary",
#'   data_template = tibble::tibble(
#'     code = integer(0),
#'     habitat = character(0),
#'     suitability = character(0),
#'     season = character(0),
#'     majorimportance = character(0)
#'   ),
#'   dir = download_dir
#' )
#' }
#'
#' @examplesIf interactive()
#' \dontrun{
#' # preview data
#' print(result)
#' }
#' @noRd
get_spp_api_v3_data <- function(x, data_prefix, data_template,
                                dir = tempdir(), version = "latest") {
  # assert arguments are valid
  assertthat::assert_that(
    is.numeric(x),
    assertthat::noNA(x),
    length(x) > 0,
    assertthat::is.string(data_prefix),
    assertthat::noNA(data_prefix),
    inherits(data_template, "data.frame"),
    assertthat::is.string(dir),
    assertthat::is.string(version),
    assertthat::noNA(version)
  )
  ## assert suitable version
  assertthat::assert_that(
    !identical(version, "latest"),
    msg = paste(
      "`version` must be specified",
      "for cached API V3 data (e.g, `version = \"2024-1\"`)"
    )
  )

  # prepare x
  ## remove duplicates
  x <- unique(x)
  ## convert to integer
  assertthat::assert_that(
    all(abs(round(x) - x) <= 1e-5),
    msg = "`x` does not contain integer numbers"
  )
  x <- as.integer(x)

  # normalize file paths
  dir <- normalize_path(dir, mustWork = FALSE)
  assertthat::assert_that(assertthat::is.writeable(dir))

  # create file path for caching data
  file_path <- file.path(
    dir, paste0("iucn-red-list-", data_prefix, "-", version, ".csv.gz")
  )
  assertthat::assert_that(
    file.exists(file_path),
    msg = paste0(
      "can't find previously downloaded data for `version = \"", version,
      "\"` at the location `dir`"
    )
  )

  # access cached data
  iucn_rl_data <- readr::read_csv(
    file_path, col_names = TRUE, na = "", progress = FALSE,
    show_col_types = FALSE,
    col_types = readr::as.col_spec(
      dplyr::bind_cols(
        tibble::tibble(id_no = integer(nrow(data_template))),
        data_template
      )
    )
  )
  assertthat::assert_that(
    identical(names(iucn_rl_data), c("id_no", names(data_template))),
    msg = "issue loading cache data"
  )

  # check that all species are present in cache
  assertthat::assert_that(
    all(x %in% iucn_rl_data$id_no),
    msg = paste(
      "cached IUCN Red List dataset cannot be used because",
      "it is missing at least one of the requested species.",
      "As such, we recommend using the latest version of the IUCN Red List",
      "to ensure consistent data for all species."
    )
  )

  # re-order columns and return result
  iucn_rl_data[, c("id_no", names(data_template)), drop = FALSE]
}
