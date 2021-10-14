#' @include internal.R
NULL

#' Get species habitat data
#'
#' Import species habitat preference data obtained from the [International Union
#' for Conservation of Nature (IUCN) Red List of Threatened
#' Species](https://www.iucnredlist.org/).
#' Please note that a token is required to download
#' data from the [IUCN Red List API](https://apiv3.iucnredlist.org/)
#' (see instructions below to obtain a token).
#'
#' @param x `integer` Taxon identifier for the species on the International
#'   Union for Conservation of Nature (IUCN) Red List of Threatened
#'   Species. For example, the taxon identifier for the species
#'   *Loxodonta africana* is `181008073`.
#'
#' @param dir `character` folder path where data should be downloaded.
#'  By default, data are downloaded to a temporary directory (i.e. `tempdir()`).
#'  **To avoid downloading the same data multiple times, it is strongly
#'  recommended to specify a persistent storage location (see Examples below).**
#'  If data are already available in folder for the specified version, then the
#'  data are imported and not re-downloaded from the IUCN Red List.
#'
#' @param version `character` indicating version of the IUCN Red List for
#'   obtaining data (e.g. `"2021-2"`).
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
#' @param delay `integer` number of seconds to wait between subsequent calls
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
#' spp_habitat_data <- get_spp_habitat_data(spp_ids, download_dir)
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
  get_spp_api_data(
    x = x,
    api_function = rredlist::rl_habitats,
    data_template =  tibble::tibble(
      code = character(0),
      habitat = character(0),
      suitability = character(0),
      season = character(0),
      majorimportance = character(0)
    ),
    data_prefix = "habitat",
    dir = dir,
    version = version,
    key = key,
    delay = delay,
    force = force,
    verbose = verbose
  )
}
