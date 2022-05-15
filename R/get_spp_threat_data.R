#' @include internal.R
NULL

#' Get species threat data
#'
#' Import species threat data obtained from the [International Union
#' for Conservation of Nature (IUCN) Red List of Threatened
#' Species](https://www.iucnredlist.org/).
#' Please note that a token is required to download
#' data from the [IUCN Red List API](https://apiv3.iucnredlist.org/)
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
  get_spp_api_data(
    x = x,
    api_function = rredlist::rl_threats,
    data_template =  tibble::tibble(
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
    version = version,
    key = key,
    delay = delay,
    force = force,
    progress_name = "querying",
    verbose = verbose
  )
}
