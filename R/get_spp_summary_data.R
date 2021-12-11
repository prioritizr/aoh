#' @include internal.R
NULL

#' Get species summary data
#'
#' Import species summary data obtained from the
#' [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/).
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
#' @inheritSection aoh Accessing the IUCN Red List API
#'
#' @inherit get_spp_habitat_data references return
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
#' # download and import summary data
#' spp_summary_data <- get_spp_summary_data(spp_ids, download_dir)
#' }
#'
#' @examplesIf interactive()
#' \dontrun{
#' # preview data
#' print(result)
#' }
#' @export
get_spp_summary_data <- function(x, dir = tempdir(), version = "latest",
                                 key = NULL, delay = 2, force = FALSE,
                                 verbose = TRUE) {
  get_spp_api_data(
    x = x,
    api_function = rredlist::rl_search,
    data_template = tibble::tibble(
      taxonid = integer(0),
      scientific_name = character(0),
      kingdom = character(0),
      phylum = character(0),
      class = character(0),
      order = character(0),
      family = character(0),
      genus = character(0),
      main_common_name = character(0),
      authority = character(0),
      published_year = integer(0),
      assessment_date = character(0),
      category = character(0),
      criteria = character(0),
      population_trend = character(0),
      marine_system = logical(0),
      freshwater_system = logical(0),
      terrestrial_system = logical(0),
      assessor = character(0),
      reviewer = character(0),
      aoo_km2 = character(0),
      eoo_km2 = character(0),
      elevation_upper = double(0),
      elevation_lower = double(0),
      depth_upper = double(0),
      depth_lower = double(0),
      errata_flag = character(0),
      errata_reason = character(0),
      amended_flag = character(0),
      amended_reason = character(0)
    ),
    data_prefix = "summary",
    dir = dir,
    version = version,
    key = key,
    delay = delay,
    force = force,
    progress_name = "querying",
    verbose = verbose
  )
}
