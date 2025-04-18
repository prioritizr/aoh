#' @include internal.R
NULL

#' Get species summary data
#'
#' Import species summary data obtained from the
#' [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/).
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
#' result <- get_spp_summary_data(spp_ids, download_dir)
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
      data_format = format_summary_data,
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
        elevation_upper = double(0),
        elevation_lower = double(0),
        depth_upper = double(0),
        depth_lower = double(0)
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
      force = force
    )
  }

  # return result
  x
}

#' Format summary data
#'
#' Format summary data from an IUCN Red List assessment.
#'
#' @param x `list` object generated from [rredlist::rl_assessment()].
#'
#' @param id_no `integer` value with taxon identifier.
#'
#' @return A `data.frame` object.
#'
#' @noRd
format_summary_data <- function(x, id_no) {
  # assert valid arguments
  assertthat::assert_that(
    is.list(x),
    assertthat::is.number(id_no)
  )

  # initialize data frame
  d <- tibble::tibble(
    id_no = id_no,
    taxonid = {x$sis_taxon_id %||% NA_integer_}[[1]],
    scientific_name = NA_character_,
    kingdom = NA_character_,
    phylum = NA_character_,
    class = NA_character_,
    order = NA_character_,
    family = NA_character_,
    genus = NA_character_,
    main_common_name = NA_character_,
    authority = NA_character_,
    published_year = {x$year_published %||% NA_character_}[[1]],
    assessment_date = {x$assessment_date %||% NA_character_}[[1]],
    category = NA_character_,
    criteria = {x$criteria %||% NA_character_}[[1]],
    population_trend = NA_character_,
    marine_system = NA,
    freshwater_system = NA,
    terrestrial_system = NA,
    elevation_upper = NA_real_,
    elevation_lower = NA_real_,
    depth_upper = NA_real_,
    depth_lower = NA_real_
  )

  # process data
  if (assertthat::has_name(x, "taxon")) {
    ## taxonomic data
    d$scientific_name <- {x$taxon$scientific_name %||% NA_character_}[[1]]
    d$kingdom <- {x$taxon$kingdom_name %||% NA_character_}[[1]]
    d$phylum <- {x$taxon$phylum_name %||% NA_character_}[[1]]
    d$class <- {x$taxon$class_name %||% NA_character_}[[1]]
    d$order <- {x$taxon$order_name %||% NA_character_}[[1]]
    d$family <- {x$taxon$family_name %||% NA_character_}[[1]]
    d$genus <- {x$taxon$genus_name %||% NA_character_}[[1]]
    d$authority <- {x$taxon$authority %||% NA_character_}[[1]]
    ## add common name
    if (
      assertthat::has_name(x$taxon, "common_names") &&
      is.data.frame(x$taxon$common_names)
    ) {
      if (assertthat::has_name(x$taxon$common_names, "name")) {
        if (
          assertthat::has_name(x$taxon$common_names, "main") &&
          is.logical(x$taxon$common_names$main) &&
          any(x$taxon$common_names$main)
        ) {
          ## if main common name defined, then pick it
          d$main_common_name <-
            x$taxon$common_names$name[which(x$taxon$common_names$main)[[1]]]
        } else if (
          assertthat::has_name(x$taxon$common_names, "language") &&
          is.character(x$taxon$common_names$language) &&
          any(x$taxon$common_names$language == "eng")
        ) {
          ## else if english name, then pick it
          d$main_common_name <-
            x$taxon$common_names$name[
              which(x$taxon$common_names$language == "eng")[[1]]
            ]
        } else {
          ## else, then pick first name
          d$main_common_name <- x$taxon$common_names$name[[1]]
        }
      }
    }
    ## add IUCN red list category
    if (assertthat::has_name(x, "red_list_category")) {
      d$category <- x$red_list_category$code[[1]]
    }
    ## add population trend
    if (
      assertthat::has_name(x, "population_trend") &&
      assertthat::has_name(x$population_trend, "description") &&
      assertthat::has_name(x$population_trend$description, "en")
    ) {
      d$population_trend <- x$population_trend$description$en[[1]]
    }
    ## add supplementary information
    if (assertthat::has_name(x, "supplementary_info")) {
      d$elevation_upper <-
        x$supplementary_info$upper_elevation_limit[[1]]
      d$elevation_lower <-
        x$supplementary_info$lower_elevation_limit[[1]]
      d$depth_upper <-
        x$supplementary_info$upper_depth_limit[[1]]
      d$depth_lower <-
        x$supplementary_info$lower_depth_limit[[1]]
    }
    ## systems
    if (is.data.frame(x$systems)) {
      if (
        assertthat::has_name(x$systems, "description") &&
        assertthat::has_name(x$systems$description, "en")
    ) {
        d$marine_system <- any(
          grepl("marine", x$systems$description$en, ignore.case = TRUE)
        )
        d$freshwater_system <- any(
          grepl("fresh", x$systems$description$en, ignore.case = TRUE)
        )
        d$terrestrial_system <- any(
          grepl("terres", x$systems$description$en, ignore.case = TRUE)
        )
      }
    }
  } else {
    d <- tibble::tibble(id_no = id_no)
  }

  # return result
  d
}
