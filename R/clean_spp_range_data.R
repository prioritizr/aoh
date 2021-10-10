#' @include internal.R
NULL

#' Clean species range data
#'
#' Clean species geographic range (i.e. extent of occurrence) data obtained
#' from the
#' [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/).
#'
#' @param x A [sf::sf()] object containing species geographic range data.
#'
#' @param crs `character or `integer` object representing a coordinate
#'   reference system. Defaults to World Behrmann (ESRI:54017).
#'
#' @param snap_tolerance `numeric` tolerance for snapping geometry to a grid
#'   for resolving invalid geometries. Defaults to 1 meter.
#'
#' @param geometry_precision `numeric` level of precision for processing the
#'   spatial data (used with [sf::st_set_precision()]). The default is
#'   1500 (higher values indicate higher precision). This level of precision is
#'   generally suitable for analyses at the national-scale. For analyses at
#'   finer-scale resolutions, consider using a greater value (e.g.
#'   10000).
#'
#' @details
#' This function applies the following data cleaning procedures:
#'
#' \enumerate{
#'
#' \item Column names are standardized. This involves converting them to lower
#'   case characters and fixing any spelling mistakes.
#'
#' \item Places where the species' presence is not "known or thought very likely
#'    to occur currently" are excluded (i.e. filtering based on
#'    `presence == 1`).
#'
#' \item Places where the species' presence is not "native to the area"
#'    are excluded (i.e. filtering based on `origin == 1`).
#'
#' \item Places where the species' seasonal occurrence is uncertain are excluded
#'    (i.e. filtering based on `seasonal != 5`).
#'
#' \item Species that are not terrestrial are excluded (i.e. filtering based on
#'    where `terrestrial == "true"`, `freshwater == "false"`, and
#'    `marine == "false"`).
#'
#' \item Fix any potential geometry issues (using [sf::st_make_valid()]).
#'
#' \item Wrap geometries to date line (using [sf::st_wrap_dateline()]).
#'
#' \item Fix any potential geometry issues (i.e. using [sf::st_make_valid()]).
#'
#' \item Reproject data to specified coordinate system
#'    (i.e. using [sf::st_transform()]).
#'
#' \item Fix any potential geometry issues (i.e. using [sf::st_make_valid()]).
#'
#' \item Snap geometries to spatial grid (using [lwgeom::st_snap_to_grid()]).
#'   Note that this step is only performed if the argument to `snap_tolerance`
#'   is greater than zero.
#'
#' \item Fix any potential geometry issues (using [sf::st_make_valid()]).
#'
#' \item Data are spatially dissolved so that each taxon is represented by
#'   a separate geometry.
#'
#' \item Fix any potential geometry issues (i.e. using [sf::st_make_valid()]).
#'
#' }
#'
#' For more information on criteria used in steps 2 -- 4, please refer to
#' IUCN SSC Red List Technical Working Group (2021).
#'
#' @return A [sf::sf()] object containing the dataset.
#'
#' @references
#' IUCN SSC Red List Technical Working Group (2021)
#' Mapping Standards and Data Quality for the IUCN Red List Spatial Data
#' (Version 1.19). International Union for Conservation of Nature (IUCN).
#' <https://nc.iucnredlist.org/redlist/resources/files/1539098236-Mapping_Standards_Version_1.16_2018.pdf>
#'
#' @examples
#' \dontrun{
#' # find file path for simulated data following the IUCN Red List format
#' path <- system.file("extdata", "SIMULATED_SPECIES.zip", package = "aoh")
#'
#' # import data
#' sim_spp_range_data <- read_spp_range_data(path)
#'
#' # clean data
#' sim_spp_range_data <- clean_spp_range_data(sim_spp_range_data)
#'
#' # preview data (only if running R in an interactive session)
#' if (interactive()) {
#'   print(sim_spp_range_data)
#' }
#'
#' # plot data (only if running R in an interactive session)
#' if (interactive()) {
#'   print(sim_spp_range_data)
#' }
#' }
#' @export
clean_spp_range_data <- function(x, crs = sf::st_crs("ESRI:54017"),
                                 snap_tolerance = 1,
                                 geometry_precision = 1500) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "sf"),
    nrow(x) > 0,
    has_iucn_format_column_names(x),
    inherits(crs, "crs")
  )
  assertthat::assert_that(
    is.numeric(x$id_no),
    is.numeric(x$presence),
    is.numeric(x$origin),
    is.numeric(x$seasonal),
    is.character(x$freshwater)
  )
  assertthat::assert_that(
    all(x$freshwater[!is.na(x$freshwater)] %in% c("true", "false")),
    msg = "freshwater column should contain \"true\" or \"false\" values"
  )
  assertthat::assert_that(
    all(x$marine[!is.na(x$freshwater)] %in% c("true", "false")),
    msg = "marine column should contain \"true\" or \"false\" values"
  )
  assertthat::assert_that(
    all(x$terrestial[!is.na(x$freshwater)] %in% c("true", "false")),
    msg = "terrestial column should contain \"true\" or \"false\" values"
  )

  # step 1: format column names
  x <- dplyr::rename_all(x, tolower)
  if ("terrestial" %in% names(x)) {
    x <- dplyr::rename(x, terrestrial = terrestial)
  }
  if ("order_" %in% names(x)) {
    x <- dplyr::rename(x, order = order_)
  }
  if (!"freshwater" %in% names(x)) {
    x$freshwater <- "false"
  }
  if (!"marine" %in% names(x)) {
    x$marine <- "false"
  }
  if (!"terrestrial" %in% names(x)) {
    x$terrestrial <- "true"
  }
  # step 2: exclude uncertain presence
  x <- x[which(x$presence == 1), , drop = FALSE]
  # step 3: exclude non-native origin
  x <- x[which(x$origin == 1), , drop = FALSE]
  # step 4: exclude uncertain seasonality
  x <- x[which(x$seasonal != 5), , drop = FALSE]
  # step 5: exclude non-terrestrial distributions
  idx <- which(
    x$terrestrial == "true" &
    x$marine == "false" &
    x$freshwater == "false"
  )
  x <- x[idx, , drop = FALSE]
  # step 6: fix any potential geometry issues
  x <- sf::st_set_precision(x, geometry_precision)
  x <- sf::st_make_valid(x)
  x <- dplyr::filter(x, !sf::st_is_empty(x))
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  # step 7: wrap geometries to dateline
  x <- sf::st_set_precision(x, geometry_precision)
  x <- suppressWarnings(sf::st_wrap_dateline(x,
    options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")))
  # step 8: fix any potential geometry issues
  x <- sf::st_set_precision(x, geometry_precision)
  x <- sf::st_make_valid(x)
  x <- dplyr::filter(x, !sf::st_is_empty(x))
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  # step 9: reproject data
  x <- sf::st_set_precision(x, geometry_precision)
  x <- sf::st_transform(x, crs)
  # step 10: fix any potential geometry issues
  x <- sf::st_set_precision(x, geometry_precision)
  x <- sf::st_make_valid(x)
  x <- dplyr::filter(x, !sf::st_is_empty(x))
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  # step 11: snap geometries to grid
  if (snap_tolerance > 0) {
    x <- sf::st_set_precision(x, geometry_precision)
    x <- lwgeom::st_snap_to_grid(x, snap_tolerance)
  }
  # step 12: fix any potential geometry issues
  x <- sf::st_set_precision(x, geometry_precision)
  x <- sf::st_make_valid(x)
  x <- dplyr::filter(x, !sf::st_is_empty(x))
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  # step 13: dissolve geometries by species, subspecies, seasonal
  ## create id
  if (is.character(x$seasonal)) {
    x$seasonal <- convert_to_seasonal_id(x$seasonal)
  }
  x$aoh_id <- withr::with_options(
    list(scipen = 1000),
    paste0("AOH_", x$id_no, "_", x$seasonal)
  )
  ## geoprocessing
  x <-
    x %>%
    dplyr::group_by(aoh_id) %>%
    dplyr::summarize(
      id_no = dplyr::first(id_no),
      category = dplyr::first(category),
      binomial = dplyr::first(binomial),
      subspecies = dplyr::first(subspecies),
      seasonal = dplyr::first(seasonal),
      kingdom = dplyr::first(kingdom),
      phylum = dplyr::first(phylum),
      class = dplyr::first(class),
      order = dplyr::first(order),
      genus = dplyr::first(genus)
    ) %>%
    dplyr::ungroup()
  # step 14: fix any potential geometry issues
  x <- sf::st_set_precision(x, geometry_precision)
  x <- sf::st_make_valid(x)
  x <- dplyr::filter(x, !sf::st_is_empty(x))
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  # return result
  x
}
