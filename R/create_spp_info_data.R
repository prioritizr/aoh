#' @include internal.R clean_spp_range_data.R
#' @include collate_spp_info_data.R
#' @include misc_terra.R misc_sf.R
NULL

#' Create species' information data
#'
#' Create data to collate all the information needed to generate
#' Area of Habitat data (Brooks *et al.* 2019).
#' Briefly, this function cleans and collates data describing species'
#' geographic ranges, habitat preferences, and elevational limits.
#' Please note that these procedures are designed for terrestrial species,
#' and will not apply to marine or freshwater species.
#'
#' @inheritParams get_spp_summary_data
#'
#' @param x [sf::sf()] Spatial data delineating species geographic ranges
#'   obtained from the [IUCN Red List](https://www.iucnredlist.org/).
#'   See below for details.
#'
#' @param cache_dir `character` Folder path for downloading and caching data.
#'  By default, a temporary directory is used (i.e., `tempdir()`).
#'  **To avoid downloading the same data multiple times, it is strongly
#'  recommended to specify a persistent storage location (see Examples below).**
#'
#' @param spp_summary_data [tibble::tibble()] Table containing summary
#'   information for each species (in the argument to `x`).
#'   Specifically, the argument should contain the following columns: `"id_no"`,
#'   `"elevation_lower"`, and `"elevation_upper"` columns.
#'   Here, `"id_no"` corresponds to the species' taxon identifier
#'   (also present in `x`), and the `"elevation_lower"` and `"elevation_upper"`
#'   columns indicate the lowest and highest elevations that contain habitat
#'   for the species.
#'   Defaults to `NULL` such that data are automatically obtained from the
#'   latest version of the [IUCN Red List](https://www.iucnredlist.org).
#'
#' @param spp_habitat_data [tibble::tibble()] Table containing habitat
#'   preference information for each species (in the argument to `x`).
#'   Specifically, the argument should contain the following columns: `"id_no"`,
#'   `"code"`, `"suitability"`, `"season"` columns.
#'   Here, `"id_no"` corresponds to the species' taxon identifier
#'   (also present in `x`), `"code"` indicates a habitat classification code
#'   that is suitable for the species (i.e., based on layer names in the
#'   argument to habitat_data), `"suitability"` indicates the level suitability
#'   of the habitat class for a given species (e.g., using values such
#'   as `"Suitable"` or `"Marginal"`), and `"season"` indicates
#'   if the habitat class is only suitable for a particular seasonal
#'   distribution (e.g., `"Breeding"`).
#'   Defaults to `NULL` such that data are automatically obtained from the
#'   latest version of the [IUCN Red List](https://www.iucnredlist.org).
#'
#' @param iucn_version  `character` Version of the
#'  IUCN Red List dataset that should be used. See documentation for the
#'  the `version` parameter in the [get_spp_summary_data()] function
#'  for further details.
#'  Defaults to `"latest"` such that the most recent version of the dataset is
#'  used.
#'
#' @param keep_iucn_rl_presence `integer` IUCN Red List presence codes
#'  to retain
#'  (see IUCN SSC Red List Technical Working Group 2021 for details).
#'  Species' ranges that are not associated with these codes are
#'  excluded during data cleaning procedures.
#'  Defaults to a numeric vector containing `1` and `2`
#'  (corresponding to *extant* and *probably extant*).
#'
#' @param keep_iucn_rl_origin `integer` IUCN Red List origin codes
#'  to retain
#'  (see IUCN SSC Red List Technical Working Group 2021 for details).
#'  Species' ranges that are not associated with these codes are
#'  excluded during data cleaning procedures.
#'  Defaults to a numeric vector containing `1`, `2`, and `6`.
#'  (corresponding to *native*, *reintroduced*, and *assisted colonization*).
#'
#' @param keep_iucn_rl_seasonal `integer` IUCN Red List seasonal codes
#'  to retain
#'  (see IUCN SSC Red List Technical Working Group 2021 for details).
#'  Species' ranges that are not associated with these codes are
#'  excluded during data cleaning procedures.
#'  Defaults to a numeric vector containing `1`, `2`, `3`, and `4`.
#'  (corresponding to *resident*, *breeding season*, *non-breeding season*,
#'  and *passage* distributions).
#'
#' @param omit_habitat_codes `character` Habitat classification codes
#'   to omit from resulting Area of Habitat data.
#'   Please see the [IUCN Red List Habitat Classification Scheme](
#'   https://www.iucnredlist.org/resources/habitat-classification-scheme)
#'   for the full range of habitat classification codes.
#'   For example,
#'   if the aim is to identify natural places that contain suitable conditions,
#'   then areas classified as anthropogenically modified
#'   ([iucn_habitat_codes_artificial()]),
#'   introduced vegetation ([iucn_habitat_codes_introduced()],
#'   or unknown habitat ([iucn_habitat_codes_misc()]) should
#'   be excluded.
#'   Defaults to [iucn_habitat_codes_marine()], such that marine
#'   habitats are excluded.
#'
#' @param crs A `st_crs()` object containing the coordinate reference system
#'  for reprojecting the species' range data.
#'  This coordinate reference system should be the same as the elevation
#'  and habitat classification data that will subsequently be used to
#'  generate Area of Habitat data (using the [create_spp_aoh_data()] or
#'  [create_spp_frc_data()] functions).
#'  Defaults to the World Behrmann coordinate reference system (ESRI:54017),
#'  which is appropriate for the elevation and habitat datasets
#'  available through the package
#'  (e.g., using the [get_global_elevation_data()] and
#'  [get_lumbierres_habitat_data()] functions).
#'
#' @param verbose `logical` Should progress be displayed while processing data?
#'  Defaults to `TRUE`.
#'
#' @section Species range data format:
#' Species range data are expected to follow the data format conventions
#' for the IUCN Red List (see [IUCN Red List
#' documentation](https://www.iucnredlist.org/resources/mappingstandards) for
#' details). Specifically, the argument to `x` should be an
#' [sf::st_sf()] object with the following columns: `id_no` (or `SISID`),
#' `presence`, `origin`, and `seasonal`.
#' It can also contain the following optional columns: `terrestrial`
#' (or `terrestial`), `freshwater`, and `marine`.
#' Below we provide a brief description of each column:
#'
#' \describe{
#'
#' \item{`id_no` (or `SISID`)}{`numeric` taxon identifier on the IUCN Red List.}
#'
#' \item{`presence`}{`numeric` identifier describing information about
#'   the presence of the taxon in the range data.}
#'
#' \item{`origin`}{`numeric`  identifier describing if the species is native
#'   to the location(s) described by the range data.}
#'
#' \item{`seasonality`}{`numeric` identifier describing if the species
#'  is occupied by the location(s) describe by the range data throughout
#'  the whole year, of if only during certain seasons.}
#'
#' \item{`terrestial`}{`character` value indicating if the range
#'  data pertain to terrestrial environments (with `"true"` or `"false"`
#'  values.)}
#'
#' \item{`freshwater`}{`character` value indicating if the range
#'  data pertain to freshwater environments (with `"true"` or `"false"`
#'  values.)}
#'
#' \item{`marine`}{`character` value indicating if the range
#'  data pertain to marine environments (with `"true"` or `"false"`
#'  values.)}
#'
#' }
#'
#' @section Data processing:
#' The species' information data are produced using the following procedures.
#'
#' 1. Species range data cleaned.
#' By default, this process involves excluding places where the
#' (i) species' presence is not _extant_ or
#' _probably extant_
#' (i.e. filtering based on `presence == 1` or `presence == 2`);
#' (ii) species' origin is not _native_,
#' _reintroduced_, or the result of _assisted colonization_
#' (i.e. filtering based on `origin == 1`, `origin == 2`, or `origin == 6`);
#' (iii) available information on which species' seasonal distribution
#' is _uncertain_ (i.e. filtering based on `seasonal != 5`); and
#' (iv) species' distribution is not terrestrial
#' (i.e. filtering based on where `terrestrial == "true"`).
#' Additionally, the species' range data are spatially dissolved so that each
#' seasonal distribution for each taxon is represented by a separate geometry.
#' Finally, geoprocessing routines are used to detect and repair
#' any invalid geometries.
#'
#' 2. Species summary and habitat preferences data are
#' imported (if needed, see [get_spp_summary_data()] and
#' [get_spp_habitat_data()] for details).
#' If these data are not available in the cache directory
#' (i.e. argument to `cache_dir`), then they are automatically downloaded
#' to the cache directory.
#'
#' 3. Species information are collated into a single dataset containing
#' their geographic ranges, habitat preferences, and elevational limits.
#' Specifically, taxon identifiers (per the `id_no`/`SISID` columns)
#' are used merge the datasets together.
#' If a species lacks lower or upper elevational limits,
#' then limits of 0 and 9,000 m are assumed respectively
#' (following Lumbierres *et al.* 2021).
#' Additionally, habitat preferences are only included in the dataset if they
#' are classified as *suitable* or *major*.
#' Migratory bird species that have *breeding* or *non-breeding*
#' distributions which lack habitat preference data are assigned
#' habitat preferences based on available data for *resident* distributions.
#' If a migratory bird species has a *resident* seasonal distribution
#' which lacks habitat preference data, then habitat preference data
#' are assigned based on available data for *breeding* distributions.
#'
#' 4. Post-processing routines are used to prepare the output data.
#'
#' @return
#' A [sf::st_sf()] object containing all the information needed
#' to create Area of Habitat data. It contains
#' cleaned versions of the species' range maps and columns containing
#' the species' elevational limits and habitat preferences.
#' Specifically, the object contains the following columns:
#' \describe{
#' \item{id_no}{`numeric` species' taxon identifier on the IUCN Red List.}
#' \item{binomial}{`character` species name.}
#' \item{seasonal}{`numeric` seasonal distribution code.}
#' \item{category}{`character` IUCN Red List threat category.}
#' \item{full_habitat_code}{`character` all habitat classification
#'   codes that contain suitable habitat for the species.
#'   If a given species has multiple suitable habitat classes,
#'   then these are denoted using a pipe-delimited format.
#'   For example, if the habitat classes denoted with the codes
#'   `"1.5"` and `"1.9"` were considered suitable for a given species, then
#'   these codes would be indicated as `"1.5|1.9"`.}
#' \item{elevation_lower}{`numeric` lower elevation threshold used to create
#'   the species' Area of Habitat data.}
#' \item{elevation_upper}{`numeric` upper elevation threshold used to create
#'   the species' Area of Habitat data.}
#' \item{geometry}{`sf::st_sfc()` geometries for species' distributions.}
#' }
#'
#' @references
#' Brooks TM, Pimm SL, Akçakaya HR, Buchanan GM, Butchart SHM, Foden W,
#' Hilton-Taylor C, Hoffmann M, Jenkins CN, Joppa L, Li BV, Menon V,
#' Ocampo-Peñuela N, Rondinini C (2019) Measuring terrestrial Area of Habitat
#' (AOH) and its Utility for the IUCN Red List. *Trends in Ecology & Evolution*,
#' 34:977--986. Available at <https://doi.org/10.1016/j.tree.2019.06.009>.
#'
#' @examples
#' \dontrun{
#' # find file path for example range data following IUCN Red List data format
#' ## N.B. the range data were not obtained from the IUCN Red List,
#' ## and were instead based on data from GBIF (https://www.gbif.org/)
#' path <- system.file("extdata", "EXAMPLE_SPECIES.zip", package = "aoh")
#'
#' # import data
#' spp_range_data <- read_spp_range_data(path)
#'
#' # specify persistent storage location for data processing
#' cache_dir <- rappdirs::user_data_dir("aoh")
#'
#' # create cache directory if needed
#' if (!file.exists(cache_dir)) {
#'   dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
#' }
#'
#' # create species information data
#' spp_info_data <- create_spp_info_data(
#'   x = spp_range_data,
#'   cache_dir = cache_dir
#' )
#' }
#'
#' @examplesIf interactive()
#' \dontrun{
#' # preview data
#' print(spp_info_data)
#' }
#'
#' @export
create_spp_info_data <- function(x,
                                spp_summary_data = NULL,
                                spp_habitat_data = NULL,
                                cache_dir = tempdir(),
                                iucn_version = "latest",
                                key = NULL,
                                force = FALSE,
                                keep_iucn_rl_presence = c(1, 2),
                                keep_iucn_rl_origin = c(1, 2, 6),
                                keep_iucn_rl_seasonal = c(1, 2, 3, 4),
                                omit_habitat_codes =
                                  iucn_habitat_codes_marine(),
                                crs = sf::st_crs("ESRI:54017"),
                                verbose = TRUE) {
  # initialization
  ## display message
  if (verbose) {
    cli::cli_progress_step("initializing")
  }
  ## initial validation
  assertthat::assert_that(
    inherits(x, "sf"),
    assertthat::is.writeable(cache_dir),
    assertthat::is.flag(force),
    assertthat::noNA(force),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose),
    inherits(crs, "crs")
  )
  assertthat::assert_that(
    !sf::st_is_longlat(crs),
    msg = "argument to \"crs\" must not correspond to longitudes/latitudes"
  )
  assertthat::assert_that(
    crs != sf::st_crs(NA),
    msg = "argument to \"crs\" must be properly defined"
  )
  assertthat::assert_that(
    assertthat::has_name(x, "id_no") ||
      assertthat::has_name(x, "SISID"),
    msg = paste0(
      "argument to \"x\" does not have a column named \"id_no\" or \"SISID\""
    )
  )
  # verify access to IUCN Red List API
  if (is.null(spp_summary_data) && is.null(spp_habitat_data)) {
    assertthat::assert_that(
      is_iucn_rl_api_available(),
      msg = "can't access the IUCN Red List API, see ?aoh"
    )
  }

  # clean species range data
  ## display message
  if (verbose) {
    cli::cli_progress_step("cleaning species range data")
  }
  ## processing
  x <- clean_spp_range_data(
    x = x,
    keep_iucn_rl_presence = keep_iucn_rl_presence,
    keep_iucn_rl_origin = keep_iucn_rl_origin,
    keep_iucn_rl_seasonal = keep_iucn_rl_seasonal,
    crs = crs
  )
  ## addition data validation
  assertthat::assert_that(
    nrow(x) > 0,
    msg = "argument to x does not contain any terrestrial species"
  )
  assertthat::assert_that(
    identical(anyDuplicated(paste0(x$id_no, x$seasonal)), 0L),
    msg = paste(
      "failed to combine multiple geometries for a species'",
      "seasonal distribution"
    )
  )
  ## clean up
  invisible(gc())

  # get species summary data
  if (is.null(spp_summary_data)) {
    #### display message
    if (verbose) {
      cli::cli_progress_step("importing species summary data")
    }
    #### processing
    spp_summary_data <- get_spp_summary_data(
      x$id_no, dir = cache_dir, version = iucn_version, key = key,
      force = force, verbose = verbose
    )
  }

  # get species habitat data
  if (is.null(spp_habitat_data)) {
    #### display message
    if (verbose) {
      cli::cli_progress_step("importing species habitat data")
    }
    #### processing
    spp_habitat_data <- get_spp_habitat_data(
      unique(x$id_no), dir = cache_dir, version = iucn_version, key = key,
      force = force, verbose = verbose
    )
  }

  # format species data
  ## display message
  if (verbose) {
    cli::cli_progress_step("collating species data")
  }
  ## main processing
  x <- collate_spp_info_data(
    x = x,
    spp_summary_data = spp_summary_data,
    spp_habitat_data = spp_habitat_data,
    omit_habitat_codes = omit_habitat_codes,
    verbose = verbose
  )
  ## clean up
  invisible(gc())

  # prepare result
  ## display message
  if (verbose) {
    cli::cli_progress_step("post-processing results")
  }
  ## processing
  x <- dplyr::select(
    x, .data$id_no, .data$binomial, .data$seasonal, .data$category,
    .data$full_habitat_code,
    .data$elevation_lower, .data$elevation_upper,
  )

  # return result
  ## display message
  if (verbose) {
    cli::cli_progress_done()
    cli::cli_alert_success("finished")
  }
  ## return
  x
}