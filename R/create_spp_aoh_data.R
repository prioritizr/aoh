#' @include internal.R clean_spp_range_data.R
#' @include get_global_elevation_data.R get_global_habitat_data.R
#' @include get_spp_habitat_data.R get_spp_summary_data.R
#' @include misc_terra.R misc_sf.R
NULL

#' Create Area of Habitat data
#'
#' Create Area of Habitat (AOH) data for species based on their altitudinal and
#' habitat preferences (Brooks *et al.* 2019).
#' Please note that these procedures are designed for terrestrial species
#' and will not apply to marine or freshwater species.
#'
#' @inheritParams get_global_habitat_data
#' @inheritParams get_spp_summary_data
#'
#' @param x [sf::sf()] Spatial data delineating species geographic ranges
#'   obtained from the [IUCN Red List](https://www.iucnredlist.org/).
#'   See below for details.
#'
#' @param output_dir `character` folder path to save raster files
#'   (GeoTIFF format) containing the Area of Habitat data.
#'
#' @param cache_dir `character` folder path for downloading and caching data.
#'  By default, a temporary directory is used (i.e. `tempdir()`).
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
#'   that is suitable for the species (i.e. based on layer names in the
#'   argument to habitat_data), `"suitability"` indicates the level suitability
#'   of the habitat class for a given species (e.g. using values such
#'   as `"Suitable"` or `"Marginal"`), and `"season"` indicates
#'   if the habitat class is only suitable for a particular seasonal
#'   distribution (e.g. `"Breeding"`).
#'   Defaults to `NULL` such that data are automatically obtained from the
#'   latest version of the [IUCN Red List](https://www.iucnredlist.org).
#'
#' @param elevation_data [terra::rast()] Raster data delineating the
#'   worldwide elevation data (e.g. Robinson *et al.* 2014).
#'   Defaults to `NULL` such that data
#'   are automatically obtained (using [get_global_elevation_data()]).
#'   If the data are obtained automatically, then a preprocessed version
#'   of the habitat data will be used to reduce processing time.
#'
#' @param habitat_data [terra::rast()] Raster data indicating the
#'   presence of different habitat classes across world
#'   (e.g. Jung *et al.* 2020a,b).
#'   Each pixel should contain an `integer` value that specifies which
#'   habitat class is present within the pixel
#'   (based on the argument to `crosswalk_data`).
#'   Defaults to `NULL` such that data are automatically obtained (using
#'   [get_global_habitat_data()]).
#'
#' @param crosswalk_data [data.frame()] Table containing data that indicate
#'   which pixel values in the argument to `habitat_data` correspond to which
#'   IUCN habitat classification codes. The argument should contain
#'   a `code` column that specifies a set of IUCN habitat classification
#'   codes (see [iucn_habitat_data()], and a `value` column that specifies
#'   different values in the argument to `habitat_data`.
#'   Defaults to `NULL` such that the crosswalk for the default habitat
#'   data are used (i.e. [crosswalk_jung_data()]).
#'
#' @param iucn_version  `character` Version of the
#'  IUCN Red List dataset that should be used. See documentation for the
#'  the `version` parameter in the [get_spp_summary_data()] function
#'  for further details.
#'  Defaults to `"latest"` such that the most recent version of the dataset is
#'  used.
#'
#' @param habitat_version `character` Version of the
#'   habitat dataset that should be used. See documentation for the
#'   the `version` parameter in the [get_global_habitat_data()] function
#'   for further details.
#'   This parameter is only used if habitat data are obtained
#'   automatically (i.e. the argument to `habitat_data` is `NULL`).
#'   Defaults to `"latest"` such that the most recent version of the dataset is
#'   used if data need to be obtained.
#'
#' @param elevation_version `character` Version of the
#'   elevation dataset that should be used. See documentation for the
#'   the `version` parameter in the [get_global_elevation_data()] function
#'   for further details.
#'   This parameter is only used if elevation data are obtained
#'   automatically (i.e. the argument to `elevation_data` is `NULL`).
#'   Defaults to `"latest"` such that the most recent version of the dataset is
#'   used if data need to be obtained.
#'
#' @param n_threads `integer` Number of computational threads to use
#'   for data processing.
#'   To reduce run time, it is strongly recommended to set this
#'   parameter based on available resources (see Examples section below).
#'   Note that parallel processing is only used for processing the
#'   habitat classification and elevation data.
#'   As such, this parameter will have no influence when using preprocessed
#'   datasets.
#'   Defaults to 1.
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
#' @param engine `character` value indicating the name of the software
#'   to use for data processing.
#'   Available options include `"terra"`, `"gdal"`, or `"grass"`
#'   (see below for details).
#'   Defaults to `"terra"`.
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
#' @section Engines:
#' This function can use different software engines for data processing
#' (specified via argument to `engine`). Although each engine produces the
#' same results, some engines are more computationally efficient than others.
#' The default `"terra"` engine uses the \pkg{terra} package for processing.
#' Although this engine is easy to install and fast for small datasets, it does
#' not scale well for larger datasets. It is generally recommended to use the
#' `"gdal"` engine for both small and moderately sized datasets.
#' Additionally, the `"grass"` engine is recommended when processing data for
#' many species across very large spatial extents.
#' For instructions on installing dependencies for the `"gdal"` and `"grass"`
#' engines, please see the README file.
#'
#' @section Data processing:
#' The Area of Habitat data are produced using the following procedures.
#'
#' \enumerate{
#'
#' \item Global elevation and habitat classification are imported
#'   (if needed,
#'   see [get_global_elevation_data()] and [get_global_habitat_data()]
#'   for details).
#'   If these data are not available in the cache directory
#'   (i.e. argument to `cache_dir`), then they are automatically downloaded
#'   to the cache directory.
#'   Note that if elevation and habitat data are supplied
#'   (i.e. as arguments to `elevation_data` and `habitat_data`), then
#'   the user-supplied datasets are used to generate Area of Habitat data.
#'
#' \item Species range data cleaned to prepare them for subsequent
#'   analysis. Briefly, this process involves excluding places where the
#'   (i) species' presence is not _extant_ or
#'    _probably extant_
#'    (i.e. filtering based on `presence == 1` or `presence == 2`);
#'   (ii) species' origin is not _native_,
#'   _reintroduced_, or the result of _assisted colonization_
#'    (i.e. filtering based on `origin == 1`, `origin == 2`, or `origin == 6`);
#'   (iii) available information on which species' seasonal distribution
#'   is _uncertain_
#'   (i.e. filtering based on `seasonal != 5`); and
#'   (iv) species' distribution is not terrestrial
#'   (i.e. filtering based on where `terrestrial == "true"`).
#'   Additionally, the species' range data are spatially dissolved so that each
#'   seasonal distribution for each taxon is represented by a separate geometry.
#'   Finally, geoprocessing routines are used to detect and repair
#'   any invalid geometries.
#'
#' \item Species' altitudinal limit and habitat affiliation data are
#'   imported
#'   (if needed,
#'   see [get_spp_summary_data()] and [get_spp_habitat_data()] for details).
#'   If these data are not available in the cache directory
#'   (i.e. argument to `cache_dir`), then they are automatically downloaded
#'   to the cache directory
#'   Note that if species' altitudinal limit and habitat affiliation
#'   data are supplied (i.e. as arguments to `spp_summary_data` and
#'   `spp_habitat_data`), then the user-supplied datasets are used to generate
#'   Area of Habitat data.
#'
#' \item The species data are collated into a single dataset containing
#'   their geographic ranges, altitudinal limits, and habitat affiliations.
#'   Specifically, taxon identifiers (per the `id_no`/`SISID` columns)
#'   are used merge the datasets together.
#'   If a species lacks lower or upper altitudinal limits,
#'   then limits of 0 and 9,000 m are assumed respectively
#'   (following Lumbierres *et al.* 2021).
#'   Additionally, the following rules are used to assign
#'   habitat affiliations for species' distributions.
#'   First, habitat affiliations are only
#'   included in the collated dataset if they are classified as *suitable*
#'   or *major*.
#'   Second, if a habitat affiliation is defined for a specific seasonal
#'   distribution of a particular species (e.g. *non-breeding*), then that
#'   habitat affiliation is only assigned to that specific seasonal
#'   distribution for the species.
#'   Third, if a habitat affiliation is not defined for a specific
#'   seasonal distribution, then the habitat is assigned to all seasonal
#'   distributions associated with the species.
#'   Fourth, because the *resident* distributions of some bird species
#'   lack specific habitat affiliation data, the habitat affiliation data
#'   for these *resident* distributions are assigned based on habitat
#'   affiliations for the species' *breeding* distribution.
#'
#' \item The Area of Habitat data are then generated for each seasonal
#'   distribution of each species. For a given species' distribution,
#'   the data are generated by
#'   (i) cropping the habitat classification and elevation data to the spatial
#'   extent  of the species' seasonal distribution;
#'   (ii) converting the habitat classification data to a binary layer
#'   denoting suitable habitat for the species' distribution
#'    (using the habitat affiliation data for the species' distribution);
#'   (iii) creating a mask based on the species' altitudinal limits
#'   and the elevation data, and then using the mask to set values
#'   in the binary layer to zero if they are outside of the species'
#'   limits;
#'   (iv) creating a mask by rasterizing the species' seasonal
#'   distribution, and then using the mask to set values in the binary
#'   layer to missing (`NA`) values if they are outside the species'
#'   distribution;
#'   (v) saving the binary layer as the Area of Habitat data
#'   for the species' distribution.
#'   Note that species' distributions that already have Area of Habitat data
#'   available in the output directory are skipped
#'   (unless the argument to `force` is `TRUE`).
#'
#' \item Post-processing routines are used to prepare the results.
#'   These routines involve updating the collated species data to include
#'   file names and spatial metadata for the Area of Habitat data.
#'
#' }
#'
#' @return
#' A [sf::st_sf()] object containing range maps for the species
#' distributions used to generate the Area of Habitat data and additional
#' columns describing the Area of Habitat data.
#' These range maps were produced by cleaning those supplied as an argument
#' to `x` so they can be used to generate the Area of Habitat data.
#' Specifically, the object contains the following columns:
#' \describe{
#' \item{id_no}{`numeric` species' taxon identifier on the IUCN Red List.}
#' \item{binomial}{`character` species name.}
#' \item{seasonal}{`numeric` seasonal distribution code.}
#' \item{full_habitat_code}{`character` all habitat classification
#'   codes that contain suitable habitat for the species.
#'   If a given species has multiple suitable habitat classes,
#'   then these are denoted using a pipe-delimited format.
#'   For example, if the habitat classes denoted with the codes
#'   `"1.5"` and `"1.9"` were considered suitable for a given species, then
#'   these codes would be indicated as `"1.5|1.9"`.}
#' \item{habitat_code}{`character` habitat
#'   codes used to create the species' Area of Habitat data.
#'   Since the argument to `habitat_data` may not contain habitat
#'   classes for all suitable habitats for a given species
#'   (e.g. the default dataset does not contain subterranean cave systems),
#'   this column contains the subset of the habitat codes listed in the
#'   `"full_habitat_code"` column that were used for processing.}
#' \item{elevation_lower}{`numeric` lower elevation threshold used to create
#'   the species' Area of Habitat data.}
#' \item{elevation_upper}{`numeric` upper elevation threshold used to create
#'   the species' Area of Habitat data.}
#' \item{xmin}{`numeric` value describing the spatial extent of
#'   the Area of Habitat data.}
#' \item{xmax}{`numeric` value describing the spatial extent of
#'   the Area of Habitat data.}
#' \item{ymin}{`numeric` value describing the spatial extent of
#'   the Area of Habitat data.}
#' \item{ymax}{`numeric` value describing the spatial extent of
#'   the Area of Habitat data.}
#' \item{path}{`character` file paths for Area of Habitat data
#'   (in GeoTIFF format).
#'   The Area of Habitat datasets denote the presence (using a value of 1)
#'   and absence (using a value of 0) inside grid cells within
#'   species' seasonal distribution.
#'   Grid cells that are located outside of a species' distribution
#'   are assigned a missing (`NA`) value.
#'   File paths that are denoted with missing (`NA`) values correspond to
#'   species distributions that were not processed (e.g. due to lack of data).}
#' }
#'
#' @references
#' Brooks TM, Pimm SL, Akçakaya HR, Buchanan GM, Butchart SHM, Foden W,
#' Hilton-Taylor C, Hoffmann M, Jenkins CN, Joppa L, Li BV, Menon V,
#' Ocampo-Peñuela N, Rondinini C (2019) Measuring terrestrial Area of Habitat
#' (AOH) and its Utility for the IUCN Red List. *Trends in Ecology & Evolution*,
#' 34:977--986. Available at <https://doi.org/10.1016/j.tree.2019.06.009>.
#'
#' Jung M, Dahal PR, Butchart SHM, Donald PF, De Lamo X, Lesiv M, Kapos V,
#' Rondinini C, and Visconti P (2020a) A global map of
#' terrestrial habitat types. *Scientific Data*, 7:1--8.
#' Available at <https://doi.org/10.1038/s41597-020-00599-8>.
#'
#' Jung M, Dahal PR, Butchart SHM, Donald PF, De Lamo X, Lesiv M, Kapos V,
#' Rondinini C, and Visconti P (2020b) A global map of
#' terrestrial habitat types (insert version) \[Data set\].
#' *Zenodo Digital Repository*.
#' Available at <https://doi.org/10.5281/zenodo.4058819>.
#'
#' Robinson N, Regetz J, and Guralnick RP (2014) EarthEnv-DEM90: A nearly-
#' global, void-free, multi-scale smoothed, 90m digital elevation model from
#' fused ASTER and SRTM data.
#' *ISPRS Journal of Photogrammetry and Remote Sensing*, 87:57--67.
#' Available at <https://doi.org/10.1016/j.isprsjprs.2013.11.002>
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
#' # specify settings for data processing
#' output_dir <- tempdir()                       # folder to save AOH data
#' cache_dir <- rappdirs::user_data_dir("aoh")   # persistent storage location
#' n_threads <- parallel::detectCores() - 1      # speed up analysis
#'
#' # create cache directory if needed
#' if (!file.exists(cache_dir)) {
#'   dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
#' }
#'
#' # create Area of Habitat data for species
#' spp_aoh_data <- create_spp_aoh_data(
#'   x = spp_range_data,
#'   output_dir = output_dir,
#'   n_threads = n_threads,
#'   cache_dir = cache_dir
#' )
#' }
#'
#' @examplesIf interactive()
#' \dontrun{
#' # preview data
#' print(result)
#' }
#'
#' @examples
#' \dontrun{
#' # import AOH data as a list of terra::rast() objects
#' spp_aoh_rasters <- lapply(spp_aoh_data$path, terra::rast)
#'
#' # print AOH data list
#' print(spp_aoh_rasters)
#'
#' # plot the data to visualize the range maps and AOH data
#' plot_spp_aoh_data(spp_aoh_data)
#' }
#' @export
create_spp_aoh_data <- function(x,
                                output_dir,
                                spp_summary_data = NULL,
                                spp_habitat_data = NULL,
                                elevation_data = NULL,
                                habitat_data = NULL,
                                crosswalk_data = NULL,
                                cache_dir = tempdir(),
                                iucn_version = "latest",
                                habitat_version = "latest",
                                elevation_version = "latest",
                                key = NULL,
                                force = FALSE,
                                n_threads = 1,
                                engine = "terra",
                                omit_habitat_codes =
                                  iucn_habitat_codes_marine(),
                                verbose = TRUE) {
  # initialization
  ## display message
  if (verbose) {
    cli::cli_progress_step("initializing")
  }
  ## initial validation
  assertthat::assert_that(
    inherits(x, "sf"),
    assertthat::is.writeable(output_dir),
    assertthat::is.writeable(cache_dir),
    assertthat::is.count(n_threads),
    assertthat::noNA(n_threads),
    assertthat::is.flag(force),
    assertthat::noNA(force),
    assertthat::is.string(engine),
    assertthat::noNA(engine),
    engine %in% c("terra", "gdal", "grass"),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )
  assertthat::assert_that(
    assertthat::has_name(x, "id_no") ||
      assertthat::has_name(x, "SISID"),
    msg = paste0(
      "argument to \"x\" does not have a column named \"id_no\" or \"SISID\""
    )
  )
  if (identical(engine, "gdal")) {
    assertthat::assert_that(
      is_gdal_available(),
      msg = "can't use GDAL for processing because it's not available."
    )
  }
  if (identical(engine, "grass")) {
    assertthat::assert_that(
      is_grass_available(),
      msg = "can't use GRASS for processing because it's not available."
    )
  }
  # verify access to IUCN Red List API
  if (is.null(spp_summary_data) && is.null(spp_habitat_data)) {
    assertthat::assert_that(
      is_iucn_rl_api_available(),
      msg = "can't access the IUCN Red List API, see ?aoh"
    )
  }

  ## elevation data
  if (is.null(elevation_data)) {
    ### display message
    if (verbose) {
      cli::cli_progress_step("importing global elevation data")
    }
    ### processing
    elevation_data <- get_global_elevation_data(
      dir = cache_dir, version = elevation_version,
      force = force, verbose = verbose
    )
  }
  assertthat::assert_that(
    inherits(elevation_data, "SpatRaster"),
    terra::nlyr(elevation_data) == 1,
    all(terra::hasValues(elevation_data))
  )
  ## habitat_data
  if (is.null(habitat_data)) {
    ### display message
    if (verbose) {
      cli::cli_progress_step("importing global habitat data")
    }
    ### processing
    habitat_data <- get_global_habitat_data(
      dir = cache_dir, version = habitat_version,
      force = force, verbose = verbose
    )
    ### get crosswalk data if needed
    if (is.null(crosswalk_data)) {
      crosswalk_data <- crosswalk_jung_data
    }
  } else {
    assertthat::assert_that(
      inherits(crosswalk_data, "data.frame"),
      msg = paste(
        "argument to \"crosswalk_data\" must be supplied when not using",
        "default habitat data"
      )
    )
  }
  assertthat::assert_that(
    inherits(habitat_data, "SpatRaster"),
    terra::nlyr(habitat_data) == 1,
    all(terra::hasValues(habitat_data))
  )
  ## verify rasters match
  assertthat::assert_that(
    terra::compareGeom(
      elevation_data, habitat_data,
      res = TRUE, stopiffalse = FALSE
    ),
    msg = paste(
      "arguments to \"elevation_data\" and \"habitat_data\" don't have the",
      "same spatial properties (e.g. coordinate system, extent, resolution)"
    )
  )
  ## crosswalk_data
  assertthat::assert_that(
    inherits(crosswalk_data, "data.frame"),
    assertthat::has_name(crosswalk_data, c("code", "value")),
    assertthat::noNA(crosswalk_data$code),
    assertthat::noNA(crosswalk_data$value),
    is.character(crosswalk_data$code),
    is.numeric(crosswalk_data$value)
  )
  assertthat::assert_that(
    all(crosswalk_data$code %in% iucn_habitat_data$code),
    msg = paste(
      "argument to \"crosswalk_data\" contains the following codes that",
      "are not valid IUCN habitat codes:",
      paste(
        paste0(
          "\"",
          setdiff(crosswalk_data$code, iucn_habitat_data$code),
          "\""
        ),
        collapse = ","
      )
    )
  )

  # clean species range data
  ## display message
  if (verbose) {
    cli::cli_progress_step("cleaning species range data")
  }
  ## processing
  x <- clean_spp_range_data(x = x, crs = terra_st_crs(elevation_data))
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

  ## spp_summary_data
  ### import data
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

  ## spp_habitat_data
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
  x <- format_spp_data(
    x = x,
    template_data = habitat_data,
    spp_summary_data = spp_summary_data,
    spp_habitat_data = spp_habitat_data,
    cache_dir = cache_dir,
    iucn_version = iucn_version,
    key = key,
    force = force,
    omit_habitat_codes = omit_habitat_codes,
    verbose = verbose
  )

  ## additional data validation
  ### check that habitat_data has all codes in spp_habitat_data
  habitat_codes <- unique(unlist(x$habitat_code, use.names = FALSE))
  missing_codes <- !habitat_codes %in% crosswalk_data$code
  if (any(missing_codes)) {
    cli::cli_alert_warning(
      paste(
        "argument to \"crosswalk_data\" is missing the following",
        sum(missing_codes),
        "habitat classification codes:",
        paste(
          paste0(
            "\"",
            stringi::stri_sort(habitat_codes[missing_codes], numeric = TRUE),
            "\""
          ),
          collapse = ", "
        )
      )
    )
  }
  assertthat::assert_that(
    any(habitat_codes %in% crosswalk_data$code),
    msg = paste(
      "argument to \"crosswalk_data\" does not contain",
      "IUCN habitat classification codes that are suitable for any species"
    )
  )
  assertthat::assert_that(
    length(habitat_codes) >= 1,
    msg = paste(
      "none of the species have any suitable habitat classes -",
      "perhaps the argument to \"spp_habitat_data\" is missing",
      "some information or the argument to \"omit_habitat_codes\"",
      "contains codes that should not be omitted?"
    )
  )
  ## remove missing codes
  habitat_codes <- habitat_codes[!missing_codes]
  ## add column with output file paths
  x$path <- file.path(output_dir, paste0(x$aoh_id, ".tif"))
  ## copy all habitat codes to full_habitat_code
  x$full_habitat_code <- x$habitat_code
  ## subset habitat codes to those that are available
  x$habitat_code <- lapply(x$full_habitat_code, function(x) {
    base::intersect(x, habitat_codes)
  })
  ## set paths to NA if the species won't be processed
  ## species won't be processed if:
  ##   they don't overlap with the template,
  ##   they have no habitat layers at all,
  ##   none of their habitat layers are available
  x$path[is.na(x$xmin)] <- NA_character_
  x$path[vapply(x$habitat_code, length, integer(1)) == 0] <- NA_character_
  ## verify that habitat data encompasses that species range data
  not_contained <- sf::st_contains(
    sf::st_as_sfc(terra_st_bbox(habitat_data)),
    sf::st_as_sfc(sf::st_bbox(x)),
    sparse = FALSE
  )[[1]]
  if (!isTRUE(not_contained)) {
    cli::cli_alert_warning(
      paste(
        "arguments to \"habitat_data\" and \"elevation_data\" do not fully",
        "contain the ranges for all the species"
      )
    )
  }

  # main processing
  ## display message
  if (verbose && (n_threads > 1)) {
    cli::cli_progress_step("generating Area of Habitat data")
  }
  ## processing
  ## use local host for processing
  result <- process_spp_aoh_data_on_local(
    x = x,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_data,
    cache_dir = cache_dir,
    engine = engine,
    force = force,
    verbose = verbose
  )

  # prepare table with metadata
  ## display message
  if (verbose) {
    cli::cli_progress_step("post-processing results")
  }
  ## processing
  x <- dplyr::select(
    x, .data$id_no, .data$binomial, .data$seasonal,
    .data$full_habitat_code, .data$habitat_code,
    .data$elevation_lower, .data$elevation_upper,
    .data$xmin, .data$xmax, .data$ymin, .data$ymax,
    .data$path
  )
  ## convert list-column to "|" delimited character-column
  x$habitat_code <- vapply(
    x$habitat_code, paste, character(1), collapse = "|"
  )
  x$full_habitat_code <- vapply(
    x$full_habitat_code, paste, character(1), collapse = "|"
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
