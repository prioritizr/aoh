#' @include internal.R clean_spp_range_data.R
#' @include get_global_elevation_data.R get_global_habitat_data.R
#' @include get_spp_habitat_data.R get_spp_summary_data.R
#' @include misc_terra.R misc_sf.R
NULL

#' Create Area of Habitat data
#'
#' Create Area of Habitat (AOH) data for species based on their altitudinal and
#' habitat preferences (Brooks *et al.* 2019).
#'
#' @inheritParams get_global_habitat_data
#' @inheritParams get_spp_summary_data
#'
#' @param x [sf::sf()] Spatial data delineating species geographic ranges
#'   obtained from the [IUCN Red List](https://www.iucnredlist.org/).
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
#'   worldwide elevation data (e.g. Jung *et al.* 2020a).
#'   Defaults to `NULL` such that data
#'   are automatically obtained from Jung *et al.* 2020b
#'   (using [get_global_elevation_data()]).
#'
#' @param habitat_data [terra::rast()] Multi-layer raster data delineating the
#'   coverage of different habitat classes across world
#'   (e.g. Jung *et al.* 2020a).
#'   Each layer should correspond to a different habitat class,
#'   and the name of each layer should contain a unique code used to identify
#'   which places contain suitable habitat for a given species
#'   (per the `"code"` column in the the argument to `spp_habitat_data`).
#'   For each layer, pixel values should indicate the percentage of available
#'   an available habitat class.
#'   **Critically, a value of 1000 should indicate 100%, and a value of 0
#'   should indicate 0%.**
#'   Defaults to `NULL` such that data
#'   are automatically obtained from Jung *et al.* 2020b
#'   (using [get_global_habitat_data()]).
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
#'   Defaults to `"latest"` such that the most recent version of the dataset is
#'   used.
#'
#' @param template_data [terra::rast()] Template raster specifying the
#'  resolution, coordinate reference system, and dimensionality for the
#'  output raster files.
#'  Defaults to [get_world_berhman_1km_rast()] which is a global 1 km resolution
#'  raster under the World Behrman coordinate reference system (ESRI:54017).
#'
#' @param parallel_n_threads `integer` Number of computational threads to use
#'   for data processing.
#'   **To reduce run time, it is strongly recommended to set this
#'   parameter based on the number of available threads (see Examples below).**
#'   Defaults to 1.
#'
#' @param parallel_cluster `character` Name of strategy for
#'  processing data in parallel. Available options are `"FORK"` and
#'  `"PSOCK"`.
#'  Defaults to `NULL` such that `"PSOCK"` is used on Microsoft
#'  Windows operating systems, and `"FORK"` otherwise.
#'  Defaults to 1.
#'
#' @param omit_habitat_codes `character` Habitat classification codes
#' to omit from resulting Area of Habitat data. If the aim is to identify
#' places that contain natural, suitable habitats, then processing should
#' exclude (i) anthropogenically modified habitat classifications and (ii)
#' unknown habitat classification types. Defaults to all artificial,
#' introduced vegetation, and unknown habitat classifications on
#' the [IUCN Red List Habitat Classification
#' Scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
#' (see [default_omit_iucn_habitat_codes()],
#'
#' @param verbose `logical` Should progress be displayed while processing data?
#'  Defaults to `TRUE`.
#'
#' @details
#' The Area of Habitat data are produced by
#' (i) cleaning the range data to prepare them for subsequent analysis
#' (see [clean_spp_range_data()] for details);
#' (ii) automatically downloading global elevation and habitat classification
#' data (if needed);
#' (iii) automatically downloading information on the
#' altitudinal limits and habitat preferences of the species from the IUCN Red
#' List (per the taxon identifiers in the `id_no` column) (if needed);
#' and (iv) cross-referencing this information to identify suitable habitat
#' located within the altitudinal limits and geographic range of each species
#' (following Brooks *et al.* 2019).
#' To account for migratory species, the spatial distribution of species'
#' seasonal distributions (e.g. breeding, non-breeding, and passage
#' distributions) are processed separately.
#' Thus a separate Area of Habitat dataset is produced for each seasonal
#' distribution of each species.
#' Please note that these procedures are designed for terrestrial species
#' and will not apply to marine species.
#'
#' @return
#' A [sf::st_sf()] object containing range maps for the species distributions
#' used to generate the Area of Habitat data and additional columns describing
#' the Area of Habitat data.
#' These range maps were produced by cleaning those supplied as an argument
#' to `x` so they can be used to generate the Area of Habitat data
#' (see [clean_spp_range_data()] for data cleaning procedures).
#' Specifically, the object contains the following columns:
#' \describe{
#' \item{id_no}{`numeric` species' taxon identifier on the IUCN Red List.}
#' \item{binomial}{`character` species name.}
#' \item{seasonal}{`numeric` seasonal distribution code.}
#' \item{full_habitat_code}{`character` all habitat classification
#'   codes that contain suitable habitat for the species.
#'   If a given species has multiple suitable habitat classes,
#'   then these are denoted using a pipe-delimeted format.
#'   For example, if the habitat classes denoted with the codes
#'   `"1.5"` and `"1.9"` were considered suitable for a given species, then
#'   these codes would be indicated as `"1.5|1.9"`.}
#' \item{habitat_code}{`character` habitat
#'   codes used to create the species' Area of Habitat data.
#'   Since the argument to `habitat_data` may not contain habitat
#'   classes for all suitable habitats for a given species
#'   (e.g. the default dataset does not contain subterranean cave systems),
#'   this column contains the subset of the habitat codes listed in the
#'   `"full_habitat_code"`column that were used for processing.}
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
#'   The Area of Habitat datasets denote the proportion of land
#'   inside different grid cells that contain suitable habitat for
#'   species' seasonal distribution.
#'   Thus a grid cell with a value of 0 has 0% of its spatial extent covered by
#'   suitable habitat, a value of 1 has 100% of its spatial extent covered by
#"   suitable habitat, and a missing (`NA`) value
#'   means that the cell is located outside of the species seasonal
#'   distribution (per the geographic range data).
#'   File paths that are denoted with missing (`NA`) values correspond to
#'   species distributions that were not processed (e.g. due to lack of habitat
#'   data).
#'    }
#' }
#'
#' @references
#' Amatulli G, Domisch S, Tuanmu M-N, Parmentier B, Ranipeta A, Malczyk J, and
#' Jetz W (2018) A suite of global, cross-scale topographic variables for
#' environmental and biodiversity modeling. *Scientific Data*, 5:180040.
#' Available at <https://doi.org/10.1038/sdata.2018.40>.
#'
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
#'   parallel_n_threads = n_threads,
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
                                template_data = get_world_berhman_1km_rast(),
                                cache_dir = tempdir(),
                                iucn_version = "latest",
                                habitat_version = "latest",
                                key = NULL,
                                force = FALSE,
                                parallel_n_threads = 1,
                                parallel_cluster = NULL,
                                omit_habitat_codes =
                                  default_omit_iucn_habitat_codes(),
                                verbose = TRUE) {

  # initialization
  ## display message
  if (verbose) {
    cli::cli_progress_step("initializing")
  }
  ## customize terra options
  tmp_rast_dir <- tempfile()
  dir.create(tmp_rast_dir, showWarnings = FALSE, recursive = TRUE)
  terra::terraOptions(progress = 0, tempdir = tmp_rast_dir)
  on.exit(terra::terraOptions(progress = 3, tempdir = tempdir()), add = TRUE)
  ## initial validation
  assertthat::assert_that(
    inherits(x, "sf"),
    assertthat::is.writeable(output_dir),
    assertthat::is.writeable(cache_dir),
    assertthat::is.count(parallel_n_threads),
    assertthat::noNA(parallel_n_threads),
    assertthat::is.flag(verbose),
    assertthat::noNA(force),
    assertthat::is.flag(force),
    assertthat::noNA(verbose),
    inherits(template_data, "SpatRaster")
  )
  ## parallel cluster
  if (is.null(parallel_cluster)) {
    parallel_cluster <- ifelse(
      identical(.Platform$OS.type, "unix"), "FORK", "PSOCK"
    )
  }
  assertthat::assert_that(
    assertthat::is.string(parallel_cluster),
    assertthat::noNA(parallel_cluster)
  )
  assertthat::assert_that(
    identical(parallel_cluster, "FORK") ||
    identical(parallel_cluster, "PSOCK"),
    msg = paste(
      "argument to \"parallel_cluster\" is not NULL,",
      "\"FORK\", or \"PSOCK\""
    )
  )

  ## elevation data
  if (is.null(elevation_data)) {
    ### display message
    if (verbose) {
      cli::cli_progress_step("importing global elevation data")
    }
    ### processing
    elevation_data <- get_global_elevation_data(
      dir = cache_dir, force = force, verbose = verbose
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
      dir = cache_dir, version = habitat_version, force = force,
      verbose = verbose
    )
  }
  assertthat::assert_that(
    inherits(habitat_data, "SpatRaster"),
    all(terra::hasValues(habitat_data))
  )
  ## additional data validation
  rng_cells <- as.matrix(terra::spatSample(
    habitat_data, size = 10000, method = "regular",
    replace = FALSE, as.df = TRUE,
    warn = FALSE
  ))
  if (mean(rowSums(is.finite(rng_cells)) > 0) > 0.01) {
    assertthat::assert_that(
      min(rng_cells, na.rm = TRUE) >= 0,
      max(rng_cells, na.rm = TRUE) <= 1000,
      msg = paste(
        "argument to \"habitat_data\" does not have values ranging between 0",
        "and 1000 (i.e. indicating 0% and 100% coverage, respectively)"
      )
    )
    if (max(rng_cells, na.rm = TRUE) < 500) {
      warning(
        paste(
          "argument to \"habitat_data\" may not have values ranging from 0",
          "to 1000 indicating 0% to 100% coverage of different habitat classes"
        ),
        immediate. = FALSE
      )
    }
  } else {
    warning(
      paste(
        "unable to verify that that argument to \"habitat_data\" contains",
        "valid data, please ensure that it contains values ranging from 0",
        "to 1000 that indicate 0% to 100% coverage (respectively)",
        "of different habitat classes"
      ),
      immediate. = FALSE
    )
  }
  ## clean up
  rm(rng_cells)

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

  # clean species range data
  ## display message
  if (verbose) {
    cli::cli_progress_step("cleaning species range data")
  }
  ## processing
  x <- clean_spp_range_data(x = x, crs = terra_st_crs(template_data))
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

  # format species data
  ## display message
  if (verbose) {
    cli::cli_progress_step("collating species data")
  }
  ## main processing
  x <- format_spp_data(
    x = x,
    template_data = template_data,
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
  missing_codes <- !habitat_codes %in% names(habitat_data)
  if (any(missing_codes)) {
    cli::cli_alert_warning(
      paste(
        "argument to \"habitat_data\" is missing layers for the following",
        sum(missing_codes),
        "habitat classification codes:",
        paste(
          paste0("\"", sort(habitat_codes[missing_codes]), "\""),
          collapse = ", "
        )
      )
    )
  }
  assertthat::assert_that(
    any(habitat_codes %in% names(habitat_data)),
    msg = paste(
      "argument to \"habitat_data\" does not have any layer names that",
      "match the habitat classification codes - did you forget to specify",
      "layer names for \"habitat_data\"?"
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

  # preliminary GIS data processing
  ## display message
  if (verbose) {
    cli::cli_progress_step("preliminary geoprocessing")
  }
  ## remove unused habitat layers
  habitat_data <- habitat_data[[habitat_codes]]
  ## crop template data if full extent not needed
  template_data <- terra::crop(
    x = template_data,
    y = sf_terra_ext(
      sf::st_bbox(
        sf::st_intersection(
          sf::st_as_sfc(sf::st_bbox(x, crs = sf::st_crs(x))),
          sf::st_as_sfc(terra_st_bbox(template_data))
        )
      )
    ),
    snap = "out",
    NAflag = -9999,
  )

  # prepare habitat data
  ## display message
  if (verbose) {
    cli::cli_progress_step("preparing habitat data")
  }
  ## processing
  ### reproject data to template
    habitat_data <- parallel_project(
      x = habitat_data,
      y = template_data,
      method = "bilinear",
      buffer = 5000,
      temp_dir = tmp_rast_dir,
      verbose = verbose,
      parallel_n_threads = parallel_n_threads,
      parallel_cluster = parallel_cluster,
      datatype = "INT2U"
    )
  ### verify that habitat data encompasses that species range data
  assertthat::assert_that(
    sf::st_contains(
      sf::st_as_sfc(terra_st_bbox(habitat_data)),
      sf::st_as_sfc(sf::st_bbox(x)),
      sparse = FALSE
    )[[1]],
    msg = paste(
      "argument to \"habitat_data\" has a spatial extent that does not",
      "fully contain species range data in the argument to \"x"
    )
  )
  ### clamp to expected values
  habitat_data <- terra::clamp(
    habitat_data,
    lower = 0,
    upper = 1000,
    values = TRUE,
    datatype = "INT2U"
  )

  # prepare elevation data
  ## display message
  if (verbose) {
    cli::cli_progress_step("preparing elevation data")
  }
  ## processing
  ### convert NA values to zeros
  elevation_data[is.na(elevation_data)] <- 0
  ### reproject data to template
  elevation_data <- parallel_project(
    x = elevation_data,
    y = template_data,
    method = "bilinear",
    buffer = 5000,
    temp_dir = tmp_rast_dir,
    verbose = FALSE,
    parallel_n_threads = 1, # no speed up to be had with one layer
    datatype = "INT2U",
  )
  ### verify that elevation data encompasses that species range data
  assertthat::assert_that(
    sf::st_contains(
      sf::st_as_sfc(terra_st_bbox(elevation_data)),
      sf::st_as_sfc(sf::st_bbox(x)),
      sparse = FALSE
    )[[1]],
    msg = paste(
      "argument to \"elevation_data\" has a spatial extent that does not",
      "fully contain species range data in the argument to \"x"
    )
  )

  # manual raster clean up
  ## display message
  if (verbose) {
    cli::cli_progress_step("standardizing data")
  }
  ## save habitat data to disk
  habitat_path <- tempfile(fileext = ".tif")
  terra::writeRaster(
    x = habitat_data,
    filename = habitat_path,
    datatype = "INT2U"
  )
  habitat_data <- terra::rast(habitat_path)
  ## save elevation data to disk
  elevation_path <- tempfile(fileext = ".tif")
  terra::writeRaster(
    x = elevation_data,
    filename = elevation_path,
    datatype = "INT2U"
  )
  elevation_data <- terra::rast(elevation_path)
  ## remove old terra files and reset terra options
  unlink(tmp_rast_dir, force = TRUE, recursive = TRUE)
  terra::terraOptions(progress = 0, tempdir = tempdir())

  # main processing
  ## display message
  if (verbose) {
    cli::cli_progress_step("generating Area of Habitat data")
  }
  ## processing
  ## use local host for processing
  result <- process_spp_aoh_data_on_local(
    x = x,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    cache_dir = cache_dir,
    force = force,
    parallel_n_threads = parallel_n_threads,
    parallel_cluster = parallel_cluster,
    verbose = verbose,
    datatype = "INT2U"
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
