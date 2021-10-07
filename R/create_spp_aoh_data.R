#' @include internal.R clean_spp_range_data.R
#' @include get_global_elevation_data.R get_global_habitat_data.R
#' @include get_spp_habitat_data.R get_spp_summary_data.R
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
#'  Defaults to [world_berhman_1km_rast()] which is a global 1 km resolution
#'  raster under the World Behrman coordinate reference system (ESRI:54017).
#'
#' @param parallel_n_threads `integer` Number of computational threads to use
#'   for data processing.
#'   **To reduce run time, it is strongly recommended to set this
#'   parameter based on the number of available threads (see Examples below).**
#'   Note that this parameter is only used if `use_ee` is `FALSE`.
#'   Defaults to 1.
#'
#' @param parallel_strategy `character` Name of strategy for
#'  processing data in parallel. Available options are `"multisession"` and
#'  `"multicore"`.
#'  Defaults to `NULL` such that `"multisession"` is used on Microsoft
#'  Windows operating systems, and `"multicore"` otherwise.
#'  Note that this parameter is only used if `use_ee` is `FALSE`.
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
#' @param use_ee `logical` Should data processing be completed using
#'  Google Earth Engine (via the \pkg{rgee}) package?
#'  Defaults to `FALSE`.
#'
#' @param verbose `logical` Should progress be displayed while downloading
#'  and processing data?
#'  Defaults to `TRUE`.
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
#' \item{seasonal}{`numeric` seasonal distribution code.}
#' \item{habitat_code}{`character` habitat
#'   codes used to create the species' Area of Habitat data.
#'   If a given species has multiple suitable habitat classes,
#'   then these are denoted using a pipe-delimeted format.
#'   For example, if the habitat classes denoted with the codes
#'   `"1.5"` and `"1.9"` were considered suitable for a given species, then
#'   these codes would be indicated as `"1.5|1.9"`.}
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
#' environmental and biodiversity modeling. Scientific Data, 5:180040.
#' <https://doi.org/10.1038/sdata.2018.40>
#'
#' Brooks TM, Pimm SL, Akçakaya HR, Buchanan GM, Butchart SHM, Foden W,
#' Hilton-Taylor C, Hoffmann M, Jenkins CN, Joppa L, Li BV, Menon V,
#' Ocampo-Peñuela N, Rondinini C (2019) Measuring terrestrial Area of Habitat
#' (AOH) and its Utility for the IUCN Red List. Trends in Ecology & Evolution.
#' 34:977--986. <doi:10.1016/j.tree.2019.06.009>
#'
#' Jung M, Dahal PR, Butchart SH, Donald PF, De Lamo X, Lesiv M, Kapos V,
#' Rondinini C, and Visconti P (2020a) A global map of
#' terrestrial habitat types. Scientific data, 7:1--8.
#' <https://doi.org/10.1038/s41597-020-00599-8>
#'
#' Jung M, Dahal PR, Butchart SH, Donald PF, De Lamo X, Lesiv M, Kapos V,
#' Rondinini C, and Visconti P (2020b) A global map of
#' terrestrial habitat types (insert version) \[Data set\]. Zenodo.
#' <https://doi.org/10.5281/zenodo.4058819>
#
#' @examples
#' \dontrun{
#' # load built in datasets
#' data(sim_spp_summary_data, package = "aoh")
#' data(sim_spp_habitat_data, package = "aoh")
#'
#' # find file path for simulated data following the IUCN Red List format
#' path <- system.file("XENOMORPHS_TERRESTRIAL_ONLY.zip")
#'
#' # import species range data
#' sim_spp_range_data <- read_spp_range_data(path)
#'
#' # specify settings for data processing
#' output_dir <- tempdir()                  # folder to save AOH data
#' cache_dir <- rappdirs::user_data_dir()   # persistent storage location
#' n_threads <- parallel::detectCores() - 1 # recommended for best performance
#'
#' # create Area of Habitat data for species
#' sim_aoh_data <- create_aoh_data(
#'   x = sim_spp_range_data,
#'   output_dir = output_dir, ## change this
#'   spp_summary_data = sim_spp_summary_data,
#'   spp_habitat_data = sim_spp_habitat_data,
#'   parallel_n_threads = n_threads,
#'   cache_dir = cache_dir
#' )
#'
#' # preview result
#' print(sim_aoh_data)
#'
#' # import AOH data
#' aoh_data <- lapply(sim_aoh_data, terra::rast)
#'
#' # compare range data (EOO) and AOH data for first species
#' #TODO
#' }
#' @export
create_spp_aoh_data <- function(x,
                                output_dir,
                                cache_dir = tempdir(),
                                spp_summary_data = NULL,
                                spp_habitat_data = NULL,
                                elevation_data = NULL,
                                habitat_data = NULL,
                                template_data = world_berhman_1km_rast(),
                                iucn_version = "latest",
                                habitat_version = "latest",
                                key = NULL,
                                force = FALSE,
                                parallel_n_threads = 1,
                                parallel_strategy = NULL,
                                omit_habitat_codes =
                                  default_omit_iucn_habitat_codes(),
                                use_ee = FALSE,
                                verbose = TRUE) {

  # initialization
  ## display message
  if (verbose) {
    cli::cli_process_start("initializing")
  }
  ## customize terra options
  tmp_rast_dir <- tempfile()
  dir.create(tmp_rast_dir, showWarnings = FALSE, recursive = TRUE)
  terra::terraOptions(progress = 0, tempdir = tmp_rast_dir)
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
    assertthat::is.flag(use_ee),
    assertthat::noNA(use_ee),
    inherits(template_data, "SpatRaster")
  )
  ## parallel cluster
  if (is.null(parallel_strategy)) {
    parallel_strategy <- ifelse(
      identical(.Platform$OS.type, "unix"), "multicore", "multisession"
    )
  }
  assertthat::assert_that(
    assertthat::is.string(parallel_strategy),
    assertthat::noNA(parallel_strategy)
  )
  assertthat::assert_that(
    identical(parallel_strategy, "multicore") ||
    identical(parallel_strategy, "multisession"),
    msg = paste(
      "argument to \"parallel_strategy\" is not NULL,",
      "\"multicore\", or \"multisession\""
    )
  )
  ## update message
  if (verbose) {
    cli::cli_process_done()
  }

  ## elevation data
  if (is.null(elevation_data)) {
    ### display message
    if (verbose) {
      cli::cli_process_start("importing global elevation data")
    }
    ### processing
    elevation_data <- get_global_elevation_data(
      dir = cache_dir, force = force, verbose = verbose
    )
    ## update message
    if (verbose) {
      cli::cli_process_done()
    }
  }
  assertthat::assert_that(
    inherits(elevation_data, "SpatRaster")
  )
  ## habitat_data
  if (is.null(habitat_data)) {
    ### display message
    if (verbose) {
      cli::cli_process_start("importing global habitat data")
    }
    ### processing
    habitat_data <- get_global_habitat_data(
      dir = cache_dir, version = habitat_version, force = force,
      verbose = verbose
    )
    ## update message
    if (verbose) {
      cli::cli_process_done()
    }
  }
  assertthat::assert_that(
    inherits(habitat_data, "SpatRaster")
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

  # format species data
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
    stop(
      paste(
        "argument to \"habitat_data\" is missing layers for the following",
        sum(missing_codes),
        "habitat classification codes:",
        paste(paste0("\"", habitat_codes[missing_codes], "\""), collapse = ", ")
      )
    )
  }
  ## add column with output file paths
  x$path <- file.path(output_dir, paste0(x$aoh_id, ".tif"))
  ## set paths to NA if the species won't be processed
  ## species won't be processed if:
  ##   they don't overlap with the template,
  ##   they have no habitat layers at all,
  ##   none of their habitat layers are available
  x$path[is.na(x$xmin)] <- NA_character_
  x$path[nchar(x$habitat_code) == 0] <- NA_character_

  # preliminary GIS data processing
  ## remove unused habitat layers
  ### display message
  if (verbose) {
    cli::cli_process_start("removing unused habitat layers")
  }
  ### processing
  habitat_data <- habitat_data[[habitat_codes]]
  ## update message
  if (verbose) {
    cli::cli_process_done()
  }

  ## crop template data if full extent not needed
  ### display message
  if (verbose) {
    cli::cli_process_start("cropping template to species range data")
  }
  ### processing
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
  ### update message
  if (verbose) {
    cli::cli_process_done()
  }

  # prepare habitat data
  ## display message
  if (verbose) {
    cli::cli_alert("preparing habitat data")
  }
  ## processing
  ### reproject data to template
  withr::with_tempdir(
    tmpdir = tmp_rast_dir,
    clean = FALSE,
    habitat_data <- fast_reproject(
      x = habitat_data,
      y = template_data,
      method = "bilinear",
      buffer = 5000,
      verbose = verbose,
      parallel_n_threads = parallel_n_threads,
      parallel_strategy = parallel_strategy,
      datatype = "INT2U",
    )
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
  #### assign names
  names(habitat_data) <- habitat_codes

  # prepare elevation data
  ## display message
  if (verbose) {
    cli::cli_process_start("preparing elevation data")
  }
  ## processing
  ### convert NA values to zeros
  elevation_data[is.na(elevation_data)] <- 0
  ### reproject data to template
  withr::with_tempdir(
    tmpdir = tmp_rast_dir,
    clean = FALSE,
    elevation_data <- fast_reproject(
      x = elevation_data,
      y = template_data,
      method = "bilinear",
      buffer = 5000,
      datatype = "INT2U",
      verbose = FALSE
    )
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
  ### update message
  if (verbose) {
    cli::cli_process_done()
  }

  # manual raster clean up
  ## display message
  if (verbose) {
    cli::cli_process_start("writing intermediate files to disk")
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
  ## update message
  if (verbose) {
    cli::cli_process_done()
  }

  # main processing
  if (isTRUE(use_ee)) {
    ## use Earth Engine for processing
    result <- process_spp_aoh_data_on_ee(
      x = x,
      habitat_data = habitat_data,
      elevation_data = elevation_data,
      cache_dir = cache_dir,
      force = force,
      verbose = verbose
    )
  } else {
    ## use local host for processing
    progressr::with_progress(
      enable = verbose,
      expr = {
        result <- process_spp_aoh_data_on_local(
          x = x,
          habitat_data = habitat_data,
          elevation_data = elevation_data,
          cache_dir = cache_dir,
          force = force,
          parallel_n_threads = parallel_n_threads,
          parallel_strategy = parallel_strategy,
          verbose = verbose
        )
      }
    )
  }

  # prepare table with metadata
  ## display message
  if (verbose) {
    cli::cli_process_start("post-processing")
  }
  ## processing
  x <- dplyr::select(
    x, id_no, seasonal,
    habitat_code, elevation_lower, elevation_upper,
    xmin, xmax, ymin, ymax,
    path
  )
  ## convert list-column to "|" delimited character-column
  x$habitat_code <- vapply(x$habitat_code, paste, character(1), collapse = "|")
  ## update message
  if (verbose) {
    cli::cli_process_done()
  }

  # return result
  ## display message
  if (verbose) {
    cli::cli_alert_success("finished")
  }
  ## return
  x
}
