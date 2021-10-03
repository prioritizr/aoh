#' @include internal.R
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
#'   obtained from the IUCN Red List (<https://www.iucnredlist.org/>).
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
#'   latest version of the IUCN Red List (<https://www.iucnredlist.org>).
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
#'   latest version of the IUCN Red List (<https://www.iucnredlist.org>).
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
#' @param template_data [terra::rast()] Raster specify the resolution,
#'  coordinate reference system, and dimensionality for the output raster files.
#'  Defaults to `NULL` such that a template raster dataset is automatically
#   generated based on the spatial extent of the species' geographic ranges
#'  (i.e. argument to `x`) assuming a 1 km \eqn{\times} 1 km resolution.
#'
#' @param parallel_n_threads `integer` Number of computational threads to use
#'   for data processing.
#'   **To reduce run time, it is strongly recommended to set this
#'   parameter based on the number of available threads (see Examples below).**
#'   Default to 1.
#'
#' @param parallel_cluster `character` Name of cluster method for
#'  processing data in parallel. Available options are `"FORK"` and `"PSOCK"`.
#'  Defaults to `NULL` such that `"FORK"` is used on Unix operating systems,
#'  and `"PSOCK"` otherwise.
#'
#' @param verbose `logical` Should progress be displayed while downloading
#'  and processing data?
#'  Defaults to `TRUE`.
#'
#' @return TODO.
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
#' # TODO
#' @export
create_aoh_data <- function(x,
                            output_dir,
                            cache_dir = tempdir(),
                            spp_summary_data = NULL,
                            spp_habitat_data = NULL,
                            elevation_data = NULL,
                            habitat_data = NULL,
                            template_data = NULL,
                            iucn_version = "latest",
                            habitat_version = "latest",
                            key = NULL,
                            force = FALSE,
                            parallel_n_threads = 1,
                            parallel_cluster = NULL,
                            verbose = TRUE) {
  # assert arguments are valid
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
    assertthat::noNA(verbose)
  )
  ## elevation data
  if (is.null(elevation_data)) {
    elevation_data <- get_global_elevation_data(
      dir = cache_dir, force = force, verbose = verbose
    )
  }
  assertthat::assert_that(
    inherits(elevation_data, "SpatRaster")
  )
  ## habitat_data
  if (is.null(habitat_data)) {
    habitat_data <- get_global_habitat_data(
      dir = cache_dir, version = habitat_version, force = force,
      verbose = verbose
    )
  }
  assertthat::assert_that(
    inherits(habitat_data, "SpatRaster")
  )
  ## spp_summary_data
  if (is.null(spp_summary_data)) {
    spp_habitat_data <- get_spp_habitat_data(
      x$id_no, dir = cache_dir, version = iucn_version, key = key,
      force = force, verbose = verbose
    )
  }
  assertthat::assert_that(
    inherits(spp_summary_data, "data.frame"),
    assertthat::has_name(spp_summary_data, "id_no"),
    assertthat::has_name(spp_summary_data, "elevation_upper"),
    assertthat::has_name(spp_summary_data, "elevation_lower"),
  )
  ## spp_habitat_data
  if (is.null(spp_habitat_data)) {
    spp_habitat_data <- get_spp_habitat_data(
      x$id_no, dir = cache_dir, version = iucn_version, key = key,
      force = force, verbose = verbose
    )
  }
  assertthat::assert_that(
    inherits(spp_habitat_data, "data.frame"),
    assertthat::has_name(spp_habitat_data, "id_no"),
    assertthat::has_name(spp_habitat_data, "code"),
    assertthat::has_name(spp_habitat_data, "habitat"),
    assertthat::has_name(spp_habitat_data, "suitability"),
    assertthat::has_name(spp_habitat_data, "season")
  )
  if (any(!x$id_no %in% spp_habitat_data$id_no)) {
    warning(
      paste(
        "argument to \"x\" contains",
        sum(!x$id_no %in% spp_habitat_data$id_no),
        "species lacking habitat data"
      ),
      immediate. = TRUE
    )
  }
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
    identical(parallel_cluster, "FORK") || identical(parallel_cluster, "PSOCK"),
    msg = paste(
      "argument to \"parallel_cluster\" is not  NULL, \"FORK\", or \"PSOCK\""
    )
  )
  ## clean species range data
  x <- clean_spp_range_data(x)
  ## create template raster
  if (is.null(template_data)) {
    # TODO
  }
  ## additional data validation
  assertthat::assert_that(
    all(x$id_no %in% spp_summary_data$id_no)
  )

  # Tabular data processing
  ## habitat_data
  ### exclude non-suitable habitats
  habitat_data$suitability <- tolower(habitat_data$suitability)
  habitat_data <-
    habitat_data[habitat_data$suitability == "suitable", , drop = FALSE]
  ## assign aoh_id column
  if (is.character(habitat_data$seasonal)) {
    habitat_data$seasonal <- convert_to_seasonal_id(habitat_data$seasonal)
  }
  habitat_data$aoh_id <- paste0(
    "AOH_", habitat_data$id_no, "_", habitat_data$seasonal
  )

  # GIS data processing
  ## prepare habitat data
  habitat_data <- terra::app(habitat_data, function(x) {
    x[which.lyr(is.na(x))] <- 0
    x
  })
  habitat_data <- terra::project(
    habitat_data,
    template_data,
    method = "bilinear"
  )
  ## prepare elevation data
  elevation_data[terra::which.lyr(is.na(elevation_data))] <- 0
  elevation_data <- terra::project(
    elevation_data,
    template_data,
    method = "bilinear"
  )

  # AOH processing
  ## prepare for parallel processing if needed
  if (identical(parallel_cluster, "FORK")) {
    ### FORK cluster
    cl <- parallel::makeForkCluster(n_threads)
    doParallel::registerDoParallel(cl)
  } else {
    ### PSOCK cluster
    cl <- parallel::makePSOCKCluster(n_threads)
    parallel::clusterExport(cl, c(
      "output_dir", "spp_summary_data", "spp_habitat_data",
      "elevation_path", "habitat_path", "template_path",
      "x_path"
    ))
    parallel::clusterEvaLQ(cl, {
      library(terra)
      library(sf)
      x <- sf::read_sf(x_path)
      elevation_path <- terra::rast(elevation_data)
      habitat_data <- terra::rast(habitat_path)
      template_data <- terra::rast(template_path)
    })
    doParallel::registerDoParallel(cl)
  }
  ## main processing
  result <- plyr::ldply(
    .data = seq_len(nrow(x)),
    .progress = ifelse(
      isTRUE(verbose) && isTRUE(n_threads == 1), "text", "none"
    ),
    .parallel = isTRUE(n_threads > 1),
    .fun = function(i) {
      # initialization
      curr_spp_id <- x$aoh_id[i]
      curr_spp_id_no <- x$id_no[i]
      curr_spp_habitat_row <- which(spp_habitat_data$aoh_id == curr_spp_id)[1]
      curr_spp_summary_row <- which(spp_summary_data$id_no == curr_spp_id_no)[1]
      curr_spp_habitat_codes <- spp_habitat_data$code[curr_spp_habitat_row]
      curr_spp_lower_elevation <-
        spp_summary_data$elevation_lower[curr_spp_summary_row]
      curr_spp_upper_elevation <-
        spp_summary_data$elevation_upper[curr_spp_summary_row]
      # prepare file paths
      curr_tmp_dir <- tempfile()
      dir.create(curr_tmp_dir, showWarnings = FALSE, recursive = TRUE)
      curr_tmp_path <- file.path(curr_tmp_dir, "aoh.tif")
      # generate bounding box for species range
      curr_spp_extent <- sf::st_bbox(x[i, , drop = FALSE])
      curr_spp_extent <- terra::ext(
        xmin = curr_spp_extent$xmin,
        xmax = curr_spp_extent$xmin,
        ymin = curr_spp_extent$ymin,
        ymax = curr_spp_extent$ymax,
      )
      # calculate habitat availability within bounding box of range
      if (length(curr_spp_habitat_codes) == 0) {
        ## no suitable habitat with just create empty raster
        curr_spp_habitat_data <- terra::crop(
          template_data,
          curr_spp_extent
        ) * 0
      } else {
        ## calculate total sum of habitat based on codes
        curr_spp_habitat_data <- sum(
          terra::crop(
            habitat_data[[curr_spp_habitat_codes]],
            terra::ext(curr_spp_data)
          )
        )
        ## apply altitudinal limits (if needed)
        if (is.finite(curr_spp_lower_elevation) ||
            is.finite(curr_spp_upper_elevation)) {
          ## if lower or upper limits not available, set as -Inf/Inf
          if (!is.finite(curr_spp_lower_elevation)) {
            curr_spp_lower_elevation <- -Inf
          }
          if (!is.finite(curr_spp_upper_elevation)) {
            curr_spp_upper_elevation <- Inf
          }
          ## create altitudinal mask
          curr_elev_mask <- terra::crop(
            x = elevation_data,
            y = terra::ext(curr_spp_data)
          )
          curr_elev_mask <- terra::clamp(
            curr_elev_mask,
            lower = curr_spp_lower_elevation,
            upper = curr_spp_upper_elevation,
            values = FALSE
          )
          ## apply altitudinal mask
          curr_spp_habitat_data <- terra::mask(
            x = curr_spp_habitat_data,
            mask = curr_elev_mask,
            maskvalues = NA,
            updatevalue = NA
          )
        }
      }
      # save raster to disk
      terra::writeRaster(
        x = curr_spp_habitat_data,
        filename = curr_tmp_path,
        overwrite = TRUE,
        gdal = "COMPRESS=DEFLATE",
        NAflag = -9999
      )
      # apply mask to habitat raster based on range data
      sf::gdal_utils(
        util = "rasterize",
        source = gpkg_path,
        destination = curr_tmp_path,
        quiet = TRUE,
        options = c(
          "-burn", "-9999",
          "-i",
          "-where" = paste0("id='", curr_spp_id, "'")
        )
      )
      # copy result to output folder
      aoh_path <- file.path(output_dir, curr_spp_id)
      file.copy(curr_tmp_dir, aoh_path)
      # clean up
      unlink(curr_tmp_dir, recursive = TRUE, force = TRUE)
      rm(curr_spp_habitat_data, curr_elev_mask)
      # return results
      aoh_path
  })
  ## post-processing
  doParallel::stopImplicitCluster()
  parallel::stopCluster(cl)

  # return table with metadata
  out <- sf::st_drop_geometry(x)
  out$path <- vapply(result, `[[`, character(1), 1)
  out
}
