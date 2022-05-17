#' @include internal.R clean_spp_range_data.R
#' @include get_global_elevation_data.R get_lumbierres_habitat_data.R
#' @include get_spp_habitat_data.R get_spp_summary_data.R
#' @include misc_terra.R misc_sf.R
NULL

#' Create fractional coverage data
#'
#' Create fractional coverage data to describe species' Area of Habitat (AOH)
#' across large spatial scales. Briefly, this function
#' creates Area of Habitat data for each seasonal distribution of each species,
#' overlays the Area of Habitat data with a spatial grid, computes
#' the proportion of suitable habitat available within each grid cell
#' (for each species separately), and stores the results as raster files
#' on disk. To reduce data storage requirements,
#' Area of Habitat data are automatically deleted during processing.
#' Please note that these procedures are designed for terrestrial species,
#' and will not apply to marine or freshwater species.
#'
#' @inheritParams create_spp_aoh_data
#'
#' @param res `numeric` Resolution for computing fractional coverage.
#'   Note that the argument to `res` must be a factor of the
#'   the resolution of the underlying Area of Habitat data.
#'   For example, a value of 5000 would be a valid argument
#'   if the underlying data had a resolution of 100 m.
#'
#' @param output_dir `character` Folder path to save raster (GeoTIFF) files
#'   containing the fractional coverage data.
#'
#' @inheritSection create_spp_aoh_data Engines
#'
#' @section Data processing:
#' The fractional coverage data generated using the following procedures.
#' After these data are generated, they stored as files on disk
#' (see Output file format section for details).
#'
#' \enumerate{
#'
#' `r readLines("man/fragments/aoh-steps.Rd")`
#'
#' \item A [terra::rast()] object is created to define
#'  a standardized grid for calculating fractional coverage data.
#'  Specifically, the grid is created by aggregating the
#'  habitat data (per argument to `habitat_data`) to the specified
#'  resolution (per argument to `res`).
#'
#' \item The Area of Habitat data are used to compute fractional
#'   coverage data. Specifically, for each seasonal distribution
#'  of each species, the Area of Habitat data are overlaid with the standardized
#'   grid to calculate the proportion of each grid cell that contains
#'   suitable habitat.
#'
#' \item Post-processing routines are used to prepare the results.
#'   These routines involve updating the collated species data to include
#'   file names and spatial metadata for the fractional coverage data.
#' }
#'
#' @section Output file format:
#' Fractional coverage data are stored in a separate raster (GeoTIFF) file for
#' each seasonal distribution of each species. Each raster file is assigned a
#' file name based on a prefix and a combination of the species' taxon
#' identifier
#' (per `id_no`/`SISID` column in `x`) and the identifier for the seasonal
#' distribution (per `seasonality` in `x`)
#' (i.e., file names are named according to `FRC_{$id_no}_${seasonality}.tif`).
#' For a given raster file, grid cell values denote the proportion of suitable
#' habitat located within each cell.
#' For example, a value of 0 corresponds to 0% fractional coverage,
#' 0.5 to 50% fractional coverage, 1 to 100% fractional coverage.
#' Missing (`NA`) values correspond to
#' grid cells that are located entirely outside of the species' distribution.
#'
#' @inherit create_spp_aoh_data return references
#'
#' @seealso
#' This function is useful for creating fractional coverage data when
#' species' Area of Habitat data are already not available.
#' If you have previously generated species' Area of Habitat data,
#' you can use the [calc_spp_frc_data()] to use these Area of Habitat data
#' to calculate the fractional coverage data directly.
#'
#' @examples
#' \dontrun{
#' # find file path for example range data following IUCN Red List data format
#' ## N.B., the range data were not obtained from the IUCN Red List,
#' ## and were instead based on data from GBIF (https://www.gbif.org/)
#' path <- system.file("extdata", "EXAMPLE_SPECIES.zip", package = "aoh")
#'
#' # import data
#' spp_range_data <- read_spp_range_data(path)
#'
#' # specify settings for data processing
#' output_dir <- tempdir()                       # folder to save coverage data
#' cache_dir <- rappdirs::user_data_dir("aoh")   # persistent storage location
#' n_threads <- parallel::detectCores() - 1      # speed up analysis
#'
#' # create cache directory if needed
#' if (!file.exists(cache_dir)) {
#'   dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
#' }
#'
#' # create species' information data
#' spp_info_data <- create_spp_info_data(
#'   x = spp_range_data,
#'   cache_dir = cache_dir
#' )
#'
#' # create fractional coverage data
#' spp_frc_data <- create_spp_frc_data(
#'   x = spp_info_data,
#'   res = 5000,
#'   output_dir = output_dir,
#'   n_threads = n_threads,
#'   cache_dir = cache_dir
#' )
#' }
#'
#' @examplesIf interactive()
#' \dontrun{
#' # preview data
#' print(spp_frc_data)
#' }
#'
#' @examples
#' \dontrun{
#' # import fractional coverage data as a list of terra::rast() objects
#' spp_frc_rasters <- lapply(spp_frc_data$path, terra::rast)
#'
#' # print list of rasters
#' print(spp_frc_rasters)
#'
#' # plot the data to visualize the range maps and fractional coverage data
#' plot_spp_frc_data(spp_frc_data)
#' }
#' @export
create_spp_frc_data <- function(x,
                                output_dir,
                                res,
                                elevation_data = NULL,
                                habitat_data = NULL,
                                crosswalk_data = NULL,
                                cache_dir = tempdir(),
                                habitat_version = "latest",
                                elevation_version = "latest",
                                force = FALSE,
                                n_threads = 1,
                                cache_limit = 1000,
                                engine = "terra",
                                verbose = TRUE) {
  create_spp_data(
    x = x,
    res = res, ## N.B., this is to produce fractional coverage data
    output_dir = output_dir,
    elevation_data = elevation_data,
    habitat_data = habitat_data,
    crosswalk_data = crosswalk_data,
    cache_dir = cache_dir,
    habitat_version = habitat_version,
    elevation_version = elevation_version,
    force = force,
    n_threads = n_threads,
    cache_limit = cache_limit,
    engine = engine,
    verbose = verbose
  )
}
