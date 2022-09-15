#' @include internal.R
NULL

#' Get global elevation data
#'
#' Import elevation data derived from Robinson *et al.* (2014).
#' If data are not available locally, they are downloaded from
#' a Zenodo repository (\doi{10.5281/zenodo.5719984}).
#'
#' @inheritParams get_jung_lvl2_habitat_data
#'
#' @details
#' The data were produced by (i) obtaining raw elevation data from
#' [EarthEnv project](https://www.earthenv.org/DEM), (ii) collating
#' the data into a single raster (GeoTIFF) file, and then (iii)
#' reprojecting the data to the World Behrmannn coordinate reference
#' systems (ESRI:54017). For further details, see the
#' [online repository](https://github.com/prioritizr/dem) containing
#' code used to process the data.
#'
#' @return A [terra::rast()] object containing the elevation data
#'  (100 m resolution). Pixel values indicate elevation (m).
#'
#' @references
#' Robinson N, Regetz J, and Guralnick RP (2014) EarthEnv-DEM90: A nearly-
#' global, void-free, multi-scale smoothed, 90m digital elevation model from
#' fused ASTER and SRTM data.
#' *ISPRS Journal of Photogrammetry and Remote Sensing*, 87:57--67.
#' \doi{10.1016/j.isprsjprs.2013.11.002}
#'
#' @examples
#' \dontrun{
#' # define persistent storage location
#' download_dir <- rappdirs::user_data_dir("aoh")
#'
#' # create download directory if needed
#' if (!file.exists(download_dir)) {
#'   dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
#' }
#'
#' # download and import elevation data
#' elev_data <- get_global_elevation_data(download_dir)
#'
#' # preview data
#' print(elev_data)
#'
#' # plot data
#' plot(elev_data)
#' }
#' @export
get_global_elevation_data <- function(dir = tempdir(),
                                      version = "latest",
                                      force = FALSE,
                                      verbose = TRUE) {
  # get data
  path <- get_zenodo_data(
    x = "10.5281/zenodo.5719984",
    file = "dem-100m-esri54017.tif",
    version = version,
    dir = dir,
    force = force,
    verbose = verbose
  )

  # return data
  terra::rast(path)
}
