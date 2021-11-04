#' @include internal.R convert_filename_to_habitat_code.R
NULL

#' Get global habitat classification data
#'
#' Import habitat classification data produced by Jung *et al.* (2020a).
#' If data are not available locally, they are downloaded from
#' an online repository.
#'
#' @param dir `character` folder path for downloading and caching data.
#'  By default, data are downloaded to a temporary directory (i.e. `tempdir()`).
#'  **To avoid downloading the same data multiple times, it is strongly
#'  recommended to specify a persistent storage location (see Examples below).**
#'
#' @param version `character` indicating the specific version of the dataset
#'  that should be downloaded. The version should be indicated using the
#'  Digital Object Identifier of the specific version required (e.g.
#'  `"10.5281/zenodo.3673586"`).
#'  Defaults to `"latest"` such that the latest
#'  release of the dataset with available habitat data is used.
#'
#' @param preprocessed `logical` Should a pre-processed version of the
#'   dataset be downloaded?
#'   If so, the data are downloaded
#'   from a GitHub repository
#'   (<https://github.com/prioritizr/aoh/releases/tag/data>).
#'   Otherwise, data are downloaded from original
#'   Zenodo Digital Repository (Jung *et al.* 2020b).
#'
#' @param force `logical` should the data be downloaded even if the
#'  the data are already available?
#'  Defaults to `FALSE`.
#'
#' @param verbose `logical` should progress be displayed while downloading
#'  data?
#'  Defaults to `TRUE`.
#'
#' @details
#' The preprocessed version of the habitat data was produced by
#' version was produced by projecting the data to conform
#' with the default template raster for processing Area of Habitat data
#' (see [get_world_behrmann_1km_rast()]).
#' Additionally, data were clamped to ensure values range between
#' valid values (i.e. from 0 to 1000).
#' The preprocessed version of the habitat data can be help
#' run time when creating the Area of Habitat data with
#' the default template raster.
#'
#' @return A [terra::rast()] object containing the level 2 habitat
#'  fractional coverage data.
#'  These data are available at the 1 km \eqn{\times} 1 km resolution.
#'  Each layer corresponds to a different habitat type, and each pixel
#'  denotes the fraction of the pixel that contains a given habitat type.
#'  **Note that pixel values are scaled to between 0 and 1000, such that
#'  a value of 0 indicates 0% coverage of a habitat type, and a value of
#'  1000 indicates 100% coverage of a habitat type.**
#'
#' @references
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
#' # define persistent storage location
#' download_dir <- rappdirs::user_data_dir("aoh")
#'
#' # create download directory if needed
#' if (!file.exists(download_dir)) {
#'   dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
#' }
#'
#' # download and import habitat data
#' habitat_data <- get_global_habitat_data(download_dir, version = "latest")
#'
#' # preview data
#' print(habitat_data)
#'
#' # plot data
#' plot(habitat_data)
#' }
#' @export
get_global_habitat_data <- function(dir = tempdir(), version = "latest",
                                    preprocessed = TRUE,
                                    force = FALSE, verbose = TRUE) {
  # get data
  if (isTRUE(preprocessed)) {
    r <- get_prep_global_habitat_data(
      dir = dir, version = version, force = force, verbose = verbose
    )
  } else {
    r <- get_raw_global_habitat_data(
      dir = dir, version = version, force = force, verbose = verbose
    )
  }

  # return result
  r

}
