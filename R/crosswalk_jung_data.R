#' @include internal.R
NULL

#' Crosswalk data for Jung *et al* 2020
#'
#' The [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/)
#' provides a
#' [habitat classification scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
#' for different habitat types.
#' To map the spatial location of each of these habitat classes,
#' Jung *et al.* (2020a) developed a global raster dataset by combining
#' a range of different land use and land cover data (Jung *et al.* 2020b).
#' This dataset provides a crosswalk table to associate the values in the
#' raster dataset with IUCN habitat classes.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @name crosswalk_jung_data
#'
#' @usage data(crosswalk_jung_data)
#'
#' @format
#' A data frame ([tibble::tibble()]) object with 126 rows and 2 columns.
#' Each row corresponds to a different IUCN habitat class.
#' It has the following columns:
#' \describe{
#' \item{code}{The `character` code for a given IUCN habitat class.}
#' \item{value}{The `numeric` value assigned to grid cells in the
#'  raster data that contain the IUCN habitat class
#'  (see [get_global_habitat_data()]).}
#' }
#'
#' @source
#' The data were obtained from \url{https://doi.org/10.5281/zenodo.3666245}
#' (Jung *et al.* 2020b).
#'
#' @seealso
#' A preprocessed version of the habitat classification data can be imported
#' using [get_global_habitat_data()].
#'
#' @inherit get_global_habitat_data references
#'
#' @aliases crosswalk_jung_data
#'
#' @examples
#' # load data
#' data(crosswalk_jung_data)
#'
#' # print data
#' print(crosswalk_jung_data)
NULL
