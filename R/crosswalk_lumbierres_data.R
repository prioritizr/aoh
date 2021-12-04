#' @include internal.R
NULL

#' Crosswalk data for Lumbierres *et al.* (2021)
#'
#' The [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/)
#' provides a
#' [habitat classification scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
#' for different habitat types.
#' To map the spatial location of each of these habitat classes,
#' Lumbierres *et al.* (2021) developed a global habitat classification dataset
#' using the Copernicus Global Land Service Land Cover
#' (CGLS-LC100) dataset (Buchhorn *et al.*, 2020; Buchhorn *et al.*, 2019).
#' This dataset provides a crosswalk table to associate the values in the
#' raster dataset with IUCN habitat classes.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @name crosswalk_lumbierres_data
#'
#' @usage data(crosswalk_lumbierres_data)
#'
#' @format
#' A data frame ([tibble::tibble()]) object with
#' `r nrow(crosswalk_lumbierres_data)` rows and
#' `r ncol(crosswalk_lumbierres_data)` columns.
#' Each row corresponds to a different IUCN habitat class.
#' It has the following columns:
#' \describe{
#' \item{code}{The `character` code for a given IUCN habitat class.}
#' \item{value}{The `numeric` value assigned to grid cells in the
#'  raster data that contain the IUCN habitat class
#'  (see [get_lumbierres_habitat_data()]).}
#' }
#'
#' @source
#' The data were obtained from \url{https://doi.org/10.5281/zenodo.5146072}
#' (Lumbierres 2021).
#'
#' @seealso
#' A preprocessed version of the habitat classification data can be imported
#' using [get_lumbierres_habitat_data()].
#'
#' @inherit get_lumbierres_habitat_data references
#'
#' @aliases crosswalk_lumbierres_data
#'
#' @examples
#' # load data
#' data(crosswalk_lumbierres_data)
#'
#' # print data
#' print(crosswalk_lumbierres_data)
NULL
