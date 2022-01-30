#' @include internal.R
NULL

#' Crosswalk data for Jung *et al.* (2020) (level 1 classification)
#'
#' The [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/)
#' provides a
#' [habitat classification scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
#' for different habitat types.
#' To map the spatial location of each of these habitat classes,
#' Jung *et al.* (2020a) developed a global raster dataset by combining
#' a range of different land use and land cover data (Jung *et al.* 2020b).
#' They created two habitat classification schemes -- termed level 1 and
#' level 2 classification schemes -- to describe coarse-scale and
#' fine-scale differences in habitat classes.
#' This dataset provides a crosswalk table to associate the values in the
#' level 1 raster dataset with IUCN habitat classes.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @name crosswalk_jung_lvl1_data
#'
#' @usage data(crosswalk_jung_lvl1_data)
#'
#' @format
#' A data frame ([tibble::tibble()]) object with
#' `r nrow(crosswalk_jung_lvl1_data)` rows and
#' `r ncol(crosswalk_jung_lvl1_data)` columns.
#' Each row corresponds to a different IUCN habitat class.
#' It has the following columns:
#' \describe{
#' \item{code}{The `character` code for a given IUCN habitat class.}
#' \item{value}{The `numeric` value assigned to grid cells in the
#'  raster data that contain the IUCN habitat class
#'  (see [get_jung_lvl1_habitat_data()]).}
#' }
#'
#' @source
#' The data were obtained from \url{https://doi.org/10.5281/zenodo.3666245}
#' (Jung *et al.* 2020b).
#'
#' @seealso
#' A preprocessed version of the habitat classification data can be imported
#' using [get_jung_lvl1_habitat_data()].
#'
#' @inherit get_jung_lvl1_habitat_data references
#'
#' @aliases crosswalk_jung_lvl1_data
#'
#' @examples
#' # load data
#' data(crosswalk_jung_lvl1_data)
#'
#' # print data
#' print(crosswalk_jung_lvl1_data)
NULL
