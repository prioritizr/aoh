#' @include internal.R
NULL

#' Crosswalk data for Jung (2020) potential habitat classification (level 1)
#'
#' The [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/)
#' provides a
#' [habitat classification scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
#' for different habitat types.
#' To map the spatial location of where restoration actions might produce
#' these habitat classes,
#' Jung (2020) developed a global raster dataset based on
#' potential vegetation data.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @name crosswalk_jung_plvl1_data
#'
#' @usage data(crosswalk_jung_plvl1_data)
#'
#' @format
#' A data frame ([tibble::tibble()]) object with
#' `r nrow(crosswalk_jung_plvl1_data)` rows and
#' `r ncol(crosswalk_jung_plvl1_data)` columns.
#' Each row corresponds to a different IUCN habitat class.
#' It has the following columns:
#' \describe{
#' \item{code}{The `character` code for a given IUCN habitat class.}
#' \item{value}{The `numeric` value assigned to grid cells in the
#'  raster data that contain the IUCN habitat class
#'  (see [get_jung_plvl1_habitat_data()]).}
#' }
#'
#' @source
#' The data were obtained from Jung (2020).
#'
#' @seealso
#' A preprocessed version of the habitat classification data can be imported
#' using [get_jung_plvl1_habitat_data()].
#'
#' @inherit get_jung_plvl1_habitat_data references
#'
#' @aliases crosswalk_jung_plvl1_data
#'
#' @examples
#' # load data
#' data(crosswalk_jung_plvl1_data)
#'
#' # print data
#' print(crosswalk_jung_plvl1_data)
NULL
