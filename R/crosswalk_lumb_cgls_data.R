#' @include internal.R
NULL

#' Crosswalk data for CGLS based on Lumbierres *et al.* (2021)
#'
#' The [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/)
#' provides a
#' [habitat classification scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
#' for different habitat types.
#' To map the spatial location of each of these habitat classes,
#' Lumbierres *et al.* (2021) developed a global habitat classification dataset
#' using the Copernicus Global Land Service Land Cover
#' (CGLS-LC100) dataset (Buchhorn *et al.*, 2019; Buchhorn *et al.*, 20200).
#' This dataset provides a crosswalk table to associate the values in the
#' raster dataset with IUCN habitat classes.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @name crosswalk_lumb_cgls_data
#'
#' @usage data(crosswalk_lumb_cgls_data)
#'
#' @format
#' A data frame ([tibble::tibble()]) object with
#' `r nrow(crosswalk_lumb_cgls_data)` rows and
#' `r ncol(crosswalk_lumb_cgls_data)` columns.
#' Each row corresponds to a different IUCN habitat class.
#' It has the following columns:
#' \describe{
#' \item{code}{The `character` code for a given IUCN habitat class.}
#' \item{value}{The `numeric` value assigned to grid cells in the
#'  raster data that contain the IUCN habitat class
#'  (see [get_lumb_cgls_habitat_data()]).}
#' }
#'
#' @source
#' The data were obtained from \url{https://doi.org/10.5281/zenodo.5146072}
#' (Lumbierres 2021).
#'
#' @seealso
#' A preprocessed version of the habitat classification data can be imported
#' using [get_lumb_cgls_habitat_data()].
#'
#' @references
#' Buchhorn M, Smets B, Bertels L, Lesiv M, Tsendbazar N-E, Herold M, and Fritz
#' SA (2019) Copernicus Global Land Service: Land Cover 100m:
#' Epoch 2015: Globe. *Dataset of the global component of the Copernicus Land
#' Monitoring Service*.
#' Available at <https://doi.org/10.5281/zenodo.3243508>.
#'
#' Buchhorn M, Smets B, Bertels L, de Roo B, Lesiv M, Tsendbazar N-E, Linlin
#' L, and Tarko A (2020) *Copernicus Global Land Service: Land Cover
#' 100m: Version 3 Globe 2015–2019: Product User Manual*. Geneve: Zenodo
#'
#' Lumbierres, M (2021). Map of habitat classes (Level 1) from the IUCN
#' Habitat. *Zenodo Digital Repository*.
#' Available at <https://doi.org/10.5281/zenodo.5146072>.
#'
#' Lumbierres M, Dahal PR, Di Marco M, Butchart SHM, Donald PF, and
#' Rondinini C (2021) Translating habitat class to land cover to map area of
#' habitat of terrestrial vertebrates. *Conservation Biology*, In press,
#' DOI:10.1111/cobi.13851.
#' Available at <https://doi.org/10.1111/cobi.13851>.
#'
#' @inherit get_lumb_cgls_habitat_data references
#'
#' @aliases crosswalk_lumb_cgls_data
#'
#' @examples
#' # load data
#' data(crosswalk_lumb_cgls_data)
#'
#' # print data
#' print(crosswalk_lumb_cgls_data)
NULL
