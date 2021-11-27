#' @include internal.R
NULL

#' IUCN habitat classification codes
#'
#' The [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/)
#' provides a
#' [habitat classification scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
#' for different habitat types worldwide.
#' This dataset provides details on the different IUCN habitat classes.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @name iucn_habitat_data
#'
#' @usage data(iucn_habitat_data)
#'
#' @format A data frame with 126 rows and 2 columns.
#' Each row corresponds to a different habitat class, and each
#' column contains information about a given habitat class.
#' It contains columns with the following values for each habitat class.
#' \describe{
#' \item{code}{A `character` value indicating the code of a class.}
#' \item{name}{A `character` value indicating the name of a class.}
#' \item{is_terrestrial}{A `logical` value indicating if the class
#'   occurs within the terrestrial environmental.}
#' \item{is_marine}{A `logical` value indicating if the class
#'  occurs within the marine environmental.}
#' \item{is_artificial}{A `logical` value indicating if the class
#'  is artificial (e.g. anthropogenically human modified).}
#' \item{is_misc}{A `logical` value indicating if the class
#'  does not correspond to a specific land cover.}
#' \item{is_introduced}{A `logical` value indicating if the class
#'  corresponds to introduced vegetation.}
#' }
#'
#' @source
#' The data were obtained from \url{https://www.iucnredlist.org/resources/habitat-classification-scheme}.
#'
#' @aliases iucn_habitat_data
#'
#' @examples
#' # load data
#' data(iucn_habitat_data)
#'
#' # print data
#' print(iucn_habitat_data)
NULL
