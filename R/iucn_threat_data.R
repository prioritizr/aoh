#' @include internal.R
NULL

#' IUCN threat classification codes
#'
#' The [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/)
#' provides a
#' [threat classification scheme](https://www.iucnredlist.org/resources/threat-classification-scheme)
#' for different threatening processes.
#' This dataset provides details on the different IUCN threat classes.
#'
#' @docType data
#'
#' @keywords datasets
#'
#' @name iucn_threat_data
#'
#' @usage data(iucn_threat_data)
#'
#' @format A data frame with 130 rows and 2 columns.
#' Each row corresponds to a different threat class, and each
#' column contains information about a given threat class.
#' It contains columns with the following values for each threat class.
#' \describe{
#' \item{code}{A `character` value indicating the code of a class.}
#' \item{name}{A `character` value indicating the name of a class.}
#' }
#'
#' @source
#' The data were obtained from \url{https://www.iucnredlist.org/resources/threat-classification-scheme}.
#'
#' @aliases iucn_threat_data
#'
#' @examples
#' # load data
#' data(iucn_threat_data)
#'
#' # print data
#' print(iucn_threat_data)
NULL
