#' @include internal.R
NULL

#' Default IUCN Red List Habitat Classification Scheme omission codes
#'
#' The [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/)
#' provides a
#' [habitat classification scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
#' for different habitat types worldwide.
#' Under this scheme, each habitat type is assigned to a code.
#' This function returns the
#' codes for habitat types that correspond to artificial, unknown,
#' miscellaneous, or invasive vegetation habitat types that should be omitted
#' from Area of Habitat calculations.
#'
#' @return A `character` vector of habitat codes.
#'
#' @examples
#' # print omission codes
#' print(default_omit_iucn_habitat_codes())
#' @export
default_omit_iucn_habitat_codes <- function() {
  # import codes conversion table
  code_data <- habitat_code_data()
  # return codes
  as.character(code_data$iucn_code[code_data$omit_from_aoh])
}
