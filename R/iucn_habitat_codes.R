#' @include internal.R
NULL

#' Artificial IUCN Red List habitat codes
#'
#' The [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/)
#' provides a
#' [habitat classification scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
#' for different habitat types worldwide.
#' This function returns codes for habitat types that correspond to *artificial*
#' areas (e.g. urban areas, pasture lands, canals).
#'
#' @return A `character` vector of codes.
#'
#' @family codes
#'
#' @examples
#' # print codes
#' print(iucn_habitat_codes_artificial())
#'
#' @export
iucn_habitat_codes_artificial <- function() {
  # import codes conversion table
  code_data <- habitat_code_data()
  # return codes
  as.character(code_data$iucn_code[code_data$is_artificial])
}

#' Miscellaneous IUCN Red List habitat codes
#'
#' The [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/)
#' provides a
#' [habitat classification scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
#' for different habitat types worldwide.
#' This function returns codes for habitat classes that correspond to
#' *unknown* or *other* habitats.
#'
#' @inherit iucn_habitat_codes_artificial return
#'
#' @family iucn_habitat_codes
#'
#' @examples
#' # print codes
#' print(iucn_habitat_codes_misc())
#'
#' @export
iucn_habitat_codes_misc <- function() {
  # import codes conversion table
  code_data <- habitat_code_data()
  # return codes
  as.character(code_data$iucn_code[code_data$is_misc])
}

#' Introduced IUCN Red List habitat codes
#'
#' The [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/)
#' provides a
#' [habitat classification scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
#' for different habitat types worldwide.
#' This function returns codes for habitat classes that correspond to
#' *introduced vegetation*.
#'
#' @inherit iucn_habitat_codes_artificial return
#'
#' @family codes
#'
#' @examples
#' # print codes
#' print(iucn_habitat_codes_introduced())
#'
#' @export
iucn_habitat_codes_introduced <- function() {
  # import codes conversion table
  code_data <- habitat_code_data()
  # return codes
  as.character(code_data$iucn_code[code_data$is_introduced])
}

#' Terrestrial IUCN Red List habitat codes
#'
#' The [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/)
#' provides a
#' [habitat classification scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
#' for different habitat types worldwide.
#' This function returns codes for habitat classes that occur on land.
#'
#' @inherit iucn_habitat_codes_artificial return
#'
#' @family codes
#'
#' @examples
#' # print codes
#' print(iucn_habitat_codes_terrestrial())
#'
#' @export
iucn_habitat_codes_terrestrial <- function() {
  # import codes conversion table
  code_data <- habitat_code_data()
  # return codes
  as.character(code_data$iucn_code[code_data$is_terrestrial])
}

#' Marine IUCN Red List habitat codes
#'
#' The [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/)
#' provides a
#' [habitat classification scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
#' for different habitat types worldwide.
#' This function returns codes for habitat classes that occur in the ocean.
#'
#' @inherit iucn_habitat_codes_artificial return
#'
#' @family codes
#'
#' @examples
#' # print codes
#' print(iucn_habitat_codes_marine())
#'
#' @export
iucn_habitat_codes_marine <- function() {
  # import codes conversion table
  code_data <- habitat_code_data()
  # return codes
  as.character(code_data$iucn_code[code_data$is_marine])
}
