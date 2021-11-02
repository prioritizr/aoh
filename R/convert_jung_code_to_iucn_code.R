#' @include internal.R
NULL

#' Convert habitat codes
#'
#' Convert habitat classification codes used by the
#' Jung *et al.* (2020) dataset to those used by the IUCN Red List.
#'
#' @param x `character` Habitat classification codes.
#'
#' @inherit get_habitat_data references
#'
#' @return `character` habitat classification codes.
#'
#' @noRd
convert_jung_codes_to_iucn_codes <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(x),
    assertthat::noNA(x),
    all(nchar(x) > 0),
    length(x) >= 0
  )
  # import codes
  code_data <- habitat_code_data()
  # further validation
  assertthat::assert_that(
    all(x %in% code_data$code),
    msg = "habitat classification code(s) not recognized"
  )
  # return codes
  code_data$iucn_code[match(x, code_data$code)]
}
