#' @include internal.R
NULL

#' Convert filename to habitat code
#'
#' Identify IUCN Red List habitat classification codes for data
#' files distributed with the Jung *et al.* (2020) dataset.
#'
#' @param x `character` file name(s).
#'
#' @details
#' This function is designed to be used for processing habitat classification
#' data downloaded from Jung *et al.* (2020).
#'
#' @inherit get_habitat_data references
#'
#' @return `character` habitat classification codes.
#'
#' @noRd
convert_filename_to_habitat_code <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(x),
    assertthat::noNA(x),
    all(nchar(x) > 0),
    length(x) >= 0
  )
  # extract Jung et al. codes
  assertthat::assert_that(
    all(startsWith(x, "iucn_habitatclassification_fraction_lvl2")),
    msg = "file name(s) is not recognized"
  )
  x <- gsub("iucn_habitatclassification_fraction_lvl2", "", x, fixed = TRUE)
  x <- strsplit(x, "_", x, fixed = TRUE)
  x <- vapply(x, FUN.VALUE = character(1), function(x) {
    x[which(nchar(x) > 0)[1]]
  })
  # import codes conversion table
  code_data <- habitat_code_data()
  # convert to IUCN codes
  assertthat::assert_that(
    all(x %in% code_data$code),
    msg = "habitat classification code(s) not recognized"
  )
  code_data$iucn_code[match(x, code_data$code)]
}
