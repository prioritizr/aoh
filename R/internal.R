#' Convert seasonal name to identifier
#'
#' Convert the name of a seasonal distribution to an integer identifier.
#'
#' @param x `character` Vector containing the names of seasonal distributions.
#'
#' @details
#' Seasonal distributions are encoded as integer identifiers using the
#' following scheme:
#' \describe{
#' \item{Resident:}{assigned a value of 1.}
#' \item{Breeding Season:}{assigned a value of 2.}
#' \item{Non-breeding Season:}{assigned a value of 3.}
#' \item{Passage:}{assigned a value of 4.}
#' \item{Seasonal Occurrence Uncertain:}{assigned a value of 5.}
#' }
#'
#' @return An `integer`vector containing identifiers.
#'
#' @examples
#' # specify seasonal names
#' x <- c(
#'   "Resident", "Breeding Season", "Non-breeding Season", "Passage",
#'   "Seasonal Occurrence Uncertain"
#' )
#'
#' # convert to identifiers
#' y <- convert_to_seasonal_id(x)
#'
#' # print result
#' print(y)
#' @noRd
convert_to_seasonal_id <- function(x) {
  # assert arguments are valid
  assertthat::assert_that(
    is.character(x),
    length(x) > 0
  )
  # standardize characters
  x <- tolower(x)
  # convert characters
  ## accounting for potential capitalization and extra punctuation
  out <- rep(5L, length(x))
  out[grepl("resident", x)] <- 1L
  out[!grepl("non", x) & grepl("breed", x)] <- 2L
  out[grepl("non", x)] <- 3L
  out[grep("passage", x)] <- 4L
  out[is.na(x)] <- NA_integer_
  # return result
  out
}
