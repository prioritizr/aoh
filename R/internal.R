# import habitat code metadata
habitat_code_data <- function() {
  readr::read_csv(
    system.file("extdata", "habitat-codes.csv", package = "aoh"),
    show_col_types = FALSE,
    progress = FALSE,
    col_types = readr::cols(
      iucn_code = readr::col_character(),
      name = readr::col_character(),
      code = readr::col_character(),
      omit_from_aoh = readr::col_logical(),
      terrestrial = readr::col_logical()
    )
  )
}
