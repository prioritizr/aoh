# Initialization
devtools::load_all()
library(readr)

# Import data
iucn_habitat_data <- readr::read_csv(
  "data-raw/iucn-habitat-data.csv",
  show_col_types = FALSE,
  progress = FALSE,
  col_types = readr::cols(
    code = readr::col_character(),
    name = readr::col_character(),
    is_terrestrial = readr::col_logical(),
    is_artificial = readr::col_logical(),
    is_misc = readr::col_logical(),
    is_introduced = readr::col_logical()
  )
)

crosswalk_jung_data <- readr::read_csv(
  "data-raw/crosswalk-jung-data.csv",
  show_col_types = FALSE,
  progress = FALSE,
  col_types = readr::cols(
    code = readr::col_character(),
    value = readr::col_integer()
  )
)

# Exports
usethis::use_data(iucn_habitat_data, overwrite = TRUE)
usethis::use_data(crosswalk_jung_data, overwrite = TRUE)
