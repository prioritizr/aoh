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

crosswalk_jung_lvl1_data <- readr::read_csv(
  "data-raw/crosswalk-jung-lvl1-data.csv",
  show_col_types = FALSE,
  progress = FALSE,
  col_types = readr::cols(
    code = readr::col_character(),
    value = readr::col_integer()
  )
)

crosswalk_jung_plvl1_data <- crosswalk_jung_lvl1_data

crosswalk_jung_lvl2_data <- readr::read_csv(
  "data-raw/crosswalk-jung-lvl2-data.csv",
  show_col_types = FALSE,
  progress = FALSE,
  col_types = readr::cols(
    code = readr::col_character(),
    value = readr::col_integer()
  )
)

crosswalk_lumb_cgls_data <- readr::read_csv(
  "data-raw/crosswalk-lumb-cgls-data.csv",
  show_col_types = FALSE,
  progress = FALSE,
  col_types = readr::cols(
    code = readr::col_character(),
    value = readr::col_integer()
  )
)

# Exports
usethis::use_data(iucn_habitat_data, overwrite = TRUE)
usethis::use_data(crosswalk_jung_lvl1_data, overwrite = TRUE)
usethis::use_data(crosswalk_jung_lvl2_data, overwrite = TRUE)
usethis::use_data(crosswalk_jung_plvl1_data, overwrite = TRUE)
usethis::use_data(crosswalk_lumb_cgls_data, overwrite = TRUE)
