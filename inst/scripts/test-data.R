# Initialization
## define variables
n_spp <- 10
country_names <- c("spain", "portugal")
habitat_version <- "10.5281/zenodo.4058819"
elevation_version <- "10.5281/zenodo.5719984"
cache_dir <- rappdirs::user_data_dir("aoh")

## load packages
devtools::load_all()
library(sf)
library(dplyr)
library(terra)

# set rng state
set.seed(501)

# Preliminary processing
## install dependencies if needed
if (!require(rnaturalearthhires)) {
  remotes::install_github("ropensci/rnaturalearthhires")
}

## import data
global_habitat_data <- get_jung_habitat_data(
  dir = cache_dir, version = habitat_version,
)
global_elevation_data <- get_global_elevation_data(
  dir = cache_dir, version = elevation_version
)

# Main processing
## simulate spatial boundary based on Portugal and Spain borders
sim_boundary_data <-
  rnaturalearth::ne_countries(scale = 10, country = country_names) %>%
  sf::st_as_sf() %>%
  sf::st_union() %>%
  sf::st_cast("POLYGON") %>%
  sf::st_as_sf(id = 1) %>%
  dplyr::mutate(area = sf::st_area(.)) %>%
  dplyr::arrange(dplyr::desc(area)) %>%
  dplyr::filter(seq_along(area) == 1) %>%
  dplyr::select(id) %>%
  sf::st_set_precision(1500) %>%
  sf::st_make_valid() %>%
  dplyr::filter(!sf::st_is_empty(.)) %>%
  {suppressWarnings(sf::st_collection_extract(., "POLYGON"))}

## create habitat data based on spatial boundary
sim_habitat_data <-
  global_habitat_data %>%
  terra::crop(
    snap = "out",
    y = {
      sim_boundary_data %>%
      sf::st_buffer(10000) %>%
      sf::st_bbox() %>%
      sf::st_as_sfc() %>%
      sf::st_transform(terra_st_crs(global_habitat_data)) %>%
      sf::st_as_sf() %>%
      sf_terra_ext()
    }
  )

## create elevation data based on spatial boundary
sim_elevation_data <-
  global_elevation_data %>%
  terra::crop(
    snap = "out",
    y = {
      sim_boundary_data %>%
      sf::st_buffer(10000) %>%
      sf::st_bbox() %>%
      sf::st_as_sfc() %>%
      sf::st_transform(terra_st_crs(global_elevation_data)) %>%
      sf::st_as_sf() %>%
      sf_terra_ext()
    }
  )

## aggregate data to 1 x 1 km
sim_habitat_data <- terra::aggregate(
  sim_habitat_data, fact = 10, fun = "modal"
)
sim_elevation_data <- terra::aggregate(
  sim_elevation_data, fact = 10, fun = "mean"
)

## identify codes to use for test data
## these are codes present in both the Jung and Lumbierres crosswalks
include_codes <- intersect(
  crosswalk_jung_data$code,
  crosswalk_lumbierres_data$code
)
omit_codes <- setdiff(iucn_habitat_data$code, include_codes)
omit_codes <- unique(c(omit_codes, iucn_habitat_codes_marine()))

## simulate data
sim_data <- simulate_spp_data(
  n = n_spp,
  boundary_data = sim_boundary_data,
  habitat_data = sim_habitat_data,
  elevation_data = sim_elevation_data,
  crosswalk_data = crosswalk_jung_data,
  omit_habitat_codes = omit_codes
)

## verify migratory species present
assertthat::assert_that(
  sum(sim_data$spp_range_data$seasonal == 2) > 0,
  msg = "simulated data does not contain any migratory species"
)

# Exports
## save boundary data
sf::write_sf(
  sim_boundary_data, "inst/testdata/sim_boundary_data.gpkg",
  overwrite = TRUE
)

## save habitat data
terra::writeRaster(
  sim_elevation_data, "inst/testdata/sim_elevation_data.tif",
   overwrite = TRUE,  datatype = "INT2S"
)

## save elevation data
terra::writeRaster(
  sim_habitat_data, "inst/testdata/sim_habitat_data.tif",
  overwrite = TRUE, datatype = "INT2U"
)

## save range data
temp_dir <- tempfile()
dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
sf::write_sf(
  sim_data$spp_range_data,
  file.path(temp_dir, "SIMULATED_SPECIES.shp")
)
zip_path <- file.path(getwd(), "inst/testdata/SIMULATED_SPECIES.zip")
if (file.exists(zip_path)) unlink(zip_path, force = TRUE)
withr::with_dir(
  temp_dir,
  utils::zip(
    zipfile = zip_path,
    files = dir(temp_dir)
  )
)

## save habitat data
write.table(
  sim_data$spp_habitat_data,
  "inst/testdata/sim_spp_habitat_data.csv",
  quote = TRUE,
  sep = ",",
  row.names = FALSE
)

## save summary data
write.table(
  sim_data$spp_summary_data,
  "inst/testdata/sim_spp_summary_data.csv",
  quote = TRUE,
  sep = ",",
  row.names = FALSE
)

## display message
message("Done!")
