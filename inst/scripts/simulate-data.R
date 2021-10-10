# Initialization
## define variables
n_spp <- 30
country_names <- c("spain", "portugal")
habitat_version <- "10.5281/zenodo.3816946"
cache_dir <- rappdirs::user_data_dir("aoh")

## load packages
devtools::load_all()
library(sf)
library(dplyr)
library(terra)

# Preliminary processing
## install dependencies if needed
if (!require(rnaturalearthhires)) {
  remotes::install_github("ropensci/rnaturalearthhires")
}

## import data
global_habitat_data <- get_global_habitat_data(
  version = habitat_version, dir = cache_dir
)
global_elevation_data <- get_global_elevation_data(dir = cache_dir)

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
  dplyr::select(id)

## create habitat data based on spatial boundary
sim_habitat_data <-
  global_habitat_data %>%
  terra::crop(
    snap = "out",
    y = {
      sim_boundary_data %>%
      sf::st_bbox() %>%
      sf::st_as_sfc() %>%
      sf::st_transform(terra_st_crs(global_habitat_data)) %>%
      sf::st_as_sf() %>%
      sf_terra_ext()
    }
  )
idx <- terra::global(sim_habitat_data, "max", na.rm = TRUE)[[1]]
sim_habitat_data <- sim_habitat_data[[which(is.finite(idx))]]

## create elevation data based on spatial boundary
sim_elevation_data <-
  global_elevation_data %>%
  terra::crop(
    snap = "out",
    y = {
      sim_boundary_data %>%
      sf::st_bbox() %>%
      sf::st_as_sfc() %>%
      sf::st_transform(terra_st_crs(global_elevation_data)) %>%
      sf::st_as_sf() %>%
      sf_terra_ext()
    }
  )

# Exports
## save boundary data
sf::write_sf(
  sim_boundary_data, "inst/extdata/sim_boundary_data.gpkg", overwrite = TRUE
)

## save habitat data
terra::writeRaster(
  sim_elevation_data, "inst/extdata/sim_elevation_data.tif", overwrite = TRUE
)

## save elevation data
terra::writeRaster(
  sim_habitat_data, "inst/extdata/sim_habitat_data.tif",
  names = names(sim_habitat_data), overwrite = TRUE, datatype = "INT2U"
)

## save range data
# TODO

## save habitat data
# TODO

## save summary data
# TODO
