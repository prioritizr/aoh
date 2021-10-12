# Initialization
## define variables
n_records_per_spp <- 5000
country_names <- c("portugal", "spain", "france")
spp_data <- tibble::tibble(
  binomial = c(
    "Calotriton asper",
    "Rana iberica",
    "Alytes dickhilleni",
    "Chioglossa lusitanica"
  ),
  taxon_id = c(59448, 58622, 979, 4657),
  terrestrial = c(TRUE, TRUE, TRUE, TRUE),
  freshwater = c(TRUE, TRUE, TRUE, TRUE),
  marine = c(FALSE, FALSE, FALSE, FALSE)
)

## load packages
devtools::load_all()
library(sf)
library(dplyr)
library(terra)
library(rgbif)
library(sfheaders)
library(rangemap)

## define functions
create_range_map <- function(x, boundary_data, binomial, taxon_id, terrestrial,
                             freshwater, marine) {
  ## create hull based on records
  h <-
    x %>%
    as.data.frame() %>%
    rangemap::rangemap_hull(
      hull_type = "concave", extent_of_occurrence = FALSE
    ) %>%
    slot("species_range") %>%
    sf::st_as_sf() %>%
    sf::st_union() %>%
    sf::st_make_valid()
  ## clip alpha hull to coastline
  h <-
    h %>%
    sf::st_intersection(boundary_data) %>%
    sf::st_union() %>%
    sf::st_make_valid()
  ## add in metadata columns
  r <- sf::st_as_sf(
    sf::st_geometry(h),
    tibble::tibble(
      binomial = binomial,
      compiler = "Derived from GBIF",
      yrcompiled = NA_real_,
      citation = NA_character_,
      subspecies = NA_character_,
      subpop = NA_character_,
      source = NA_character_,
      island = NA_character_,
      tax_comm = NA_character_,
      dist_comm = NA_character_,
      generalisd = NA_integer_,
      legend = NA_character_,
      kingdom = NA_character_,
      phylum = NA_character_,
      class = NA_character_,
      `order_` = NA_character_,
      family = NA_character_,
      genus = strsplit(binomial, " ", fixed = TRUE)[[1]][[1]],
      category = NA_character_,
      marine =  ifelse(marine, "true", "false"),
      terrestial = ifelse(terrestrial, "true", "false"),
      freshwater = ifelse(freshwater, "true", "false")
    )
  )
  ## return result
  r
}

# Preliminary processing
## sort data
spp_data <-
  spp_data %>%
  dplyr::arrange(binomial)

## fetch coastline data
boundary_data <-
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
  {suppressWarnings(sf::st_collection_extract(., "POLYGON"))} %>%
  sfheaders::sf_remove_holes()

## download records
spp_records <- lapply(spp_data$binomial, function(x) {
  ### fetch records
  key <- rgbif::name_suggest(q = x, rank = "species")$data$key[1]
  records <- rgbif::occ_search(
    taxonKey = key,
    limit = n_records_per_spp,
    hasCoordinate = TRUE,
    basisOfRecord = c("HUMAN_OBSERVATION", "OBSERVATION"),
    eventDate = "2000,2022",
    hasGeospatialIssue = FALSE
  )
  ### format records
  out <-
    records %>%
    {suppressMessages(print(.))} %>%
    tibble::as_tibble() %>%
    dplyr::filter(is.finite(decimalLongitude), is.finite(decimalLatitude)) %>%
    dplyr::mutate(Species = x) %>%
    dplyr::select(Species, decimalLongitude, decimalLatitude) %>%
    dplyr::rename(Longitude = "decimalLongitude", Latitude = "decimalLatitude")
  ## return result
  out
})

# Main processing
## create maps
spp_range_map <-
  lapply(seq_len(nrow(spp_data)), function(i) {
    create_range_map(
       x = spp_records[[i]],
       boundary_data = boundary_data,
       binomial = spp_data$binomial[[i]],
       taxon_id = spp_data$taxon_id[[i]],
       terrestrial = spp_data$terrestrial[[i]],
       freshwater = spp_data$freshwater[[i]],
       marine = spp_data$marine[[i]]
     )
    }
  ) %>%
  do.call(what = dplyr::bind_rows)

# Exports
## save data to disk
temp_dir <- tempfile()
dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
sf::write_sf(
  spp_range_map,
  file.path(temp_dir, "EXAMPLE_SPECIES.shp")
)
zip_path <- file.path(getwd(), "inst/extdata/EXAMPLE_SPECIES.zip")
withr::with_dir(
  temp_dir,
  utils::zip(
    zipfile = zip_path,
    files = dir(temp_dir)
  )
)
