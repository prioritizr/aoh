# System command to execute:
# R CMD BATCH --no-restore --no-save jung2-habitat-data.R

# Initialization
## load packages
devtools::load_all()
library(aoh)
library(raster)
library(terra)
library(sf)
library(rappdirs)
library(piggyback)
library(gdalUtils)

## set variables
### set number of threads
n_threads <- max(1, parallel::detectCores() - 2)

### change this to where you want to save the outputs
output_dir <-  tempdir()

### set version to process
version <- latest_zenodo_version(
  x = "10.5281/zenodo.4058356",
  file = function(x) {
    any(
      startsWith(x, "iucn_habitatclassification_composite_lvl2") &
      endsWith(x, ".zip")
    )
  }
)

# Preliminary processing
## print version
cli::cli_alert_info(paste0("Version: ", version))
cli::cli_alert_info(paste0("GDAL_CACHEMAX: ", Sys.getenv("GDAL_CACHEMAX")))

## specify cache directory
cache_dir <- rappdirs::user_data_dir("aoh")

## create cache directory if needed
cache_dir <- user_data_dir("aoh")
if (!file.exists(cache_dir)) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
}

## create output directory
if (!file.exists(output_dir)) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
}

## download data
archive_path <- get_zenodo_data(
  x = version,
  dir = cache_dir,
  force = FALSE,
  file = function(x) {
    any(
      startsWith(x, "iucn_habitatclassification_composite_lvl2") &
      endsWith(x, ".zip")
    )
  }
)

## unzip path
temp_dir <- tempfile()
dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
utils::unzip(archive_path, exdir = temp_dir)
raw_path <- dir(temp_dir, "^.*\\.tif$", full.names = TRUE, recursive = TRUE)
assertthat::assert_that(
  length(raw_path) == 1,
  msg = "failed to find 100 m resolution composite layer"
)

## construct output path
output_path <- gsub(
  ".", "-", gsub("/", "_", version, fixed = TRUE), fixed = TRUE
)
output_path <- file.path(temp_dir, paste0("jung-lvl2-", output_path, ".tif"))
output_path <- gsub("\\", "/", output_path, fixed = TRUE)

# Main processing
## import habitat data
raw_data <- terra::rast(raw_path)

## import elevation data
elev_data <- get_global_elevation_data(
  dir = rappdirs::user_data_dir("aoh"),
  version = "latest",
  force = FALSE,
  verbose = TRUE
)

## project habitat data to match elevation data
habitat_data <- terra_gdal_project(
  x = raw_data,
  y = elev_data,
  filename = output_path,
  method = "near",
  n_threads = n_threads,
  datatype = "INT2U",
  cache_limit = 5000,
  tiled = TRUE,
  bigtiff = TRUE,
  compress = "DEFLATE",
  verbose = TRUE
)

## verification
habitat_data <- terra::rast(output_path)
assertthat::assert_that(
  terra::compareGeom(habitat_data, elev_data, res = TRUE, stopiffalse = FALSE),
  msg = "GDAL processing didn't work correctly"
)

# Exports
## upload data to GitHub
withr::with_dir(output_dir, {
  piggyback::pb_upload(
    file = output_path,
    repo = "prioritizr/aoh",
    tag = "data",
    overwrite = TRUE
  )
})

## clean up
unlink(output_path, force = TRUE)
