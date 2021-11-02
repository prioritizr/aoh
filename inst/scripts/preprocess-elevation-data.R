# System command to execute:
# R CMD BATCH --no-restore --no-save habitat-data-script.R

# Initialization
## load packages
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
output_dir <-  "~/aoh-data"

# Preliminary processing
## specify cache directory
cache_dir <- rappdirs::user_data_dir("aoh")

## create cache directory if needed
cache_dir <- user_data_dir("aoh")
if (!file.exists(cache_dir)) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
}

## update output directory based on input data filename
output_dir <- path.expand(output_dir)

## create output directory
if (!file.exists(output_dir)) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
}

# Main processing
## import data
elevation_data <- get_global_elevation_data(
  cache_dir, preprocessed = FALSE, verbose = TRUE
)
template_data <- get_world_berhman_1km_rast()

## convert NA values to zeros
elevation_data[is.na(elevation_data)] <- 0

## project data
elevation_data <- terra_gdal_project(
  x = elevation_data,
  y = template_data,
  parallel_n_threads = n_threads,
  filename = tempfile(fileext = ".tif"),
  verbose = TRUE,
  datatype = "INT2U"
)

## clamp values
elevation_data <- terra::clamp(elevation_data, lower = 0, values = TRUE)

# Exports
## save raster to disk
## create file path
curr_path <- file.path(output_dir, "prep-elevation_1KMmd_GMTED.tif")
### save raster
terra::writeRaster(
  elevation_data, curr_path, overwrite = TRUE
)
## assert all files saved
assertthat::assert_that(file.exists(curr_path))

## upload data to GitHub
withr::with_dir(output_dir, {
  piggyback::pb_upload(
    file = "prep-elevation_1KMmd_GMTED.tif",
    repo = "prioritizr/aoh",
    tag = "data",
    overwrite = TRUE
  )
})
