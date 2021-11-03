# System command to execute:
# R CMD BATCH --no-restore --no-save preprocess-habitat-data.R

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
output_dir <-  "~/aoh-data"

### set version to process
version <- aoh:::latest_version_habitat_data("10.5281/zenodo.4058356")

# Preliminary processing
## print version
cli::cli_alert_info(paste0("Version: ", version))

## specify cache directory
cache_dir <- rappdirs::user_data_dir("aoh")

## create cache directory if needed
cache_dir <- user_data_dir("aoh")
if (!file.exists(cache_dir)) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
}

## update output directory based on input data filename
output_dir <- file.path(
  path.expand(output_dir),
  gsub(".", "-", gsub("/", "_", version, fixed = TRUE), fixed = TRUE)
)

## create output directory
if (!file.exists(output_dir)) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
}

# Main processing
## import data
habitat_data <- get_global_habitat_data(
  cache_dir, preprocessed = FALSE, version = version
)
template_data <- get_world_berhman_1km_rast()

## project data
habitat_data <- aoh:::project_habitat_data(
  x = habitat_data,
  template_data = template_data,
  parallel_n_threads = n_threads,
  use_gdal = TRUE,
  temp_dir = tempdir(),
  verbose = TRUE
)

# Exports
## save rasters to disk
code_data <- aoh:::habitat_code_data()
for (i in seq_len(terra::nlyr(habitat_data))) {
  ### get code for layer
  curr_idx <- which(code_data$iucn_code == names(habitat_data)[i])
  curr_code <- code_data$code[curr_idx]
  assertthat::assert_that(
    assertthat::is.string(curr_code),
    assertthat::noNA(curr_code),
    msg = "could not identify habitat code"
  )
  ### create file path
  curr_path <- file.path(output_dir, paste0(curr_code, ".tif"))
  ### save raster
  terra::writeRaster(
    habitat_data[[i]], curr_path, overwrite = TRUE, datatype = "INT2U"
  )
  ## assert all files saved
  assertthat::assert_that(file.exists(curr_path))
}

## zip directory
withr::with_dir(output_dir, {
  utils::zip(zipfile = paste0(output_dir, ".zip"), files = dir(output_dir))
})

## upload data to GitHub
withr::with_dir(dirname(output_dir), {
  piggyback::pb_upload(
    file = basename(paste0(output_dir, ".zip")),
    repo = "prioritizr/aoh",
    tag = "data",
    overwrite = TRUE
  )
})
