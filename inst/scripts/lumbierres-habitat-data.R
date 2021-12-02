# System command to execute:
# R CMD BATCH --no-restore --no-save lumbierres-habitat-data.R

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
### note that latest_zenodo_version() fails for Zenodo archives that only have
## one version, so here we manually specify the first version if it fails
version <- try(
  latest_zenodo_version(
    x = "10.5281/zenodo.5146072",
    file = "habitat_CGLS.tiff"
  ),
  silent = TRUE
)
if (inherits(version, "try-error")) {
 version <- "10.5281/zenodo.5146073"
}

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
raw_path <- get_zenodo_data(
  x = version,
  dir = cache_dir,
  force = FALSE,
  file = "habitat_CGLS.tiff"
)

## construct output path
output_path <- gsub(
  ".", "-", gsub("/", "_", version, fixed = TRUE), fixed = TRUE
)
output_path <- file.path(temp_dir, paste0("lumbierres-", output_path, ".tif"))
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
  datatype = "INT1U",
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
