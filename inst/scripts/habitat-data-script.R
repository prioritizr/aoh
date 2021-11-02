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
output_dir <- tempfile()

### set version to process
version <- aoh:::latest_version_habitat_data("10.5281/zenodo.4058356")

# Preliminary processing
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
  gsub("-", ".", gsub("/", "_", version, fixed = TRUE), fixed = TRUE)
)

## create output directory
if (!file.exists(output_dir)) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
}

# Main processing
## import data
habitat_data <- get_global_habitat_data(cache_dir, version = version)
template_data <- get_world_berhman_1km_rast()

## project data
### create temp files
proj_files <- replicate(
  n = terra::nlyr(habitat_data),
  expr = tempfile(fileext = ".tif")
)
### set up
pb <- cli::cli_progress_bar(
  clear = FALSE,
  total = terra::nlyr(habitat_data),
  format = paste0(
    "{.alert-info projecting habitat data} ",
    "{cli::pb_bar} [{cli::pb_percent} | {cli::pb_eta_str}]"
  ),
  format_done = paste0(
    "{.alert-success projecting habitat data} [{cli::pb_elapsed}]"
  )
)
### processing
habitat_data <- terra::rast(
  plyr::llply(
    seq_along(proj_files),
    function(i) {
      x <- aoh::terra_gdal_project(
        x = habitat_data[[i]],
        y = template_data,
        filename = proj_files[i],
        parallel_n_threads = n_threads,
        verbose = FALSE,
        datatype = "INT2U"
      )
      cli::cli_progress_update(id = pb)
      x
    }
  )
)
### clean up
cli::cli_progress_done(id = pb)
rm(pb)

## clamp data
### set up
pb <- cli::cli_progress_bar(
  clear = TRUE,
  total = terra::nlyr(habitat_data),
  format = paste0(
    "{.alert-info clamping habitat data} ",
    "{cli::pb_bar} [{cli::pb_percent} | {cli::pb_eta_str}]"
  ),
  format_done = paste0(
    "{cli::symbol$tick} clamping habitat data [{cli::pb_elapsed}]"
  )
)
### processing
habitat_data <- terra::rast(
  plyr::llply(terra::as.list(habitat_data), function(x) {
    x <- terra::clamp(
      x = x,
      lower = 0,
      upper = 1000,
      values = TRUE,
      datatype = "INT2U",
    )
    cli::cli_progress_update(id = pb)
    x
  })
)
### clean up
cli::cli_progress_done(id = pb)
rm(pb)
unlink(proj_files, force = TRUE)

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
