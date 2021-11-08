# System command to execute:
# R CMD BATCH --no-restore --no-save aoh-example-script.R

# Initialization
## load packages
devtools::load_all()
library(aoh)
library(raster)
library(terra)
library(sf)
library(rappdirs)

## set variables
## define available datasets
input_file_options <- c(
  "amphibians" = "AMPHIBIANS.zip",
  "birds" = "BOTW.zip",
  "mammals" = "MAMMALS_TERRESTRIAL_ONLY.zip",
  "reptiles" = "REPTILES.zip"
)

### parse command-line arguments
cmd_args <- commandArgs(trailingOnly = TRUE)
assertthat::assert_that(
  length(cmd_args) == 1,
  msg = "command line argument must be specified"
)
assertthat::assert_that(
  isTRUE(cmd_args %in% names(input_file_options)),
  msg = paste(
    "command line argument must be one of ",
    paste(paste0("\"", names(input_file_options), "\""), collapse = ", ")
  )
)

### possible options based on for IUCN file name conventions
input_file <- input_file_options[[cmd_args]]
cli::cli_alert_info(paste0("processing file: ", input_file))

### change this to the folder where the zip file is located
input_dir <- rappdirs::user_data_dir("iucn-red-list-data")

### change this to where you want to save the outputs
output_dir <- "~/aoh-data"

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
  path.expand(output_dir), tools::file_path_sans_ext(basename(input_file))
)

## create output directory
if (!file.exists(output_dir)) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
}

# Main processing
## import data
spp_data <- read_spp_range_data(file.path(input_dir, input_file))

## create data
result_data <- create_spp_aoh_data(
  x = spp_data, output_dir = output_dir, cache_dir = cache_dir,
  use_gdal = FALSE
)

# Exports
## save table
output_path <- file.path(
  output_dir,
  paste0(tools::file_path_sans_ext(basename(input_file)), ".rds")
)
saveRDS(result_data, output_path, compress = "xz")
