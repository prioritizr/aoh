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
  "birds" = "BOTW.7z",
  "mammals" = "MAMMALS_TERRESTRIAL_ONLY.zip",
  "reptiles" = "REPTILES.zip"
)

## define number of threads
n_threads <- max(parallel::detectCores() - 2, 1)

## define processing options
engine <- "gdal"
fraction_coverage_resolution <- 5000

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

## create output directories for AOH and fractional coverage data
aoh_output_dir <-  file.path(output_dir, "aoh")
frc_output_dir <-  file.path(output_dir, "frc")

## create output directories
if (!file.exists(aoh_output_dir)) {
  dir.create(aoh_output_dir, showWarnings = FALSE, recursive = TRUE)
}
if (!file.exists(frc_output_dir)) {
  dir.create(frc_output_dir, showWarnings = FALSE, recursive = TRUE)
}

# Main processing
## import data
spp_data <- read_spp_range_data(file.path(input_dir, input_file), n= 10)

## create Area of Habitat data
aoh_data <- create_spp_aoh_data(
  x = spp_data,
  output_dir = aoh_output_dir,
  cache_dir = cache_dir,
  engine = engine,
  n_threads = n_threads
)

## clean up
rm(spp_data)
gc()

## calculate fractional coverage
frac_data <- calc_spp_frac_data(
  x = aoh_data,
  res = fraction_coverage_resolution,
  output_dir = frc_output_dir,
  cache_dir = cache_dir,
  engine = "gdal",
  n_threads = n_threads
)

# Exports
## save Area of Habitat data object
saveRDS(
  object = aoh_data,
  file = file.path(
    output_dir,
    paste0(tools::file_path_sans_ext(basename(input_file)), "_AOH.rds")
  ),
  compress = "xz"
)

## save fractional coverage data object
saveRDS(
  object = frac_data,
  file = file.path(
    output_dir,
    paste0(tools::file_path_sans_ext(basename(input_file)), "_FRC.rds")
  ),
  compress = "xz"
)
