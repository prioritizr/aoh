# System command to execute:
# R CMD BATCH --no-restore --no-save aoh-example-script.R

# Initialization
## set options
options(future.globals.onReference = "warning")

## load packages
library(aoh)
library(terra)
library(sf)
library(rappdirs)

## set variables
### possible options based on for IUCN file name conventions
input_file <- "AMPHIBIANS.zip"
# input_file <- "MAMMALS_TERRESTRIAL_ONLY.zip"
# input_file <- "REPTILES.zip"

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
  parallel_n_threads = parallel::detectCores() - 1
)

# Exports
## save table
output_path <- file.path(
  output_dir,
  paste0(tools::file_path_sans_ext(basename(input_file)), ".rds")
)
saveRDS(result_data, output_path, compress = "xz")
