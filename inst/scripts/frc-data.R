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
  "birds-part-1" = "BOTW.7z",
  "birds-part-2" = "BOTW.7z",
  "birds-part-3" = "BOTW.7z",
  "birds-part-4" = "BOTW.7z",
  "birds-part-5" = "BOTW.7z",
  "birds-part-6" = "BOTW.7z",
  "mammals" = "MAMMALS_TERRESTRIAL_ONLY.zip",
  "reptiles" = "REPTILES.zip"
)

## define processing options
engine <- "gdal"
n_threads <- max(parallel::detectCores() - 2, 1)
cache_limit <- 5000
res <- 1000

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
output_dir <- "~/frc-data"

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
if (!file.exists(output_dir)) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
}

# Main processing
## import data
x <- read_spp_range_data(file.path(input_dir, input_file))
if (startsWith(cmd_args, "birds-part-")) {
  ### parse options for partitioned bird run
  out_name <- gsub("birds-", "BOTW-", cmd_args, fixed = TRUE)
  i <- as.numeric(gsub("birds-part-", "", cmd_args, fixed = TRUE))
  n <- sum(grepl("birds-part-", names(input_file_options), fixed = TRUE))
  x <- x[parallel::splitIndices(nrow(x), n)[[i]], , drop = FALSE]
} else {
  ### parse options for other runs
  out_name <- tools::file_path_sans_ext(basename(input_file))
}
## garbage collection
gc()

## exclude species in BirdLife data that are not on the IUCN Red List
if (identical(input_file, "BOTW.7z")) {
  ### exclude species
  exclude_ids <- c(
    22682860, 22700886, 22724592, 22683873, 61450351, 22735845,
    22709707, 155257132, 155257123, 22709791, 22723656
  )
  x <- x[which(!x$SISID %in% exclude_ids), , drop = FALSE]
  ### garbage collection
  gc()
}

## create fractional coverage data
result_data <- create_spp_frc_data(
  x = x,
  res = res,
  output_dir = output_dir,
  cache_dir = cache_dir,
  engine = engine,
  n_threads = n_threads,
  cache_limit = cache_limit
)

# Exports
## save data
saveRDS(
  object = result_data,
  file = file.path(dirname(output_dir), paste0("FRC_", out_name, ".rds")),
  compress = "xz"
)
