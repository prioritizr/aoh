# Initialization
## load packages
library(aoh)
library(raster)
library(terra)
library(sf)
library(rappdirs)

## set variables
## define available datasets
input_file_options <- c(
  "amphibians" = "AMPHIBIANS.zip",
  "birds" = "BOTW_2023_1.7z",
  "birds-part-1" = "BOTW_2023_1.7z",
  "birds-part-2" = "BOTW_2023_1.7z",
  "birds-part-3" = "BOTW_2023_1.7z",
  "birds-part-4" = "BOTW_2023_1.7z",
  "birds-part-5" = "BOTW_2023_1.7z",
  "birds-part-6" = "BOTW_2023_1.7z",
  "mammals-land" = "MAMMALS_TERRESTRIAL_ONLY.zip",
  "mammals-land-freshwater" = "MAMMALS_FRESHWATER.zip",
  "mammals-land-marine" = "MAMMALS_MARINE_AND_TERRESTRIAL.zip",
  "reptiles" = "REPTILES.zip"
)

## define processing options
engine <- "gdal"
n_threads <- max(parallel::detectCores() - 2, 1)
cache_limit <- 5000

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

### set geometry processing
if (isTRUE(startsWith(input_file, "BOTW"))) {
  geometry_precision <- 1e10
} else {
  geometry_precision <- 1e6
}

### change this to the folder where the zip file is located
input_dir <- rappdirs::user_data_dir("iucn-red-list-data")

### change this to where you want to save the outputs
output_dir <- "~/aoh-data"

## change this to where you want to catch IUCN Red List data
cache_dir <- paste0(output_dir, "/iucn-red-list-cache")

# Preliminary processing
## update output directory based on input data filename
output_dir <- file.path(
  path.expand(output_dir), tools::file_path_sans_ext(basename(input_file))
)
if (!file.exists(output_dir)) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
}

## create cache directory if needed
if (!file.exists(cache_dir)) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
}

# Main processing
## import data
x <- read_spp_range_data(file.path(input_dir, input_file))

## determine id column
id_column <- ifelse("sisid" %in% names(x), "sisid", "SISID")
assertthat::assert_that(
  id_column %in% names(x),
  msg = "can't identify id column (neither \"sisid\" or \"SISID\" present)"
)

## processing data in chunks
if (startsWith(cmd_args, "birds-part-")) {
  ### parse options for partitioned bird run
  out_name <- gsub("birds-", "BOTW-", cmd_args, fixed = TRUE)
  i <- as.numeric(gsub("birds-part-", "", cmd_args, fixed = TRUE))
  n <- sum(grepl("birds-part-", names(input_file_options), fixed = TRUE))
  ### assign each species id to a different partition
  spp_ids <- c(na.omit(unique(x[[id_column]])))
  chunks <- parallel::splitIndices(length(spp_ids), n)
  ### subset data for processing
  x <- x[which(x[[id_column]] %in% spp_ids[chunks[[i]]]), , drop = FALSE]
  ### update output directory to save in a folder for the partition
  output_dir <- paste0(output_dir, "-part-", i)
} else {
  ### parse options for other runs
  out_name <- tools::file_path_sans_ext(basename(input_file))
}
## garbage collection
gc()

## exclude species in BirdLife data that are not on the IUCN Red List
if (isTRUE(startsWith(toupper(input_file), "BOTW"))) {
  ### exclude species
  #### N.B. all speciesin 2023-1 version of BirdLife data are present on
  #### IUCN Red List so we don't need to exclude any species for this version
  exclude_ids <- c()
  x <- x[which(!x[[id_column]] %in% exclude_ids), , drop = FALSE]
  ### garbage collection
  gc()
}

## ensure output directory exists
if (!file.exists(output_dir)) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
}

## create information data
info_data <- create_spp_info_data(
  x = x,
  cache_dir = cache_dir,
  geometry_precision = geometry_precision
)

## create Area of Habitat data
result_data <- create_spp_aoh_data(
  x = info_data,
  output_dir = output_dir,
  cache_dir = cache_dir,
  engine = engine,
  n_threads = n_threads,
  cache_limit = cache_limit,
  rasterize_touches = TRUE
)

# Exports
## save data
saveRDS(
  object = result_data,
  file = file.path(dirname(output_dir), paste0("AOH_", out_name, ".rds")),
  compress = "xz"
)

## session information
sessionInfo()
