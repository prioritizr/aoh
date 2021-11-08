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
cli::cli_alert_info(paste0("GDAL_CACHEMAX: ", Sys.getenv("GDAL_CACHEMAX")))

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

## prepare folder for download
data_dir <-  file.path(
  cache_dir,
  paste0(
    "comp1km-",
    gsub(".", "-", gsub("/", "_", version, fixed = TRUE), fixed = TRUE)
  )
)
if (!file.exists(data_dir)) {
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
}

## download habitat data if needed
data_path <- file.path(
  data_dir, "iucn_habitatclassification_composite_lvl2_ver004.zip"
)
if (!file.exists(data_path)) {
  z <- zen4R::ZenodoManager$new()
  f <- z$getRecordByDOI(version)$listFiles()
  i <- which(f$filename == basename(data_path))
  assertthat::assert_that(length(i) == 1, msg = "failed to find download URL")
  curl::curl_download(
    url = f$download[i],
    destfile = data_path,
    quiet = FALSE
  )
}

## unzip path
temp_dir <- tempfile()
dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
utils::unzip(data_path, exdir = temp_dir)
input_path <- dir(temp_dir, "^.*\\.tif$", full.names = TRUE, recursive = TRUE)
assertthat::assert_that(
  length(input_path) == 1,
  msg = "failed to find 100 m resolution composite layer"
)

# Main processing
## import habitat codes
code_data <- habitat_code_data()
raw_habitat_data <- terra::rast(input_path)
template_data <- get_world_behrmann_1km_rast()

## identify classes to process
jung_habitat_classes <- names(get_global_habitat_data(
  cache_dir, preprocessed = FALSE, version = version
))
habitat_idx <- which(
  (code_data$iucn_code %in% jung_habitat_classes) &
  (code_data$terrestrial)
)
habitat_classes <- code_data$code[habitat_idx]

## disaggregate template data
template_hires_path <- tempfile(fileext = ".tif")
template_hires_data <- aoh::terra_gdal_disaggregate(
  x = template_data,
  fact = 10,
  n_threads = n_threads,
  filename = template_hires_path,
  datatype = "INT2U",
  tiled = FALSE
)

## crop high res data to be valid
### find max possible extent
glb_ext <- c(xmin = -180, ymin = -90, xmax = 180, ymax = 90)
glb_ext <- sf::st_set_crs(sf::st_as_sfc(sf::st_bbox(glb_ext)), 4326)
glb_ext <- sf::st_transform(glb_ext, terra_st_crs(template_hires_data))
glb_ext <- sf::st_bbox(glb_ext)

### find new extent
new_highres_template_ext <- terra::ext(c(
  xmin = terra::xmin(template_hires_data),
  xmax = (
    terra::xmin(template_hires_data) +
    (terra::xres(template_hires_data) *
      round((
        (glb_ext$xmax[[1]] - terra::xmin(template_hires_data)) /
        terra::xres(template_hires_data))
      )
    )
  ),
  ymin = terra::ymin(template_hires_data),
  ymax = (
    terra::ymin(template_hires_data) +
    (terra::yres(template_hires_data) *
      round(
        ((glb_ext$ymax[[1]] - terra::ymin(template_hires_data)) /
        terra::yres(template_hires_data))
      )
    )
  )
))
### crop data
template_hires_data <- aoh::terra_gdal_crop(
  x = template_hires_data,
  ext = new_highres_template_ext,
  n_threads = n_threads,
  datatype = "INT2U",
  tiled = TRUE,
  verbose = TRUE
)

## project habitat data to match template
raw_habitat_data <- aoh::terra_gdal_project(
  x = raw_habitat_data,
  y = template_hires_data,
  method = "near",
  n_threads = n_threads,
  datatype = "INT2U",
  cache_limit = 5000,
  tiled = TRUE,
  verbose = TRUE
)

## iterate over each habitat code and process
result <- plyr::llply(habitat_classes, .progress = "text", function(x) {
  message("######################################")
  message(paste("## starting class", x))
  message("######################################")
  ## generate files
  f1 <- tempfile(fileext = ".tif")
  f2 <- tempfile(fileext = ".tif")
  f3 <- tempfile(fileext = ".tif")
  curr_path <- file.path(output_dir, paste0(x, ".tif"))
  ## identify pixels with class
  curr_rast <- terra_gdal_calc(
    x = raw_habitat_data,
    expr = paste0("(X == ", x, ") * 1"),
    filename = f1,
    datatype = "INT1U",
    n_threads = n_threads,
    tiled = TRUE,
    verbose = FALSE
  )
  ## aggregate data
  curr_rast <- aoh::terra_gdal_project(
    x = curr_rast,
    y = template_data,
    method = "sum",
    n_threads = n_threads,
    datatype = "INT1U",
    filename = f2,
    cache_limit = 1000,
    tiled = FALSE,
    verbose = TRUE
  )
  ## convert to fraction
  curr_rast <- terra::app(
    x = curr_rast,
    wopt = list(datatype = "INT2U"),
    filename = f3,
    function(x) {
      floor((x / 100) * 1000)
    }
  )
  print(terra::global(curr_rast, "min", na.rm = TRUE))
  print(terra::global(curr_rast, "max", na.rm = TRUE))
  ## export raster
  file.copy(f3, curr_path, overwrite = TRUE)
  ## remove unused files
  rm(curr_rast)
  gc()
  unlink(f1, force = TRUE)
  unlink(f2, force = TRUE)
  unlink(f3, force = TRUE)
  ## return result
  TRUE
})

# Exports
## zip directory
zip_path <- file.path(
  dirname(output_dir),
  paste0("prep-", basename(output_dir), ".zip")
)
withr::with_dir(output_dir, {
  utils::zip(
    zipfile = zip_path,
    files = dir(output_dir)
  )
})

## upload data to GitHub
withr::with_dir(dirname(output_dir), {
  piggyback::pb_upload(
    file = zip_path,
    repo = "prioritizr/aoh",
    tag = "data",
    overwrite = TRUE
  )
})
