# Initialization
## load packages
devtools::load_all()
library(terra)
library(sf)
library(dplyr)

## define parameters
n_threads <- 6

## verify that suitable version of terra is available
assertthat::assert_that(
  packageVersion("terra") >= "1.4.8",
  msg = paste0(
    "The developmental version of the terra R package is required,",
    " please install it using:\nremotes::install_github('rspatial/terra')"
  )
)

# Preliminary processing
## define raster CRS
rast_crs <- sf::st_crs("ESRI:54017")

## create global extent in CRS
rast_extent <-
  expand.grid(x = c(-180, 180), y = c(-90, 90)) %>%
  mutate(z = runif(nrow(.))) %>%
  sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(4326)) %>%
  sf::st_transform(rast_crs) %>%
  sf::st_bbox()

# Main processing
## create empty raster
x <- create_template_rast(
  xres = 100, yres = 100, crs = rast_crs, bbox = rast_extent
)

## create raster on disk
rast_path <- file.path(tempdir(), "world_behrmann_100m.tif")
wkt_path <- tempfile(fileext = ".wkt")

### create wkt file
writeLines(terra::crs(x), wkt_path)

### build command
cmd <- paste0(
  "gdal_create ",
  "-of GTiff ",
  "-outsize ", terra::ncol(x), " ", terra::nrow(x), " ",
  "-bands 1 ",
  "-burn 1 ",
  "-a_srs ", wkt_path, " ",
  "-a_ullr ",
  terra::xmin(x), " ", terra::ymax(x), " ",
  terra::xmax(x), " ", terra::ymin(x), " ",
  "-co \"COMPRESS=LZW\" ",
  "-co \"NUM_THREADS=", n_threads,"\" ",
  "-a_nodata 0 ",
  "-ot Byte ",
  rast_path
)

## run command
message("GDAL command:")
message(cmd)
system(cmd)

## verify success
rast <- terra::rast(rast_path)
terra::compareGeom(x, rast, res = TRUE)
rm(rast)

# Exports
withr::with_dir(tempdir(), {
  piggyback::pb_upload(
    file = rast_path,
    repo = "prioritizr/aoh",
    tag = "data",
    overwrite = TRUE
  )
})

## clean up
rm(rast_path)
rm(wkt_path)

## display message
message("Done!")
