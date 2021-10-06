# Initialization
## load packages
library(terra)
library(sf)
library(dplyr)

## verify that suitable version of terra is available
assertthat::assert_that(
  packageVersion("terra") >= "1.4.8",
  msg = paste0(
    "The developmental version of the terra R package is required,",
    " please install it using:\nremotes::install_github('rspatial/terra')"
  )
)

## define functions
source("R/misc_terra.R")

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
## create blank raster
rast <- create_template_rast(
  xres = 1000, yres = 1000, crs = rast_crs, bbox = rast_extent
)
## populate raster
try({
  rast <- try(terra::setValues(rast, rep(1, terra::ncell(rast))))
})
## ensure populated correctly despite potential errors
assertthat::assert_that(
  identical(c(terra::values(rast)), rep(1, terra::ncell(rast)))
)

# Exports
terra::writeRaster(
  x = rast,
  filename = "inst/extdata/world_behrman_1km_rast.tif",
  overwrite = TRUE,
  NAflag = -9999,
  gdal = "COMPRESS=DEFLATE"
)
