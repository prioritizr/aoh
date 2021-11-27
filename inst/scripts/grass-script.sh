#!/bin/bash

# Purpose: Script to generate Area of Habitat data

# Environmental variables
#
# ## Region parameters
# xmin `numeric` Lower x-coordinate for region.
# xmax `numeric` Upper x-coordinate for region.
# ymin `numeric` Lower y-coordinate for region.
# ymax `numeric` Upper y-coordinate for region.
# xres `numeric` Resolution of x-axis for region.
# yres `numeric` Resolution of y-axis for region.
#
# ## Input parameters
# habitat_data `character` File path for habitat raster data.
# elevation_data `character` File path for elevation raster data.
# range_data `character` File path for species range vector data.
# reclass_data `character` String containing suitable habitat layers
#
# ## Data parameters
# elevation_upper `numeric` File path for reclassififying habitat data
# elevation_lower `numeric` File path for reclassififying habitat data
#
# ## General prameters
# verbosity `character` Should information be displayed during processing?
#'  Availaable options include `"verbose"` and `"quiet".
#
# n_threads `integer` number of threads for processing.

# ## Export parameters
# @param path `character` File path to save data

# Initialization
## define region setting
g.region n=$ymax s=$ymin e=$xmax w=$xmin ewres=$xres nsres=$yres --$verbosity

# Main processing
## rasterize range data
v.import input=$range_data output=range extent=region --$verbosity
v.to.rast input=range type=area memory=4000 use=val value=1 output=mask --$verbosity
r.mask raster=mask --$verbosity

## create a reclassify habitat raster
r.import input=$habitat_data output=habitat memory=4000 extent=region resolution=region --$verbosity
r.reclass input=habitat output=suitable rules=$reclass_data --$verbosity

## set data according to elevation limits
r.import input=$elevation_data output=elev memory=4000 extent=region resolution=regio --$verbosity
r.mapcalc --$verbosity <<EOF
aoh = int(suitable * ((elev >= $elevation_lower) & (elev <= $elevation_upper)))
EOF

# Exports
r.out.gdal input=aoh output=$path format=GTiff type=Byte nodata=255 createopt="BIGTIFF=YES,COMPRESS=LZW,NUM_THREADS=$n_threads" --$verbosity

# Exit
exit 0
