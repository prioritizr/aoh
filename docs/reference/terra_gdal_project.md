# Project a raster using GDAL

This function is a wrapper for
[`gdalUtilities::gdalwarp()`](https://rdrr.io/pkg/gdalUtilities/man/gdalwarp.html)
for use with terra objects.

## Usage

``` r
terra_gdal_project(
  x,
  y,
  method = "bilinear",
  n_threads = 1,
  filename = tempfile(fileext = ".tif"),
  datatype = "FLT4S",
  cache_limit = 200,
  tiled = FALSE,
  bigtiff = FALSE,
  compress = "LZW",
  verbose = TRUE,
  NAflag = NULL,
  output_raster = TRUE
)
```

## Arguments

- x:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  Raster object with source data.

- y:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  Raster object specifying spatial properties for output data.

- method:

  `character` Name of interpolation method.

- n_threads:

  `integer` Number of computational threads to use for data processing.
  To reduce run time, it is strongly recommended to set this parameter
  based on available resources (see Examples section below). Defaults to
  1.

- filename:

  `character` Filename for output raster. Defaults to
  `tempfile(fileext = ".tif")`.

- datatype:

  `character` Value indicating the data type for saving data. Defaults
  to `"FLT4S"`.

- cache_limit:

  `integer` Number of MB to use for GDAL caching. Defaults to 200.

- tiled:

  `logical` Value indicating if GeoTIFF files should be tiled. Defaults
  to `FALSE`.

- bigtiff:

  `logical` Value indicating the data should be stored in BIGTIFF
  format. Defaults to `FALSE`.

- compress:

  `character` Value indicating compression format. Available options
  include `"LZW"` and `"DEFLATE"`. Defaults to `"LZW"`.

- verbose:

  `logical` Should information be displayed during processing? Defaults
  to `TRUE`.

- NAflag:

  `numeric` Value for representing missing (`NA`) values. A `"none"`
  value can also be used to indicate that no flag should be set.
  Defaults to `NULL` such that the value is determined automatically.

- output_raster:

  `logical` Should a raster
  ([`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html))
  object be returned? If `FALSE` then the file path for the resulting
  file is returned. Defaults to `TRUE`.

## Value

A
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
raster object.

## Examples

``` r
# please ensure that the gdalUtilities package is installed
# to run this example

# create raster with data
x <- rast(
  ncols = 40, nrows = 40, xmin = -110, xmax = -90, ymin = 40, ymax=60,
  crs = "+proj=longlat +datum=WGS84"
)
values(x) <- seq_len(ncell(x))

# create raster to define spatial properties for projection
y <- rast(
  ncols = 94, nrows = 124, xmin = -944881, xmax = 935118, ymin = 4664377,
  ymax = 7144377,
  crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
)

# project data
z <- terra_gdal_project(x, y)

# preview result
print(z)
#> class       : SpatRaster 
#> size        : 124, 94, 1  (nrow, ncol, nlyr)
#> resolution  : 19999.99, 20000  (x, y)
#> extent      : -944881, 935118, 4664377, 7144377  (xmin, xmax, ymin, ymax)
#> coord. ref. : +proj=lcc +lat_0=0 +lon_0=-100 +lat_1=48 +lat_2=33 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 
#> source      : file13f091abe2944.tif 
#> name        : lyr.1 
```
