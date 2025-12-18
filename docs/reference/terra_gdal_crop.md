# Crop a raster using GDAL

This function is a wrapper for
[`gdalUtilities::gdal_translate()`](https://rdrr.io/pkg/gdalUtilities/man/gdal_translate.html)
for use with terra objects.

## Usage

``` r
terra_gdal_crop(
  x,
  ext,
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

- ext:

  [`terra::ext()`](https://rspatial.github.io/terra/reference/ext.html)
  Raster extent object.

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

# create extent for cropping
y <- ext(x) - c(5, 2.5, 1, 1.5)

# crop data
z <- terra_gdal_crop(x, y)

# preview result
print(z)
#> class       : SpatRaster 
#> size        : 35, 25, 1  (nrow, ncol, nlyr)
#> resolution  : 0.5, 0.5  (x, y)
#> extent      : -105, -92.5, 41, 58.5  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source      : file13f09257fa442.tif 
#> name        : lyr.1 
```
