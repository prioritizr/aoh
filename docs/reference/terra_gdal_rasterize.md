# Convert vector data into a raster using GDAL

This function is a wrapper for
[`gdalUtilities::gdal_rasterize()`](https://rdrr.io/pkg/gdalUtilities/man/gdal_rasterize.html)
for use with terra objects.

## Usage

``` r
terra_gdal_rasterize(
  x,
  sf,
  burn = 1,
  init = 0,
  invert = FALSE,
  update = FALSE,
  touches = FALSE,
  n_threads = 1,
  filename = tempfile(fileext = ".tif"),
  sf_filename = tempfile(fileext = ".gpkg"),
  datatype = "FLT4S",
  cache_limit = 200,
  tiled = FALSE,
  bigtiff = FALSE,
  nbits = NULL,
  compress = "LZW",
  NAflag = NULL,
  output_raster = TRUE
)
```

## Arguments

- x:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  Raster object with source data.

- sf:

  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html)
  Spatial object to rasterize.

- burn:

  `numeric` Value for encoding the vector data. Defaults to 1.

- init:

  `numeric` Value for encoding background cells that do not overlap with
  the vector data. Defaults to 0.

- invert:

  `logical` Should the burn process be inverted? Defaults to `FALSE`.

- update:

  `logical` Should the result by producing by updating the argument to
  `x`? If `FALSE` then the argument to `x` is only used to specify the
  spatial properties of the resulting raster (i.e., values have on the
  result), Defaults to `FALSE`.

- touches:

  `logical` Should cells of `x` that are overlap with any part of `sf`
  be treated as covered by `sf`? Defaults to `FALSE`, such that only
  cells that have their centroid covered by `sf` are treated as covered.

- n_threads:

  `integer` Number of computational threads to use for data processing.
  To reduce run time, it is strongly recommended to set this parameter
  based on available resources (see Examples section below). Defaults to
  1.

- filename:

  `character` Filename for output raster. Defaults to
  `tempfile(fileext = ".tif")`.

- sf_filename:

  `character` File name to temporarily save argument to `sf`. Defaults
  to a temporary (geopackage) file.

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

- nbits:

  `integer` Number of bits for output data. Defaults to `NULL` such that
  the number of bits is automatically determined.

- compress:

  `character` Value indicating compression format. Available options
  include `"LZW"` and `"DEFLATE"`. Defaults to `"LZW"`.

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
# import vector data
f <- system.file("ex/lux.shp", package = "terra")
sf <- read_sf(f)

# create template raster
x <- rast(vect(sf), ncols = 75, nrows = 100)
x <- terra::setValues(x, runif(terra::ncell(x)))

# rasterize vector data
z <- terra_gdal_rasterize(x, sf, burn = 5)

# plot result
plot(z)
```
