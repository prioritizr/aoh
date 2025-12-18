# GDAL calculate

This function is a wrapper for the `gdal_calc.py` script for use with
terra objects.

## Usage

``` r
terra_gdal_calc(
  x,
  expr,
  y = NULL,
  z = NULL,
  n_threads = 1,
  filename = tempfile(fileext = ".tif"),
  datatype = "FLT4S",
  tiled = FALSE,
  bigtiff = FALSE,
  compress = "LZW",
  predictor = 1,
  nbits = NULL,
  verbose = TRUE,
  NAflag = NULL,
  output_raster = TRUE
)
```

## Arguments

- x:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  Raster object with source data.

- expr:

  `character` Value containing expression.

- y:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  Optional raster for calculations.

- z:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  Optional raster for calculations.

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

- tiled:

  `logical` Value indicating if GeoTIFF files should be tiled. Defaults
  to `FALSE`.

- bigtiff:

  `logical` Value indicating the data should be stored in BIGTIFF
  format. Defaults to `FALSE`.

- compress:

  `character` Value indicating compression format. Available options
  include `"LZW"` and `"DEFLATE"`. Defaults to `"LZW"`.

- predictor:

  `integer` Predictor for GeoTIFF compression (see [GDAL
  documentation](https://gdal.org/en/stable/drivers/raster/gtiff.html)).
  Defaults to 1 such that no predictor is used for compression.

- nbits:

  `integer` Number of bits for output data. Defaults to `NULL` such that
  the number of bits is automatically determined.

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

## Troubleshooting

The function aims to automatically determine the best approach to run
the `gdal_calc.py` script. However, it is not always successful. As
such, there are several environmental variables that can be configured
to manually specify exactly how the `gdal_calc.py` script should be
executed. These variables specify the following behavior.

- `GDAL_PYTHON`:

  Specifies the file path for the Python executable used to run the
  `gdal_calc.py` script. For example, this variable could be configured
  as: `sys.setenv("GDAL_PYTHON", "python")`,
  `sys.setenv("GDAL_PYTHON", "python3")`, or
  `sys.setenv("GDAL_PYTHON", "python.exe")`.

- `GDAL_CALC`:

  Specifies the file path for the `gdal_calc.py` script. For example,
  this variable could be configured as:
  `sys.setenv("GDAL_PYTHON", "gdal_calc.py")`, or
  ` sys.setenv("GDAL_PYTHON", "C:\\OSGeo4W\\apps\\Python39\\Scripts\\gdal_calc.py")`.

- `GDAL_ESCAPE`:

  Specifies whether symbols in the mathematical expressions used to
  perform the calculations should be escaped. On Windows systems, the
  default behavior is to escape these symbols. This behavior can be
  disabled using the following code:
  `sys.setenv("GDAL_ESCAPE", "false")`.

## See also

See the package README for instructions to install the GDAL dependencies
for this function. The
[`is_osgeo4w_available()`](https://prioritizr.github.io/aoh/reference/is_osgeo4w_available.md)
and
[`is_gdal_calc_available()`](https://prioritizr.github.io/aoh/reference/is_gdal_calc_available.md)
can be used to check if the installation was successful.

## Examples

``` r
# please ensure that the Python and the GDAL system binaries are
# installed to run the example,
# see ?is_gdal_calc_available for more details

# \dontrun{
# create raster with data
x <- rast(
  ncols = 40, nrows = 40, xmin = -110, xmax = -90, ymin = 40, ymax=60,
  crs = "+proj=longlat +datum=WGS84"
)
values(x) <- seq_len(ncell(x))

# run calculation
y <- terra_gdal_calc(x, "(X < 20) * 1")
#> ℹ System command: gdal_calc.py -X "/tmp/Rtmpc9ujEP/file13f096eea8a92.tif" --outfile="/tmp/Rtmpc9ujEP/file13f09596c994d.tif" --calc="(X < 20) * 1" --type="Float32" --co="NUM_THREADS=1" --co="COMPRESS=LZW" --co="PREDICTOR=1"

# preview result
print(y)
#> class       : SpatRaster 
#> size        : 40, 40, 1  (nrow, ncol, nlyr)
#> resolution  : 0.5, 0.5  (x, y)
#> extent      : -110, -90, 40, 60  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source      : file13f09596c994d.tif 
#> name        : lyr.1 
# }
```
