# Is gdal_calc.py available?

Check if `gdal_calc.py` Python script is available.

## Usage

``` r
is_gdal_calc_available()
```

## Value

A `logical` value indicating if it is available.

## Details

The `gdal_calc.py` Python script is used to process Area of Habitat data
when using the GDAL engine (within
[`create_spp_aoh_data()`](https://prioritizr.github.io/aoh/reference/create_spp_aoh_data.md))
. This function determines if this script is available on the system. On
Windows systems, it first tries to access this script using the OSGeo4W
software (available at <https://trac.osgeo.org/osgeo4w/>). If that
fails, or if you are not using a Windows system: it then tries to access
this script using default system variables.

## Examples

``` r
# see if gdal_calc python script is available
print(is_gdal_calc_available())
#> [1] TRUE
```
