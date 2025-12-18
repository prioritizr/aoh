# System GDAL version

Find the version of the Geospatial Data Abstraction Library (GDAL) that
is currently installed on the system.

## Usage

``` r
system_gdal_version()
```

## Value

A `character` value describing the version of GDAL installed. If GDAL is
not installed, then a missing (`NA`) value is returned.

## Examples

``` r
# show version of GDAL installed
print(system_gdal_version())
#> [1] "3.9.3"
```
