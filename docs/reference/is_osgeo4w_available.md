# Is OSGeo4W available?

Check if OSGeo4W software is available.

## Usage

``` r
is_osgeo4w_available()
```

## Value

A `logical` value indicating if it is available.

## Details

This software is used to provide GDAL Python scripts on Windows systems
that are used to generate Area of Habitat data via the GDAL engine. It
can be installed from <https://trac.osgeo.org/osgeo4w/>. Note that macOS
and Linux systems do not require this software. By default, it is
assumed that the software is installed in the `"C:/OSGeo4W"` directory.
If the software is installed at a different location, then the
`"OSGEO4W_ROOT"` environmental variable can be used to specify a
different location.

## Examples

``` r
# see if OSGeo4W is available at the default location
print(is_osgeo4w_available())
#> [1] FALSE

# \dontrun{
# specify a different location for OSGeo4W, and
# then see if OSGeo4W is available at this location
Sys.setenv("OSGEO4W_ROOT" = "C:/software/OSGeo4W")
print(is_osgeo4w_available())
#> [1] FALSE
# }
```
