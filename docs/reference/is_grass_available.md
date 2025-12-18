# Is GRASS available?

Check if the Geographic Resources Analysis Support System (GRASS) is
available for processing data.

## Usage

``` r
is_grass_available()
```

## Value

A `logical` value indicating if it is available.

## Details

The function verifies that (1) the rgrass package in installed, (2) the
link2GI package in installed, (3) GRASS is installed (i.e., via
[link2GI::findGRASS](https://r-spatial.github.io/link2GI/reference/findGRASS.html)),
and (4) the version of GRASS installed is at least 7.8.7. If any of
these checks fail, then GRASS is not considered available.

## Examples

``` r
# \dontrun{
# check if GRASS is available?
print(is_grass_available())
#> [1] TRUE
# }
```
