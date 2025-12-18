# Repair geometry

Repair the geometry of a
[`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html)
object.

## Usage

``` r
st_repair_geometry(x, geometry_precision = 1e+05)
```

## Arguments

- x:

  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) object.

- geometry_precision:

  `numeric` level of precision for processing the spatial data (used
  with
  [`sf::st_set_precision()`](https://r-spatial.github.io/sf/reference/st_precision.html)).
  The default is 100000 (higher values indicate higher precision).
  Although this level of precision is generally suitable for fine-scale
  analyses, it might result in unnecessarily long computation times
  (e.g., 1500 is suitable for national-scale analyses). If you encounter
  geometry errors, increasing the argument to the parameter can
  sometimes resolve these issues.

## Details

This function works by first using the
[`sf::st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html)
function to attempt to fix geometry issues. Since the
[`sf::st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html)
function sometimes produce incorrect geometries in rare cases (e.g.,
when fixing invalid geometries that cross the dateline), this function
then uses the `st_prepair()` function from the prepr package to fix
those geometries instead (see <https://github.com/prioritizr/prepr> for
details).

## Installation

This function uses the prepr package to help repair geometries in
certain cases. Because the prepr package is not available on the
Comprehensive R Archive Network (CRAN), it must be installed from its
online code repository. To achieve this, please use the following code:

    if (!require(remotes)) install.packages("remotes")
    remotes::install_github("prioritizr/prepr")

Note that the prepr package has system dependencies that need to be
installed before the package itself can be installed (see package README
file for platform-specific instructions).

## Examples

``` r
# create sf object
p1 <- st_sf(
  id = 1,
  geometry = st_as_sfc("POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))", crs = 3857)
)

# repair geometry
p2 <- st_repair_geometry(p1)

# print object
print(p2)
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 10 ymax: 10
#> Projected CRS: WGS 84 / Pseudo-Mercator
#>   id                       geometry
#> 1  1 MULTIPOLYGON (((0 0, 0 10, ...
```
