# Simulate species data

Simulate species data for creating Area of Habitat data (Brooks *et al.*
2019). Specifically, data are simulated to define species geographic
ranges, summary information, and habitat preferences.

## Usage

``` r
simulate_spp_data(
  n,
  boundary_data,
  habitat_data = NULL,
  elevation_data = NULL,
  crosswalk_data = NULL,
  rf_scale_min = 0.5,
  rf_scale_max = 0.7,
  cache_dir = tempdir(),
  habitat_version = "latest",
  force = FALSE,
  omit_habitat_codes = iucn_habitat_codes_marine(),
  verbose = TRUE
)
```

## Arguments

- n:

  `integer` Number of species to simulate.

- boundary_data:

  [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html)
  Spatial object delineating the spatial extent and boundary for
  simulating species ranges.

- habitat_data:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  Raster data indicating the presence of different habitat classes
  across world (e.g., Jung *et al.* 2020a,b; Lumbierres *et al.* 2021).
  Each grid cell should contain an `integer` value that specifies which
  habitat class is present within the cell (based on the argument to
  `crosswalk_data`). Defaults to `NULL` such that data are automatically
  obtained (using
  [`get_lumb_cgls_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_lumb_cgls_habitat_data.md)).

- elevation_data:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  Raster data delineating the worldwide elevation data (e.g., Robinson
  *et al.* 2014). Defaults to `NULL` such that data are automatically
  obtained (using
  [`get_global_elevation_data()`](https://prioritizr.github.io/aoh/reference/get_global_elevation_data.md)).
  If the data are obtained automatically, then a preprocessed version of
  the habitat data will be used to reduce processing time.

- crosswalk_data:

  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) Table
  containing data that indicate which grid cell values in the argument
  to `habitat_data` correspond to which IUCN habitat classification
  codes. The argument should contain a `code` column that specifies a
  set of IUCN habitat classification codes (see
  [`iucn_habitat_data()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_data.md),
  and a `value` column that specifies different values in the argument
  to `habitat_data`. Defaults to `NULL` such that the crosswalk for the
  default habitat data are used (i.e.,
  [`crosswalk_lumb_cgls_data()`](https://prioritizr.github.io/aoh/reference/crosswalk_lumb_cgls_data.md)).

- rf_scale_min:

  `numeric` Minimum scaling parameter used to control the smallest
  possible level of spatial auto-correlation for simulated species
  ranges. Defaults to 0.5.

- rf_scale_max:

  `numeric` Minimum scaling parameter used to control the largest
  possible level of spatial auto-correlation for simulated species
  ranges. Defaults to `0.7`.

- cache_dir:

  `character` Folder path for downloading and caching data. By default,
  a temporary directory is used (i.e.,
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html)). **To avoid
  downloading the same data multiple times, it is strongly recommended
  to specify a persistent storage location (see Examples below).**

- habitat_version:

  `character` Version of the habitat dataset that should be used. See
  documentation for the the `version` parameter in the
  [`get_lumb_cgls_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_lumb_cgls_habitat_data.md)
  function for further details. This parameter is only used if habitat
  data are obtained automatically (i.e., the argument to `habitat_data`
  is `NULL`). Defaults to `"latest"` such that the most recent version
  of the dataset is used if data need to be obtained.

- force:

  `logical` Should the data be downloaded even if the the data are
  already available? Defaults to `FALSE`.

- omit_habitat_codes:

  `character` Habitat classification codes to omit from resulting Area
  of Habitat data. Please see the [IUCN Red List Habitat Classification
  Scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
  for the full range of habitat classification codes. For example, if
  the aim is to identify natural places that contain suitable
  conditions, then areas classified as anthropogenically modified
  ([`iucn_habitat_codes_artificial()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_artificial.md)),
  introduced vegetation
  ([`iucn_habitat_codes_introduced()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_introduced.md),
  or unknown habitat
  ([`iucn_habitat_codes_misc()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_misc.md))
  should be excluded. Defaults to
  [`iucn_habitat_codes_marine()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_marine.md),
  such that marine habitats are excluded.

- verbose:

  `logical` Should progress be displayed while processing data? Defaults
  to `TRUE`.

## Value

A `list` object containing simulated data that are formatted following
conventions used by the [International Union for Conservation of Nature
(IUCN) Red List of Threatened Species](https://www.iucnredlist.org/). It
contains the following elements:

- spp_range_data:

  A [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html)
  object containing the species' geographic range data.

- spp_summary_data:

  A
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  object containing summary information about the species (including
  elevational limit information.

- spp_habitat_data:

  A
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  object containing habitat preferences for the species.

- spp_threat_data:

  A
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  object containing threat information for the species.

## References

Brooks TM, Pimm SL, Akçakaya HR, Buchanan GM, Butchart SHM, Foden W,
Hilton-Taylor C, Hoffmann M, Jenkins CN, Joppa L, Li BV, Menon V,
Ocampo-Peñuela N, Rondinini C (2019) Measuring terrestrial Area of
Habitat (AOH) and its utility for the IUCN Red List. *Trends in Ecology
& Evolution*, 34, 977–986.
[doi:10.1016/j.tree.2019.06.009](https://doi.org/10.1016/j.tree.2019.06.009)

## See also

See
[`create_spp_aoh_data()`](https://prioritizr.github.io/aoh/reference/create_spp_aoh_data.md)
for creating Area of Habitat maps using data for real or simulated
species.

## Examples

``` r
# please ensure that the fields and smoothr packages are installed
# to run these examples

# \dontrun{
# define persistent storage location
download_dir <- rappdirs::user_data_dir("aoh")

# create download directory if needed
if (!file.exists(download_dir)) {
  dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
}

# specify file path for boundary data
boundary_path <- system.file("shape/nc.shp", package = "sf")

# import boundary data to simulate species data
boundary_data <- sf::st_union(sf::read_sf(boundary_path))

# set random number generator seeds for consistency
set.seed(500)

# simulate data for 5 species
x <- simulate_spp_data(
  n = 5, boundary_data = boundary_data, cache_dir = download_dir
)
#> ℹ importing global elevation data
#> ✔ importing global elevation data [7.2s]
#> 
#> ℹ importing global habitat data
#> ✔ importing global habitat data [38.6s]
#> 

# preview species range data
print(x$spp_range_data)
#> Simple feature collection with 17 features and 26 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -84.31762 ymin: 33.88392 xmax: -75.45662 ymax: 36.58807
#> Geodetic CRS:  WGS 84
#> # A tibble: 17 × 27
#>    id_no seasonal presence origin                     geometry binomial compiler
#>  * <int>    <dbl>    <dbl>  <dbl>               <GEOMETRY [°]> <chr>    <chr>   
#>  1   799        1        1      1 POLYGON ((-78.71844 34.0082… Simulus… Simulat…
#>  2   799        1        3      2 POLYGON ((-78.40331 34.2545… Simulus… Simulat…
#>  3  2102        1        1      1 MULTIPOLYGON (((-76.49551 3… Simulus… Simulat…
#>  4  2102        3        1      1 MULTIPOLYGON (((-76.86322 3… Simulus… Simulat…
#>  5  2102        4        1      1 MULTIPOLYGON (((-82.74959 3… Simulus… Simulat…
#>  6  2102        3        4      4 MULTIPOLYGON (((-77.15074 3… Simulus… Simulat…
#>  7  2102        3        3      3 MULTIPOLYGON (((-77.22261 3… Simulus… Simulat…
#>  8  4082        1        1      1 MULTIPOLYGON (((-78.57828 3… Simulus… Simulat…
#>  9  4082        1        1      6 MULTIPOLYGON (((-78.49659 3… Simulus… Simulat…
#> 10  4082        1        4      2 MULTIPOLYGON (((-78.2597 35… Simulus… Simulat…
#> 11  5167        1        1      1 MULTIPOLYGON (((-77.96057 3… Simulus… Simulat…
#> 12  5167        1        4      6 MULTIPOLYGON (((-79.6485 35… Simulus… Simulat…
#> 13  5167        1        1      6 MULTIPOLYGON (((-83.27596 3… Simulus… Simulat…
#> 14  5479        1        1      1 MULTIPOLYGON (((-76.48416 3… Simulus… Simulat…
#> 15  5479        1        1      6 MULTIPOLYGON (((-80.38436 3… Simulus… Simulat…
#> 16  5479        1        5      5 MULTIPOLYGON (((-79.79211 3… Simulus… Simulat…
#> 17  5479        1        3      3 MULTIPOLYGON (((-80.18005 3… Simulus… Simulat…
#> # ℹ 20 more variables: yrcompiled <dbl>, citation <chr>, subspecies <chr>,
#> #   subpop <chr>, source <chr>, island <chr>, tax_comm <chr>, dist_comm <chr>,
#> #   generalisd <int>, legend <chr>, kingdom <chr>, phylum <chr>, class <chr>,
#> #   order_ <chr>, family <chr>, genus <chr>, category <chr>, marine <chr>,
#> #   terrestial <chr>, freshwater <chr>

# preview species habitat preference data
print(x$spp_habitat_data)
#> # A tibble: 21 × 6
#>    id_no code  habitat                        suitability season majorimportance
#>    <int> <chr> <chr>                          <chr>       <chr>  <chr>          
#>  1   799 4.4   Grassland - Temperate          Suitable    Resid… NA             
#>  2   799 14.3  Plantations                    Suitable    Resid… NA             
#>  3   799 14.1  Arable Land                    Suitable    Resid… NA             
#>  4  2102 4.4   Grassland - Temperate          Suitable    Resid… NA             
#>  5  2102 3.4   Shrubland - Temperate          Suitable    Resid… NA             
#>  6  2102 5.15  Wetlands (inland) - Seasonal/… Suitable    Resid… NA             
#>  7  2102 3.4   Shrubland - Temperate          Suitable    Non-b… NA             
#>  8  2102 14.1  Arable Land                    Suitable    Non-b… NA             
#>  9  2102 14.3  Plantations                    Suitable    Non-b… NA             
#> 10  2102 14.3  Plantations                    Suitable    Passa… NA             
#> # ℹ 11 more rows

# preview species summary data
print(x$spp_summary_data)
#> # A tibble: 5 × 23
#>   id_no taxonid scientific_name   kingdom phylum class order family genus  
#>   <int>   <int> <chr>             <chr>   <chr>  <chr> <chr> <chr>  <chr>  
#> 1   799     799 Simulus spp. 799  NA      NA     NA    NA    NA     Simulus
#> 2  2102    2102 Simulus spp. 2102 NA      NA     NA    NA    NA     Simulus
#> 3  4082    4082 Simulus spp. 4082 NA      NA     NA    NA    NA     Simulus
#> 4  5167    5167 Simulus spp. 5167 NA      NA     NA    NA    NA     Simulus
#> 5  5479    5479 Simulus spp. 5479 NA      NA     NA    NA    NA     Simulus
#> # ℹ 14 more variables: main_common_name <chr>, authority <chr>,
#> #   published_year <chr>, assessment_date <chr>, category <chr>,
#> #   criteria <chr>, population_trend <chr>, marine_system <chr>,
#> #   freshwater_system <chr>, terrestrial_system <chr>, elevation_upper <dbl>,
#> #   elevation_lower <dbl>, depth_upper <dbl>, depth_lower <dbl>
# }
```
