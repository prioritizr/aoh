# Crosswalk data for CGLS based on Lumbierres *et al.* (2021)

The [International Union for Conservation of Nature (IUCN) Red List of
Threatened Species](https://www.iucnredlist.org/) provides a [habitat
classification
scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
for different habitat types. To map the spatial location of each of
these habitat classes, Lumbierres *et al.* (2021) developed a global
habitat classification dataset using the Copernicus Global Land Service
Land Cover (CGLS-LC100) dataset (Buchhorn *et al.*, 2019; Buchhorn *et
al.*, 20200). This dataset provides a crosswalk table to associate the
values in the raster dataset with IUCN habitat classes.

## Usage

``` r
data(crosswalk_lumb_cgls_data)
```

## Format

A data frame
([`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html))
object with 159 rows and 2 columns. Each row corresponds to a different
IUCN habitat class. It has the following columns:

- code:

  The `character` code for a given IUCN habitat class.

- value:

  The `numeric` value assigned to grid cells in the raster data that
  contain the IUCN habitat class (see
  [`get_lumb_cgls_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_lumb_cgls_habitat_data.md)).

## Source

The data were derived from Lumbierres *et al.* (2021).

## References

Buchhorn M, Smets B, Bertels L, Lesiv M, Tsendbazar N-E, Herold M, and
Fritz SA (2019) Copernicus Global Land Service: Land Cover 100m: Epoch
2015: Globe. *Dataset of the global component of the Copernicus Land
Monitoring Service*.
[doi:10.5281/zenodo.3243508](https://doi.org/10.5281/zenodo.3243508)

Buchhorn M, Smets B, Bertels L, de Roo B, Lesiv M, Tsendbazar N-E,
Linlin L, and Tarko A (2020) *Copernicus Global Land Service: Land Cover
100m: Version 3 Globe 2015–2019: Product User Manual*. Geneve: Zenodo.
[doi:10.5281/zenodo.3606295](https://doi.org/10.5281/zenodo.3606295)

Lumbierres M, Dahal PR, Di Marco M, Butchart SHM, Donald PF, and
Rondinini C (2021) Translating habitat class to land cover to map area
of habitat of terrestrial vertebrates. *Conservation Biology*, 36,
e13851. [doi:10.1111/cobi.13851](https://doi.org/10.1111/cobi.13851)

## See also

A preprocessed version of the habitat classification data can be
imported using
[`get_lumb_cgls_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_lumb_cgls_habitat_data.md).

## Examples

``` r
# load data
data(crosswalk_lumb_cgls_data)

# print data
print(crosswalk_lumb_cgls_data)
#> # A tibble: 159 × 2
#>    code  value
#>    <chr> <int>
#>  1 1       100
#>  2 1.1     100
#>  3 1.2     100
#>  4 1.3     100
#>  5 1.4     100
#>  6 1.5     100
#>  7 1.6     100
#>  8 1.7     100
#>  9 1.8     100
#> 10 1.9     100
#> # ℹ 149 more rows
```
