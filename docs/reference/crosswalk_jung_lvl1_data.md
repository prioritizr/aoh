# Crosswalk data for Jung *et al.* (2020) (level 1 classification)

The [International Union for Conservation of Nature (IUCN) Red List of
Threatened Species](https://www.iucnredlist.org/) provides a [habitat
classification
scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
for different habitat types. To map the spatial location of each of
these habitat classes, Jung *et al.* (2020a) developed a global raster
dataset by combining a range of different land use and land cover data
(Jung *et al.* 2020b). They created two habitat classification schemes –
termed level 1 and level 2 classification schemes – to describe
coarse-scale and fine-scale differences in habitat classes. This dataset
provides a crosswalk table to associate the values in the level 1 raster
dataset with IUCN habitat classes.

## Usage

``` r
data(crosswalk_jung_lvl1_data)
```

## Format

A data frame
([`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html))
object with 126 rows and 2 columns. Each row corresponds to a different
IUCN habitat class. It has the following columns:

- code:

  The `character` code for a given IUCN habitat class.

- value:

  The `numeric` value assigned to grid cells in the raster data that
  contain the IUCN habitat class (see
  [`get_jung_lvl1_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_jung_lvl1_habitat_data.md)).

## Source

The data were obtained from Jung *et al.* (2020b).

## References

Jung M, Dahal PR, Butchart SHM, Donald PF, De Lamo X, Lesiv M, Kapos V,
Rondinini C, and Visconti P (2020a) A global map of terrestrial habitat
types. *Scientific Data*, 7, 1–8.
[doi:10.1038/s41597-020-00599-8](https://doi.org/10.1038/s41597-020-00599-8)

Jung M, Dahal PR, Butchart SHM, Donald PF, De Lamo X, Lesiv M, Kapos V,
Rondinini C, and Visconti P (2020b) A global map of terrestrial habitat
types (insert version) \[Data set\]. *Zenodo*.
[doi:10.5281/zenodo.4058819](https://doi.org/10.5281/zenodo.4058819)

## See also

A preprocessed version of the habitat classification data can be
imported using
[`get_jung_lvl1_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_jung_lvl1_habitat_data.md).

## Examples

``` r
# load data
data(crosswalk_jung_lvl1_data)

# print data
print(crosswalk_jung_lvl1_data)
#> # A tibble: 126 × 2
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
#> # ℹ 116 more rows
```
