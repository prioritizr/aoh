# Crosswalk data for Jung (2020) potential habitat classification (level 1)

The [International Union for Conservation of Nature (IUCN) Red List of
Threatened Species](https://www.iucnredlist.org/) provides a [habitat
classification
scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
for different habitat types. To map the spatial location of where
restoration actions might produce these habitat classes, Jung (2020)
developed a global raster dataset based on potential vegetation data.

## Usage

``` r
data(crosswalk_jung_plvl1_data)
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
  [`get_jung_plvl1_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_jung_plvl1_habitat_data.md)).

## Source

The data were obtained from Jung (2020).

## References

Jung M (2020) A layer of global potential habitats (insert version)
\[Data set\]. *Zenodo*.
[doi:10.5281/zenodo.4038749](https://doi.org/10.5281/zenodo.4038749)

## See also

A preprocessed version of the habitat classification data can be
imported using
[`get_jung_plvl1_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_jung_plvl1_habitat_data.md).

## Examples

``` r
# load data
data(crosswalk_jung_plvl1_data)

# print data
print(crosswalk_jung_plvl1_data)
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
