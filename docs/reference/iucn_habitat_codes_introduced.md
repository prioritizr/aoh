# IUCN Red List codes for introduced habitats

The [International Union for Conservation of Nature (IUCN) Red List of
Threatened Species](https://www.iucnredlist.org/) provides a [habitat
classification
scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
for different habitat types worldwide. This function returns codes for
habitat classes that correspond to *introduced vegetation*.

## Usage

``` r
iucn_habitat_codes_introduced()
```

## Value

A `character` vector of codes.

## See also

Other codes:
[`iucn_habitat_codes_artificial()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_artificial.md),
[`iucn_habitat_codes_marine()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_marine.md),
[`iucn_habitat_codes_terrestrial()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_terrestrial.md)

## Examples

``` r
# print codes
print(iucn_habitat_codes_introduced())
#> [1] "16"
```
