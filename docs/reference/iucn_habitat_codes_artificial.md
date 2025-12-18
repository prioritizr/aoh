# IUCN Red List codes for artificial habitats

The [International Union for Conservation of Nature (IUCN) Red List of
Threatened Species](https://www.iucnredlist.org/) provides a [habitat
classification
scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
for different habitat types worldwide. This function returns codes for
habitat types that correspond to *artificial* areas (e.g., urban areas,
pasture lands, canals).

## Usage

``` r
iucn_habitat_codes_artificial()
```

## Value

A `character` vector of codes.

## See also

Other codes:
[`iucn_habitat_codes_introduced()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_introduced.md),
[`iucn_habitat_codes_marine()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_marine.md),
[`iucn_habitat_codes_terrestrial()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_terrestrial.md)

## Examples

``` r
# print codes
print(iucn_habitat_codes_artificial())
#>  [1] "14"    "14.1"  "14.2"  "14.3"  "14.4"  "14.5"  "14.6"  "15"    "15.1" 
#> [10] "15.2"  "15.3"  "15.4"  "15.5"  "15.6"  "15.7"  "15.8"  "15.9"  "15.10"
#> [19] "15.11" "15.12" "15.13"
```
