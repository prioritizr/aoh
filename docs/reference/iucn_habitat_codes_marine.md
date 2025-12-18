# IUCN Red List codes for marine habitats

The [International Union for Conservation of Nature (IUCN) Red List of
Threatened Species](https://www.iucnredlist.org/) provides a [habitat
classification
scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
for different habitat types worldwide. This function returns codes for
habitat classes that occur in the ocean.

## Usage

``` r
iucn_habitat_codes_marine()
```

## Value

A `character` vector of codes.

## See also

Other codes:
[`iucn_habitat_codes_artificial()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_artificial.md),
[`iucn_habitat_codes_introduced()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_introduced.md),
[`iucn_habitat_codes_terrestrial()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_terrestrial.md)

## Examples

``` r
# print codes
print(iucn_habitat_codes_marine())
#>  [1] "9"      "9.1"    "9.2"    "9.3"    "9.4"    "9.5"    "9.6"    "9.7"   
#>  [9] "9.8"    "9.8.1"  "9.8.2"  "9.8.3"  "9.8.4"  "9.8.5"  "9.8.6"  "9.9"   
#> [17] "9.10"   "10"     "10.1"   "10.2"   "10.3"   "10.4"   "11"     "11.1"  
#> [25] "11.1.1" "11.2"   "11.1.2" "11.3"   "11.4"   "11.5"   "11.6"   "15.11" 
#> [33] "15.12" 
```
