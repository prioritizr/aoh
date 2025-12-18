# IUCN Red List codes for terrestrial habitats

The [International Union for Conservation of Nature (IUCN) Red List of
Threatened Species](https://www.iucnredlist.org/) provides a [habitat
classification
scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
for different habitat types worldwide. This function returns codes for
habitat classes that occur on land.

## Usage

``` r
iucn_habitat_codes_terrestrial()
```

## Value

A `character` vector of codes.

## See also

Other codes:
[`iucn_habitat_codes_artificial()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_artificial.md),
[`iucn_habitat_codes_introduced()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_introduced.md),
[`iucn_habitat_codes_marine()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_marine.md)

## Examples

``` r
# print codes
print(iucn_habitat_codes_terrestrial())
#>  [1] "1"     "1.1"   "1.2"   "1.3"   "1.4"   "1.5"   "1.6"   "1.7"   "1.8"  
#> [10] "1.9"   "2"     "2.1"   "2.2"   "3"     "3.1"   "3.2"   "3.3"   "3.4"  
#> [19] "3.5"   "3.6"   "3.7"   "3.8"   "4"     "4.1"   "4.2"   "4.3"   "4.4"  
#> [28] "4.5"   "4.6"   "4.7"   "5"     "5.1"   "5.2"   "5.3"   "5.4"   "5.5"  
#> [37] "5.6"   "5.7"   "5.8"   "5.9"   "5.10"  "5.11"  "5.12"  "5.13"  "5.14" 
#> [46] "5.15"  "5.16"  "5.17"  "5.18"  "6"     "7"     "7.1"   "7.2"   "8"    
#> [55] "8.1"   "8.2"   "8.3"   "12"    "12.1"  "12.2"  "12.3"  "12.4"  "12.5" 
#> [64] "12.7"  "12.6"  "13"    "13.1"  "13.2"  "13.3"  "13.4"  "13.5"  "14"   
#> [73] "14.1"  "14.2"  "14.3"  "14.4"  "14.5"  "14.6"  "15"    "15.1"  "15.2" 
#> [82] "15.3"  "15.4"  "15.5"  "15.6"  "15.7"  "15.8"  "15.9"  "15.10" "15.13"
#> [91] "16"    "17"    "18"   
```
