# IUCN threat classification codes

The [International Union for Conservation of Nature (IUCN) Red List of
Threatened Species](https://www.iucnredlist.org/) provides a [threat
classification
scheme](https://www.iucnredlist.org/resources/threat-classification-scheme)
for different threatening processes. This dataset provides details on
the different IUCN threat classes.

## Usage

``` r
data(iucn_threat_data)
```

## Format

A data frame with 130 rows and 2 columns. Each row corresponds to a
different threat class, and each column contains information about a
given threat class. It contains columns with the following values for
each threat class.

- code:

  A `character` value indicating the code of a class.

- name:

  A `character` value indicating the name of a class.

## Source

The data were obtained from
<https://www.iucnredlist.org/resources/threat-classification-scheme>.

## Examples

``` r
# load data
data(iucn_threat_data)

# print data
print(iucn_threat_data)
#> # A tibble: 130 × 2
#>    code  name                                
#>    <chr> <chr>                               
#>  1 1     Residential & commercial development
#>  2 1.1   Housing & urban areas               
#>  3 1.2   Commercial & industrial areas       
#>  4 1.3   Tourism & recreation areas          
#>  5 2     Agriculture & aquaculture           
#>  6 2.1   Annual & perennial non-timber crops 
#>  7 2.1.1 Shifting agriculture                
#>  8 2.1.2 Small-holder farming                
#>  9 2.1.3 Agro-industry farming               
#> 10 2.1.4 Scale Unknown/Unrecorded            
#> # ℹ 120 more rows
```
