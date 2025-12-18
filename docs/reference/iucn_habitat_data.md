# IUCN habitat classification codes

The [International Union for Conservation of Nature (IUCN) Red List of
Threatened Species](https://www.iucnredlist.org/) provides a [habitat
classification
scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
for different habitat types worldwide. This dataset provides details on
the different IUCN habitat classes.

## Usage

``` r
data(iucn_habitat_data)
```

## Format

A data frame with 126 rows and 2 columns. Each row corresponds to a
different habitat class, and each column contains information about a
given habitat class. It contains columns with the following values for
each habitat class.

- code:

  A `character` value indicating the code of a class.

- name:

  A `character` value indicating the name of a class.

- is_terrestrial:

  A `logical` value indicating if the class occurs within the
  terrestrial environmental.

- is_marine:

  A `logical` value indicating if the class occurs within the marine
  environmental.

- is_artificial:

  A `logical` value indicating if the class is artificial (e.g.,
  anthropogenically human modified).

- is_misc:

  A `logical` value indicating if the class does not correspond to a
  specific land cover.

- is_introduced:

  A `logical` value indicating if the class corresponds to introduced
  vegetation.

## Source

The data were obtained from
<https://www.iucnredlist.org/resources/habitat-classification-scheme>.

## Examples

``` r
# load data
data(iucn_habitat_data)

# print data
print(iucn_habitat_data)
#> # A tibble: 126 × 7
#>    code  name       is_terrestrial is_marine is_artificial is_misc is_introduced
#>    <chr> <chr>      <lgl>          <lgl>     <lgl>         <lgl>   <lgl>        
#>  1 1     Forest     TRUE           FALSE     FALSE         FALSE   FALSE        
#>  2 1.1   Forest - … TRUE           FALSE     FALSE         FALSE   FALSE        
#>  3 1.2   Forest - … TRUE           FALSE     FALSE         FALSE   FALSE        
#>  4 1.3   Forest - … TRUE           FALSE     FALSE         FALSE   FALSE        
#>  5 1.4   Forest - … TRUE           FALSE     FALSE         FALSE   FALSE        
#>  6 1.5   Forest - … TRUE           FALSE     FALSE         FALSE   FALSE        
#>  7 1.6   Forest - … TRUE           FALSE     FALSE         FALSE   FALSE        
#>  8 1.7   Forest - … TRUE           FALSE     FALSE         FALSE   FALSE        
#>  9 1.8   Forest - … TRUE           FALSE     FALSE         FALSE   FALSE        
#> 10 1.9   Forest - … TRUE           FALSE     FALSE         FALSE   FALSE        
#> # ℹ 116 more rows
```
