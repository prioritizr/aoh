# Get Jung *et al.* (2020) habitat classification data (level 2)

Import habitat classification data (level 2) derived from Jung *et al.*
(2020a). If data are not available locally, they are downloaded from a
Zenodo repository
([doi:10.5281/zenodo.6622029](https://doi.org/10.5281/zenodo.6622029) ).

## Usage

``` r
get_jung_lvl2_habitat_data(
  dir = tempdir(),
  version = "latest",
  force = FALSE,
  verbose = TRUE
)
```

## Arguments

- dir:

  `character` Folder path for downloading and caching data. By default,
  data are downloaded to a temporary directory (i.e.,
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html)). **To avoid
  downloading the same data multiple times, it is strongly recommended
  to specify a persistent storage location (see Examples below).**

- version:

  `character` Value indicating the specific version of the dataset that
  should be downloaded. The version should be indicated using the
  Digital Object Identifier of the specific version required (e.g.
  `"10.5281/zenodo.3673586"`). Defaults to `"latest"` such that the
  latest release of the dataset with available habitat data is used.

- force:

  `logical` Should the data be downloaded even if the the data are
  already available? Defaults to `FALSE`.

- verbose:

  `logical` Should progress be displayed while downloading data?
  Defaults to `TRUE`.

## Value

A
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
object containing the habitat data (100 m resolution). Pixel values
indicate the habitat classification codes.

## Details

The data were produced by obtaining the level 2 habitat classification
data from a Zenodo repository (Jung *et al.* 2020b), and resampling the
data (using nearest neighbor interpolation) to the World Behrmannn
coordinate reference systems (ESRI:54017).

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

See
[`crosswalk_jung_lvl2_data()`](https://prioritizr.github.io/aoh/reference/crosswalk_jung_lvl2_data.md)
for details on which grid values correspond to which habitat
classification codes.

## Examples

``` r
# \dontrun{
# define persistent storage location
download_dir <- rappdirs::user_data_dir("aoh")

# create download directory if needed
if (!file.exists(download_dir)) {
  dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
}

# download and import habitat data
habitat_data <- get_jung_lvl2_habitat_data(download_dir, version = "latest")

# preview data
print(habitat_data)
#> class       : SpatRaster 
#> size        : 132926, 347351, 1  (nrow, ncol, nlyr)
#> resolution  : 100, 100  (x, y)
#> extent      : -17367531, 17367569, -6005523, 7287077  (xmin, xmax, ymin, ymax)
#> coord. ref. : World_Behrmann 
#> source      : jung-lvl2-10-5281_zenodo-4058819.tif 
#> name        : jung-lvl2-10-5281_zenodo-4058819 

# plot data
plot(habitat_data)

# }
```
