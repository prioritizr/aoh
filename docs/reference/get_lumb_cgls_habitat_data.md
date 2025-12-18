# Get Lumbierres *et al.* (2021) CGLS habitat classification data

Import habitat classification data derived from the Copernicus Global
Land Service Land Cover (CGLS-LC100) dataset (Buchhorn *et al.*, 2019;
Buchhorn *et al.*, 20200) following Lumbierres *et al.* (2021). If data
are not available locally, they are downloaded from a Zenodo repository
([doi:10.5281/zenodo.6622059](https://doi.org/10.5281/zenodo.6622059) ).

## Usage

``` r
get_lumb_cgls_habitat_data(
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

The data were produced by obtaining the level 1 habitat classification
data (derived from Lumbierres *et al.* 2021), and resampling the data
(using nearest neighbor interpolation) to the World Behrmannn coordinate
reference system (ESRI:54017).

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

See
[`crosswalk_lumb_cgls_data()`](https://prioritizr.github.io/aoh/reference/crosswalk_lumb_cgls_data.md)
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
habitat_data <- get_lumb_cgls_habitat_data(download_dir, version = "latest")

# preview data
print(habitat_data)
#> class       : SpatRaster 
#> size        : 132926, 347351, 1  (nrow, ncol, nlyr)
#> resolution  : 100, 100  (x, y)
#> extent      : -17367531, 17367569, -6005523, 7287077  (xmin, xmax, ymin, ymax)
#> coord. ref. : World_Behrmann 
#> source      : lumbierres-10-5281_zenodo-5146073-v2.tif 
#> name        : habitat_CGLS 

# plot data
plot(habitat_data)

# }
```
