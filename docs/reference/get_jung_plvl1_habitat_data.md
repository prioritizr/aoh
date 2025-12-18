# Get Jung (2020) potential habitat classification data (level 1)

Import potential habitat classification data derived from Jung (2020).
If data are not available locally, they are downloaded from a Zenodo
repository
([doi:10.5281/zenodo.6622090](https://doi.org/10.5281/zenodo.6622090) ).

## Usage

``` r
get_jung_plvl1_habitat_data(
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
object containing the potential habitat data (100 m resolution). Pixel
values indicate the potential habitat classification codes.

## Details

The data predict the potential habitat types that would be available in
a given location if not for anthropogenic activities (see Hengl *et al.*
2018 for details). As such, they can be used to identify locations where
restoration activities could produce certain habitat types. This means
that they can, in turn, be used to predict the location of suitable
habitat for species following the restoration activities. Since the data
do not show the historic distribution of habitat types, they cannot be
used to examine patterns of habitat loss.

The data were produced by obtaining the level 1 habitat classification
data from a Zenodo repository (Jung 2020), and resampling the data
(using nearest neighbor interpolation) to the World Behrmannn coordinate
reference systems (ESRI:54017).

## References

Jung M (2020) A layer of global potential habitats (insert version)
\[Data set\]. *Zenodo*.
[doi:10.5281/zenodo.4038749](https://doi.org/10.5281/zenodo.4038749)

## See also

See
[`crosswalk_jung_lvl1_data()`](https://prioritizr.github.io/aoh/reference/crosswalk_jung_lvl1_data.md)
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

# download and import potential habitat data
ph_data <- get_jung_plvl1_habitat_data(download_dir, version = "latest")

# preview data
print(ph_data)
#> class       : SpatRaster 
#> size        : 132926, 347351, 1  (nrow, ncol, nlyr)
#> resolution  : 100, 100  (x, y)
#> extent      : -17367531, 17367569, -6005523, 7287077  (xmin, xmax, ymin, ymax)
#> coord. ref. : World_Behrmann 
#> source      : jung-plvl1-10-5281_zenodo-4038749.tif 
#> name        : jung-plvl1-10-5281_zenodo-4038749 

# plot data
plot(ph_data)

# }
```
