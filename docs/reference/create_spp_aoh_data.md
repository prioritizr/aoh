# Create Area of Habitat data

Create Area of Habitat (AOH) data for species(Brooks *et al.* 2019).
Briefly, this function creates Area of Habitat data for each seasonal
distribution of each species and then stores the results as raster files
on disk. Please note that these procedures are designed for terrestrial
species, and will not apply to marine or freshwater species.

## Usage

``` r
create_spp_aoh_data(
  x,
  output_dir,
  elevation_data = NULL,
  habitat_data = NULL,
  crosswalk_data = NULL,
  cache_dir = tempdir(),
  habitat_version = "latest",
  elevation_version = "latest",
  force = FALSE,
  n_threads = 1,
  cache_limit = 1000,
  engine = "terra",
  rasterize_touches = FALSE,
  verbose = TRUE
)
```

## Arguments

- x:

  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) Spatial
  data delineating species' geographic ranges, habitat preferences, and
  elevational limits. This object should be created using the
  [`create_spp_info_data()`](https://prioritizr.github.io/aoh/reference/create_spp_info_data.md)
  function.

- output_dir:

  `character` Folder path to save raster (GeoTIFF) files containing the
  Area of Habitat data.

- elevation_data:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  Raster data delineating the worldwide elevation data (e.g., Robinson
  *et al.* 2014). Defaults to `NULL` such that data are automatically
  obtained (using
  [`get_global_elevation_data()`](https://prioritizr.github.io/aoh/reference/get_global_elevation_data.md)).
  If the data are obtained automatically, then a preprocessed version of
  the habitat data will be used to reduce processing time.

- habitat_data:

  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  Raster data indicating the presence of different habitat classes
  across world (e.g., Jung *et al.* 2020a,b; Lumbierres *et al.* 2021).
  Each grid cell should contain an `integer` value that specifies which
  habitat class is present within the cell (based on the argument to
  `crosswalk_data`). Defaults to `NULL` such that data are automatically
  obtained (using
  [`get_lumb_cgls_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_lumb_cgls_habitat_data.md)).

- crosswalk_data:

  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) Table
  containing data that indicate which grid cell values in the argument
  to `habitat_data` correspond to which IUCN habitat classification
  codes. The argument should contain a `code` column that specifies a
  set of IUCN habitat classification codes (see
  [`iucn_habitat_data()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_data.md),
  and a `value` column that specifies different values in the argument
  to `habitat_data`. Defaults to `NULL` such that the crosswalk for the
  default habitat data are used (i.e.,
  [`crosswalk_lumb_cgls_data()`](https://prioritizr.github.io/aoh/reference/crosswalk_lumb_cgls_data.md)).

- cache_dir:

  `character` Folder path for downloading and caching data. By default,
  a temporary directory is used (i.e.,
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html)). **To avoid
  downloading the same data multiple times, it is strongly recommended
  to specify a persistent storage location (see Examples below).**

- habitat_version:

  `character` Version of the habitat dataset that should be used. See
  documentation for the the `version` parameter in the
  [`get_lumb_cgls_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_lumb_cgls_habitat_data.md)
  function for further details. This parameter is only used if habitat
  data are obtained automatically (i.e., the argument to `habitat_data`
  is `NULL`). Defaults to `"latest"` such that the most recent version
  of the dataset is used if data need to be obtained.

- elevation_version:

  `character` Version of the elevation dataset that should be used. See
  documentation for the the `version` parameter in the
  [`get_global_elevation_data()`](https://prioritizr.github.io/aoh/reference/get_global_elevation_data.md)
  function for further details. This parameter is only used if elevation
  data are obtained automatically (i.e., the argument to
  `elevation_data` is `NULL`). Defaults to `"latest"` such that the most
  recent version of the dataset is used if data need to be obtained.

- force:

  `logical` Should the data be downloaded even if the the data are
  already available? Defaults to `FALSE`.

- n_threads:

  `integer` Number of computational threads to use for data processing.
  To reduce run time, it is strongly recommended to set this parameter
  based on available resources (see Examples section below). Note that
  parallel processing is only used for processing the habitat
  classification and elevation data. As such, this parameter will have
  no influence when using preprocessed datasets. Defaults to 1.

- cache_limit:

  `integer` Amount of memory (Mb) for caching when processing spatial
  data with the Geospatial Data Abstraction Library (GDAL). This
  parameter is only used when using the `"gdal"` engine. If possible, it
  is recommended to set this as parameter to at 5000 (assuming there is
  at least 8Gb memory available on the system). Defaults to 1000.

- engine:

  `character` Value indicating the name of the software to use for data
  processing. Available options include `"terra"`, `"gdal"`, or
  `"grass"` (see below for details). Defaults to `"terra"`.

- rasterize_touches:

  `logical` How should `x` (the species' range data) be rasterized when
  overlapped with the elevation and habitat raster data? If
  `rasterize_touches = FALSE`, the species' range data are treated as
  overlapping with a raster cell, if the range data overlap with the
  centroid of the raster cell. If `rasterize_touches = TRUE`, then the
  species' range data are treated as overlapping with a raster cell, if
  the range data overlap with any part of the raster cell. Since some
  species' ranges might be too small to overlap with the centroid of any
  raster cells (meaning that the output Area of Habitat map does not
  contain any suitable habitat for the species), it may be preferable to
  use `rasterize_touches = TRUE`. Note that `rasterize_touches = TRUE`
  is not compatible with the GRASS engine. Defaults to `FALSE`
  (following Lumbierres *et al.* 2022).

- verbose:

  `logical` Should progress be displayed while processing data? Defaults
  to `TRUE`.

## Value

A [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html)
object. This object is an updated version of the argument to `x`, and
contains additional columns describing the output raster files.
Specifically, it contains the following columns:

- id_no:

  `numeric` species' taxon identifier on the IUCN Red List.

- binomial:

  `character` species name.

- category:

  `character` IUCN Red List threat category.

- migratory:

  `logical` indicating if the species was processed as a migratory
  species (i.e., it had a *breeding*, *non-breeding*, or *passage*
  seasonal distribution).

- seasonal:

  `numeric` seasonal distribution code.

- full_habitat_code:

  `character` all habitat classification codes that contain suitable
  habitat for the species. If a given species has multiple suitable
  habitat classes, then these are denoted using a pipe-delimited format.
  For example, if the habitat classes denoted with the codes `"1.5"` and
  `"1.9"` were considered suitable for a given species, then these codes
  would be indicated as `"1.5|1.9"`.

- habitat_code:

  `character` habitat codes used to create the species' Area of Habitat
  data. Since the argument to `habitat_data` may not contain habitat
  classes for all suitable habitats for a given species (e.g., the
  default dataset does not contain subterranean cave systems), this
  column contains the subset of the habitat codes listed in the
  `"full_habitat_code"` column that were used for processing.

- elevation_lower:

  `numeric` lower elevation threshold used to create the species' Area
  of Habitat data.

- elevation_upper:

  `numeric` upper elevation threshold used to create the species' Area
  of Habitat data.

- elevation_upper:

  `numeric` upper elevation threshold used to create the species' Area
  of Habitat data.

- xmin:

  `numeric` value describing the spatial extent of the output raster
  file.

- xmax:

  `numeric` value describing the spatial extent of the output raster
  file.

- ymin:

  `numeric` value describing the spatial extent of the output raster
  file.

- ymax:

  `numeric` value describing the spatial extent of the output raster
  file.

- path:

  `character` file paths for the output raster files (see Output file
  format section for details).

## Engines

This function can use different software engines for data processing
(specified via argument to `engine`). Although each engine produces the
same results, some engines are more computationally efficient than
others. The default `"terra"` engine uses the terra package for
processing. Although this engine is easy to install and fast for small
datasets, it does not scale well for larger datasets. It is generally
recommended to use the `"gdal"` engine to perform data processing with
the [Geospatial Data Abstraction Library (GDAL)](https://gdal.org/) can
be used for data processing. The `"grass"` engine can also be used to
perform data processing with the [Geographic Resources Analysis Support
System (GRASS)](https://grass.osgeo.org/). Note that the `"grass"`
engine requires both the GDAL and GRASS software to be installed. For
instructions on installing dependencies for these engines, please see
the README file.

## Data processing

The Area of Habitat data are produced using the following procedures.
After these data are generated, they stored as files on disk (see Output
file format section for details).

1.  Global elevation and habitat classification are imported, (if
    needed,, see
    [`get_global_elevation_data()`](https://prioritizr.github.io/aoh/reference/get_global_elevation_data.md)
    and
    [`get_lumb_cgls_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_lumb_cgls_habitat_data.md),
    for details)., If these data are not available in the cache
    directory, (i.e. argument to `cache_dir`), then they are
    automatically downloaded, to the cache directory., Note that if
    elevation and habitat data are supplied, (i.e. as arguments to
    `elevation_data` and `habitat_data`), then, the user-supplied
    datasets are used to generate Area of Habitat data., ,

2.  The Area of Habitat data are then generated for each seasonal,
    distribution of each species. For a given species' distribution,,
    the data are generated by, (i) cropping the habitat classification
    and elevation data to the spatial, extent of the species' seasonal
    distribution;, (ii) converting the habitat classification data to a
    binary layer, denoting suitable habitat for the species'
    distribution, (using the habitat affiliation data for the species'
    distribution);, (iii) creating a mask based on the species'
    elevational limits, and the elevation data, and then using the mask
    to set values, in the binary layer to zero if they are outside of
    the species', limits;, (iv) creating a mask by rasterizing the
    species' seasonal, distribution, and then using the mask to set
    values in the binary, layer to missing (`NA`) values if they are
    outside the species', distribution;, (v) saving the binary layer as
    the Area of Habitat data, for the species' distribution., Note that
    species' distributions that already have Area of Habitat data,
    available in the output directory are skipped, (unless the argument
    to `force` is `TRUE`).

3.  Post-processing routines are used to prepare the results. These
    routines involve updating the collated species data to include file
    names and spatial metadata for the Area of Habitat data.

## Output file format

Area of Habitat data are stored in a separate raster (GeoTIFF) file for
each seasonal distribution of each species. Each raster file is assigned
a file name based on a prefix and a combination of the species' taxon
identifier (per `id_no`/`SISID` column in `x`) and the identifier for
the seasonal distribution (per `seasonality` in `x`) (i.e., file names
are named according to `AOH_{$id_no}_${seasonality}.tif`). For a given
raster file, grid cell values denote the presence (using a value of 1)
and absence (using a value of 0) of suitable habitat inside a given
species' distribution. Missing (`NA`) values correspond to grid cells
that are located outside of the species' distribution.

## References

Brooks TM, Pimm SL, Akçakaya HR, Buchanan GM, Butchart SHM, Foden W,
Hilton-Taylor C, Hoffmann M, Jenkins CN, Joppa L, Li BV, Menon V,
Ocampo-Peñuela N, Rondinini C (2019) Measuring terrestrial Area of
Habitat (AOH) and its utility for the IUCN Red List. *Trends in Ecology
& Evolution*, 34, 977–986.
[doi:10.1016/j.tree.2019.06.009](https://doi.org/10.1016/j.tree.2019.06.009)

Jung M, Dahal PR, Butchart SHM, Donald PF, De Lamo X, Lesiv M, Kapos V,
Rondinini C, and Visconti P (2020a) A global map of terrestrial habitat
types. *Scientific Data*, 7, 1–8.
[doi:10.1038/s41597-020-00599-8](https://doi.org/10.1038/s41597-020-00599-8)

Jung M, Dahal PR, Butchart SHM, Donald PF, De Lamo X, Lesiv M, Kapos V,
Rondinini C, and Visconti P (2020b) A global map of terrestrial habitat
types (insert version) \[Data set\]. *Zenodo*.
[doi:10.5281/zenodo.4058819](https://doi.org/10.5281/zenodo.4058819)

Lumbierres M, Dahal PR, Di Marco M, Butchart SHM, Donald PF, and
Rondinini C (2021) Translating habitat class to land cover to map area
of habitat of terrestrial vertebrates. *Conservation Biology*, 36,
e13851. [doi:10.1111/cobi.13851](https://doi.org/10.1111/cobi.13851)

Lumbierres M, Dahal PR, Soria CD, Di Marco M, Butchart SHM, Donald PF,
and Rondinini C (2022) Area of Habitat maps for the world’s terrestrial
birds and mammals. *Scientific Data*, 9, 749.

Robinson N, Regetz J, and Guralnick RP (2014) EarthEnv-DEM90: A nearly-
global, void-free, multi-scale smoothed, 90m digital elevation model
from fused ASTER and SRTM data. *ISPRS Journal of Photogrammetry and
Remote Sensing*, 87, 57–67.
[doi:10.1016/j.isprsjprs.2013.11.002](https://doi.org/10.1016/j.isprsjprs.2013.11.002)

## Examples

``` r
# \dontrun{
# find file path for example range data following IUCN Red List data format
## N.B., the range data were not obtained from the IUCN Red List,
## and were instead based on data from GBIF (https://www.gbif.org/)
path <- system.file("extdata", "EXAMPLE_SPECIES.zip", package = "aoh")

# import data
spp_range_data <- read_spp_range_data(path)

# specify settings for data processing
output_dir <- tempdir()                       # folder to save AOH data
cache_dir <- rappdirs::user_data_dir("aoh")   # persistent storage location
n_threads <- parallel::detectCores() - 1      # speed up analysis

# create cache directory if needed
if (!file.exists(cache_dir)) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
}

# create species information data
spp_info_data <- create_spp_info_data(
  x = spp_range_data,
  cache_dir = cache_dir
)
#> ℹ initializing
#> ✔ initializing [581ms]
#> 
#> ℹ cleaning species range data
#> ✔ cleaning species range data [3.6s]
#> 
#> ℹ importing species summary data
#> ✔ importing species summary data [773ms]
#> 
#> ℹ importing species habitat data
#> ✔ importing species habitat data [568ms]
#> 
#> ℹ collating species data
#> ✔ collating species data [244ms]
#> 
#> ℹ post-processing results
#> ✔ post-processing results [14ms]
#> 
#> ✔ finished

# create Area of Habitat data for species
# N.B., the default based on Lumbierres et al. (2021)
spp_aoh_data <- create_spp_aoh_data(
  x = spp_info_data,
  output_dir = output_dir,
  n_threads = n_threads,
  cache_dir = cache_dir
)
#> ℹ initializing
#> ✔ initializing [5ms]
#> 
#> ℹ importing global elevation data
#> ✔ importing global elevation data [6.8s]
#> 
#> ℹ importing global habitat data
#> ! `crosswalk_data` is missing the following 2 habitat classification codes: "7.1", "7.2"
#> ℹ importing global habitat data
#> ✔ importing global habitat data [4.1s]
#> 
#> ℹ generating Area of Habitat data
#>     skipping 4 species distributions already processed
#> ✔ generating Area of Habitat data [33ms]
#> 
#> ℹ post-processing results
#> ✔ post-processing results [12ms]
#> 
#> ✔ finished
# }

if (FALSE) { # interactive()
# \dontrun{
# preview data
print(spp_aoh_data)
# }
}
# \dontrun{
# import AOH data as a list of terra::rast() objects
spp_aoh_rasters <- lapply(spp_aoh_data$path, terra::rast)

# print AOH data list
print(spp_aoh_rasters)
#> [[1]]
#> class       : SpatRaster 
#> size        : 2593, 3701, 1  (nrow, ncol, nlyr)
#> resolution  : 100, 100  (x, y)
#> extent      : -467931, -97831, 4364377, 4623677  (xmin, xmax, ymin, ymax)
#> coord. ref. : World_Behrmann 
#> source      : 979_1.tif 
#> name        : lyr1 
#> min value   :    0 
#> max value   :    1 
#> 
#> [[2]]
#> class       : SpatRaster 
#> size        : 2266, 5670, 1  (nrow, ncol, nlyr)
#> resolution  : 100, 100  (x, y)
#> extent      : -248331, 318669, 4838277, 5064877  (xmin, xmax, ymin, ymax)
#> coord. ref. : World_Behrmann 
#> source      : 59448_1.tif 
#> name        : lyr1 
#> min value   :    0 
#> max value   :    1 
#> 
#> [[3]]
#> class       : SpatRaster 
#> size        : 5149, 5361, 1  (nrow, ncol, nlyr)
#> resolution  : 100, 100  (x, y)
#> extent      : -914731, -378631, 4551877, 5066777  (xmin, xmax, ymin, ymax)
#> coord. ref. : World_Behrmann 
#> source      : 4657_1.tif 
#> name        : lyr1 
#> min value   :    0 
#> max value   :    1 
#> 
#> [[4]]
#> class       : SpatRaster 
#> size        : 4978, 7512, 1  (nrow, ncol, nlyr)
#> resolution  : 100, 100  (x, y)
#> extent      : -904331, -153131, 4568977, 5066777  (xmin, xmax, ymin, ymax)
#> coord. ref. : World_Behrmann 
#> source      : 58622_1.tif 
#> name        : lyr1 
#> min value   :    0 
#> max value   :    1 
#> 

# plot the data to visualize the range maps and AOH data
plot_spp_aoh_data(spp_aoh_data)

# }
```
