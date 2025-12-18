# Create species' information data

Create data to collate all the information needed to generate Area of
Habitat data (Brooks *et al.* 2019). Briefly, this function cleans and
collates data describing species' geographic ranges, habitat
preferences, and elevational limits. Please note that these procedures
are designed for terrestrial species, and will not apply to marine or
freshwater species.

## Usage

``` r
create_spp_info_data(
  x,
  spp_summary_data = NULL,
  spp_habitat_data = NULL,
  cache_dir = tempdir(),
  iucn_version = "latest",
  key = NULL,
  force = FALSE,
  keep_iucn_rl_presence = c(1, 2),
  keep_iucn_rl_origin = c(1, 2, 6),
  keep_iucn_rl_seasonal = c(1, 2, 3, 4),
  omit_habitat_codes = iucn_habitat_codes_marine(),
  adjust_elevational_limits = TRUE,
  adjust_habitat_codes = TRUE,
  crs = sf::st_crs("ESRI:54017"),
  geometry_precision = 1e+05,
  verbose = TRUE
)
```

## Arguments

- x:

  [`sf::sf()`](https://r-spatial.github.io/sf/reference/sf.html) Spatial
  data delineating species geographic ranges obtained from the [IUCN Red
  List](https://www.iucnredlist.org/). See below for details.

- spp_summary_data:

  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  Table containing summary information for each species (in the argument
  to `x`). Specifically, the argument should contain the following
  columns: `"id_no"`, `"elevation_lower"`, and `"elevation_upper"`
  columns. Here, `"id_no"` corresponds to the species' taxon identifier
  (also present in `x`), and the `"elevation_lower"` and
  `"elevation_upper"` columns indicate the lowest and highest elevations
  that contain habitat for the species. Defaults to `NULL` such that
  data are automatically obtained from the latest version of the [IUCN
  Red List](https://www.iucnredlist.org).

- spp_habitat_data:

  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
  Table containing habitat preference information for each species (in
  the argument to `x`). Specifically, the argument should contain the
  following columns: `"id_no"`, `"code"`, `"suitability"`, `"season"`
  columns. Here, `"id_no"` corresponds to the species' taxon identifier
  (also present in `x`), `"code"` indicates a habitat classification
  code that is suitable for the species (i.e., based on layer names in
  the argument to habitat_data), `"suitability"` indicates the level
  suitability of the habitat class for a given species (e.g., using
  values such as `"Suitable"` or `"Marginal"`), and `"season"` indicates
  if the habitat class is only suitable for a particular seasonal
  distribution (e.g., `"Breeding"`). Defaults to `NULL` such that data
  are automatically obtained from the latest version of the [IUCN Red
  List](https://www.iucnredlist.org).

- cache_dir:

  `character` Folder path for downloading and caching data. By default,
  a temporary directory is used (i.e.,
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html)). **To avoid
  downloading the same data multiple times, it is strongly recommended
  to specify a persistent storage location (see Examples below).**

- iucn_version:

  `character` Version of the IUCN Red List dataset that should be used.
  See documentation for the the `version` parameter in the
  [`get_spp_summary_data()`](https://prioritizr.github.io/aoh/reference/get_spp_summary_data.md)
  function for further details. Defaults to `"latest"` such that the
  most recent version of the dataset is used.

- key:

  `character` Token for querying the IUCN Red List API. Defaults to
  `NULL` such that the token accessed from the `"IUCN_REDLIST_KEY"`
  environmental variable (which can be specified in the .Renviron file).

- force:

  `logical` Should the data be downloaded even if the the data are
  already available? Defaults to `FALSE`.

- keep_iucn_rl_presence:

  `integer` IUCN Red List presence codes to retain (see IUCN SSC Red
  List Technical Working Group 2021 for details). Species' ranges that
  are not associated with these codes are excluded during data cleaning
  procedures. Defaults to a numeric vector containing `1` and `2`
  (corresponding to *extant* and *probably extant*).

- keep_iucn_rl_origin:

  `integer` IUCN Red List origin codes to retain (see IUCN SSC Red List
  Technical Working Group 2021 for details). Species' ranges that are
  not associated with these codes are excluded during data cleaning
  procedures. Defaults to a numeric vector containing `1`, `2`, and `6`.
  (corresponding to *native*, *reintroduced*, and *assisted
  colonization*).

- keep_iucn_rl_seasonal:

  `integer` IUCN Red List seasonal codes to retain (see IUCN SSC Red
  List Technical Working Group 2021 for details). Species' ranges that
  are not associated with these codes are excluded during data cleaning
  procedures. Defaults to a numeric vector containing `1`, `2`, `3`, and
  `4`. (corresponding to *resident*, *breeding season*, *non-breeding
  season*, and *passage* distributions).

- omit_habitat_codes:

  `character` Habitat classification codes to omit from resulting Area
  of Habitat data. Please see the [IUCN Red List Habitat Classification
  Scheme](https://www.iucnredlist.org/resources/habitat-classification-scheme)
  for the full range of habitat classification codes. For example, if
  the aim is to identify natural places that contain suitable
  conditions, then areas classified as anthropogenically modified
  ([`iucn_habitat_codes_artificial()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_artificial.md)),
  introduced vegetation
  ([`iucn_habitat_codes_introduced()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_introduced.md),
  or unknown habitat
  ([`iucn_habitat_codes_misc()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_misc.md))
  should be excluded. Defaults to
  [`iucn_habitat_codes_marine()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_marine.md),
  such that marine habitats are excluded.

- adjust_elevational_limits:

  `logical` Should elevation limits be adjusted to correct for errors?
  Defaults to `TRUE` to automatically fix errors (see Data processing
  section below for details).

- adjust_habitat_codes:

  `logical` Should habitat codes be adjusted so they are assigned to
  species' distributions following guidelines for Key Biodiversity
  areas? Defaults to `TRUE` (see Data processing section below for
  details). Otherwise, if `FALSE`, habitat codes are assigned to
  species' distributions following exact matches (e.g., only codes
  described for a *resident* distribution are assigned to the *resident*
  distribution).

- crs:

  A [`st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
  object containing the coordinate reference system for reprojecting the
  species' range data. This coordinate reference system should be the
  same as the elevation and habitat classification data that will
  subsequently be used to generate Area of Habitat data (using the
  [`create_spp_aoh_data()`](https://prioritizr.github.io/aoh/reference/create_spp_aoh_data.md)
  or
  [`create_spp_frc_data()`](https://prioritizr.github.io/aoh/reference/create_spp_frc_data.md)
  functions). Defaults to the World Behrmann coordinate reference system
  (ESRI:54017), which is appropriate for the elevation and habitat
  datasets available through the package (e.g., using the
  [`get_global_elevation_data()`](https://prioritizr.github.io/aoh/reference/get_global_elevation_data.md)
  and
  [`get_lumb_cgls_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_lumb_cgls_habitat_data.md)
  functions).

- geometry_precision:

  `numeric` level of precision for processing the spatial data (used
  with
  [`sf::st_set_precision()`](https://r-spatial.github.io/sf/reference/st_precision.html)).
  The default is 100000 (higher values indicate higher precision).
  Although this level of precision is generally suitable for fine-scale
  analyses, it might result in unnecessarily long computation times
  (e.g., 1500 is suitable for national-scale analyses). If you encounter
  geometry errors, increasing the argument to the parameter can
  sometimes resolve these issues.

- verbose:

  `logical` Should progress be displayed while processing data? Defaults
  to `TRUE`.

## Value

A [`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html)
object containing all the information needed to create Area of Habitat
data. It contains cleaned versions of the species' range maps and
columns containing the species' elevational limits and habitat
preferences. Specifically, the object contains the following columns:

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

- elevation_lower:

  `numeric` lower elevation threshold used to create the species' Area
  of Habitat data.

- elevation_upper:

  `numeric` upper elevation threshold used to create the species' Area
  of Habitat data.

- geometry:

  [`sf::st_sfc()`](https://r-spatial.github.io/sf/reference/sfc.html)
  geometries for species' distributions.

## Species range data format

Species range data are expected to follow the data format conventions
for the IUCN Red List (see [IUCN Red List
documentation](https://www.iucnredlist.org/resources/mappingstandards)
for details). Specifically, the argument to `x` should be an
[`sf::st_sf()`](https://r-spatial.github.io/sf/reference/sf.html) object
with the following columns: `id_no`, `presence`, `origin`, and
`seasonal`. It can also contain the following optional columns:
`terrestrial` (or `terrestial`), `freshwater`, and `marine`. Below we
provide a brief description of each column:

- `id_no`:

  `numeric` taxon identifier on the IUCN Red List.

- `presence`:

  `numeric` identifier describing information about the presence of the
  taxon in the range data.

- `origin`:

  `numeric` identifier describing if the species is native to the
  location(s) described by the range data.

- `seasonality`:

  `numeric` identifier describing if the species is occupied by the
  location(s) describe by the range data throughout the whole year, of
  if only during certain seasons.

- `terrestial`:

  `character` value indicating if the range data pertain to terrestrial
  environments (with `"true"` or `"false"` values.)

- `freshwater`:

  `character` value indicating if the range data pertain to freshwater
  environments (with `"true"` or `"false"` values.)

- `marine`:

  `character` value indicating if the range data pertain to marine
  environments (with `"true"` or `"false"` values.)

## Data processing

The species' information data are produced using the following
procedures.

1.  Species range data cleaned. By default, the range data are cleaned
    following guidelines for the identification of Key Biodiversity
    Areas (KBA Standards and Appeals Committee of IUCN SSC/WCPA 2022).
    Specifically, the default cleaning procedures involves excluding
    places where the (i) species' presence is not *extant* or *probably
    extant* (i.e. filtering based on `presence == 1` or
    `presence == 2`); (ii) species' origin is not *native*,
    *reintroduced*, or the result of *assisted colonization* (i.e.
    filtering based on `origin == 1`, `origin == 2`, or
    `origin == 6`); (iii) available information on which species'
    seasonal distribution is *uncertain* (i.e. filtering based on
    `seasonal != 5`); and (iv) species' distribution is not terrestrial
    (i.e. filtering based on where `terrestrial == "true"`).
    Additionally, the species' range data are spatially dissolved so
    that each seasonal distribution for each taxon is represented by a
    separate geometry. Finally, geoprocessing routines are used to
    detect and repair any invalid geometries.

2.  Species summary and habitat preferences data are imported (if
    needed, see
    [`get_spp_summary_data()`](https://prioritizr.github.io/aoh/reference/get_spp_summary_data.md)
    and
    [`get_spp_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_spp_habitat_data.md)
    for details). If these data are not available in the cache directory
    (i.e. argument to `cache_dir`), then they are automatically
    downloaded to the cache directory.

3.  If specified (per `adjust_elevational_limits = TRUE`), then the
    elevational limit values in the species summary data are adjusted to
    correct for errors. These adjustments applied based on the following
    procedures: (i) if a species lacks lower or upper elevational
    limits, then limits of -500 m and 9,000 m are assumed
    (respectively); (ii) since the IUCN Red List assigns lower limit
    values of 0 m for many species that have parts of their distribution
    in areas below sea level, lower elevational limit values equal to 0
    m are replaced with -500 m; (iii) lower elevational limit values
    below -500 m are replaced with -500 m; (iv) upper elevational limit
    values above 9000 are is replaced with 9000 m; (v) if a lower
    elevational limit is greater than an upper elevational limit, then
    limits of -500 m and 9000 m are assumed (respectively); (vi) if a
    lower elevational limit is within 50 m of an upper elevational
    limit, then limits are adjusted such that there is a 50 m difference
    between them. Otherwise, if `FALSE`, then elevation limit values are
    not altered.

4.  Each is species is classified as either migratory or non-migratory,
    based on the presence of *breeding*, *non-breeding*, or *passage*
    distributions in the species range data (i.e., x\`). For example, if
    a species only has a *resident* distribution in the species range
    data, then it is classified as a non-migratory species. If a species
    has a *resident* and a *breeding* distribution in the species range
    data, then it is classified as a migratory species.

5.  If specified (per `adjust_habitat_codes = TRUE`), then the habitat
    codes in the species habitat preferences data are adjusted based on
    guidelines for the identification of Key Biodiversity Areas (KBA
    Standards and Appeals Committee of IUCN SSC/WCPA 2022). These
    adjustments are based on the following procedures: (i) *resident*
    distributions for non-migratory species are assigned habitat codes
    described in the species habitat preference data for the species'
    *resident*, *breeding*, *non-breeding*, *passage*, *seasonal
    occurrence uncertain*, and missing (`NA`) seasonal
    distributions; (ii) *resident* distributions for migratory species
    are assigned habitat codes described in the species habitat
    preference data for the species' *resident*, *breeding*,
    *non-breeding*, *seasonal occurrence uncertain*, and missing (`NA`)
    seasonal distributions; (ii) *breeding* distributions are assigned
    habitat codes described for the species' *resident*, *breeding*,
    *seasonal occurrence uncertain* and missing (`NA`) seasonal
    distributions; (iii) *non-breeding* distributions are assigned
    habitat codes described for the species' *resident*, *non-breeding*,
    *seasonal occurrence uncertain*, and missing (`NA`) seasonal
    distributions; and (iv) *passage* distributions are assigned habitat
    codes described for the species' *resident*, *passage*, *seasonal
    occurrence uncertain*, and missing (`NA`) seasonal distributions. If
    the adjustments are not applied (per
    `adjust_habitat_codes = FALSE`), then the habitat codes are assigned
    to species' distributions based on exact matches (e.g., only codes
    described for a *resident* distribution are assigned to the
    *resident* distribution).

6.  Species information are collated into a single dataset containing
    their geographic ranges, migratory status, habitat preferences, and
    elevational limits. Specifically, taxon identifiers (per the
    `id_no`/`SISID` columns) are used merge the datasets together.

7.  Post-processing routines are used to prepare the output data.

## References

Brooks TM, Pimm SL, Akçakaya HR, Buchanan GM, Butchart SHM, Foden W,
Hilton-Taylor C, Hoffmann M, Jenkins CN, Joppa L, Li BV, Menon V,
Ocampo-Peñuela N, Rondinini C (2019) Measuring terrestrial Area of
Habitat (AOH) and its utility for the IUCN Red List. *Trends in Ecology
& Evolution*, 34, 977–986.
[doi:10.1016/j.tree.2019.06.009](https://doi.org/10.1016/j.tree.2019.06.009)

KBA Standards and Appeals Committee of IUCN SSC/WCPA (2022). *Guidelines
for using A Global Standard for the Identification* *of Key Biodiversity
Areas*. Version 1.2. Gland, Switzerland: IUCN.

## Examples

``` r
# \dontrun{
# find file path for example range data following IUCN Red List data format
## N.B., the range data were not obtained from the IUCN Red List,
## and were instead based on data from GBIF (https://www.gbif.org/)
path <- system.file("extdata", "EXAMPLE_SPECIES.zip", package = "aoh")

# import data
spp_range_data <- read_spp_range_data(path)

# specify persistent storage location for data processing
cache_dir <- rappdirs::user_data_dir("aoh")

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
#> ✔ initializing [436ms]
#> 
#> ℹ cleaning species range data
#> ✔ cleaning species range data [3.3s]
#> 
#> ℹ importing species summary data
#> ✔ importing species summary data [747ms]
#> 
#> ℹ importing species habitat data
#> ✔ importing species habitat data [414ms]
#> 
#> ℹ collating species data
#> ✔ collating species data [238ms]
#> 
#> ℹ post-processing results
#> ✔ post-processing results [11ms]
#> 
#> ✔ finished
# }

if (FALSE) { # interactive()
# \dontrun{
# preview data
print(spp_info_data)
# }
}
```
