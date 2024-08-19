# aoh 0.0.2.14

- Update `create_spp_info_data()` to make data cleaning functionality more
  robust for the BirdLife species' range dataset.
- Update built-in helper script for processing area of habitat data to
  include (i) mammal speices with terrestrial and freshwater distributions and
  (ii)  mammal speices with terrestrial and marine distributions
  (see `inst/scripts/aoh-data.R`)
- New built-in helper script to download all species identifiers from the
  IUCN Red List (see `inst/scripts/iucn-species-list.R`)

# aoh 0.0.2.13

- Update `read_spp_range_data()` and `create_spp_info_data()` to fix
  incompatibility issue with latest version of the BirdLife species range
  dataset (#57). Thanks to Jianqiao Zhao for the bug report.
- Update `create_spp_aoh_data()` and `create_spp_frc_data()` so that they
  provide a correct error message when the argument to `x` does not
  contain an `"id_no"` column.

# aoh 0.0.2.12

- Fix bug in `create_spp_info_data()` in assigning habitat types for resident
  distributions of migratory and non-migratory species. This bug meant that (i)
  habitat types for resident distributions of migratory species would include
  those exclusively affiliated with the species' passage distributions and (ii)
  habitat types for resident distributions of non-migratory species would not
  include those exclusively affiliated with the species' passage distributions.
  Thanks to Jianqiao Zhao for bug report.
- Update built-in testing data to include multiple migratory species.
- Update internal R script for creating test dataset
  (i.e., `inst/scripts/test-data.R`) to be compatible with current version of
  the package.
- Update `is_gdal_calc_available()` to be more robust.
- Fix URLs.

# aoh 0.0.2.11

- Fix bug in `create_spp_info_data()` so that the IUCN Red List API key can be
  specified using the `key` parameter.
- Fix aliasing for package manual entry.

# aoh 0.0.2.10

- Update _prepr_ and _ggmap_ package dependencies.

# aoh 0.0.2.9

- Remove _rgdal_ package dependency.
- Updates URLs.
- Update functions for downloading preprocessed data from Zenodo.
- Update `plot_spp_aoh_data()` and `plot_spp_frc_data()` to be compatible
  with changes to the _ggmap_ package for creating maps with a basemap.

# aoh 0.0.2.8

- Fix failing tests on continuous integration services (due to issues unzipping
  test files).

# aoh 0.0.2.7

- Update `read_spp_range_data()` to that it can import data from a zip
  file that contains multiple ESRI Shapefiles (e.g., the 2022-2 IUCN Red List
  release for reptile data).

# aoh 0.0.2.6

- Update `create_spp_aoh_data()` and `create_spp_frc_data()` to have a
  new `rasterize_touches` parameter (#48). This parameter can be toggled so
  that when rasterizing species' range data, raster cells that partially
  overlap with any part of the species' range are treated as covered by
  the species' range. This functionality may be especially useful for
  species with very small geographic ranges.
- Fix bug in `create_spp_aoh_data()` and `create_spp_frc_data()` that causes
  Python errors when using the GDAL engine and a `habitat_data` or
  `elevation_data` raster that is stored only in memory (and not associated with
  any file on disk).
- Fix bug in `create_spp_aoh_data()` and `create_spp_frc_data()` that causes
  the GRASS engine to throws errors.
- Update package dependency versions.

# aoh 0.0.2.5

- Fix compatibility with updates to `terra::compareGeom()`.

# aoh 0.0.2.4

- Update data cleaning procedures for `create_spp_info_data()` so that a
  useful error message is provided if all species are excluded.

# aoh 0.0.2.3

- Fix bug in `create_spp_aoh_data()` when using GRASS engine that caused the
  function to throw an error because it failed to initialize a new GRASS
  project correctly.
- Update `get_spp_habitat_data()`, `get_spp_summary_data()`,
  `get_spp_threat_data()`,  `get_spp_summary_data()` functions so that they
  do not throw warnings related to the _dplyr_ package.

# aoh 0.0.2.2

- New `iucn_threat_data` built-in dataset denoting threat information
  on the IUCN Red List Threat Classification.
- Fix compatibility of tests with updates to IUCN Red List.
- Fix compatibility with _tidyselect_ package (>= 1.2.0).
- Fix badges in README.
- The _rgrass_ package is now used for GRASS functionality (due to
  upcoming deprecation of the _rgrass7_ package).

# aoh 0.0.2.1

- Update `create_spp_info_data()` so that data cleaning procedures replace
  0 m lower elevation limits with 500 m (#39). This is because the IUCN Red
  List assigns lower limit values of 0 m for many species that have parts
  of their distribution in areas below sea level.
- Update `create_spp_info_data()` so that data can be processed using
  EPSG:4326 coordinate reference system. Instead of throwing an error,
  the function will now display an alert.
- Fix bug in `create_spp_aoh_data()` with the `terra` engine that caused
  negative elevations to be rounded to zero.

# aoh 0.0.2.0

- Initial stable release.
- Add tests for `st_repair_geometry()`.
- Add URLs to citations in README.
- Update references in documentation.

# aoh 0.0.1.6

- Fix spelling and formatting mistakes in vignette.
- Update `st_repair_geometry()` to be more robust to geometry issues.

# aoh 0.0.1.5

- Update documentation for the `create_spp_info_data()` function.
- Update `create_spp_info_data()` function so that the methodology used to
  correct errors in species' elevational limits now follows best practices.
  This behavior is controlled using the `adjust_elevational_limits`. If
  elevational limits should not be altered during processing, this can be
  specified by setting `adjust_elevational_limits = FALSE`.
- Update `create_spp_info_data()` function so that IUCN habitat
  codes are adjusted to be assigned based on guidelines for the identification
  of Key Biodiversity Areas. Broadly speaking, these guidelines assign IUCN
  habitat codes to species' distributions in a manner to minimize omission
  errors. This behavior is controlled by the `adjust_habitat_codes`
  parameter. If habitat codes should be assigned based on exact matches
  (e.g., Resident distributions should only be associated with habitat codes
  described for Resident distributions), this can be specified by setting
  `adjust_habitat_codes = FALSE`.
- Update `create_spp_info_data()`, `create_spp_aoh_data()`,
  `create_spp_frc_data()`, and `calc_spp_frc_data()` functions so that
  the returned `sf::st_sf()` object now contains an additional `migratory`
  column indicating if the species was processed as a migratory species
  or not.
- Update `create_spp_info_data()`, `create_spp_aoh_data()`,
  `create_spp_frc_data()`, and `calc_spp_frc_data()` functions so that
  the order of the columns now places the IUCN threat status information
  (i.e., `"category"` column) closer to the start of the table.
- Update tests to be robust to Zenodo website outages.

# aoh 0.0.1.4

- Update`st_repair_geometry()` to avoid unneeded geometry duplication.
  This reduces memory requirements for`crate_spp_info_data()`.

# aoh 0.0.1.3

- Fix issues with using GDAL engine for processing data on Windows.
  Specifically, the package will now correctly work with the GDAL Python
  bindings that are installed as part of the OSGeo4W software (#32).
- Fix issues where functions for downloading datasets would fail to
  load data from cache, and needlessly re-download data.
- Fix issue where attempting to download data or query data version
  numbers would throw an error due to errors with the Zenodo API.

# aoh 0.0.1.2

- Suppress meaningless warnings triggered for GRASS on macOS.
- Increase version requirement for _terra_ package.
- Fix IUCN Red List API tests.

# aoh 0.0.1.1

- Improve test coverage.
- Convenience functions for applying GDAL commands to _terra_ package objects
  are now exported. These functions include `terra_gdal_calc()`,
  `terra_gdal_crop()`, `terra_gdal_project()`, and `terra_gdal_rasterize()`.
- Fix bug in `terra_gdal_project()`.

# aoh 0.0.1.0

- Initial developmental release.

# aoh 0.0.0.99999

- Initial work on developing package.
