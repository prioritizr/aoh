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
