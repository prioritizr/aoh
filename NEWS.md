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
