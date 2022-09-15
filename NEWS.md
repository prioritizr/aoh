# aoh 0.0.1.3

- Fix issues with using GDAL engine for processing data on Windows.
  Specifically, the package will now correctly work with the GDAL Python
  bindings that are installed as part of the OSGeo4W software.
- Fix issues where functions for downloading datasets would fail to
  load data from cache, and needlessly re-download data.
- Fix issue where attempting to download data or query data version
  numbers would throw an error due to erros with the Zenodo API.

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
