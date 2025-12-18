# Package index

## Summary

Summary of the package

- [`aoh`](https://prioritizr.github.io/aoh/reference/aoh.md)
  [`aoh-package`](https://prioritizr.github.io/aoh/reference/aoh.md) :
  aoh: Create Area of Habitat Data

## Area of Habitat

Functions for working with Area of Habitat data

- [`create_spp_info_data()`](https://prioritizr.github.io/aoh/reference/create_spp_info_data.md)
  : Create species' information data
- [`create_spp_aoh_data()`](https://prioritizr.github.io/aoh/reference/create_spp_aoh_data.md)
  : Create Area of Habitat data
- [`create_spp_frc_data()`](https://prioritizr.github.io/aoh/reference/create_spp_frc_data.md)
  : Create fractional coverage data
- [`calc_spp_frc_data()`](https://prioritizr.github.io/aoh/reference/calc_spp_frc_data.md)
  : Calculate fractional coverage data
- [`plot_spp_aoh_data()`](https://prioritizr.github.io/aoh/reference/plot_spp_aoh_data.md)
  : Plot species range and Area of Habitat data
- [`plot_spp_frc_data()`](https://prioritizr.github.io/aoh/reference/plot_spp_frc_data.md)
  : Plot species range and fractional coverage data

## Importing data

Functions for downloading and importing data

- [`read_spp_range_data()`](https://prioritizr.github.io/aoh/reference/read_spp_range_data.md)
  : Read species range data

- [`get_global_elevation_data()`](https://prioritizr.github.io/aoh/reference/get_global_elevation_data.md)
  : Get global elevation data

- [`get_jung_lvl1_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_jung_lvl1_habitat_data.md)
  :

  Get Jung *et al.* (2020) habitat classification data (level 1)

- [`get_jung_lvl2_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_jung_lvl2_habitat_data.md)
  :

  Get Jung *et al.* (2020) habitat classification data (level 2)

- [`get_jung_plvl1_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_jung_plvl1_habitat_data.md)
  : Get Jung (2020) potential habitat classification data (level 1)

- [`get_lumb_cgls_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_lumb_cgls_habitat_data.md)
  :

  Get Lumbierres *et al.* (2021) CGLS habitat classification data

- [`get_spp_habitat_data()`](https://prioritizr.github.io/aoh/reference/get_spp_habitat_data.md)
  : Get species habitat data

- [`get_spp_summary_data()`](https://prioritizr.github.io/aoh/reference/get_spp_summary_data.md)
  : Get species summary data

- [`get_spp_threat_data()`](https://prioritizr.github.io/aoh/reference/get_spp_threat_data.md)
  : Get species threat data

## Crosswalk data

Crosswalks for IUCN habitat classification codes

- [`crosswalk_jung_lvl1_data`](https://prioritizr.github.io/aoh/reference/crosswalk_jung_lvl1_data.md)
  :

  Crosswalk data for Jung *et al.* (2020) (level 1 classification)

- [`crosswalk_jung_lvl2_data`](https://prioritizr.github.io/aoh/reference/crosswalk_jung_lvl2_data.md)
  :

  Crosswalk data for Jung *et al.* (2020) (level 2 classification)

- [`crosswalk_jung_plvl1_data`](https://prioritizr.github.io/aoh/reference/crosswalk_jung_plvl1_data.md)
  : Crosswalk data for Jung (2020) potential habitat classification
  (level 1)

- [`crosswalk_lumb_cgls_data`](https://prioritizr.github.io/aoh/reference/crosswalk_lumb_cgls_data.md)
  :

  Crosswalk data for CGLS based on Lumbierres *et al.* (2021)

## IUCN classifications

Data and functions for querying classification codes

- [`iucn_habitat_data`](https://prioritizr.github.io/aoh/reference/iucn_habitat_data.md)
  : IUCN habitat classification codes
- [`iucn_threat_data`](https://prioritizr.github.io/aoh/reference/iucn_threat_data.md)
  : IUCN threat classification codes
- [`iucn_habitat_codes_artificial()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_artificial.md)
  : IUCN Red List codes for artificial habitats
- [`iucn_habitat_codes_introduced()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_introduced.md)
  : IUCN Red List codes for introduced habitats
- [`iucn_habitat_codes_marine()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_marine.md)
  : IUCN Red List codes for marine habitats
- [`iucn_habitat_codes_misc()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_misc.md)
  : IUCN Red List codes for miscellaneous habitats
- [`iucn_habitat_codes_terrestrial()`](https://prioritizr.github.io/aoh/reference/iucn_habitat_codes_terrestrial.md)
  : IUCN Red List codes for terrestrial habitats

## Simulating data

Functions for simulating data

- [`simulate_spp_data()`](https://prioritizr.github.io/aoh/reference/simulate_spp_data.md)
  : Simulate species data

## External software

Functions to check if external software or services are available

- [`is_gdal_calc_available()`](https://prioritizr.github.io/aoh/reference/is_gdal_calc_available.md)
  : Is gdal_calc.py available?
- [`is_grass_available()`](https://prioritizr.github.io/aoh/reference/is_grass_available.md)
  : Is GRASS available?
- [`is_iucn_rl_api_available()`](https://prioritizr.github.io/aoh/reference/is_iucn_rl_api_available.md)
  : Is IUCN Red List API available?
- [`is_osgeo4w_available()`](https://prioritizr.github.io/aoh/reference/is_osgeo4w_available.md)
  : Is OSGeo4W available?
- [`system_gdal_version()`](https://prioritizr.github.io/aoh/reference/system_gdal_version.md)
  : System GDAL version

## Miscellaneous functions

Assorted functions with various uses

- [`terra_combine()`](https://prioritizr.github.io/aoh/reference/terra_combine.md)
  : Combine rasters
- [`terra_gdal_calc()`](https://prioritizr.github.io/aoh/reference/terra_gdal_calc.md)
  : GDAL calculate
- [`terra_gdal_crop()`](https://prioritizr.github.io/aoh/reference/terra_gdal_crop.md)
  : Crop a raster using GDAL
- [`terra_gdal_project()`](https://prioritizr.github.io/aoh/reference/terra_gdal_project.md)
  : Project a raster using GDAL
- [`terra_gdal_rasterize()`](https://prioritizr.github.io/aoh/reference/terra_gdal_rasterize.md)
  : Convert vector data into a raster using GDAL
- [`st_repair_geometry()`](https://prioritizr.github.io/aoh/reference/st_repair_geometry.md)
  : Repair geometry
