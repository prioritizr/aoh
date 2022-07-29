# load package
print("terra version")
print(packageVersion("terra"))
library(terra)
print("sf linking to:")
print(sf::sf_extSoftVersion())
library(sf)
devtools::load_all()

# specify file path
f <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
elevation_data <- terra::rast(
  system.file("testdata", "sim_elevation_data.tif", package = "aoh")
)
habitat_data <- terra::rast(
  system.file("testdata", "sim_habitat_data.tif", package = "aoh")
)
spp_habitat_data <- read.csv(
  system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh"),
  sep = ",", header = TRUE
)
spp_summary_data <- read.csv(
  system.file("testdata", "sim_spp_summary_data.csv", package = "aoh"),
  sep = ",", header = TRUE
)

# prepare data
d <- create_spp_aoh_data(
  x = create_spp_info_data(
    x = read_spp_range_data(f),
    spp_habitat_data = spp_habitat_data,
    spp_summary_data = spp_summary_data,
    verbose = interactive()
  ),
  output_dir = tempdir(),
  habitat_data = habitat_data,
  elevation_data = elevation_data,
  crosswalk_data = crosswalk_jung_lvl2_data,
  verbose = interactive()
)

# print result
print(d)
