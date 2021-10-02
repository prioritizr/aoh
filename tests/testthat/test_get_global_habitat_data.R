test_that("habitat_codes()", {
  testthat::describe("habitat_codes()", {
    it("converts valid file names", {
      expect_equal(
        habitat_codes(paste0(
          "iucn_habitatclassification_fraction_lvl2__1000_Marine -",
          "Oceanic__ver004.tif"
        )),
        "10"
      )
      expect_equal(
        habitat_codes(paste0(
          "iucn_habitatclassification_fraction_lvl2__106_Forest –",
          "Subtropical-tropical moist lowland__ver004.tif"
        )),
        "1.6"
      )
      expect_equal(
        habitat_codes(paste0(
          "iucn_habitatclassification_fraction_lvl2__516_Wetlands inland –",
          "Permanent saline, brackish or alkaline__ver004.tif"
        )),
        "5.16"
      )
      expect_equal(
        habitat_codes(
          c(
            paste0(
              "iucn_habitatclassification_fraction_lvl2__516_Wetlands inland –",
              "Permanent saline, brackish or alkaline__ver004.tif"
            ),
            paste0(
              "iucn_habitatclassification_fraction_lvl2__106_Forest –",
              "Subtropical-tropical moist lowland__ver004.tif"
            )
          )
        ),
        c("5.16", "1.6")
      )
    })
    it("throws expected errors", {
      expect_error(habitat_codes("asdf"))
      expect_error(
        habitat_codes("iucn_habitatclassification_fraction_lvl2__123345_")
      )
    })
  })
})

testthat::test_that("get_global_habitat_data()", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  # tests
  testthat::describe("get_global_habitat_data()", {
    it("downloads version-specific dataset", {
      d <- get_global_habitat_data(
        version = "10.5281/zenodo.4058356", force = TRUE, verbose = FALSE
      )
      expect_is("SpatRaster")
    })
    it("downloads latest dataset", {
      d <<- get_global_habitat_data(
        version = "latest", force = TRUE, verbose = FALSE
      )
      expect_is("SpatRaster")
    })
    it("has WGS1984 coordinate reference system", {
      expect_true(sf::st_crs(terra::crs(d)) == sf::st_crs(4326))
    })
    it("has global coverage", {
      expect_lte(terra::xmin(d), -180)
      expect_gte(terra::xmax(d), 180)
      expect_lte(terra::xmin(d), -89)
      expect_gte(terra::ymax(d), 89)
    })
  })
})
