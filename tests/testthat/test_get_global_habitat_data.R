testthat::test_that("get_global_habitat_data()", {
  # skip if needed
  skip_on_cran()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  # tests
  testthat::describe("get_global_habitat_data()", {
    it("downloads version-specific dataset", {
      expect_is(
        d <<- get_global_habitat_data(
          version = "10.5281/zenodo.4058356", force = TRUE, verbose = FALSE
        ),
        "SpatRaster"
      )
    })
    it("downloads latest dataset", {
      expect_is(
        d <<- get_global_habitat_data(
          version = "latest", force = TRUE, verbose = FALSE
        ),
        "SpatRaster"
      )
    })
    it("has WGS84 coordinate reference system", {
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
