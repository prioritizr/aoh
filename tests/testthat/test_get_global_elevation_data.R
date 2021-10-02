test_that("get_global_elevation_data()", {
  skip_on_cran()
  skip_if_offline()
  skip_if_local_and_slow_internet()
  testthat::describe("get_global_elevation_data()", {
    it("downloads data", {
      d <<- get_global_elevation_data(force = TRUE, verbose = FALSE)
      expect_is("SpatRaster")
    })
    it("has WGS1984 coordinate reference system", {
      expect_true(sf::st_crs(terra::crs(d)) == sf::st_crs(4326))
    })
    it("has (near) global coverage", {
      expect_lte(terra::xmin(d), -180)
      expect_gte(terra::xmax(d), 180)
      expect_lte(terra::xmin(d), -55)
      expect_gte(terra::ymax(d), 83)
    })
  })
})
