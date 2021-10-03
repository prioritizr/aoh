context("habitat_codes()")

test_that("correct results", {
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

test_that("throws errors for invalid files", {
  expect_error(habitat_codes("asdf"))
  expect_error(
    habitat_codes("iucn_habitatclassification_fraction_lvl2__123345_")
  )
})
