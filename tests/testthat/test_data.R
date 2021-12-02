context("built-in data")

test_that("iucn_habitat_data", {
  # load data
  data(iucn_habitat_data)
  # tests
  expect_is(iucn_habitat_data, "tbl_df")
  expect_named(
    iucn_habitat_data,
    c(
      "code", "name", "is_terrestrial", "is_marine", "is_artificial",
      "is_misc", "is_introduced"
    )
  )
  expect_is(iucn_habitat_data$code, "character")
  expect_equal(anyDuplicated(iucn_habitat_data$code), 0L)
  expect_is(iucn_habitat_data$name, "character")
  expect_is(iucn_habitat_data$is_terrestrial, "logical")
  expect_is(iucn_habitat_data$is_marine, "logical")
  expect_is(iucn_habitat_data$is_artificial, "logical")
  expect_is(iucn_habitat_data$is_misc, "logical")
  expect_is(iucn_habitat_data$is_introduced, "logical")
})

test_that("crosswalk_jung_data", {
  # load data
  data(iucn_habitat_data)
  data(crosswalk_jung_data)
  # tests
  expect_is(crosswalk_jung_data, "tbl_df")
  expect_named(crosswalk_jung_data, c("code", "value"))
  expect_is(crosswalk_jung_data$code, "character")
  expect_equal(anyDuplicated(iucn_habitat_data$code), 0L)
  expect_true(all(crosswalk_jung_data$code %in% iucn_habitat_data$code))
  expect_is(crosswalk_jung_data$value, "integer")
})

test_that("crosswalk_lumbierres_data", {
  # load data
  data(iucn_habitat_data)
  data(crosswalk_lumbierres_data)
  # tests
  expect_is(crosswalk_lumbierres_data, "tbl_df")
  expect_named(crosswalk_lumbierres_data, c("code", "value"))
  expect_is(crosswalk_lumbierres_data$code, "character")
  expect_equal(anyDuplicated(iucn_habitat_data$code), 0L)
  expect_true(all(crosswalk_lumbierres_data$code %in% iucn_habitat_data$code))
  expect_is(crosswalk_lumbierres_data$value, "integer")
})
