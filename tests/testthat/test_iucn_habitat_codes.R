context("iucn_habitat_codes")

test_that("iucn_habitat_codes_artificial()", {
  x <- iucn_habitat_codes_artificial()
  expect_is(x, "character")
  expect_gt(length(x), 0)
})

test_that("iucn_habitat_codes_misc()", {
  x <- iucn_habitat_codes_misc()
  expect_is(x, "character")
  expect_gt(length(x), 0)
})

test_that("iucn_habitat_codes_introduced()", {
  x <- iucn_habitat_codes_introduced()
  expect_is(x, "character")
  expect_gt(length(x), 0)
})

test_that("iucn_habitat_codes_terrestrial()", {
  x <- iucn_habitat_codes_terrestrial()
  expect_is(x, "character")
  expect_gt(length(x), 0)
})

test_that("iucn_habitat_codes_marine()", {
  x <- iucn_habitat_codes_marine()
  expect_is(x, "character")
  expect_gt(length(x), 0)
})
