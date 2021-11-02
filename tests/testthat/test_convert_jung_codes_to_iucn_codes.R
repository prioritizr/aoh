context("convert_jung_codes_to_iucn_codes()")

test_that("single code", {
  expect_equal(convert_jung_codes_to_iucn_codes("100"), "1")
})

test_that("multiple codes", {
  expect_equal(convert_jung_codes_to_iucn_codes(c("400", "300")), c("4", "3"))
})

test_that("invalid codes", {
  expect_error(convert_jung_codes_to_iucn_codes("asdf"))
  expect_error(convert_jung_codes_to_iucn_codes(c("100", "asdf", "400")))
})
