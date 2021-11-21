context("is_iucn_rl_api_available")

test_that("expected TRUE", {
  skip_on_cran()
  skip_if_iucn_key_missing()
  expect_true(is_iucn_rl_api_available())
})

test_that("expected FALSE", {
  skip_on_cran()
  expect_false(is_iucn_rl_api_available(key = "null"))
})
