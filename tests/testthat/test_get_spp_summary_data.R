context("get_spp_summary_data()")

test_that("single taxon identifier", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("vcr")
  # set parameters
  id_no <- c(18)
  # create objects
  vcr::use_cassette("summary-single", {
    x1 <- get_spp_summary_data(id_no, force = TRUE, verbose = interactive())
  })
  Sys.sleep(2)
  x2 <- get_spp_summary_data(id_no, force = FALSE, verbose = interactive())
  # tests
  expect_is(x1, "data.frame")
  expect_is(x2, "data.frame")
  expect_gte(nrow(x1), 1)
  expect_named(
    x1,
    c(
      "id_no", "taxonid", "scientific_name", "kingdom", "phylum", "class",
       "order", "family", "genus", "main_common_name", "authority",
       "published_year", "assessment_date", "category", "criteria",
       "population_trend", "marine_system", "freshwater_system",
       "terrestrial_system",
       "assessor", "reviewer", "aoo_km2", "eoo_km2", "elevation_upper",
       "elevation_lower", "depth_upper", "depth_lower", "errata_flag",
       "errata_reason", "amended_flag", "amended_reason"
     )
  )
  expect_true(all(id_no %in% x1$id_no))
  expect_equal(anyDuplicated(x1$id_no), 0L)
  expect_identical(x1, x2)
})

test_that("multiple taxon identifiers", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("vcr")
  # tests
  id_no <- c(18, 137, 138, 139)
  # create objects
  vcr::use_cassette("summary-multiple", {
    x1 <- get_spp_summary_data(id_no, force = TRUE, verbose = interactive())
  })
  Sys.sleep(2)
  x2 <- get_spp_summary_data(id_no, force = FALSE, verbose = interactive())
  # tests
  expect_is(x1, "data.frame")
  expect_is(x2, "data.frame")
  expect_gte(nrow(x1), 1)
  expect_named(
    x1,
    c(
      "id_no", "taxonid", "scientific_name", "kingdom", "phylum", "class",
       "order", "family", "genus", "main_common_name", "authority",
       "published_year", "assessment_date", "category", "criteria",
       "population_trend", "marine_system", "freshwater_system",
       "terrestrial_system",
       "assessor", "reviewer", "aoo_km2", "eoo_km2", "elevation_upper",
       "elevation_lower", "depth_upper", "depth_lower", "errata_flag",
       "errata_reason", "amended_flag", "amended_reason"
     )
  )
  expect_true(all(id_no %in% x1$id_no))
  expect_equal(anyDuplicated(x1$id_no), 0L)
  expect_identical(x1, x2)
})

test_that("some taxon missing summary data", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("vcr")
  # set parameters
  id_no <- c(18, -100)
  # create objects
  vcr::use_cassette("summary-missing-x1", {
    x1 <- get_spp_summary_data(id_no[1], force = TRUE, verbose = interactive())
  })
  vcr::use_cassette("summary-missing-x2", {
    x2 <- get_spp_summary_data(id_no, force = TRUE, verbose = interactive())
  })
  # tests
  ## x1
  expect_is(x1, "data.frame")
  expect_true(all(id_no[1] %in% x1$id_no))
  expect_equal(nrow(x1), 1)
  expect_named(
    x1,
    c(
      "id_no", "taxonid", "scientific_name", "kingdom", "phylum", "class",
       "order", "family", "genus", "main_common_name", "authority",
       "published_year", "assessment_date", "category", "criteria",
       "population_trend", "marine_system", "freshwater_system",
       "terrestrial_system",
       "assessor", "reviewer", "aoo_km2", "eoo_km2", "elevation_upper",
       "elevation_lower", "depth_upper", "depth_lower", "errata_flag",
       "errata_reason", "amended_flag", "amended_reason"
     )
  )
  ## x2
  expect_is(x2, "data.frame")
  expect_true(all(id_no == x2$id_no))
  expect_equal(nrow(x2), 2)
  expect_equal(x1, x2[1, ])
  expect_true(all(vapply(x2[2, -1], FUN.VALUE = logical(1), is.na)))
})

test_that("taxon with multiple records", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("vcr")
  # set parameters
  id_no <- c(178652, 177906)
  # create objects
  vcr::use_cassette("summary-multiple-records", {
    x <- get_spp_summary_data(id_no, force = TRUE, verbose = interactive())
  })
  # tests
  expect_is(x, "data.frame")
  expect_equal(nrow(x), 2)
  expect_named(
    x,
    c(
      "id_no", "taxonid", "scientific_name", "kingdom", "phylum", "class",
       "order", "family", "genus", "main_common_name", "authority",
       "published_year", "assessment_date", "category", "criteria",
       "population_trend", "marine_system", "freshwater_system",
       "terrestrial_system",
       "assessor", "reviewer", "aoo_km2", "eoo_km2", "elevation_upper",
       "elevation_lower", "depth_upper", "depth_lower", "errata_flag",
       "errata_reason", "amended_flag", "amended_reason"
     )
  )
  expect_equal(anyDuplicated(x$id_no), 0L)
})
