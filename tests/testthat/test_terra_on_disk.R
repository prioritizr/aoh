context("terra_on_disk()")

test_that("memory", {
  # create data
  x <- terra::rast(
   ncols = 40, nrows = 40, xmin = -110, xmax = -90, ymin = 40, ymax = 60,
   crs = "+proj=longlat +datum=WGS84"
  )
  terra::values(x) <- seq_len(terra::ncell(x))
  # tests
  expect_false(terra_on_disk(x))
})

test_that("disk", {
  # create data
  x <- terra::rast(system.file("ex", "elev.tif", package = "terra"))
  # tests
  expect_true(terra_on_disk(x))
})

test_that("multiple layers (single source)", {
  # create data
  x <- terra::rast(system.file("ex", "logo.tif", package = "terra"))
  # tests
  expect_true(terra_on_disk(x))
})

test_that("multiple layers (multiple sources)", {
  # create data
  x <- terra::rast(
    c(
      system.file("ex", "logo.tif", package = "terra"),
      system.file("ex", "logo.tif", package = "terra")
    )
  )
  # tests
  expect_false(terra_on_disk(x))
})
