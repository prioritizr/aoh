context("plot_spp_aoh_data()")

test_that("no base map", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  elevation_data <- terra::rast(
    system.file("testdata", "sim_elevation_data.tif", package = "aoh")
  )
  habitat_data <- terra::rast(
    system.file("testdata", "sim_habitat_data.tif", package = "aoh")
  )
  spp_habitat_data <- read.csv(
    system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh"),
    sep = ",", header = TRUE
  )
  spp_summary_data <- read.csv(
    system.file("testdata", "sim_spp_summary_data.csv", package = "aoh"),
    sep = ",", header = TRUE
  )
  # create data
  expect_is(
    x <<- create_spp_aoh_data(
      x = read_spp_range_data(f, n = 4),
      output_dir = tempdir(),
      habitat_data = habitat_data,
      elevation_data = elevation_data,
      spp_habitat_data = spp_habitat_data,
      spp_summary_data = spp_summary_data,
      verbose = FALSE
    ),
    "tbl_df"
  )
  # tests
  expect_is(
    p <<- plot_spp_aoh_data(x, zoom = NULL, maptype = NULL),
    "gg"
  )
  expect_is(print(p), "gg")
})

test_that("base map", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("ggmap")
  # specify file path
  f <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  elevation_data <- terra::rast(
    system.file("testdata", "sim_elevation_data.tif", package = "aoh")
  )
  habitat_data <- terra::rast(
    system.file("testdata", "sim_habitat_data.tif", package = "aoh")
  )
  spp_habitat_data <- read.csv(
    system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh"),
    sep = ",", header = TRUE
  )
  spp_summary_data <- read.csv(
    system.file("testdata", "sim_spp_summary_data.csv", package = "aoh"),
    sep = ",", header = TRUE
  )
  # create data
  expect_is(
    x <<- create_spp_aoh_data(
      x = read_spp_range_data(f, n = 4),
      output_dir = tempdir(),
      habitat_data = habitat_data,
      elevation_data = elevation_data,
      spp_habitat_data = spp_habitat_data,
      spp_summary_data = spp_summary_data,
      verbose = FALSE
    ),
    "tbl_df"
  )
  # tests
  expect_is(
    p <<- plot_spp_aoh_data(x, zoom = 3, maptype = "toner"),
    "gg"
  )
  expect_is(print(p), "gg")
})

test_that("customized", {
  # skip if needed
  skip_on_cran()
  # specify file path
  f <- system.file("testdata", "SIMULATED_SPECIES.zip", package = "aoh")
  elevation_data <- terra::rast(
    system.file("testdata", "sim_elevation_data.tif", package = "aoh")
  )
  habitat_data <- terra::rast(
    system.file("testdata", "sim_habitat_data.tif", package = "aoh")
  )
  spp_habitat_data <- read.csv(
    system.file("testdata", "sim_spp_habitat_data.csv", package = "aoh"),
    sep = ",", header = TRUE
  )
  spp_summary_data <- read.csv(
    system.file("testdata", "sim_spp_summary_data.csv", package = "aoh"),
    sep = ",", header = TRUE
  )
  # create data
  expect_is(
    x <<- create_spp_aoh_data(
      x = read_spp_range_data(f, n = 4),
      output_dir = tempdir(),
      habitat_data = habitat_data,
      elevation_data = elevation_data,
      spp_habitat_data = spp_habitat_data,
      spp_summary_data = spp_summary_data,
      verbose = FALSE
    ),
    "tbl_df"
  )
  # tests
  expect_is(
    p <<-
      plot_spp_aoh_data(x, zoom = NULL, maptype = NULL) +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::scale_color_manual(values = c("range" = "blue")) +
      ggplot2::scale_size_manual(values = c("range" = 10)) +
      ggplot2::theme(
        strip.text = ggplot2::element_text(color = "white"),
        strip.background = ggplot2::element_rect(
          fill = "black", color = "black"
        )
      ),
    "gg"
  )
  expect_is(print(p), "gg")
})
