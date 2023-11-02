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
  # prepare data
  x <- create_spp_aoh_data(
    x = create_spp_info_data(
      x = read_spp_range_data(f, n = 4),
      spp_habitat_data = spp_habitat_data,
      spp_summary_data = spp_summary_data,
      verbose = interactive()
    ),
    output_dir = tempdir(),
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data,
    verbose = interactive()
  )
  # create object
  p <- plot_spp_aoh_data(x, zoom = NULL, maptype = NULL)
  # tests
  expect_is(p, "gg")
  expect_is(suppressWarnings(print(p)), "gg")
})

test_that("base map", {
  # skip if needed
  skip_on_cran()
  skip_if_not_installed("ggmap")
  skip_if_not(nzchar(Sys.getenv("GGMAP_STADIAMAPS_API_KEY")))
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
  # prepare data
  x <- create_spp_aoh_data(
    x = create_spp_info_data(
      x = read_spp_range_data(f, n = 4),
      spp_habitat_data = spp_habitat_data,
      spp_summary_data = spp_summary_data,
      verbose = interactive()
    ),
    output_dir = tempdir(),
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data,
    verbose = interactive()
  )
  # create object
  p <- plot_spp_aoh_data(x, zoom = 3, maptype = "stamen_toner")
  # tests
  expect_is(p, "gg")
  expect_is(suppressWarnings(print(p)), "gg")
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
  # prepare data
  x <- create_spp_aoh_data(
    x = create_spp_info_data(
      x = read_spp_range_data(f, n = 4),
      spp_habitat_data = spp_habitat_data,
      spp_summary_data = spp_summary_data,
      verbose = interactive()
    ),
    output_dir = tempdir(),
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_jung_lvl2_data,
    verbose = interactive()
  )
  # create object
  p <-
    plot_spp_aoh_data(x, zoom = NULL, maptype = NULL) +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::scale_color_manual(values = c("range" = "blue")) +
    ggplot2::scale_size_manual(values = c("range" = 10)) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(color = "white"),
      strip.background = ggplot2::element_rect(
        fill = "black", color = "black"
      )
    )
  # tests
  expect_is(p, "gg")
  expect_is(suppressWarnings(print(p)), "gg")
})
