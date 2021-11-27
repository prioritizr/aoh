#' @include internal.R
NULL

#' Process species' Area of Habitat data using GDAL
#'
#' Generate Area of Habitat data for a single species' distribution using the
#' GDAL software for processing.
#'
#' @inheritParams engine_spp_aoh_terra
#'
#' @inherit engine_spp_aoh_terra return
#'
#' @noRd
engine_spp_aoh_gdal <- function(range_data,
                                habitat_data,
                                elevation_data,
                                habitat_values,
                                lower_elevation,
                                upper_elevation,
                                extent,
                                path,
                                n_threads = 1) {
  # validate arguments
  assertthat::assert_that(
    inherits(range_data, "sf"),
    inherits(habitat_data, "SpatRaster"),
    inherits(elevation_data, "SpatRaster"),
    is.numeric(habitat_values),
    length(habitat_values) > 0,
    assertthat::noNA(habitat_values),
    assertthat::is.number(lower_elevation),
    assertthat::noNA(lower_elevation),
    assertthat::is.number(upper_elevation),
    assertthat::noNA(upper_elevation),
    inherits(extent, "SpatExtent"),
    assertthat::is.string(path),
    assertthat::noNA(path),
    assertthat::noNA(n_threads),
    assertthat::is.number(n_threads)
  )

  # create temporary directory for processing
  tmp_dir <- gsub("\\", "/", tempfile(), fixed = TRUE)
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)

  # create temporary files
  f1 <- tempfile(tmpdir = tmp_dir, fileext = ".gpkg")
  f2 <- tempfile(tmpdir = tmp_dir, fileext = ".tif")
  f3 <- tempfile(tmpdir = tmp_dir, fileext = ".vrt")

  # crop habitat data
  terra_gdal_crop(
    x = habitat_data,
    ext = extent,
    filename = f2,
    tiled = FALSE,
    datatype = "INT2U",
    n_threads = n_threads,
    bigtiff = TRUE,
    output_raster = FALSE,
    verbose = FALSE
  )

  # mask data by species range
  range_data$x <- 1
  terra_gdal_rasterize(
    x = f2,
    sf = range_data[, "x", drop = FALSE],
    sf_filename = f1,
    invert = TRUE,
    update = TRUE,
    burn = 65535, ## default NA value for raster
    datatype = "INT2U",
    n_threads = n_threads,
    bigtiff = TRUE,
    output_raster = FALSE,
    verbose = FALSE
  )

  # crop elevation data
  terra_gdal_crop(
    x = elevation_data,
    ext = extent,
    filename = f3,
    tiled = FALSE,
    datatype = "INT2S",
    bigtiff = TRUE,
    n_threads = n_threads,
    output_raster = FALSE,
    verbose = FALSE
  )

  # convert to presence/absence of suitable habitat
  ## generate calculate expression
  habitat_intervals <- R.utils::seqToIntervals(habitat_values)
  calc_expr <- paste(
    paste0(
      "((X > (", habitat_intervals[, 1], " - 0.5)) & ",
      "(X < (", habitat_intervals[, 2], " + 0.5)))"
    ),
    collapse = " | "
  )
  calc_expr <- paste0(
    "((", calc_expr, ") & ",
    "(Y >= ", lower_elevation, ") & (Y <= ", upper_elevation, ")) * 1"
  )


  ## apply processing
  terra_gdal_calc(
    x = f2,
    y = f3,
    expr = calc_expr,
    filename = path,
    tiled = FALSE,
    datatype = "INT1U",
    n_threads = n_threads,
    bigtiff = TRUE,
    output_raster = FALSE,
    verbose = FALSE
  )

  # clean up
  unlink(tmp_dir, force = TRUE, recursive = TRUE)

  # return result
  invisible(TRUE)
}
