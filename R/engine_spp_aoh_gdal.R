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
                                n_threads = 1,
                                cache_limit = 1000) {
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

  # setup terra processing
  terra::terraOptions(progress = 0, tempdir = tmp_dir)
  on.exit(terra::terraOptions(progress = 3, tempdir = tempdir()))

  # create temporary files
  f1 <- tempfile(tmpdir = tmp_dir, fileext = ".gpkg")
  f2 <- tempfile(tmpdir = tmp_dir, fileext = ".vrt")
  f3 <- tempfile(tmpdir = tmp_dir, fileext = ".vrt")
  f4 <- tempfile(tmpdir = tmp_dir, fileext = ".tif")

  # crop habitat data
  terra_gdal_crop(
    x = habitat_data,
    ext = extent,
    filename = f2,
    tiled = FALSE,
    datatype = "INT2U",
    cache_limit = cache_limit,
    NAflag = "none",
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
    cache_limit = cache_limit,
    bigtiff = TRUE,
    NAflag = "none",
    n_threads = n_threads,
    output_raster = FALSE,
    verbose = FALSE
  )

  # species range
  # mask data by species range
  range_data$x <- 1
  terra_gdal_rasterize(
    x = terra::rast(f2),
    sf = range_data[, "x", drop = FALSE],
    sf_filename = f1,
    filename = f4,
    burn = 1,
    update = FALSE,
    datatype = "INT1U",
    cache_limit = cache_limit,
    n_threads = n_threads,
    compress = "LZW",
    bigtiff = TRUE,
    tiled = TRUE,
    nbits = 1,
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
    "(Y >= ", lower_elevation, ") & (Y <= ", upper_elevation, "))"
  )
  calc_expr <- paste0(
    "((", calc_expr, ") * (Z == 1)) + ",
    "(2 * (Z != 1))"
  )

  ## apply processing
  terra_gdal_calc(
    x = f2,
    y = f3,
    z = f4,
    expr = calc_expr,
    filename = path,
    tiled = TRUE,
    datatype = "INT1U",
    n_threads = n_threads,
    compress = "DEFLATE",
    bigtiff = TRUE,
    nbits = 2,
    NAflag = 2,
    output_raster = FALSE,
    verbose = FALSE
  )

  # clean up
  unlink(tmp_dir, force = TRUE, recursive = TRUE)

  # return result
  invisible(TRUE)
}
