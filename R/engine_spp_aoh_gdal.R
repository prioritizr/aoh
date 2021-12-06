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
  f2 <- tempfile(tmpdir = tmp_dir, fileext = ".vrt")
  f3 <- tempfile(tmpdir = tmp_dir, fileext = ".vrt")
  f4 <- tempfile(tmpdir = tmp_dir, fileext = ".vrt")
  f5 <- tempfile(tmpdir = tmp_dir, fileext = ".tif")

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

  # force correct NODATA value
  nodata_value <- geotiff_NAflag(terra::sources(habitat_data)$source[[1]])
  if (isTRUE(nodata_value == 65535)) {
    f3 <- f2
  } else {
    r2 <- terra::rast(f2)
    terra_gdal_project(
      x = r2,
      y = r2,
      method = "near",
      n_threads = n_threads,
      filename = f3,
      datatype = "INT2U",
      tiled = FALSE,
      NAflag = 65535,
      bigtiff = TRUE,
      output_raster = FALSE,
      verbose = FALSE
    )
    rm(r2)
  }

  # crop elevation data
  terra_gdal_crop(
    x = elevation_data,
    ext = extent,
    filename = f4,
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
    y = f4,
    expr = calc_expr,
    filename = f5,
    tiled = FALSE,
    datatype = "INT1U",
    n_threads = n_threads,
    compress = "DEFLATE",
    bigtiff = TRUE,
    NAflag = 0,
    output_raster = FALSE,
    verbose = FALSE
  )

  # mask data by species range
  range_data$x <- 1
  terra_gdal_rasterize(
    x = f5,
    sf = range_data[, "x", drop = FALSE],
    sf_filename = f1,
    invert = TRUE,
    update = TRUE,
    burn = 255, ## default NA value for raster
    datatype = "INT1U",
    n_threads = n_threads,
    compress = "DEFLATE",
    bigtiff = TRUE,
    output_raster = FALSE,
    verbose = FALSE
  )

  # set no data value to 255 to effectively mask out species range
  system(paste0("gdal_edit.py ", f5, " -a_nodata 255"))

  # copy file
  file.copy(f5, path)

  # clean up
  unlink(tmp_dir, force = TRUE, recursive = TRUE)

  # return result
  invisible(TRUE)
}
