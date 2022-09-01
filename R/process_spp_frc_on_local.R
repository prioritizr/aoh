#' @include internal.R
NULL

#' Process fractional coverage data for a species locally
#'
#' Calculate fractional coverage of species' Area of Habitat using computational
#' resources that are available locally.
#'
#' @param aoh_path `character` File path for Area of Habitat data.
#
#' @param path `character` File path for output fractional coverage data.
#'
#' @inheritParams calc_spp_frc_data
#'
#' @inherit process_spp_aoh_data_on_local return
#'
#' @noRd
process_spp_frc_on_local <- function(aoh_path,
                                     template_data,
                                     path,
                                     engine = "terra",
                                     n_threads = 1,
                                     cache_limit = 200) {
  # assert that arguments are valid
  assertthat::assert_that(
    assertthat::is.string(aoh_path),
    assertthat::noNA(aoh_path),
    inherits(template_data, "SpatRaster"),
    assertthat::is.string(engine),
    assertthat::noNA(engine),
    engine %in% c("terra", "gdal"),
    assertthat::is.string(path),
    assertthat::noNA(path),
    assertthat::is.count(n_threads),
    assertthat::noNA(n_threads)
  )

  # reset terra options on exit
  on.exit(terra::terraOptions(progress = 3, tempdir = tempdir()))

  # configure terra for processing
  tmp_dir <- normalize_path(tempfile(), mustWork = FALSE)
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  terra::terraOptions(progress = 0, tempdir = tmp_dir)

  # processing
  ## import aoh raster
  r <- terra::rast(aoh_path)

  ## compute extent for output
  curr_grid <- terra::crop(x = template_data, y = terra::ext(r), snap = "out")

  ## compute aggregation factor (assuming square grid cells)
  fact <- terra::xres(template_data) / terra::xres(r)

  # extend raster using using specified engine
  if (identical(engine, "terra")) {
    r <- terra::extend(
      x = r,
      y = curr_grid,
      datatype = "INT1U",
      gdal = c(
        "COMPRESS=LZW", "BIGTIFF=YES",
        paste0("NUM_THREADS=", n_threads)
      )
    )
  } else {
    r <- terra_gdal_crop(
      x = r,
      ext = terra::ext(curr_grid),
      filename = normalize_path(
        tempfile(tmpdir = tmp_dir, fileext = ".tif"),
        mustWork = FALSE
      ),
      datatype = "INT1U",
      cache_limit = cache_limit,
      bigtiff = TRUE,
      tiled = TRUE,
      compress = "LZW",
      n_threads = n_threads,
      verbose = FALSE
    )
  }

  # aggregate raster and compute fractional coverage
  ## note that we don't use GDAL for this because
  ## (i) gdal_translate doesn't provide a "sum" method, and
  ## (ii) gdal_warp sets grid cells values as NA if they are predominantly
  ## covered by missing values
  r <- terra::app(
    x = terra::aggregate(
      x = r,
      fact = fact,
      fun = "sum",
      na.rm = TRUE,
      wopt = list(
        datatype = "INT2U",
        gdal = c(
          "COMPRESS=LZW", "BIGTIFF=YES",
          "TILED=YES", paste0("NUM_THREADS=", n_threads)
        )
      )
    ),
    fun = `/`,
    e2 = fact ^ 2,
    filename = path,
    overwrite = TRUE,
    wopt = list(
      NAflag = -1,
      datatype = "FLT4S",
      gdal = c(
        "COMPRESS=DEFLATE", "BIGTIFF=YES",
        "TILED=YES", paste0("NUM_THREADS=", n_threads)
      )
    )
  )

  # clean up
  rm(r)
  unlink(tmp_dir, force = TRUE, recursive = TRUE)

  # verify success
  assertthat::assert_that(
    file.exists(path),
    msg = paste("failed to save fractional coverage data:", path)
  )

  # return TRUE
  invisible(TRUE)
}
