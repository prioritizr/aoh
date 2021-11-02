#' @include internal.R
NULL

#' Project habitat data
#'
#' Project habitat data to conform to specific spatial properties.
#'
#' @param x [terra::rast()] Raster object with habitat data.
#'
#' @param temp_dir `character` Folder path for storing temporary files.
#'
#' @inheritParams create_spp_aoh_data
#'
#' @return A [terra::rast()] raster object.
#'
#' @noRd
project_habitat_data <- function(x, template_data,
                                 parallel_n_threads = 1,
                                 use_gdal = is_gdal_available(),
                                 temp_dir = tempdir(),
                                 verbose = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    inherits(template_data, "SpatRaster"),
    assertthat::is.flag(use_gdal),
    assertthat::noNA(use_gdal),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )

  # display message
  if (verbose) {
    pb <- cli::cli_progress_bar(
      clear = FALSE,
      total = terra::nlyr(x),
      format = paste0(
        "{.alert-info projecting habitat data} ",
        "{cli::pb_bar} [{cli::pb_percent} | {cli::pb_eta_str} ]"
      ),
      format_done = paste0(
        "{.alert-success projecting habitat data} [{cli::pb_elapsed}]"
      )
    )
  }

  # project data
  if (use_gdal) {
    ## if using gdal
    proj_files <- replicate(
      n = terra::nlyr(x),
      expr = tempfile(tmpdir = temp_dir, fileext = ".tif")
    )
    ## main processing
    x <- terra::rast(
      plyr::llply(
        seq_along(proj_files),
        function(i) {
          x <- terra_gdal_project(
            x = x[[i]],
            y = template_data,
            filename = proj_files[i],
            parallel_n_threads = parallel_n_threads,
            verbose = FALSE,
            datatype = "INT2U"
          )
          if (verbose) {
            cli::cli_progress_update(id = pb)
          }
          x
        }
      )
    )
  } else {
    ## otherwise use default terra processing
    x <- terra::rast(
      plyr::llply(terra::as.list(x), function(x) {
        x <- terra::project(
          x = x,
          y = template_data,
          datatype = "INT2U"
        )
        if (verbose) {
          cli::cli_progress_update(id = pb)
        }
        x
      })
    )
  }

  # update message
  if (verbose) {
    cli::cli_progress_done(id = pb)
    rm(pb)
  }

  # clamp habitat data
  ## display message
  if (verbose) {
    pb <- cli::cli_progress_bar(
      clear = TRUE,
      total = terra::nlyr(x),
      format = paste0(
        "{.alert-info clamping habitat data} ",
        "{cli::pb_bar} [{cli::pb_percent} | {cli::pb_eta_str} ]"
      ),
      format_done = paste0(
        "{cli::symbol$tick} clamping habitat data [{cli::pb_elapsed}]"
      )
    )
  }
  ## processing
  x <- terra::rast(
    plyr::llply(terra::as.list(x), function(x) {
      x <- terra::clamp(
        x = x,
        lower = 0,
        upper = 1000,
        values = TRUE,
        datatype = "INT2U",
      )
      if (verbose) {
        cli::cli_progress_update(id = pb)
      }
      x
    })
  )
  ## update message
  if (verbose) {
    cli::cli_progress_done(id = pb)
    rm(pb)
  }
  ## delete previous temp files
  if (use_gdal) {
    unlink(proj_files, force = TRUE)
  }

  # return result
  x
}
