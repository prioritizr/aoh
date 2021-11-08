#' @include internal.R  misc_terra.R misc_sf.R
NULL

#' Process species Area of Habitat data locally
#'
#' Process species Area of Habitat (AOH) data using computational
#' resources that are available locally.
#'
#' @inheritParams create_spp_aoh_data
#'
#' @param x [sf::sf()] Spatial data delineating species geographic ranges
#'   obtained from the [IUCN Red List](https://www.iucnredlist.org/).
#'   These data should have previously been cleaned (via
#'   [clean_spp_range_data()] and contain the following additional columns:
#'   `"habitat_code"`, `"elevation_lower"`, `"elevation_upper"`,
#'   `"xmin"`, `"xmax"`, `"ymin"`, `"ymax"`, `"path"`.
#'
#' @param elevation_data [terra::rast()] Raster data delineating the
#'   elevation data.
#'
#' @param habitat_data [terra::rast()] Multi-layer raster data delineating the
#'   coverage of different habitat classes.
#'
#' @param ... arguments passed to [terra::writeRaster()] for processing.
#'
#' @noRd
process_spp_aoh_data_on_local <- function(x,
                                          habitat_data,
                                          elevation_data,
                                          cache_dir = tempdir(),
                                          force = FALSE,
                                          verbose = TRUE,
                                          ...) {
  # assert that arguments are valid
  ## initial validation
  assertthat::assert_that(
    inherits(x, "sf"),
    assertthat::has_name(x, "id_no"),
    assertthat::has_name(x, "seasonal"),
    assertthat::has_name(x, "aoh_id"),
    assertthat::has_name(x, "path"),
    assertthat::has_name(x, "elevation_lower"),
    assertthat::has_name(x, "elevation_upper"),
    assertthat::has_name(x, "habitat_code"),
    assertthat::noNA(x$id_no),
    assertthat::noNA(x$seasonal),
    assertthat::noNA(x$aoh_id),
    assertthat::noNA(x$habitat_code),
    assertthat::noNA(x$elevation_lower),
    assertthat::noNA(x$elevation_upper),
    assertthat::noNA(x$habitat_code),
    inherits(habitat_data, "SpatRaster"),
    inherits(elevation_data, "SpatRaster"),
    terra::compareGeom(habitat_data[[1]], elevation_data, stopOnError = FALSE),
    assertthat::is.string(cache_dir),
    assertthat::noNA(cache_dir),
    assertthat::is.writeable(cache_dir),
    assertthat::is.flag(force),
    assertthat::noNA(force),
    assertthat::is.flag(verbose),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )

  # prepare prepare variables for data processing
  wopt <- list(...)

  # determine which species need processing
  if (!force & any(file.exists(x$path))) {
    idx <- which(!is.na(x$path) & !file.exists(x$path))
    if (verbose) {
      message(
        paste(
          "    skipping",
          sum(file.exists(x$path)),
          "species distributions already processed"
        )
      )
    }
  } else {
    idx <- which(!is.na(x$path))
  }

  # create custom progress bar
  if (isTRUE(verbose)) {
    pb <- cli::cli_progress_bar(
      clear = FALSE,
      total = length(idx),
      format = paste0(
        "{.alert-info creating AOH} ",
        "{cli::pb_bar} [{cli::pb_percent} | {cli::pb_eta_str}]"
      ),
      format_done = paste0(
        "{.alert-success creating AOH [{cli::pb_elapsed}]}"
      )
    )
  }

  # main processing
  result <- suppressWarnings(plyr::llply(
    .data = idx,
    .fun = function(i) {

      ## extract data for current iteration
      curr_spp_path <- x$path[i]
      curr_spp_habitat_codes <- x$habitat_code[[i]]
      curr_spp_lower_elevation <- x$elevation_lower[i]
      curr_spp_upper_elevation <- x$elevation_upper[i]
      curr_spp_extent <- aoh::sf_terra_ext(x[i, , drop = FALSE])

      ## create temporary directory for species
      curr_spp_tmp_dir <- gsub("\\", "/", tempfile(), fixed = TRUE)
      dir.create(curr_spp_tmp_dir, showWarnings = FALSE, recursive = TRUE)
      terra::terraOptions(progress = 0, tempdir = curr_spp_tmp_dir)

      ## calculate total sum of habitat based on codes
      curr_spp_habitat_data <- terra::app(
        terra::crop(
          x = habitat_data[[curr_spp_habitat_codes]],
          y = curr_spp_extent,
          snap = "out",
          wopt = wopt
        ),
        "sum",
        na.rm = TRUE,
        wopt = wopt
      )
      ## apply altitudinal limits (if needed)
      if (is.finite(curr_spp_lower_elevation) ||
          is.finite(curr_spp_upper_elevation)) {
        ### create altitudinal mask
        curr_elev_mask <- terra::crop(
          x = elevation_data,
          y = terra::ext(curr_spp_habitat_data),
          snap = "out",
          wopt = wopt
        )
        curr_elev_mask <- terra::clamp(
          x = curr_elev_mask,
          lower = curr_spp_lower_elevation,
          upper = curr_spp_upper_elevation,
          values = FALSE,
          wopt = wopt
        )
        ### apply altitudinal mask
        curr_spp_habitat_data <- terra::mask(
          x = curr_spp_habitat_data,
          mask = curr_elev_mask,
          maskvalue = NA_integer_,
          updatevalue = 0,
          wopt = wopt
        )
      }

      ## apply mask
      curr_spp_habitat_data <- terra::mask(
        x = curr_spp_habitat_data,
        mask = terra_fasterize(sf = x[i, ], raster = curr_spp_habitat_data),
        updatevalue = NA_integer_,
        wopt = wopt
      )

      ## clamp data
      curr_spp_habitat_data <- terra::clamp(
        x = curr_spp_habitat_data,
        lower = 0, upper = 1000, values = TRUE,
        wopt = wopt
      )

      ## rescale data to proportion
      curr_spp_habitat_data <- terra::app(
        x = curr_spp_habitat_data,
        fun = function(x) x / 1000,
        wopt = list(datatype = "FLT4S")
      )

      ## save data
      terra::writeRaster(
        x = curr_spp_habitat_data,
        filename = curr_spp_path,
        overwrite = TRUE
      )
      file.exists(curr_spp_path)

      ## clean up
      unlink(curr_spp_tmp_dir, force = TRUE, recursive = TRUE)
      suppressWarnings(rm(curr_spp_habitat_data, curr_elev_mask))
      gc()

      ## update progress bar if needed
      if (isTRUE(verbose)) {
        cli::cli_progress_update(id = pb)
      }

      ## return success
      TRUE
  }))

  # close progress bar if needed
  if (isTRUE(verbose)) {
    cli::cli_progress_done(id = pb)
    rm(pb)
  }

  # return result
  result
}
