#' @include internal.R misc_terra.R misc_sf.R
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
#' @param ... arguments passed to [terra::writeRaster()] for processing.
#'
#' @noRd
process_spp_aoh_data_on_local <- function(x,
                                          habitat_data,
                                          elevation_data,
                                          crosswalk_data,
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

  # combine habitat and elevation data into a single object
  raster_data <- terra::rast(list(habitat_data, elevation_data))

  # create custom progress bar
  verbose <- FALSE
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
      curr_spp_habitat_values <- crosswalk_data$value[
        which(crosswalk_data$code %in% curr_spp_habitat_codes)
      ]

      ## create temporary directory for species
      curr_spp_tmp_dir <- gsub("\\", "/", tempfile(), fixed = TRUE)
      dir.create(curr_spp_tmp_dir, showWarnings = FALSE, recursive = TRUE)
      terra::terraOptions(progress = 0, tempdir = curr_spp_tmp_dir)

      ## crop data to species range extent
      cli::cli_progress_step("cropping extent")
      curr_spp_habitat_data <- terra::crop(
        x = raster_data,
        y = curr_spp_extent,
        snap = "out",
        wopt = wopt
      )

      ## convert to presence/absence of suitable habitat
      cli::cli_progress_step("converting to presence/absence")
      curr_spp_habitat_data <- terra::lapp(
        terra::crop(
          x = raster_data,
          y = curr_spp_extent,
          snap = "out",
          wopt = wopt
        ),
        function(x, y) {
          1 * ((x %in% curr_spp_habitat_values) &
          (y >= curr_spp_lower_elevation) &
          (y <= curr_spp_upper_elevation))
        },
        wopt = wopt
      )

      ## create mask for species range
      cli::cli_progress_step("creating mask")
      curr_spp_mask <- terra_fasterize(
        sf = x[i, ], raster = curr_spp_habitat_data
      )

      ## mask data by species range
      cli::cli_progress_step("masking to range")
      curr_spp_habitat_data <- terra::mask(
        x = curr_spp_habitat_data,
        mask = curr_spp_mask,
        updatevalue = NA_integer_,
        filename = curr_spp_path,
        wopt = wopt
      )

      ## verify success
      assertthat::assert_that(
        file.exists(curr_spp_path),
        msg = paste("failed to save raster:", curr_spp_path)
      )

      ## clean up
      unlink(curr_spp_tmp_dir, force = TRUE, recursive = TRUE)
      suppressWarnings(rm(curr_spp_habitat_data))
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
