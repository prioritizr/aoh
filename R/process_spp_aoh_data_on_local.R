#' @include internal.R misc_terra.R misc_sf.R
NULL

#' Process species Area of Habitat data locally
#'
#' Process species Area of Habitat (AOH) data using computational
#' resources that are available locally.
#'
#' @inheritParams create_spp_aoh_data
#'
#' @param frac_template_data [terra::rast()] Raster data to serve as
#'  template for computing fractional coverage data.
#'
#' @param x [sf::sf()] Spatial data delineating species geographic ranges
#'   obtained from the [IUCN Red List](https://www.iucnredlist.org/).
#'   These data should have previously been cleaned (via
#'   [clean_spp_range_data()] and contain the following additional columns:
#'   `"habitat_code"`, `"elevation_lower"`, `"elevation_upper"`,
#'   `"xmin"`, `"xmax"`, `"ymin"`, `"ymax"`, `"path"`.
#'
#' @noRd
process_spp_aoh_data_on_local <- function(x,
                                          habitat_data,
                                          elevation_data,
                                          crosswalk_data,
                                          frac_template_data = NULL,
                                          cache_dir = tempdir(),
                                          engine = "terra",
                                          n_threads = 1,
                                          force = FALSE,
                                          verbose = TRUE) {
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
    inherits(frac_template_data, c("NULL", "SpatRaster")),
    terra::compareGeom(habitat_data[[1]], elevation_data, stopOnError = FALSE),
    assertthat::is.string(cache_dir),
    assertthat::noNA(cache_dir),
    assertthat::is.string(engine),
    assertthat::noNA(engine),
    engine %in% c("terra", "gdal", "grass"),
    assertthat::is.writeable(cache_dir),
    assertthat::is.flag(force),
    assertthat::noNA(force),
    assertthat::is.flag(verbose),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )

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
    cli::cli_progress_update(id = pb, set = 0)
  }

  # initialize GRASS session if needed
  if (identical(engine, "grass")) {
    ## create temporary directory for processing
    grass_dir <- gsub("\\", "/", tempfile(), fixed = TRUE)
    dir.create(grass_dir, showWarnings = FALSE, recursive = TRUE)
    data_dir <- gsub("\\", "/", tempfile(), fixed = TRUE)
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

    ## set up GRASS connection
    link2GI::initProj(projRootDir = grass_dir, projFolders = "aoh/")
    link2GI::linkGRASS7(
      x = x[1, "xmin", drop = FALSE],
      gisdbase = grass_dir,
      location = "aoh"
    )

    ## force habitat to disk
    h_on_disk <- terra_on_disk(habitat_data)
    habitat_data <- terra_force_disk(
      habitat_data, overwrite = TRUE, datatype = "INT2U",
      gdal = c("COMPRESS=LZW", "BIGTIFF=YES")
    )

    ## force elevation data to disk
    e_on_disk <- terra_on_disk(elevation_data)
    elevation_data <- terra_force_disk(
      elevation_data, overwrite = TRUE, datatype = "INT2S",
      gdal = c("COMPRESS=LZW", "BIGTIFF=YES")
    )

    ## import habitat data
    rgrass7::execGRASS(
      "r.external",
      redirect = TRUE, legacyExec = TRUE,
      parameters = list(
        input = terra::sources(habitat_data)$source[[1]],
        output = "habitat"
      )
    )

    ## import habitat data
    rgrass7::execGRASS(
      "r.external",
      redirect = TRUE, legacyExec = TRUE,
      parameters = list(
        input = terra::sources(elevation_data)$source[[1]],
        output = "elev"
      )
    )

  }

  # main processing
  result <- suppressWarnings(plyr::llply(
    .data = idx,
    .fun = function(i) {

      ## extract data for current iteration
      curr_spp_path <- x$path[i]
      if (is.null(frac_template_data)) {
        curr_aoh_path <- curr_spp_path
      } else {
        curr_aoh_path <- tempfile(fileext = ".tif")
      }
      curr_spp_lower_elevation <- x$elevation_lower[i]
      curr_spp_upper_elevation <- x$elevation_upper[i]
      curr_spp_extent <- terra::ext(c(
        xmin = x$xmin[i], xmax = x$xmax[i],
        ymin = x$ymin[i], ymax = x$ymax[i]
      ))
      curr_spp_habitat_values <- sort(unique(crosswalk_data$value[
        which(crosswalk_data$code %in% x$habitat_code[[i]])
      ]))

      ## process data using engine
      if (identical(engine, "terra")) {
        engine_spp_aoh_terra(
          range_data = x[i, ],
          habitat_data = habitat_data,
          elevation_data = elevation_data,
          habitat_values = curr_spp_habitat_values,
          lower_elevation = curr_spp_lower_elevation,
          upper_elevation = curr_spp_upper_elevation,
          extent = curr_spp_extent,
          path = curr_aoh_path
        )
      } else if (identical(engine, "gdal")) {
        engine_spp_aoh_gdal(
          range_data = x[i, ],
          habitat_data = habitat_data,
          elevation_data = elevation_data,
          habitat_values = curr_spp_habitat_values,
          lower_elevation = curr_spp_lower_elevation,
          upper_elevation = curr_spp_upper_elevation,
          extent = curr_spp_extent,
          path = curr_aoh_path,
          n_threads = n_threads
        )
      }  else if (identical(engine, "grass")) {
        engine_spp_aoh_grass(
          range_data = x[i, ],
          habitat_data = habitat_data,
          elevation_data = elevation_data,
          habitat_values = curr_spp_habitat_values,
          lower_elevation = curr_spp_lower_elevation,
          upper_elevation = curr_spp_upper_elevation,
          extent = curr_spp_extent,
          path = curr_aoh_path,
          verbose = FALSE
        )
      }

      ## verify success
      assertthat::assert_that(
        file.exists(curr_aoh_path),
        msg = paste("failed to save AOH data:", curr_aoh_path)
      )

      ## compute fractional coverage data if needed
      if (!is.null(frac_template_data)) {
        ### processing
        process_spp_frac_data_on_local(
          aoh_path = curr_aoh_path,
          template_data = frac_template_data,
          path = curr_spp_path,
          engine = engine,
          n_threads = n_threads
        )
        ### clean up
        unlink(curr_aoh_path, force = TRUE)
        ### verify success
        assertthat::assert_that(
          file.exists(curr_spp_path),
          msg = paste("failed to save fractional coverage data:", curr_spp_path)
        )
      }

      ## update progress bar if needed
      if (isTRUE(verbose)) {
        cli::cli_progress_update(id = pb)
      }

      ## return success
      TRUE
  }))


  # clean up GRASS
  if (identical(engine, "grass")) {
    if (!h_on_disk) {
      f <- terra::sources(habitat_data)$source[[1]]
      rm(habitat_data)
      unlink(f, force = TRUE)
    }
    if (!e_on_disk) {
      f <- terra::sources(elevation_data)$source[[1]]
      rm(elevation_data)
      unlink(f, force = TRUE)
    }
    unlink(grass_dir, force = TRUE, recursive = TRUE)
  }

  # close progress bar if needed
  if (isTRUE(verbose)) {
    cli::cli_progress_done(id = pb)
    rm(pb)
  }

  # return result
  result
}
