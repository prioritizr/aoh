#' @include internal.R misc_terra.R misc_sf.R
NULL

#' Process species data locally
#'
#' Process species data using computational resources that are available
#' locally.
#'
#' @inheritParams create_spp_aoh_data
#'
#' @param frc_template_data [terra::rast()] Raster data to serve as
#'  template for computing fractional coverage data.
#'
#' @param x [sf::sf()] Spatial data delineating species geographic ranges
#'   obtained from the [IUCN Red List](https://www.iucnredlist.org/).
#'   These data should have previously been cleaned (via
#'   `clean_spp_range_data()` and contain the following additional columns:
#'   `"habitat_code"`, `"elevation_lower"`, `"elevation_upper"`,
#'   `"xmin"`, `"xmax"`, `"ymin"`, `"ymax"`, `"path"`.
#'
#' @noRd
process_spp_data_on_local <- function(x,
                                      habitat_data,
                                      elevation_data,
                                      crosswalk_data,
                                      frc_template_data = NULL,
                                      cache_dir = tempdir(),
                                      engine = "terra",
                                      n_threads = 1,
                                      cache_limit = 200,
                                      rasterize_touches = FALSE,
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
    inherits(frc_template_data, c("NULL", "SpatRaster")),
    terra::compareGeom(habitat_data[[1]], elevation_data, stopOnError = FALSE),
    assertthat::is.string(cache_dir),
    assertthat::noNA(cache_dir),
    assertthat::is.string(engine),
    assertthat::noNA(engine),
    engine %in% c("terra", "gdal", "grass"),
    assertthat::is.flag(force),
    assertthat::noNA(force),
    assertthat::is.flag(rasterize_touches),
    assertthat::noNA(rasterize_touches),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )

  # normalize file paths
  cache_dir <- normalize_path(cache_dir, mustWork = FALSE)
  assertthat::assert_that(assertthat::is.writeable(cache_dir))

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
        "{.alert-info processing} ",
        "{cli::pb_bar} [{cli::pb_percent} | {cli::pb_eta_str}]"
      ),
      format_done = paste0(
        "{.alert-success processing [{cli::pb_elapsed}]}"
      )
    )
    cli::cli_progress_update(id = pb, set = 0)
  }

  # ensure raster data on disk
  ## force habitat to disk
  h_on_disk <- terra_on_disk(habitat_data)
  habitat_data <- terra_force_disk(
    habitat_data,
    overwrite = TRUE,
    datatype = ifelse(
      nzchar(terra::datatype(habitat_data)),
      terra::datatype(habitat_data),
      "INT4S"
    ),
    gdal = c("COMPRESS=LZW", "BIGTIFF=YES")
  )

  ## force elevation data to disk
  e_on_disk <- terra_on_disk(elevation_data)
  elevation_data <- terra_force_disk(
    elevation_data,
    overwrite = TRUE,
    datatype = ifelse(
      nzchar(terra::datatype(elevation_data)),
      terra::datatype(elevation_data),
      "INT4S"
    ),
    gdal = c("COMPRESS=LZW", "BIGTIFF=YES")
  )

  # initialize GRASS session if needed
  if (identical(engine, "grass")) {
    ## create temporary directory for processing
    grass_dir <- normalize_path(tempfile(), mustWork = FALSE)
    dir.create(grass_dir, showWarnings = FALSE, recursive = TRUE)
    data_dir <- normalize_path(tempfile(), mustWork = FALSE)
    dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

    ## initialize GRASS
    if (identical(.Platform$OS.type, "unix")) {
      ### Unix setup
      loc <- rgrass::initGRASS(
        home = grass_dir,
        SG = sp::SpatialGrid(
          sp::GridTopology(
            cellcentre.offset = c(0, 0),
            cellsize = c(1, 1),
            cells.dim = c(5, 5)
          ),
          proj4string = sp::CRS(terra::crs(elevation_data))
        ),
        gisDbase = data_dir,
        override = TRUE
      )
    } else {
      ### Windows setup
      link2GI::initProj(projRootDir = grass_dir, projFolders = "aoh/")
      suppressWarnings(
        link2GI::linkGRASS(
          x = x[1, 1],
          gisdbase = grass_dir,
          location = "aoh",
          quiet = FALSE
        )
      )
    }

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

    ## create a VRT containing cropped version of habitat data
    e_grass_vrt_path <- normalize_path(
      tempfile(fileext = ".vrt"),
      mustWork = FALSE
    )
    terra_gdal_crop(
      x = elevation_data,
      ext = terra::ext(c(
        xmin = min(x$xmin), xmax = max(x$xmax),
        ymin = min(x$ymin), ymax = max(x$ymax)
      )),
      filename = e_grass_vrt_path,
      tiled = FALSE,
      datatype = "INT2S",
      cache_limit = cache_limit,
      bigtiff = TRUE,
      NAflag = "none",
      n_threads = n_threads,
      output_raster = FALSE,
      verbose = FALSE
    )

    ## create a VRT containing cropped version of elevation data
    h_grass_vrt_path <- normalize_path(
      tempfile(fileext = ".vrt"),
      mustWork = FALSE
    )
    terra_gdal_crop(
      x = habitat_data,
      ext = terra::ext(c(
        xmin = min(x$xmin), xmax = max(x$xmax),
        ymin = min(x$ymin), ymax = max(x$ymax)
      )),
      filename = h_grass_vrt_path,
      tiled = FALSE,
      datatype = "INT2U",
      cache_limit = cache_limit,
      bigtiff = TRUE,
      NAflag = "none",
      n_threads = n_threads,
      output_raster = FALSE,
      verbose = FALSE
    )

    ## import habitat data
    rgrass::execGRASS(
      "r.external",
      redirect = TRUE,
      legacyExec = TRUE,
      Sys_show.output.on.console = FALSE,
      flags = c("o", "r"),
      parameters = list(
        input = h_grass_vrt_path,
        output = "habitat"
      )
    )

    ## import elevation data
    rgrass::execGRASS(
      "r.external",
      redirect = TRUE,
      legacyExec = TRUE,
      Sys_show.output.on.console = FALSE,
      flags = c("o", "r"),
      parameters = list(
        input = e_grass_vrt_path,
        output = "elev"
      )
    )

  }

  # main processing
  result <- suppressWarnings(lapply(idx, function(i) {
      ## determine path for saving AOH data
      curr_spp_path <- x$path[i]
      if (is.null(frc_template_data)) {
        curr_aoh_path <- curr_spp_path
      } else {
        curr_aoh_path <- normalize_path(
          tempfile(fileext = ".tif"),
          mustWork = FALSE
        )
      }

      ## process AOH data
      withr::with_envvar(
        c(
          "GDAL_CACHEMAX" = as.integer(cache_limit),
          "GDAL_DISABLE_READDIR_ON_OPEN" = "TRUE"
        ),
        process_spp_aoh_on_local(
            x = x[i, , drop = FALSE],
            habitat_data = habitat_data,
            elevation_data = elevation_data,
            habitat_values = sort(unique(crosswalk_data$value[
              which(crosswalk_data$code %in% x$habitat_code[[i]])
            ])),
            lower_elevation = x$elevation_lower[i],
            upper_elevation = x$elevation_upper[i],
            extent = terra::ext(c(
              xmin = x$xmin[i], xmax = x$xmax[i],
              ymin = x$ymin[i], ymax = x$ymax[i]
            )),
            path = curr_aoh_path,
            engine = engine,
            n_threads = n_threads,
            cache_limit = cache_limit,
            rasterize_touches = rasterize_touches
        )
      )

      ## compute fractional coverage data if needed
      if (!is.null(frc_template_data)) {
        ### process data
        withr::with_envvar(
          c(
            "GDAL_CACHEMAX" = as.integer(cache_limit),
            "GDAL_DISABLE_READDIR_ON_OPEN" = "TRUE"
          ),
          process_spp_frc_on_local(
            aoh_path = curr_aoh_path,
            template_data = frc_template_data,
            path = curr_spp_path,
            engine = engine,
            n_threads = n_threads
          )
        )
        ### clean up
        unlink(curr_aoh_path, force = TRUE)
      }

      ## update progress bar if needed
      if (isTRUE(verbose)) {
        cli::cli_progress_update(id = pb)
      }

      ## return success
      TRUE
  }))

  # clean up rasters
  # nocov start
  if (!h_on_disk) {
    f <- terra::sources(habitat_data)[[1]]
    rm(habitat_data)
    unlink(f, force = TRUE)
  }
  if (!e_on_disk) {
    f <- terra::sources(elevation_data)[[1]]
    rm(elevation_data)
    unlink(f, force = TRUE)
  }
  # nocov end

  # clean up GRASS
  if (identical(engine, "grass")) {
    # nocov start
    unlink(grass_dir, force = TRUE, recursive = TRUE)
    unlink(e_grass_vrt_path, force = TRUE, recursive = TRUE)
    unlink(h_grass_vrt_path, force = TRUE, recursive = TRUE)
    # nocov end
  }

  # close progress bar if needed
  if (isTRUE(verbose)) {
    cli::cli_progress_done(id = pb)
    rm(pb)
  }

  # return result
  result
}
