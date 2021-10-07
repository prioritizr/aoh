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
#' @noRd
process_spp_aoh_data_on_local <- function(x,
                                          habitat_data,
                                          elevation_data,
                                          cache_dir = tempdir(),
                                          force = FALSE,
                                          parallel_n_threads = 1,
                                          parallel_strategy = NULL,
                                          verbose = TRUE) {
  # assert that arguments are valid
  ## initial validation
  assertthat::assert_that(
    inherits(x, "sf"),
    assertthat::noNA(x$elevation_lower),
    assertthat::noNA(x$elevation_upper),
    inherits(habitat_data, "SpatRaster"),
    inherits(elevation_data, "SpatRaster"),
    assertthat::is.string(cache_dir),
    assertthat::noNA(cache_dir),
    assertthat::is.writeable(cache_dir),
    assertthat::is.flag(force),
    assertthat::noNA(force),
    assertthat::is.count(parallel_n_threads),
    assertthat::noNA(parallel_n_threads),
    assertthat::is.string(parallel_strategy),
    assertthat::noNA(parallel_strategy),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose),
    terra::compareGeom(habitat_data[[1]], elevation_data, stopOnError = FALSE)
  )
  ## parallel cluster
  if (is.null(parallel_strategy)) {
    parallel_strategy <- ifelse(
      identical(.Platform$OS.type, "unix"), "multicore", "multisession"
    )
  }
  assertthat::assert_that(
    assertthat::is.string(parallel_strategy),
    assertthat::noNA(parallel_strategy)
  )
  assertthat::assert_that(
    identical(parallel_strategy, "multicore") ||
    identical(parallel_strategy, "multisession"),
    msg = paste(
      "argument to \"parallel_strategy\" is not NULL,",
      "\"multicore\", or \"multisession\""
    )
  )

  # preliminary processing
  habitat_codes <- names(habitat_data)

  # AOH processing
  ## determine which species need processing
  if (!force & any(file.exists(x$path))) {
    idx <- which(!is.na(x$path))
    # idx <- which(!is.na(x$path) & !file.exists(x$path))
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
  ## prepare for parallel processing if needed
  pb <- progressr::progressor(steps = length(idx))
  if (isTRUE(parallel_n_threads > 1)) {
    if (identical(parallel_strategy, "multicore")) {
      ### create cluster (store existing future plan if needed)
      old_plan <- future::plan("multicore", workers = parallel_n_threads)
    } else {
      ## multi session cluster
      #### save large large objects to disk
      x_path <- tempfile(fileext = ".rda")
      saveRDS(x, x_path, compress = "xz")
      if (terra::isMemory(elevation_data)) {
      } else {
        elevation_path <- terra::sources(elevation_data)$source
      }
      if (terra::isMemory(habitat_data)) {
        habitat_path <- terra::wrap(habitat_data)
      } else {
        habitat_path <- terra::sources(habitat_data)$source
      }
      ### initialize cluster
      cl <- parallel::makePSOCKCluster(parallel_n_threads)
      parallel::clusterExport(cl, envir = environment(), c(
        "output_dir", "spp_summary_data", "spp_habitat_data",
        "elevation_path", "habitat_path", "x_path", "pb"
      ))
      parallel::clusterEvaLQ(cl, {
        library(terra)
        library(sf)
        x <- readRDS(x_path)
        elevation_path <- terra::rast(elevation_path)
        habitat_data <- terra::rast(habitat_path)
      })
      ### create cluster (store existing future plan if needed)
      old_plan <- future::plan("cluster", workers = cl)
    }
    ## set old plan if needed
    on.exit(future::plan(old_plan), add = TRUE)
  }
  ## main processing
  result <- furrr::future_map_lgl(
    .x = idx,
    .f = function(i) {
      ### initialization
      curr_spp_path <- x$path[i]
      curr_spp_habitat_codes <- x$habitat_code[[i]]
      curr_spp_lower_elevation <- x$elevation_lower[i]
      curr_spp_upper_elevation <- x$elevation_upper[i]
      ### create temporary directory for species
      curr_spp_tmp_dir <- tempfile()
      dir.create(curr_spp_tmp_dir, showWarnings = FALSE, recursive = TRUE)
      terra::terraOptions(progress = 0, tempdir = curr_spp_tmp_dir)
      ### generate bounding box for species range
      curr_spp_extent <- sf_terra_ext(x[i, , drop = FALSE])
      ### calculate total sum of habitat based on codes
      curr_spp_habitat_data <- sum(
        terra::crop(
          x = habitat_data[[curr_spp_habitat_codes]],
          y = curr_spp_extent,
          snap = "out",
          datatype = "INT2U"
        ),
        na.rm = TRUE
      )
      ### apply altitudinal limits (if needed)
      if (is.finite(curr_spp_lower_elevation) ||
          is.finite(curr_spp_upper_elevation)) {
        #### create altitudinal mask
        curr_elev_mask <- terra::crop(
          x = elevation_data,
          y = terra::ext(curr_spp_habitat_data),
          snap = "out",
          datatype = "INT2U"
        )
        curr_elev_mask <- terra::clamp(
          x = curr_elev_mask,
          lower = curr_spp_lower_elevation,
          upper = curr_spp_upper_elevation,
          values = FALSE,
          datatype = "INT2U"
        )
        ## apply altitudinal mask
        curr_spp_habitat_data <- terra::mask(
          x = curr_spp_habitat_data,
          mask = curr_elev_mask,
          maskvalue = NA_integer_,
          updatevalue = 0,
          datatype = "INT2U"
        )
      }
      ### apply mask
      curr_spp_habitat_data <- terra::mask(
        x = curr_spp_habitat_data,
        mask = terra::vect(x[i, ]),
        updatevalue = NA_integer_,
        datatype = "INT2U"
      )
      ### rescale data to proportion
      curr_spp_habitat_data <- terra::app(
        x = curr_spp_habitat_data,
        function(x) x / (1000 * terra::nlyr(habitat_data)),
        wopt = list(datatype = "FLT4S")
      )
      ### save data
      terra::writeRaster(
        x = curr_spp_habitat_data,
        filename = curr_spp_path,
        overwrite = TRUE,
        datatype = "FLT4S"
      )
      file.exists(curr_spp_path)
      ### clean up
      unlink(curr_spp_tmp_dir, force = TRUE, recursive = TRUE)
      rm(curr_spp_habitat_data, curr_elev_mask)
      gc()
      ### increment progress bar
      pb()
      ### return success
      TRUE
  })
  ## clean up
  if (isTRUE(parallel_n_threads > 1)) {
    if (identical(parallel_strategy, "multisession")) {
      cl <- parallel::stopCluster(cl)
    }
  }

  # return result
  result
}
