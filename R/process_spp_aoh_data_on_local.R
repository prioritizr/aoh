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
                                          parallel_n_threads = 1,
                                          parallel_cluster = NULL,
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
    assertthat::noNA(x$path),
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
    assertthat::is.count(parallel_n_threads),
    assertthat::noNA(parallel_n_threads),
    assertthat::is.string(parallel_cluster),
    assertthat::noNA(parallel_cluster),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )
  ## validate parallel configuration
  if (is.null(parallel_cluster)) {
    parallel_cluster <- ifelse(
      identical(.Platform$OS.type, "unix"), "FORK", "PSOCK"
    )
  }
  assertthat::assert_that(
    assertthat::is.string(parallel_cluster),
    assertthat::noNA(parallel_cluster)
  )
  assertthat::assert_that(
    identical(parallel_cluster, "FORK") ||
    identical(parallel_cluster, "PSOCK"),
    msg = paste(
      "argument to \"parallel_cluster\" is not NULL,",
      "\"FORK\", or \"PSOCK\""
    )
  )

  # prepare prepare variables for data processing
  wopt <- list(...)

  # determine which species need processing
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

  # prepare for parallel processing if needed
  pb <- progressr::progressor(steps = length(idx))
  if (isTRUE(parallel_n_threads > 1)) {
    ## prepare data
    ### elevation_data
    if (all(terra::inMemory(elevation_data))) {
      elevation_path <- tempfile(fileext = ".tif")
      terra::writeRaster(
        x = elevation_data, filename = elevation_path, wopt = wopt
      )
    } else {
      elevation_path <- terra::sources(elevation_data)$source
    }
    ### habitat_data
    if (all(terra::inMemory(habitat_data))) {
      habitat_path <- tempfile(fileext = ".tif")
      terra::writeRaster(
        x = habitat_data, filename = habitat_path, wopt = wopt
      )
    } else {
      habitat_path <- terra::sources(habitat_data)$source
    }
    ### x
    x_path <- tempfile(fileext = ".rda")
    saveRDS(x, x_path, compress = "xz")

    ## create cluster
    if (identical(parallel_cluster, "FORK")) {
      cl <- parallel::makeCluster(parallel_n_threads, "FORK")
    } else {
      cl <- parallel::makeCluster(parallel_n_threads, "PSOCK")
      parallel::clusterExport(
        cl = cl,
        envir = environment(),
        varlist = c(
          "x_path", "habitat_path", "elevation_path", "pb", "wopt",
          "parallel_n_threads"
        )
      )
    }
    ## set up workers
    on.exit(parallel::stopCluster(cl), add = TRUE)

    ## set up future plan
    old_plan <- future::plan("cluster", workers = cl)
    on.exit(future::plan(old_plan), add = TRUE)
  }

  # main processing
  result <- furrr::future_map_lgl(
    .x = idx,
    .options = furrr::furrr_options(seed = TRUE),
    .f = function(i) {
      ## initialization
      if (!all(
        exists("habitat_data2"),
        exists("elevation_data2"),
        exists("x2"))
      ) {
        if (isTRUE(parallel_n_threads > 1)) {
          ## if parallel processing
          assign("x2", readRDS(x_path), envir = globalenv())
          assign(
            "habitat_data2", terra::rast(habitat_path), envir = globalenv()
          )
          assign(
            "elevation_data2", terra::rast(elevation_path), envir = globalenv()
          )
        } else {
          ## if local processing
          x2 <- x
          habitat_data2 <- habitat_data
          elevation_data2 <- elevation_data
        }
      }

      ## extract data for current iteration
      curr_spp_path <- x2$path[i]
      curr_spp_habitat_codes <- x2$habitat_code[[i]]
      curr_spp_lower_elevation <- x2$elevation_lower[i]
      curr_spp_upper_elevation <- x2$elevation_upper[i]
      curr_spp_extent <- aoh::sf_terra_ext(x2[i, , drop = FALSE])

      ## create temporary directory for species
      curr_spp_tmp_dir <- tempfile()
      dir.create(curr_spp_tmp_dir, showWarnings = FALSE, recursive = TRUE)
      terra::terraOptions(progress = 0, tempdir = curr_spp_tmp_dir)

      ## calculate total sum of habitat based on codes
      curr_spp_habitat_data <- terra::app(
        terra::crop(
          x = habitat_data2[[curr_spp_habitat_codes]],
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
          x = elevation_data2,
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
        mask = terra::vect(x[i, ]),
        updatevalue = NA_integer_,
        wopt = wopt
      )

      ## rescale data to proportion
      curr_spp_habitat_data <- terra::app(
        x = curr_spp_habitat_data,
        function(x) x / (1000 * terra::nlyr(habitat_data2))
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

      ## increment progress bar
      pb()

      ## return success
      TRUE
  })

  # return result
  result
}
