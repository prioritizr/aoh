#' @include internal.R
NULL

#' Fast reproject
#'
#' Reproject a [terra::rast()] object to a conform to the spatial properties
#' of another [terra::rast()] object.
#'
#' @param x [terra::rast()] Raster object. This object specifies the
#'   [terra::crs()] coordinate reference system used for the output object.
#'
#' @param y [terra::rast()] Raster object. This object specifies the
#'   spatial extent for the output object.
#'
#' @param method `character` Name for reprojection method.
#'   See [terra::project()] for available options.
#'   Defaults to `"bilinear"`.
#'
#' @param buffer `numeric` Buffer for clipping data.
#'   Defaults to 0.
#'
#' @param verbose `logical` Should progress bars be displayed during processing?
#'   Defaults to `TRUE`.
#'
#' @param ... arguments passed to [terra::crop()] and [terra::project()].
#'
#' @inheritParams process_spp_aoh_data_on_local
#'
#' @details
#' This function speeds up data reprojection by manually excluding
#' data from `x` that do not overlap with the spatial extent of
#' `y` (accounting for a pre-specified buffer).
#'
#' @return A [terra::ext()] object.
#'
#' @noRd
fast_reproject <- function(x,
                           y,
                           method = "bilinear",
                           buffer = 0,
                           parallel_n_threads = 1,
                           parallel_strategy = "multisession",
                           temp_dir = tempdir(),
                           verbose = TRUE,
                           ...) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    inherits(y, "SpatRaster"),
    assertthat::is.number(buffer),
    assertthat::noNA(buffer)
  )
  write_args <- list(...)

  # identify spatial extent for cropping data prior to reprojection
  crop_ext <- try(
    intersecting_ext(x, y, buffer = 5000),
    silent = TRUE
  )

  # prepare for parallel processing if needed
  pb <- progressr::progressor(steps = length(x))
  paths <- lapply(seq_len(terra::nlyrs(x), function(x) {
    tempfile(tmpdir = tempdir(), fileext = ".tif")
  })
  if (isTRUE(parallel_n_threads > 1)) {
    path
    if (identical(parallel_strategy, "multicore")) {
      ### create cluster (store existing future plan if needed)
      old_plan <- future::plan("multicore", workers = parallel_n_threads)
    } else {
      ## multi session cluster
      #### save large large objects to disk
      if (terra::isMemory(x)) {
        x_path <- terra::wrap(x)
      } else {
        x_path <- terra::sources(x)$source
      }
      if (terra::isMemory(y)) {
        y_path <- terra::wrap(y)
      } else {
        y_path <- terra::sources(y)$source
      }
      ### initialize cluster
      cl <- parallel::makePSOCKCluster(parallel_n_threads)
      parallel::clusterExport(cl, c(
        "x_path", "y_path", "pb", "write_args", "crop_ext", "method",
        "parallel_n_threads", "paths"
      ))
      parallel::clusterEvaLQ(cl, {
        library(terra)
        x <- terra::rast(x)
        y <- terra::rast(y)
      })
      ### create cluster (store existing future plan if needed)
      old_plan <- future::plan("cluster", workers = cl)
    }
    ## set old plan if needed
    on.exit(future::plan(old_plan), add = TRUE)
  }

  # process data
  x <- furrr::future_map(
    seq_along(terra::nlyrs(x),
    function(i) {
      ## extract layer
      p <- paths[[i]]
      i <- x[[i]]
      ## crop layer
      if (!inherits(crop_ext, "try-error")) {
        i <- do.call(terra::crop, append(list(x = i, y = crop_ext), write_args))
      }
      ## reproject layer
      i <- do.call(
        terra::project,
        append(list(x = i, y = y, method = method), write_args)
      )
      ## if parallel then prepare result for host
      if (parallel_n_threads > 1) {
        if (is.character(x_path)) {
          ## sink to disk if too large for memory
          do.call(terra::writeRaster, list(x = i, filename = p, write_args))
          i <- p
        } else {
          ## wrap
          i <- terra::wrap(i)
        }
      }
      ##  update progress bar
      pb()
      ## return result
      list(p)
    }
  )

  # process result


  # crop x to extent of y if possible
  if (!inherits(crop_ext, "try-error")) {
    x <- terra::rast(
      plyr::llply(
        as.list(x),
        function(x) {
          suppressMessages(

          )
        }
      )
    )
  }


  ### reproject to y
  #### create parallelization


}

#' Intersecting extent
#'
#' Generate a [terra::ext()] object containing the spatial extent of one
#' [terra::rast()] raster object inside another [terra::rast()] object.
#'
#' @param x [terra::rast()] Raster object. This object specifies the
#'   [terra::crs()] coordinate reference system used for the output object.
#'
#' @param y [terra::rast()] Raster object. This object specifies the
#'   spatial extent for the output object.
#'
#' @param buffer `numeric` buffer applied to the spatial extent of `y` before
#'   reprojecting to the coordinate reference system of `x`. Defaults to 0.
#'
#' @return A [terra::ext()] object.
#'
#' @noRd
intersecting_ext <- function(x, y, buffer = 0) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    inherits(y, "SpatRaster"),
    assertthat::is.number(buffer)
  )
  # processing
  ## extract extent for y
  y_ext <- sf::st_as_sfc(terra_st_bbox(y))
  ## extract extent for x
  x_ext <- sf::st_as_sfc(terra_st_bbox(x))
  ## reproject to x coordinate system without buffer
  y_ext_no_buffer <- sf::st_bbox(
    sf::st_transform(y_ext, sf::st_crs(x))
  )
  ## reproject to x coordinate system with buffer
  y_ext_buffer <- sf::st_bbox(
    sf::st_transform(sf::st_buffer(y_ext, buffer), sf::st_crs(x))
  )
  ## handle NAs
  if (any(is.na(c(y_ext_buffer)))) {
    ## create new extent
    y_ext_buffer2 <- c(xmin = 0, xmax = 0, ymin = 0, ymax = 0)
    y_ext_buffer2[["xmin"]] <- ifelse(
      is.na(y_ext_buffer$xmin), y_ext_no_buffer$xmin, y_ext_buffer$xmin
    )
    y_ext_buffer2[["xmax"]] <- ifelse(
      is.na(y_ext_buffer$xmax), y_ext_no_buffer$xmax, y_ext_buffer$xmax
    )
    y_ext_buffer2[["ymin"]] <- ifelse(
      is.na(y_ext_buffer$ymin), y_ext_no_buffer$ymin, y_ext_buffer$ymin
    )
    y_ext_buffer2[["ymax"]] <- ifelse(
      is.na(y_ext_buffer$ymax), y_ext_no_buffer$ymax, y_ext_buffer$ymax
    )
    ## handle if any remaining NAs
    if (any(is.na(y_ext_buffer2))) {
      ### if x is in lon/lat, and we still get NAs for reprojecting extent
      ## of y into x, then this is due to precision issues with
      ## data in x occurring at the global scale, so we can manually
      ## assume a global extent for x
      if (terra::is.lonlat(x)) {
        y_ext_buffer2 <-
          sf::st_bbox(
            c(xmin = -180, xmax = 180, ymin = -90, ymax = 90), crs =
            sf::st_crs(x)
          )
      } else {
        stop("failed to find intersecting extents")
      }
    }
  } else {
    y_ext_buffer2 <- y_ext_buffer
  }
  ## clip to x coordinate system
  y_ext <- sf::st_bbox(sf::st_intersection(y_ext_buffer2, x_ext))
  ## return extent of intersecting areas
  terra::ext(c(y_ext$xmin, y_ext$xmax, y_ext$ymin, y_ext$ymax))
}
