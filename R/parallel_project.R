#' @include internal.R
NULL

#' Parallel project
#'
#' Project a [terra::rast()] object to a conform to the spatial properties
#' of another [terra::rast()] object.
#' Additionally, parallel processing can be used to reduce run time.
#'
#' @param x [terra::rast()] Raster object with data needs to be
#'   projected.
#'
#' @param y [terra::rast()] Raster object with spatial properties
#' that are used to define the output (i.e. spatial extent, resolution,
#' and coordinate reference system).
#'
#' @param method `character` Name for projection method.
#'   See [terra::project()] for available options.
#'   Defaults to `"bilinear"`.
#'
#' @param buffer `numeric` Buffer for clipping data.
#'   Defaults to 0.
#'
#' @param temp_dir `character` Directory for saving temporary files.
#'   Defaults to [base::tempdir()].
#'
#' @param ... arguments passed to [terra::crop()] and [terra::project()].
#'
#' @inheritParams create_spp_aoh_data
#'
#' @details
#' This function speeds up data projection by manually excluding
#' data from `x` that do not overlap with the spatial extent of
#' `y` (accounting for a pre-specified buffer).
#'
#' @return A [terra::ext()] object.
#'
#' @examples
#' # define raster object with data for projection
#' x <- rast(
#'   ncols = 40, nrows = 40,
#'   xmin = -110, xmax = -90, ymin = 40, ymax = 60,
#'   crs = "+proj=longlat +datum=WGS84"
#' )
#' x <- rast(lapply(seq_len(10), function(i) {
#'  init(x, "runif")
#' })
#'
#' # preview object
#' print(x)
#'
#' # define raster object with spatial properties used to perform
#' # the projection processing
#' y <- rast(
#'   ncols = 94, nrows = 124,
#'   xmin = -944881, xmax = 935118, ymin = 4664377, ymax = 7144377,
#'   crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
#' )
#' print(y)
#'
#' # project data using a single thread
#' z <- parallel_project(x, y, parallel_n_threads = 1)
#' print(z)
#'
#' # project data using multiple threads
#' \dontrun{
#' z2 <- parallel_project(x, y, parallel_n_threads = 1)
#' print(z2)
#' }
#' @export
parallel_project <- function(x,
                             y,
                             method = "bilinear",
                             buffer = 0,
                             parallel_n_threads = 1,
                             parallel_cluster = NULL,
                             temp_dir = tempdir(),
                             verbose = TRUE,
                             ...) {
  # assert valid arguments
  ## initial validation
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    inherits(y, "SpatRaster"),
    assertthat::is.number(buffer),
    assertthat::noNA(buffer)
  )
  ## parallel cluster
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

  # store terra::writeRaster arguments
  wopt <- list(...)

  # identify spatial extent for cropping data prior to reprojection
  crop_ext <- try(
    intersecting_ext(x, y, buffer = 5000),
    silent = TRUE
  )
  crop_ext_list <- NULL

  # prepare for parallel processing if needed
  pb <- progressr::progressor(steps = terra::nlyr(x))
  paths <- lapply(seq_len(terra::nlyr(x)), function(x) {
    tempfile(tmpdir = temp_dir, fileext = ".tif")
  })

  # prepare data for parallelization
  if (isTRUE(parallel_n_threads > 1)) {
    ## prepare data
    if (all(terra::inMemory(x))) {
      x_path <- tempfile(tmpdir = temp_dir, fileext = ".tif")
      terra::writeRaster(x = x, filename = x_path, wopt = wopt)
    } else {
      x_path <- terra::sources(x)$source
    }
    if (all(terra::inMemory(y))) {
      y_path <- tempfile(tmpdir = temp_dir, fileext = ".tif")
      terra::writeRaster(x = y, filename = y_path, wopt = wopt)
    } else {
      y_path <- terra::sources(y)$source
    }
    if (!inherits(crop_ext, "try-error")) {
      crop_ext_list <- unlist(as.list(crop_ext))
    } else {
      crop_ext_list <- NULL
    }
    ## create cluster
    if (identical(parallel_cluster, "FORK")) {
      cl <- parallel::makeCluster(parallel_n_threads, "FORK")
    } else {
      cl <- parallel::makeCluster(parallel_n_threads, "PSOCK")
      parallel::clusterExport(
        cl = cl,
        envir = environment(),
        varlist = c(
          "x_path", "y_path", "pb", "wopt", "method", "paths",
          "crop_ext_list", "parallel_n_threads", "parallel_cluster"
        )
      )
    }
    ## setup workers
    on.exit(parallel::stopCluster(cl), add = TRUE)
    ## set up future plan
    old_plan <- future::plan("cluster", workers = cl)
    on.exit(future::plan(old_plan), add = TRUE)
  }

  # process data
  x <- furrr::future_map(
    .x = seq_len(terra::nlyr(x)),
    .f = function(i) {
      ## import layers if in parallel
      if (
        isTRUE(parallel_n_threads > 1) &&
        !identical(Sys.getenv("setup"), "TRUE")
      ) {
        m <<- 1
        x <<- terra::rast(x_path)
        y <<- terra::rast(y_path)
        if (!is.null(crop_ext_list)) {
          crop_ext <<- terra::ext(crop_ext_list)
        }
        Sys.setenv("setup" = "TRUE")
      }
      ## extract layer
      xi <- x[[i]]
      ## crop layer
      if (!is.null(crop_ext_list)) {
        xi <- do.call(terra::crop, append(list(x = xi, y = crop_ext), wopt))
      }
      ## reproject layer
      xi <- do.call(
        terra::project,
        append(list(x = xi, y = y, method = method), wopt)
      )
      ## if parallel then prepare result for host
      if (isTRUE(parallel_n_threads > 1)) {
        terra::writeRaster(x = xi, filename = paths[[i]], wopt = wopt)
      }
      ##  update progress bar
      pb()
      ## return result
      if (isTRUE(parallel_n_threads > 1)) {
        return(paths[[i]])
      } else {
        return(xi)
      }
    }
  )
  # process result
  if (inherits(x[[1]], "character")) {
    x <- terra::rast(unlist(x, recursive = FALSE, use.names = FALSE))
  } else {
    x <- terra::rast(x)
  }

  # return result
  x
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
