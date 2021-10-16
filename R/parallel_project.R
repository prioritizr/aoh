#' @include internal.R misc_terra.R misc_sf.R
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
#'  init(x, runif)
#' }))
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
    intersecting_ext(x, y, buffer = max(terra::res(y) * 5)),
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
      x_import <- tempfile(tmpdir = temp_dir, fileext = ".tif")
      terra::writeRaster(x = x, filename = x_import, wopt = wopt)
    } else {
      x_import <- terra::sources(x)$source
    }
    if (all(terra::inMemory(y))) {
      y_import <- tempfile(tmpdir = temp_dir, fileext = ".tif")
      terra::writeRaster(x = y, filename = y_import, wopt = wopt)
    } else {
      y_import <- terra::sources(y)$source
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
          "x_import", "y_import",
          "pb", "wopt", "method", "paths",
          "crop_ext_list", "parallel_n_threads"
        )
      )
    }
    ## setup workers
    on.exit(parallel::stopCluster(cl), add = TRUE)
    ## set up future plan
    old_plan <- future::plan("cluster", workers = cl)
    on.exit(future::plan(old_plan), add = TRUE)
  } else {
    x_import <- x
    y_import <- y
  }

  # process data
  idx <- seq_len(terra::nlyr(x))
  x <- future.apply::future_lapply(
    X = idx,
    future.envir = baseenv(),
    future.seed = FALSE,
    future.globals = FALSE,
    FUN = function(i) {
      ## initialization (alas clusterEvalQ not compatible with terra)
      if (isTRUE(parallel_n_threads > 1)) {
        ## if parallel processing
        x2 <- terra::rast(x_import)
        y2 <- terra::rast(y_import)
      } else {
        ## if local processing
        x2 <- x_import
        y2 <- y_import
      }

      ## extract layer
      xi <- x2[[i]]

      # crop layer
      if (!is.null(crop_ext_list)) {
        crop_ext2 <- terra::ext(crop_ext_list)
        xi <- do.call(terra::crop, append(list(x = xi, y = crop_ext2), wopt))
      }

      ## reproject layer
      xi <- do.call(
        terra::project,
        append(list(x = xi, y = y2, method = method), wopt)
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


  print("")
  print("here1")
  print("")

  # process result
  if (inherits(x[[1]], "character")) {
    x <- terra::rast(unlist(x, recursive = FALSE, use.names = FALSE))
  } else {
    x <- terra::rast(x)
  }

  print("")
  print("here2")
  print("")


  # return result
  x
}
