#' Combine rasters
#'
#' Align and combine a list of multiple [terra::rast()] objects into a
#' single [terra::rast()] object with multiple layers.
#'
#' @param x `list` of [terra::rast()] objects.
#'
#' @return A [terra::rast()] object.
#'
#' @examples
#' # create a raster
#' x <- rast(
#'   ncols = 10, nrows = 10,
#'   xmin = 0, xmax = 10, ymin = 0, ymax = 10,
#'   crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
#' )
#' values(x) <- runif(ncell(x))
#' names(x) <- "lyr1"
#' print(x)
#'
#' # create another raster
#' y <- rast(
#'   ncols = 5, nrows = 5,
#'   xmin = 90, xmax = 95, ymin = 80, ymax = 85,
#'   crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=WGS84"
#' )
#' values(y) <- runif(ncell(y))
#' names(y) <- "lyr2"
#' print(y)
#'
#' # combine them together
#' z <- terra_combine(list(x, y))
#'
#' # plot combined raster
#' plot(z)
#'
#' @export
terra_combine <- function(x) {
  # assert valid arguments
  assertthat::assert_that(is.list(x))
  assertthat::assert_that(
    all(vapply(x, inherits, logical(1), "SpatRaster")),
    msg = "all elements in the argument to x must SpatRaster objects"
  )
  res <-
  assertthat::assert_that(
    all(vapply(terra::as.list(x), FUN.VALUE = logical(1), function(r) {
      identical(terra::res(x[[1]]), terra::res(r))
    })),
    msg = "all elements in the argument to x must have the same resolution"
  )
  assertthat::assert_that(
    all(vapply(terra::as.list(x), FUN.VALUE = logical(1), function(r) {
      identical(terra::origin(x[[1]]), terra::origin(r))
    })),
    msg = "all elements in the argument to x must have the same origin"
  )

  # compute maximum extent
  max_ext <- lapply(terra::as.list(x), function(x) {
    sf::st_as_sfc(terra_st_bbox(x), crs = terra::crs(x))[[1]]
  })
  max_ext <- sf_terra_ext(sf::st_bbox(sf::st_union(sf::st_combine(
    sf::st_set_crs(sf::st_sfc(max_ext), terra_st_crs(x[[1]]))
  ))))

  # store names
  x_names <- unlist(
    lapply(terra::as.list(x), names),
    use.names = FALSE, recursive = TRUE
  )

  # expand extents as needed
  x <- terra::rast(lapply(terra::as.list(x), function(x) {
    if (terra::ext(x) == max_ext) return(x)
    terra::extend(x = x, y = max_ext)
  }))

  # assign names
  names(x) <- x_names

  # return result
  x
}
