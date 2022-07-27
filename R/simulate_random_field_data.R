#' @include internal.R
NULL

#' Simulate data using random fields
#'
#' Simulate spatially auto-correlated data using Gaussian random fields.
#'
#' @param x [terra::rast()] Raster object to use as a template.
#'
#' @param n `integer` Number of layers to simulate.
#'
#' @param scale `numeric` Parameter to control level of spatial
#'   auto-correlation.
#'
#' @param transform `function` Transform values output from the simulation.
#'
#' @return [terra::rast()] object.
#'
#' @examples
#' \dontrun{
#' # create raster
#' r <- rast(ncol = 10, nrow = 10, xmn = 0, xmx = 1, ymn = 0, ymx = 1)
#' values(r) <- 1
#'
#' # simulate data using a Gaussian field
#' x <- simulate_rf_data(r, n = 1, scale = 0.2)
#'
#' # plot simulated data
#' plot(x, main = "simulated data")
#' }
#' @noRd
simulate_random_field_data <- function(x, n, scale, transform = identity) {
  # assert valid arguments
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    assertthat::is.number(n),
    is.finite(terra::global(x, "max", na.rm = TRUE)[[1]]),
    assertthat::is.number(scale),
    assertthat::noNA(scale))

  # create object for simulation
  obj <- fields::circulantEmbeddingSetup(
    grid = list(
      x = seq(0, 5, length.out = terra::nrow(x)),
      y = seq(0, 5, length.out = terra::ncol(x))
    ),
    Covariance = "Exponential",
    aRange = scale
  )

  # generate populate rasters with values
  r <- terra::rast(lapply(seq_len(n), function(i) {
    ## populate with simulated values
    v <- transform(c(t(fields::circulantEmbedding(obj))))
    r <- terra::setValues(x[[1]], v[seq_len(ncell(x[[1]]))])
    ## apply mask for consistency
    r <- raster::mask(r, x[[1]])
    ## return result
    r
  }))

  # return result
  r
}
