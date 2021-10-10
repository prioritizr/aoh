#' @include internal.R
NULL

#' Simulate data using random fields
#'
#' Simulate spatially auto-correlated data using the \pkg{RandomFields} package.
#'
#' @param x [terra::rast()] Raster object to use as a template.
#'
#' @param n `integer` Number of species to simulate.
#'
#' @param model [RandomFields::RP()] Model object
#'   to use for simulating data.
#'
#' @param transform `function` Transform values output from the simulation.
#'
#' @param ... additional arguments passed to
#'   [RandomFields::RFsimulate()].
#'
#' @return [terra::rast()] object with a layer for each species.
#'
#' @examples
#' \dontrun{
#' # create raster
#' r <- rast(ncol = 10, nrow = 10, xmn = 0, xmx = 1, ymn = 0, ymx = 1)
#' values(r) <- 1
#'
#' # simulate data using a Gaussian field
#' x <- simulate_rf_data(r, n = 1, model = RandomFields::RMgauss())
#'
#' # plot simulated data
#' plot(x, main = "random Gaussian field")
#' }
#' @noRd
simulate_rf_data <- function(x, n, model, transform = identity, ...) {
  # assert valid arguments
  if (!requireNamespace("RandomFields", quietly = TRUE))
    stop("the \"RandomFields\" package needs to be installed to simulate data")
  assertthat::assert_that(
    inherits(x, "SpatRaster"),
    assertthat::is.number(n),
    is.finite(terra::global(sim_rast, "max", na.rm = TRUE)[[1]]),
    inherits(model, "RMmodel"),
    inherits(transform, "function"))

  # generate values for rasters
  coords <- terra::crds(x, df = FALSE)
  mtx <- RandomFields::RFsimulate(
    model = model,
    x = coords[, 1],
    y = coords[, 2],
    n = n,
    spConform = FALSE,
    ...
  )

  # convert to matrix if not a matrix
  if (!inherits(mtx, "matrix"))
    mtx <- matrix(mtx, ncol = 1)

  # generate populate rasters with values
  r <- terra::rast(lapply(seq_len(ncol(mtx)), function(i) {
    r <- x
    r[!is.na(x)] <- transform(mtx[, i])
    r
  }))

  # return result
  return(r)
}
