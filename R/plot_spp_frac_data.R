#' @include internal.R
NULL

#' Plot species range and fractional coverage data
#'
#' Create a map to compare species geographic range and fractional
#' coverage data.
#'
#' @param x [sf::st_sf()] Object containing the species data.
#'   This object should be produced using the [calc_spp_frac_data()]
#'   function.
#'
#' @inheritParams plot_spp_aoh_data
#'
#' @inherit plot_spp_aoh_data details return
#'
#' @examples
#' \dontrun{
#' # find file path for example range data following IUCN Red List data format
#' ## N.B. the range data were not obtained from the IUCN Red List,
#' ## and were instead based on data from GBIF (https://www.gbif.org/)
#' path <- system.file("extdata", "EXAMPLE_SPECIES.zip", package = "aoh")
#'
#' # import data
#' spp_range_data <- read_spp_range_data(path)
#'
#' # specify settings for data processing
#' output_dir <- tempdir()                       # folder to save AOH data
#' cache_dir <- rappdirs::user_data_dir("aoh")   # persistent storage location
#' n_threads <- parallel::detectCores() - 1      # speed up analysis
#'
#' # create cache directory if needed
#' if (!file.exists(cache_dir)) {
#'   dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
#' }
#'
#' # create Area of Habitat data for species
#' spp_aoh_data <- create_spp_aoh_data(
#'   x = spp_range_data,
#'   output_dir = output_dir,
#'   n_threads = n_threads,
#'   cache_dir = cache_dir
#' )
#'
#' # create fraction coverage dat for species
#' spp_frac_data <- calc_spp_frac_data(
#'   x = spp_aoh_data,
#'   res = 5000,
#'   output_dir = output_dir
#' )
#' # plot the data to visualize the range maps and fractional coverage data
#' p <- plot_spp_frac_data(spp_frac_data)
#' print(p)
#'
#' # this plot can be customized using ggplot2 functions
#' # for example, let's style the plot and update the colors
#' ## load ggplot2 package
#' library(ggplot2)
#'
#' ## customize plot
#' p2 <-
#'  p +
#'  scale_fill_viridis_c() +
#'  scale_color_manual(values = c("range" = "blue")) +
#'  scale_size_manual(values = c("range" = 1.5)) +
#'  theme(
#'    strip.text = ggplot2::element_text(color = "white"),
#'    strip.background = ggplot2::element_rect(
#'      fill = "black", color = "black"
#'    )
#'  )
#'
#' ## print customized plot
#' print(p2)
#' }
#'
#' \dontrun{
#' # we can also plot the data with a base map too
#' ## note that you might need to install ggmap to run this example
#' if (require(ggmap)) {
#'   ## create customized map with basemap
#'   p3 <-
#'     plot_spp_frac_data(spp_frac_data, zoom = 7, maptype = "toner") +
#'     scale_fill_viridis_c() +
#'     scale_color_manual(values = c("range" = "red")) +
#'     scale_size_manual(values = c("range" = 1.5)) +
#'     theme(
#'       strip.text = ggplot2::element_text(color = "white"),
#'       strip.background = ggplot2::element_rect(
#'       fill = "black", color = "black"
#'     )
#'   )
#'
#'   ## print customized plot
#'   print(p3)
#' }
#' }
#' @export
plot_spp_frac_data <- function(x, max_plot = 9, expand = 0.05,
                               zoom = NULL, maptype = NULL, maxcell = 50000,
                               ...) {
  plot_spp_data(
    x = x, max_plot = max_plot, expand = expand, zoom = zoom,
    maptype = maptype, maxcell = maxcell, binary = FALSE, ...
  )
}
