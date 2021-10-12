#' @include internal.R
NULL

#' Plot species range and Area of Habitat data
#'
#' Create a map to compare species geographic range and Area of Habitat data.
#'
#' @param x [sf::st_sf()] Object containing the species data.
#'   This object should be produced using the [create_spp_aoh_data()]
#'   function.
#'
#' @param max_plot `integer` Maximum number of Area of Habitat datasets
#'   to plot.
#'   Defaults to 9.
#'
#' @param zoom `numeric` Value indicating the zoom level for the basemap.
#'   See documentation for the `zoom` parameter in the [ggmap::get_stamenmap()]
#'   function for details.
#'   Defaults to `NULL` such that no basemap is shown.
#'
#' @param maptype `character` Value indicating the name of the
#'   the basemap to use for the plot.
#'   See documentation for the `maptype` parameter in the
#'   [ggmap::get_stamenmap()]
#'   function for details.
#'   Defaults to `NULL` such that no basemap is shown.
#'
#' @param ... Additional arguments passed to [ggmap::get_stamenmap()].
#'
#' @details
#' Note that the Area of Habitat data are automatically projected to a
#' geographic coordinate system (EPSG:4326) when they are plotted with
#' a base map. This means that the Area of Habitat data shown in
#' maps that contain a base map might look slightly different from
#' underlying dataset.
#'
#' @return A [ggplot2::ggplot()] object.
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
#' # create Area of Habitat data for species
#' spp_aoh_data <- create_spp_aoh_data(
#'   x = spp_range_data,
#'   output_dir = output_dir,
#'   parallel_n_threads = n_threads,
#'   cache_dir = cache_dir
#' )
#'
#' # plot the data to visualize the range maps and AOH data
#' p <- plot_spp_aoh_data(spp_aoh_data)
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
#'  scale_fill_viridis() +
#'  scale_color_manual(c("range" = "red")) +
#'  scale_size_manual(c("range" = 2)) +
#'  theme(
#'    axis.title = element_blank(),
#'    strip.text = element_text(color = "white"),
#'    strip.background = element_rect(fill = "black", color = "black")
#'  )
#'
#' ## print customized plot
#' print(p2)
#' }
#'
#' \dontrun{
#' # we can also plot the data with a base map too
#' if (require(ggmap)) {
#'   p3 <-
#'     plot_spp_aoh_data(spp_aoh_data, maptype = "toner") +
#'     scale_fill_viridis() +
#'     scale_color_manual(c("range" = "red")) +
#'     scale_size_manual(c("range" = 2)) +
#'     theme(
#'       axis.title = element_blank(),
#'       strip.text = element_text(color = "white"),
#'       strip.background = element_rect(fill = "black", color = "black")
#'     )
#' }
#' }
#' @export
plot_spp_aoh_data <- function(x, max_plot = 9,
                              zoom = NULL, maptype = NULL, ...) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(x, "sf"),
    assertthat::has_name(x, "id_no"),
    assertthat::has_name(x, "binomial"),
    assertthat::has_name(x, "path"),
    msg = "argument to \"x\" should be the output from create_aoh_data()"
  )
  assertthat::assert_that(
    assertthat::is.count(max_plot),
    assertthat::noNA(max_plot)
  )
  if (!is.null(maptype)) {
    if (!requireNamespace("ggmap", quietly = TRUE)) {
      stop("the \"ggmap\" package must be installed to include a basemap")
    }
    assertthat::assert_that(
      assertthat::is.string(maptype),
      assertthat::noNA(maptype)
    )
    assertthat::assert_that(
      assertthat::is.number(zoom),
      assertthat::noNA(zoom),
      msg = c(
        "argument to \"zoom\" must be a number when \"maptype\" is specified"
      )
    )
  }
  if (is.null(maptype) && !is.null(zoom)) {
    cli::cli_alert("argument to \"zoom\" has no effect if \"maptype\" is NULL")
  }
  if (nrow(x) > max_plot) {
    warning(
      paste(
        "plotting the first 9 out of", nrow(x), "Area of Habitat datasets;",
        "use max_plot =", nrow(x), "to plot all"
      ),
      immediate. = TRUE
    )
    x <- x[seq_len(max_plot), , drop = FALSE]
  }

  # prepare data
  ## range data
  x$type <- factor("range", levels = "range")
  x$filename <- basename(x$path)
  if (!is.null(zoom) && !is.null(maptype)) {
    x <- sf::st_transform(x, 4326)
  }

  ## AOH data
  gg_tile_data <- lapply(seq_len(nrow(x)), function(i) {
    ## import raster
    r <- terra::rast(x$path[[i]])
    ## reproject if needed
    if (!is.null(zoom) && !is.null(maptype)) {
      r <- terra::project(r, "epsg:4326")
    }
    ## extract data
    d <- terra::as.data.frame(r[[1]], xy = TRUE, na.rm = TRUE)
    names(d) <- c("x", "y", "value")
    d$binomial <- x$binomial[[i]]
    d$filename <- x$filename[[i]]
    ## return list with data
    list(
      data = d,
      width = terra::xres(r),
      height = terra::yres(r)
    )
  })

  # prepare base plot
  if (is.null(maptype)) {
    g <- ggplot2::ggplot()
  } else {
    g <- ggmap::ggmap(get_ggmap_basemap(x, zoom = zoom, maptype = maptype, ...))
  }

  # create plot
  p <- g
  for (i in seq_along(gg_tile_data)) {
    p <-
      p +
      ggplot2::geom_tile(
        ggplot2::aes(x = .data$x, y = .data$y, fill = .data$value),
        height = gg_tile_data[[i]]$height,
        width = gg_tile_data[[i]]$width,
        data = gg_tile_data[[i]]$data
      )
  }
  p <-
    p +
    ggplot2::geom_sf(
      ggplot2::aes(color = .data$type, size = .data$type),
      fill = NA,
      inherit.aes = FALSE,
      data = x
    ) +
    ggplot2::facet_wrap(~ binomial + filename) +
    ggplot2::labs(
      colour = "Geographic range",
      fill = "Area of Habitat",
    ) +
    ggplot2::guides(size = "none")

  # return plot
  p
}

#' Get a basemap using ggmap
#'
#' Get a basemap for plotting spatial data using the \pkg{ggmap} package.
#'
#' @inheritParams plot_spp_aoh_data
#'
#' @param x [sf::st_sf()] object.
#'
#' @return A [ggmap::get_stamenmap()] object.
get_ggmap_basemap <- function(x, ...) {
  assertthat::assert_that(sf::st_crs(x) == sf::st_crs(4326))
  ggmap::get_stamenmap(unname(sf::st_bbox(x)), ...)
}