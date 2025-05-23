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
#' @param expand `numeric` Proportion to expand the plotting limits.
#'   Defaults to 0.05 such that plot limits are extended 5% beyond the
#'   spatial extent of the data.
#'
#' @param zoom `numeric` Value indicating the zoom level for the basemap.
#'   See documentation for the `zoom` parameter in the [ggmap::get_stadiamap()]
#'   function for details.
#'   Defaults to `NULL` such that no basemap is shown.
#'
#' @param maptype `character` Value indicating the name of the
#'   the basemap to use for the plot.
#'   See documentation for the `maptype` parameter in the
#'   [ggmap::get_stadiamap()]
#'   function for details.
#'   Defaults to `NULL` such that no basemap is shown.
#'   Note that the \pkg{ggmap} package must be installed to show a basemap.
#'
#' @param maxcell `integer` Maximum number of grid cells for mapping.
#'   Defaults to 50000.
#'
#' @param ... Additional arguments passed to [ggmap::get_stadiamap()].
#'
#' @details
#' Note that data are automatically projected to a
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
#' ## N.B., the range data were not obtained from the IUCN Red List,
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
#' # create species information data
#' spp_info_data <- create_spp_info_data(
#'   x = spp_range_data,
#'   cache_dir = cache_dir
#' )
#'
#' # create Area of Habitat data for species
#' spp_aoh_data <- create_spp_aoh_data(
#'   x = spp_info_data,
#'   output_dir = output_dir,
#'   n_threads = n_threads,
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
#'  scale_fill_viridis_d() +
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
#'     plot_spp_aoh_data(spp_aoh_data, zoom = 7, maptype = "stamen_toner") +
#'     scale_fill_manual(
#'       values = c("suitable" = "blue", "not suitable" = "transparent")
#'     ) +
#'     scale_color_manual(values = c("range" = "red")) +
#'     scale_size_manual(values = c("range" = 2)) +
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
plot_spp_aoh_data <- function(x, max_plot = 9, expand = 0.05,
                              zoom = NULL, maptype = NULL, maxcell = 50000,
                              ...) {
  plot_spp_data(
    x = x, max_plot = max_plot, expand = expand, zoom = zoom,
    maptype = maptype, maxcell = maxcell, binary = TRUE, ...
  )
}

#' Plot species range and raster data
#'
#' Create a map to compare species geographic range and raster data.
#'
#' @param x [sf::st_sf()] Object containing the species data.
#'   This object should be produced using the [create_spp_aoh_data()],
#'   [create_spp_frc_data()], or [calc_spp_frc_data()] functions.
#'
#' @param binary `logical` Value indicating if the data sohuld be
#'   converted to binary values or not.
#'   Defaults to `FALSE`.
#'
#' @inheritParams plot_spp_aoh_data
#'
#' @inherit plot_spp_data details return
#'
#' @noRd
plot_spp_data <- function(x, max_plot = 9, expand = 0.05,
                          zoom = NULL, maptype = NULL, maxcell = 50000,
                          binary = FALSE, ...) {
  # assert argument is valid
  assertthat::assert_that(
    inherits(x, "sf"),
    assertthat::has_name(x, "id_no"),
    assertthat::has_name(x, "binomial"),
    assertthat::has_name(x, "xmin"),
    assertthat::has_name(x, "xmax"),
    assertthat::has_name(x, "ymin"),
    assertthat::has_name(x, "ymax"),
    msg = "`x` should be the output from `create_spp_aoh_data()`"
  )
  assertthat::assert_that(
    assertthat::is.count(max_plot),
    assertthat::noNA(max_plot),
    assertthat::is.number(expand),
    assertthat::noNA(expand),
    assertthat::is.flag(binary),
    assertthat::noNA(binary),
    isTRUE(expand >= 0)
  )
  if (!is.null(maptype)) {
    if (!requireNamespace("ggmap", quietly = TRUE)) {
      # nocov start
      stop(
        "please install the \"ggmap\" package to create plots with a basemap"
      )
      # nocov end
    }
    assertthat::assert_that(
      assertthat::is.string(maptype),
      assertthat::noNA(maptype)
    )
    assertthat::assert_that(
      assertthat::is.number(zoom),
      assertthat::noNA(zoom),
      msg = c(
        "`zoom` must be a number when `maptype` is specified"
      )
    )
  }
  if (is.null(maptype) && !is.null(zoom)) {
    cli::cli_alert("`zoom` has no effect if `maptype = NULL`") # nocov
  }
  if (nrow(x) > max_plot) {
    # nocov start
    warning(
      paste0(
        "plotting the first 9 out of ", nrow(x), " Area of Habitat datasets, ",
        "use `max_plot = ", nrow(x), "` to plot all datasets"
      ),
      immediate. = TRUE
    )
    x <- x[seq_len(max_plot), , drop = FALSE]
    # nocov end
  }

  # prepare data
  ## range data
  x$type <- ordered(factor("range", levels = "range"))
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
    if (terra::ncell(r) > maxcell) {
      d <- terra::spatSample(
        r[[1]], size = maxcell, method = "regular", xy = TRUE, warn = FALSE
      )
    } else {
      d <- terra::as.data.frame(r, xy = TRUE)
    }
    names(d) <- c("x", "y", "value")
    d <- d[is.finite(d$value), , drop = FALSE]
    if (isTRUE(binary)) {
      d$value <- dplyr::if_else(d$value > 0.5, "suitable", "not suitable")
    } else {
      d$value <- d$value * 100
    }
    d$binomial <- x$binomial[[i]]
    d$filename <- x$filename[[i]]
    # determine resolution for plotting
    rxres <- abs(diff(d$x))
    rxres <- min(rxres[rxres > 0], na.rm = TRUE)
    ryres <- abs(diff(d$y))
    ryres <- min(ryres[ryres > 0], na.rm = TRUE)
    ## return list with data
    list(data = d, width = rxres, height = ryres)
  })

  # prepare base plot
  if (is.null(maptype)) {
    g <- ggplot2::ggplot()
  } else {
    g <- ggmap::ggmap(
      get_ggmap_basemap(x, expand = expand, zoom = zoom, maptype = maptype, ...)
    )
  }

  # assign labels
  if (isTRUE(binary)) {
    lab <- "Area of Habitat"
  } else {
    lab <- "Area of Habitat (%)"
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
      fill = lab,
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
#' @return A [ggmap::get_stadiamap()] object.
#'
#' @noRd
get_ggmap_basemap <- function(x, expand = 0.05, ...) {
  assertthat::assert_that(sf::st_crs(x) == sf::st_crs(4326))
  assertthat::assert_that(
    nzchar(Sys.getenv("GGMAP_STADIAMAPS_API_KEY")),
    msg = paste(
      "StadiaMaps API key must be registered to plot a basemap,",
      "see `?ggmap::register_stadiamaps` for details."
    )
  )
  bb <- as.list(sf::st_bbox(x))
  bb2 <- bb
  if (expand > 0) {
    xf <- abs(bb[["xmax"]] - bb[["xmin"]]) * expand
    yf <- abs(bb[["ymax"]] - bb[["ymin"]]) * expand
    bb2[["xmin"]] <- bb[["xmin"]] - xf
    bb2[["xmax"]] <- bb[["xmax"]] + xf
    bb2[["ymin"]] <- bb[["ymin"]] - yf
    bb2[["ymax"]] <- bb[["ymax"]] + yf
  }
  ggmap::get_stadiamap(unname(unlist(bb2)), ...)
}
