#' @include internal.R misc_terra.R misc_sf.R
NULL

#' Collate species extent data
#'
#' Collate species extent data for processing Area of Habitat data.
#'
#' @param x `sf::st_sf()` Spatial object.
#'
#' @param template_data `terra::rast()` Raster object.
#'
#' @return
#' A [sf::st_sf()] spatial object containing an updated version of the
#' argument to `x` that contains the following additional
#' columns: `"xmin"`, `"xmax"`, `"ymin"` and `"ymax"`.
#'
#' @noRd
collate_spp_extent_data <- function(x, template_data) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "sf"),
    nrow(x) > 0,
    inherits(template_data, "SpatRaster"),
    sf::st_crs(x) == terra_st_crs(template_data)
  )

  # add spatial extent columns
  ## create empty version of data template
  empty_template <- terra::rast(
    xmin = terra::xmin(template_data),
    xmax = terra::xmax(template_data),
    ymin = terra::ymin(template_data),
    ymax = terra::ymax(template_data),
    resolution = terra::res(template_data),
    crs =  terra::crs(template_data),
  )
  ## add columns for xmin, xmax, ymin, and ymax
  ### note that we return NAs if the species doesn't overlap with the template
  x <- dplyr::bind_cols(
    x,
    dplyr::bind_rows(lapply(seq_len(nrow(x)), function(i) {
      if (
        sf::st_intersects(
          x = sf::st_as_sfc(terra_st_bbox(empty_template)),
          y = sf::st_as_sfc(sf::st_bbox(x[i, ])),
          sparse = FALSE
        )[[1]]
      ) {
        ex <- terra::ext(
          terra::crop(
            x = empty_template,
            y = sf_terra_ext(x[i, ]),
            snap = "out"
          )
        )
      } else {
        # nocov start
        ex <- list(
          xmin = NA_real_, xmax = NA_real_, ymin = NA_real_, ymax = NA_real_
        )
        # nocov end
      }
      data.frame(
        xmin = ex$xmin,
        xmax = ex$xmax,
        ymin = ex$ymin,
        ymax = ex$ymax
      )
    }))
  )

  # re-order columns
  x <- dplyr::select(x, dplyr::everything(), -"geometry", "geometry")

  # return result
  x
}
