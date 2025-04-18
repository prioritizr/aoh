#' Repair geometry
#'
#' Repair the geometry of a [sf::st_sf()] object.
#'
#' @param x [sf::sf()] object.
#'
#' @param geometry_precision `numeric` level of precision for processing the
#'   spatial data (used with [sf::st_set_precision()]). The default is
#'   100000 (higher values indicate higher precision).
#'   Although this level of precision is generally suitable for fine-scale
#'   analyses, it might result in unnecessarily long computation times
#'   (e.g., 1500 is suitable for national-scale analyses). If you encounter
#'   geometry errors, increasing the argument to the
#'   parameter can sometimes resolve these issues.
#'
#' @details
#' This function works by first using the [sf::st_make_valid()] function
#' to attempt to fix geometry issues. Since the [sf::st_make_valid()] function
#' sometimes produce incorrect geometries in rare cases
#' (e.g., when fixing invalid geometries that cross the dateline),
#' this function then uses the `st_prepair()` function from the \pkg{prepr}
#' package to fix those geometries instead
#' (see <https://github.com/prioritizr/prepr> for details).
#'
#' @section Installation:
#' This function uses the \pkg{prepr} package to help repair geometries
#' in certain cases. Because the \pkg{prepr} package is not available on
#' the Comprehensive R Archive Network (CRAN), it must be installed from
#' its online code repository. To achieve this, please
#' use the following code:
#' ```
#' if (!require(remotes)) install.packages("remotes")
#' remotes::install_github("prioritizr/prepr")
#' ```
#'
#' Note that the \pkg{prepr} package has system dependencies that need to be
#' installed before the package itself can be installed
#' (see package README file for platform-specific instructions).
#'
#' @examples
#' # create sf object
#' p1 <- st_sf(
#'   id = 1,
#'   geometry = st_as_sfc("POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))", crs = 3857)
#' )
#'
#' # repair geometry
#' p2 <- st_repair_geometry(p1)
#'
#' # print object
#' print(p2)
#' @export
st_repair_geometry <- function(x, geometry_precision = 1e5) {
  # assert arguments are valid
  assertthat::assert_that(
    inherits(x, "sf"),
    !assertthat::has_name(x, "_repair_id"),
    assertthat::is.count(geometry_precision),
    assertthat::noNA(geometry_precision)
  )

  # add in identifier column to keep track of geometries
  x[["_repair_id"]] <- seq_len(nrow(x))

  # set precision
  x <- sf::st_set_precision(x, geometry_precision)

  # remove CRS to avoid GIS operations using spherical calculations
  x_crs <- sf::st_crs(x)
  sf::st_crs(x) <- sf::st_crs(NA)

  # apply first pass for fixing geometry
  x2 <- sf::st_make_valid(x)

  # remove empty geometries
  x2 <- x2[!sf::st_is_empty(x2), , drop = FALSE]

  # extract polygons (if needed)
  x2 <- suppressWarnings(sf::st_collection_extract(x2, "POLYGON"))

  # dissolve by repair id
  # nocov start
  if (!identical(anyDuplicated(x2[["_repair_id"]]), 0L)) {
    x2 <- split(x2, x2[["_repair_id"]])
    x2_df <- tibble::tibble(`_repair_id` = as.integer(names(x2)))
    x2 <- lapply(x2, sf::st_geometry)
    x2 <- lapply(x2, sf::st_union)
    x2 <- do.call(c, x2)
    x_df <- match(x2_df[["_repair_id"]], x[["_repair_id"]])
    x_df <- sf::st_drop_geometry(x)[x_df, , drop = FALSE]
    x_df <- x_df[, setdiff(names(x_df), "_repair_id"), drop = FALSE]
    x2_df <- tibble::as_tibble(cbind(x2_df, x_df))
    x2_df <- x2_df[, names(sf::st_drop_geometry(x)), , drop = FALSE]
    x2_df$geometry <- x2
    x2 <- sf::st_sf(x2_df)
    rm(x_df, x2_df)
  }
  # nocov end

  # add CRS information back
  sf::st_crs(x) <- x_crs
  sf::st_crs(x2) <- x_crs

  # detect if any invalid geometries persist
  ## subset repaired polygons
  x_sub <- x[match(x2[["_repair_id"]], x[["_repair_id"]]), , drop = FALSE]

  ## detect if invalid polygons based on changes in area
  area_threshold <- ifelse(sf::st_is_longlat(x), 1, 1e+4)
  invalid_idx <- which(
    abs(
      as.numeric(sf::st_area(sf::st_set_crs(x_sub, NA))) -
      as.numeric(sf::st_area(sf::st_set_crs(x2, NA)))
    ) >= area_threshold
  )
  ## refine detections to only include polygons that span width of planet
  ## note this only works if x has a defined CRS
  if (sf::st_crs(x) != st_crs(NA)) {
    ## compute global extent in coordinate system of x (if crs defined)
    global_bbox <- sf::st_as_sfc(
      "POLYGON((-180 -90, 180 -90, 180 90, -180 90, -180 -90))",
      crs = 4326
    )
    if (sf::st_crs(x) != sf::st_crs(4326)) {
      global_bbox <- sf::st_transform(global_bbox, sf::st_crs(x))
    }
    global_bbox <- sf::st_bbox(global_bbox)
    ## compute distance threshold for invalid outputs from st_make_valid()
    dist_threshold <- unname(global_bbox$xmax - global_bbox$xmin) * 0.7
    ##  detect if invalid polygons based on total width across planet
    invalid_bbox_idx <- which(
      vapply(sf::st_geometry(x2), FUN.VALUE = logical(1), function(y) {
        any(
          vapply(
            suppressWarnings(sf::st_cast(y, "POLYGON")),
            FUN.VALUE = logical(1),
            function(w) {
              b <- sf::st_bbox(w)
              (b$xmax - b$xmin) > dist_threshold
            }
          )
        )
      })
    )
    ## subset geometries
    invalid_idx <- intersect(invalid_idx, invalid_bbox_idx)
  }

  # manually fix geometries if needed
  if (length(invalid_idx) > 0) { # nocov start
    ### verify that prepr package is installed
    assertthat::assert_that(
      requireNamespace("prepr", quietly = TRUE),
      msg = paste(
        "the \"prepr\" package needs to be installed, use: \n",
        "remotes::install_github(\"prioritizr/prepr\")"
      )
    )
    ### find geometries to repair
    invalid_ids <- x_sub[["_repair_id"]][invalid_idx]
    rm(x_sub)
    ### fix geometries
    x2 <- rbind(
      x2[!x2[["_repair_id"]] %in% invalid_ids, , drop = FALSE],
      prepr::st_prepair(
        x[x[["_repair_id"]] %in% invalid_ids, , drop = FALSE]
      )
    )
  } # nocov end

  # remove custom id column
  geom_col <- attr(x2, "sf_column")
  x2 <- x2[, setdiff(names(x2), c("_repair_id", geom_col)), drop = FALSE]

  # return result
  x2
}
