#' @include internal.R
NULL

#' Process species' Area of Habitat data using GRASS
#'
#' Generate Area of Habitat data for a single species' distribution using the
#' GRASS software for processing.
#'
#' @inheritParams engine_spp_aoh_terra
#' @inheritParams create_spp_aoh_data
#'
#' @inherit engine_spp_aoh_terra return
#'
#' @noRd
engine_spp_aoh_grass <- function(range_data,
                                 habitat_data,
                                 elevation_data,
                                 habitat_values,
                                 lower_elevation,
                                 upper_elevation,
                                 extent,
                                 path,
                                 n_threads = 1,
                                 verbose = TRUE) {
  # validate arguments
  assertthat::assert_that(
    inherits(range_data, "sf"),
    inherits(habitat_data, "SpatRaster"),
    inherits(elevation_data, "SpatRaster"),
    is.numeric(habitat_values),
    length(habitat_values) > 0,
    assertthat::noNA(habitat_values),
    assertthat::is.number(lower_elevation),
    assertthat::noNA(lower_elevation),
    assertthat::is.number(upper_elevation),
    assertthat::noNA(upper_elevation),
    inherits(extent, "SpatExtent"),
    assertthat::is.string(path),
    assertthat::noNA(path),
    terra_on_disk(habitat_data),
    terra_on_disk(elevation_data),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )

  # create temporary directory for processing
  tmp_dir <- gsub("\\", "/", tempfile(), fixed = TRUE)
  dir.create(paste0(tmp_dir, "/MAPSET"), showWarnings = FALSE, recursive = TRUE)

  # save species rnage data to disk
  spp_path <- tempfile(tmpdir = tmp_dir, fileext = ".gpkg")
  range_data$x <- 1
  sf::write_sf(range_data[, "x", drop = FALSE], spp_path, overwrite = TRUE)

  # generate string to apply habitat mask
  habitat_intervals <- R.utils::seqToIntervals(habitat_values)
  habitat_mask <- vapply(
    seq_len(nrow(habitat_intervals)), FUN.VALUE = character(1), function(i) {
      if (habitat_intervals[i, 1] == habitat_intervals[i, 2]) {
        return(as.character(habitat_intervals[i, 1]))
      }
      paste0(habitat_intervals[i, 1], " thru ", habitat_intervals[i, 2])
    }
  )
  habitat_mask <- paste(habitat_mask, collapse = " ")
  reclass_data <- c(paste(habitat_mask, "= 1"), "* = 0")
  reclass_path <- tempfile(tmpdir = tmp_dir, fileext = ".txt")
  writeLines(reclass_data, reclass_path)

  # build command for GRASS script
  cmd <- paste0(
    "grass ",
    "--tmp-location \"", spp_path, "\" ",
    "--exec sh \"",
    system.file("scripts", "grass-script.sh", package = "aoh"),
    "\""
  )

  # compile variables
  envvar <- list(
    xmin = extent$xmin,
    xmax = extent$xmax,
    ymin = extent$ymin,
    ymax = extent$ymax,
    xres = terra::xres(habitat_data),
    yres = terra::yres(elevation_data),
    habitat_data = terra::sources(habitat_data)$source[[1]],
    elevation_data = terra::sources(elevation_data)$source[[1]],
    range_data = spp_path,
    reclass_data = reclass_path,
    elevation_upper = upper_elevation,
    elevation_lower = lower_elevation,
    path = path,
    verbosity = ifelse(isTRUE(verbose), "verbose", "quiet"),
    n_threads = n_threads
  )

  # run command
  invisible(capture.output(withr::with_envvar(envvar, system(cmd))))

  # clean up
  unlink(tmp_dir, force = TRUE, recursive = TRUE)

  # return result
  invisible(TRUE)
}
