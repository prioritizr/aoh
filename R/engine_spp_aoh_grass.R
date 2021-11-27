#' @include internal.R
NULL

#' Process species' Area of Habitat data using GRASS
#'
#' Generate Area of Habitat data for a single species' distribution using the
#' GRASS software for processing.
#'
#' @param idx `integer` index corresponding to species for processing.
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
                                 memory = 4000,
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
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)

  # save species range data to disk
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

  # initialize region
  rgrass7::execGRASS(
    "g.region",
    parameters = list(
      n = as.character(extent$ymax),
      s = as.character(extent$ymin),
      e = as.character(extent$xmax),
      w = as.character(extent$xmin),
      ewres = as.character(terra::xres(habitat_data)),
      nsres = as.character(terra::yres(habitat_data))
    )
  )

  # import vector data
  rgrass7::execGRASS(
    "v.import",
    redirect = TRUE, legacyExec = TRUE,
    flags = c("overwrite", ifelse(verbose, "verbose", "quiet")),
    parameters = list(
      input = spp_path,
      output = "range",
      extent = "region"
    )
  )

  # rasterize vector data
  rgrass7::execGRASS(
    "v.to.rast",
    redirect = TRUE, legacyExec = TRUE,
    flags = c("overwrite", ifelse(verbose, "verbose", "quiet")),
    parameters = list(
      input = "range",
      type = "area",
      use = "val",
      value = 1,
      output = "mask",
      memory = memory
    )
  )

  # set mask based on range data
  rgrass7::execGRASS(
    "r.mask", redirect = TRUE, legacyExec = TRUE,
    flags = c("overwrite"), parameters = list(raster = "mask")
  )

  # reclassify habitat data
  rgrass7::execGRASS(
    "r.reclass",
    flags = c("overwrite", ifelse(verbose, "verbose", "quiet")),
    redirect = TRUE, legacyExec = TRUE,
    parameters = list(
      input = "habitat",
      output = "suitable",
      rules = reclass_path
    )
  )

  # calculate Area of Habitat
  rgrass7::execGRASS(
    "r.mapcalc",
    flags = c("overwrite", ifelse(verbose, "verbose", "quiet")),
    redirect = TRUE, legacyExec = TRUE,
    parameters = list(
      expression = paste0(
        "aoh = int(suitable * ",
        "((elev >= ", lower_elevation, ") & (elev <= ", upper_elevation, ")))"
      )
    )
  )

  # save data
  rgrass7::execGRASS(
    "r.out.gdal",
    redirect = TRUE, legacyExec = TRUE,
    flags = c("overwrite", "c", ifelse(verbose, "verbose", "quiet")),
    parameters = list(
      input = "aoh",
      output = path,
      format = "GTiff",
      type = "Byte",
      nodata = 255,
      createopt = paste0("BIGTIFF=YES,COMPRESS=LZW,NUM_THREADS=", n_threads)
    )
  )

  # remove mask
  rgrass7::execGRASS(
    "g.remove",
    redirect = TRUE, legacyExec = TRUE,
    flags = "f",
    parameters = list(
      type = "raster",
      name = "mask"
    )
  )

  # clean up
  unlink(tmp_dir, force = TRUE, recursive = TRUE)

  # return result
  invisible(TRUE)
}
