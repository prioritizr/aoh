#' @include internal.R
NULL

#' Calculate fractional coverage data
#'
#' Calculate fractional coverage of species' Area of Habitat.
#'
#' @inheritParams plot_spp_aoh_data
#' @inheritParams create_spp_frc_data
#'
#' @param template_data [terra::rast()] Raster data to use as a template
#'   for computing fractional coverage.
#'   Note that the argument should have the same spatial properties
#'   as the elevation and habitat data used to generate the
#'   Area of Habitat data.
#'   Defaults to `NULL` such that template data are automatically imported
#'   as the default global habitat dataset (using
#'   [get_lumb_cgls_habitat_data()]).
#'
#' @param engine `character` Value indicating the name of the software
#'   to use for data processing.
#'   Available options include `"terra"` or `"gdal"`.
#'   Defaults to `"terra"`.
#'
#' @param output_dir `character` `character` Folder path to save raster
#'   (GeoTIFF) files containing the fractional coverage data.
#'
#' @param ... Arguments passed to [get_lumb_cgls_habitat_data()].
#'
#' @details
#' This function works by
#' (i) creating a template grid based on the specified resolution,
#' (ii) aggregating the Area of Habitat data to the match the
#' spatial origin and resolution of the template grid, and
#' (iii) dividing the aggregated values based on the aggregation
#' factor to express values as fractional coverage.
#'
#' @inheritSection create_spp_frc_data Output file format
#'
#' @return An updated version of the argument to `x` with updated values
#' for the `path`, `xmin`, `xmax`, `ymin`, and `ymax` columns.
#' The
#'
#' @seealso
#' This function is useful for creating fractional coverage data when
#' you have previously generated species' Area of Habitat data.
#' If you have not previously generated species' Area of Habitat data,
#' you can use the [create_spp_frc_data()] to create fractional coverage
#' data directly.
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
#' output_aoh_dir <- tempdir()                   # folder to save AOH data
#' output_frc_dir <- tempdir()                   # folder to save coverage data
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
#'   output_dir = output_aoh_dir,
#'   n_threads = n_threads,
#'   cache_dir = cache_dir
#' )
#'
#' # compute fractional coverage across a 5 x 5 km spatial grid
#' spp_aoh_frc_data <- calc_spp_frc_data(
#'   x = spp_aoh_data,
#'   output_dir = output_frc_dir,
#'   res = 5000,
#'   cache_dir = cache_dir
#' )
#' }
#'
#' @examplesIf interactive()
#' \dontrun{
#' # preview data
#' print(spp_aoh_frc_data)
#' }
#'
#' @examples
#' \dontrun{
#' # plot the data to visualize the range maps and fractional coverage data
#' plot_spp_frc_data(spp_aoh_frc_data)
#'}
#' @export
calc_spp_frc_data <- function(x,
                               res,
                               output_dir,
                               template_data = NULL,
                               cache_dir = tempdir(),
                               force = FALSE,
                               n_threads = 1,
                               engine = "terra",
                               cache_limit = 1000,
                               verbose = TRUE,
                               ...) {
  # assert arguments are valid
  ## initial validation
  assertthat::assert_that(
    inherits(x, "sf"),
    assertthat::has_name(x, "id_no"),
    assertthat::has_name(x, "binomial"),
    assertthat::has_name(x, "xmin"),
    assertthat::has_name(x, "xmax"),
    assertthat::has_name(x, "ymin"),
    assertthat::has_name(x, "ymax"),
    msg = "`x` should be the output from create_spp_aoh_data()"
  )
  assertthat::assert_that(
    assertthat::is.string(output_dir),
    assertthat::noNA(output_dir),
    assertthat::is.writeable(output_dir),
    assertthat::is.string(cache_dir),
    assertthat::noNA(cache_dir),
    assertthat::is.writeable(cache_dir),
    assertthat::is.number(res),
    assertthat::noNA(res),
    assertthat::is.number(cache_limit),
    assertthat::noNA(cache_limit),
    assertthat::is.flag(force),
    assertthat::noNA(force),
    assertthat::is.count(n_threads),
    assertthat::noNA(n_threads),
    assertthat::is.string(engine),
    assertthat::noNA(engine),
    engine %in% c("terra", "gdal")
  )
  assertthat::assert_that(
    cache_limit <= 9999,
    msg = "`cache_limit` must exceed 9999"
  )
  if (isTRUE(identical(engine, "gdal"))) {
    assertthat::assert_that(
      requireNamespace("gdalUtilities", quietly = TRUE),
      msg = paste(
        "the \"gdalUtilities\" package needs to be installed, use",
        "install.packages(`\"gdalUtilities\"`)"
      )
    )
  }
  ## template data
  if (is.null(template_data)) {
    ### display message
    if (verbose) {
      cli::cli_progress_step("importing global habitat data")
    }
    ### processing
    template_data <- get_lumb_cgls_habitat_data(
      dir = cache_dir, force = force, verbose = verbose, ...
    )
  }
  assertthat::assert_that(
    inherits(template_data, "SpatRaster"),
    terra::nlyr(template_data) == 1
  )

  # sanitize directory path
  output_dir <- gsub("\\", "/", output_dir, fixed = TRUE)

  # compute aggregation factor
  assertthat::assert_that(
    terra::xres(template_data) == terra::yres(template_data),
    msg = "`template_data` must have square cells"
  )
  fact <- res / terra::xres(template_data)
  assertthat::assert_that(
    assertthat::is.count(fact),
    assertthat::noNA(fact),
    msg = paste(
      "`res` does not correspond to a valid aggregation factor",
      "for `template_data`"
    )
  )

  # reset terra options on exit
  on.exit(terra::terraOptions(progress = 3, tempdir = tempdir()))

  # create spatial grid representing aggregated spatial properties
  template_data <- terra::aggregate(
    x = terra::rast(
      xmin = terra::xmin(template_data),
      xmax = terra::xmax(template_data),
      ymin = terra::ymin(template_data),
      ymax = terra::ymax(template_data),
      res = terra::res(template_data),
      crs = terra::crs(template_data)
    ),
    fact = fact
  )

  # prepare output paths
  aoh_path <- x$path
  idx <- which(!is.na(aoh_path))
  x$path <- NA_character_
  x$path[idx] <- file.path(
    output_dir, paste0("FRC_", x$id_no[idx], "_", x$seasonal[idx], ".tif")
  )

  # skip indices for already processed files
  if (!isTRUE(force)) {
    idx2 <- file.exists(x$path[idx])
    if (verbose) {
      if (any(idx2)) {
        message(
          paste(
            "    skipping",
            sum(idx2),
            "species distributions already processed"
          )
        )
      }
    }
  }

  # create custom progress bar
  if (isTRUE(verbose)) {
    pb <- cli::cli_progress_bar(
      clear = FALSE,
      total = sum(!idx2),
      format = paste0(
        "{.alert-info processing } ",
        "{cli::pb_bar} [{cli::pb_percent} | {cli::pb_eta_str}]"
      ),
      format_done = paste0(
        "{.alert-success processing [{cli::pb_elapsed}]}"
      )
    )
    cli::cli_progress_update(id = pb, set = 0)
  }

  # main processing
  result <- lapply(idx[!idx2], function(i) {
    ## processing fractional coverage data
    withr::with_envvar(
      c(
        "GDAL_CACHEMAX" = as.integer(cache_limit),
        "GDAL_DISABLE_READDIR_ON_OPEN" = "TRUE"
      ),
      process_spp_frc_on_local(
        aoh_path = aoh_path[i],
        template_data = template_data,
        path = x$path[i],
        engine = engine,
        n_threads = n_threads,
        cache_limit = cache_limit
      )
    )
    ## update progress bar if needed
    if (isTRUE(verbose)) {
      cli::cli_progress_update(id = pb)
    }
  })

  # prepare results
  exts <- vapply(
    x$path[idx], FUN.VALUE = numeric(4), USE.NAMES = FALSE, function(p) {
      unlist(
        as.list(terra::ext(terra::rast(p))),
        recursive = FALSE, use.names = FALSE
      )
    }
  )
  x$xmin[idx] <- exts[1, ]
  x$xmax[idx] <- exts[2, ]
  x$ymin[idx] <- exts[3, ]
  x$ymax[idx] <- exts[4, ]

  # return result
  x
}
