#' @include internal.R
NULL

#' Calculate fractional coverage data
#'
#' Calculate fractional coverage of species' Area of Habitat.
#'
#' @inheritParams plot_spp_aoh_data
#' @inheritParams create_spp_aoh_data
#'
#' @param res `numeric` Resolution for computing fractional coverage.
#'   Note that the argument to `res` must be a factor of the
#'   the resolution of the underlying Area of Habitat data.
#'   For example, a value of 5000 would be a valid argument
#'   if the underlying data had a resolution of 100 m.
#'
#' @param template_data [terra::rast()] Raster data to use as a template
#'   for computing fractional coverage.
#'   Note that the argument should have the same spatial properties
#'   as the elevation and habitat data used to generate the
#'   Area of Habitat data.
#'   Defaults to `NULL` such that template data are automatically imported
#'   as the default global habitat dataset (using
#'   [get_lumbierres_habitat_data()]).
#'
#' @param engine `character` Value indicating the name of the software
#'   to use for data processing.
#'   Available options include `"terra"` or `"gdal"`.
#'   Defaults to `"terra"`.
#'
#' @param output_dir `character` `character` Folder path to save raster files
#'   (GeoTIFF format) containing the aggregated Area of Habitat data.
#'
#' @param ... Arguments passed to [get_lumbierres_habitat_data()].
#'
#' @details
#' This function works by
#' (i) creating a template grid based on the specified resolution,
#' (ii) aggregating the Area of Habitat data to the match the
#' spatial origin and resolution of the template grid, and
#' (iii) dividing the aggregated values based on the aggregation
#' factor to express values as fractional coverage.
#'
#' @return An updated version of the argument to `x` with updated values
#' for the `path`, `xmin`, `xmax`, `ymin`, and `ymax` columns.
#' The raster (GeoTIFF) files specified in the `path` column now
#' contain factional coverage values.
#' Here, a value 0 corresponds to 0% coverage, 0.5 to 50% coverage, 1 to 100%
#' coverage.
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
#' # compute fractional coverage across a 5 x 5 km spatial grid
#' spp_aoh_frac_data <- calc_spp_frac_data(
#'   x = spp_aoh_data,
#'   output_dir = output_dir,
#'   res = 5000,
#'   cache_dir = cache_dir
#' )
#' }
#'
#' @examplesIf interactive()
#' \dontrun{
#' # preview data
#' print(spp_aoh_frac_data)
#' }
#'
#' @examples
#' \dontrun{
#' # plot the data to visualize the range maps and aggregated AOH data
#' plot_spp_frac_data(spp_aoh_frac_data)
#'}
#' @export
calc_spp_frac_data <- function(x,
                               res,
                               output_dir,
                               template_data = NULL,
                               cache_dir = tempdir(),
                               force = FALSE,
                               n_threads = 1,
                               engine = "terra",
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
    msg = "argument to \"x\" should be the output from create_aoh_data()"
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
    assertthat::is.flag(force),
    assertthat::noNA(force),
    assertthat::is.count(n_threads),
    assertthat::noNA(n_threads),
    assertthat::is.string(engine),
    assertthat::noNA(engine),
    engine %in% c("terra", "gdal")
  )
  if (isTRUE(identical(engine, "gdal"))) {
    assertthat::assert_that(
      is_gdal_available(),
      msg = "can't use GDAL for processing because it's not available."
    )
  }
  ## template data
  if (is.null(template_data)) {
    ### display message
    if (verbose) {
      cli::cli_progress_step("importing global habitat data")
    }
    ### processing
    template_data <- get_lumbierres_habitat_data(
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
    msg = "argument to \"template_data\" must have square cells"
  )
  fact <- res / terra::xres(template_data)
  assertthat::assert_that(
    assertthat::is.count(fact),
    assertthat::noNA(fact),
    msg = paste(
      "argument to \"res\" does not correspond to a valid aggregation factor",
      "for the argument to \"template_data\""
    )
  )

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
      total = length(idx),
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

  # reset terra options on exit
  on.exit(terra::terraOptions(progress = 3, tempdir = tempdir()))

  # main processing
  result <- lapply(idx, function(i) {

    # configure terra for processing
    tmp_dir <- gsub("\\", "/", tempfile(), fixed = TRUE)
    dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
    terra::terraOptions(progress = 0, tempdir = tmp_dir)

    # processing
    if (!isTRUE(force) & file.exists(x$path[i])) {
      ## if just skipping the file then...
      ## just import the processed data
      r <- terra::rast(x$path[i])
    } else {
      ## if processing the file then...
      ## import raw raster
      r <- terra::rast(aoh_path[i])

      ## compute extent for output
      curr_grid <- terra::crop(
        x = template_data, y = terra::ext(r), snap = "out"
      )

      # extend raster using using specified engine
      if (identical(engine, "terra")) {
        r <- terra::extend(
          x = r,
          y = curr_grid,
          datatype = "INT1U",
          gdal = c("COMPRESS=LZW", "BIGTIFF=YES")
        )
      } else {
        r <- terra_gdal_crop(
          x = r,
          ext = terra::ext(curr_grid),
          datatype = "INT1U",
          bigtiff = TRUE,
          compress = "LZW",
          n_threads = n_threads,
          verbose = FALSE
        )
      }

      # aggregate raster and compute fractional coverage
      ## note that we don't use GDAL for this because
      ## (i) gdal_translate doesn't provide a "sum" method, and
      ## (ii) gdal_warp sets grid cells values as NA if they are predominantly
      ## covered by missing values
      r <- terra::app(
        x = terra::aggregate(
          x = r,
          fact = fact,
          fun = "sum",
          na.rm = TRUE,
          wopt = list(
            datatype = "INT2U",
            gdal = c("COMPRESS=LZW", "BIGTIFF=YES")
          )
        ),
        fun = `/`,
        e2 = fact ^ 2,
        filename = x$path[i],
        overwrite = TRUE,
        wopt = list(datatype = "FLT4S", gdal = c("COMPRESS=LZW", "BIGTIFF=YES"))
      )
    }

    # prepare restores
    out <- list(
      xmin = terra::xmin(r),
      xmax = terra::xmax(r),
      ymin = terra::ymin(r),
      ymax = terra::ymax(r)
    )

    # clean up
    rm(r)
    unlink(tmp_dir, force = TRUE, recursive = TRUE)

    # update progress bar if needed
    if (isTRUE(verbose)) {
      cli::cli_progress_update(id = pb)
    }

    # return result
    out
  })

  # prepare results
  x$xmin[idx] <- vapply(result, `[[`, numeric(1), "xmin")
  x$xmax[idx] <- vapply(result, `[[`, numeric(1), "xmax")
  x$ymin[idx] <- vapply(result, `[[`, numeric(1), "ymin")
  x$ymax[idx] <- vapply(result, `[[`, numeric(1), "ymax")

  # return result
  x
}
