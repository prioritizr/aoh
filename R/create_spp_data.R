#' @include internal.R collate_spp_extent_data.R
NULL

#' Create species data
#'
#' Internal function used to process species' Area of Habitat data
#' fractional coverage data.
#'
#' @inheritParams create_spp_aoh_data
#' @inheritParams create_spp_frc_data
#'
#' @inherit create_spp_aoh_data return
#'
#' @noRd
create_spp_data <- function(x,
                            res,
                            output_dir,
                            elevation_data = NULL,
                            habitat_data = NULL,
                            crosswalk_data = NULL,
                            cache_dir = tempdir(),
                            habitat_version = "latest",
                            elevation_version = "latest",
                            force = FALSE,
                            n_threads = 1,
                            cache_limit = 1000,
                            engine = "terra",
                            verbose = TRUE) {

  # initialization
  ## display message
  if (verbose) {
    cli::cli_progress_step("initializing")
  }
  ## initial validation
  assertthat::assert_that(
    inherits(x, "sf"),
    nrow(x) > 0,
    assertthat::has_name(x, "id_no"),
    is.numeric(x$id_no),
    assertthat::noNA(x$id_no),
    assertthat::has_name(x, "binomial"),
    is.character(x$binomial),
    assertthat::has_name(x, "seasonal"),
    is.numeric(x$seasonal),
    assertthat::noNA(x$seasonal),
    assertthat::has_name(x, "full_habitat_code"),
    is.character(x$full_habitat_code),
    assertthat::has_name(x, "elevation_lower"),
    is.numeric(x$elevation_lower),
    assertthat::has_name(x, "elevation_upper"),
    is.numeric(x$elevation_upper),
    assertthat::has_name(x, "geometry"),
    inherits(x$geometry, "sfc")
  )
  assertthat::assert_that(
    identical(anyDuplicated(paste0(x$id_no, x$seasonal)), 0L),
    msg = paste0(
      "argument to \"x\" must not contain multiple entries with",
      "the same combination of values for the \"id_no\" and \"seasonal\"",
      "columns"
    )
  )
  ## normalize directories
  output_dir <- normalize_path(output_dir, mustWork = FALSE)
  cache_dir <- normalize_path(cache_dir, mustWork = FALSE)
  ## additional validation
  assertthat::assert_that(
    assertthat::is.writeable(output_dir),
    assertthat::is.writeable(cache_dir),
    assertthat::is.count(n_threads),
    assertthat::noNA(n_threads),
    assertthat::is.flag(force),
    assertthat::noNA(force),
    assertthat::is.count(cache_limit),
    assertthat::noNA(cache_limit),
    assertthat::is.string(engine),
    assertthat::noNA(engine),
    engine %in% c("terra", "gdal", "grass"),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )
  if (!is.null(res)) {
    assertthat::assert_that(
      assertthat::is.count(res),
      assertthat::noNA(res)
    )
  }
  assertthat::assert_that(
    cache_limit <= 9999,
    msg = "argument to \"cache_limit\" cannot exceed 9999"
  )
  assertthat::assert_that(
    assertthat::has_name(x, "id_no") ||
      assertthat::has_name(x, "SISID"),
    msg = paste0(
      "argument to \"x\" does not have a column named \"id_no\" or \"SISID\""
    )
  )
  if (identical(engine, "gdal")) {
  assertthat::assert_that(
    requireNamespace("gdalUtilities", quietly = TRUE),
    msg = paste(
      "the \"gdalUtilities\" package needs to be installed, use",
      "install.packages(\"gdalUtilities\")"
    )
  )
  }
  if (identical(engine, "grass")) {
    assertthat::assert_that(
      is_grass_available(),
      msg = "can't use GRASS for processing because it's not available."
    )
  }
  if (identical(engine, "grass")) {
    assertthat::assert_that(
      is_gdal_calc_available(),
      msg = paste(
        "can't use GRASS for processing because it requires GDAL,",
        "which is not available."
      )
    )
  }
  ## elevation data
  if (is.null(elevation_data)) {
    ### display message
    if (verbose) {
      cli::cli_progress_step("importing global elevation data")
    }
    ### processing
    elevation_data <- get_global_elevation_data(
      dir = cache_dir, version = elevation_version,
      force = force, verbose = verbose
    )
  }
  assertthat::assert_that(
    inherits(elevation_data, "SpatRaster"),
    terra::nlyr(elevation_data) == 1,
    all(terra::hasValues(elevation_data))
  )
  ## habitat_data
  if (is.null(habitat_data)) {
    ### display message
    if (verbose) {
      cli::cli_progress_step("importing global habitat data")
    }
    ### processing
    habitat_data <- get_lumb_cgls_habitat_data(
      dir = cache_dir, version = habitat_version,
      force = force, verbose = verbose
    )
    ### get crosswalk data if needed
    if (is.null(crosswalk_data)) {
      crosswalk_data <- crosswalk_lumb_cgls_data
    }
  } else {
    assertthat::assert_that(
      inherits(crosswalk_data, "data.frame"),
      msg = paste(
        "argument to \"crosswalk_data\" must be supplied when not using",
        "default habitat data"
      )
    )
  }
  assertthat::assert_that(
    inherits(habitat_data, "SpatRaster"),
    terra::nlyr(habitat_data) == 1,
    all(terra::hasValues(habitat_data))
  )
  ## verify rasters match
  assertthat::assert_that(
    terra::compareGeom(
      elevation_data, habitat_data,
      res = TRUE, stopOnError = FALSE
    ),
    msg = paste(
      "arguments to \"elevation_data\" and \"habitat_data\" don't have the",
      "same spatial properties (e.g., coordinate system, extent, resolution)"
    )
  )
  ## verify species info data has same crs as raster data
  assertthat::assert_that(
    sf::st_crs(x) == terra_st_crs(elevation_data),
    msg = paste(
      "arguments to \"x\" and \"elevation_data\" don't have the",
      "same spatial coordinate reference system"
    )
  )
  ## crosswalk_data
  assertthat::assert_that(
    inherits(crosswalk_data, "data.frame"),
    assertthat::has_name(crosswalk_data, c("code", "value")),
    assertthat::noNA(crosswalk_data$code),
    assertthat::noNA(crosswalk_data$value),
    is.character(crosswalk_data$code),
    is.numeric(crosswalk_data$value)
  )
  assertthat::assert_that(
    all(crosswalk_data$code %in% iucn_habitat_data$code),
    msg = paste(
      "argument to \"crosswalk_data\" contains the following codes that",
      "are not valid IUCN habitat codes:",
      paste(
        paste0(
          "\"",
          setdiff(crosswalk_data$code, iucn_habitat_data$code),
          "\""
        ),
        collapse = ","
      )
    )
  )

  # calculations for fractional coverage
  if (!is.null(res)) {
    ## compute aggregation factor
    assertthat::assert_that(
      terra::xres(habitat_data) == terra::yres(habitat_data),
      msg = "argument to \"habitat_data\" must have square cells"
    )
    fact <- res / terra::xres(habitat_data)
    assertthat::assert_that(
      assertthat::is.count(fact),
      assertthat::noNA(fact),
      msg = paste(
        "argument to \"res\" does not correspond to a valid aggregation factor",
        "for the arguments to \"habitat_data\" and \"elevation_data\""
      )
    )
    ## create spatial grid representing aggregated spatial properties
    frc_template <- terra::aggregate(
      x = terra::rast(
        xmin = terra::xmin(habitat_data),
        xmax = terra::xmax(habitat_data),
        ymin = terra::ymin(habitat_data),
        ymax = terra::ymax(habitat_data),
        res = terra::res(habitat_data),
        crs = terra::crs(habitat_data)
      ),
      fact = fact
    )
  } else {
    frc_template <- NULL
  }

  ## additional data validation
  ### check that habitat_data has all codes in spp_habitat_data
  habitat_codes <- strsplit(x$full_habitat_code, split = "|", fixed = TRUE)
  habitat_codes <- unique(unlist(habitat_codes, use.names = FALSE))
  missing_codes <- !habitat_codes %in% crosswalk_data$code
  if (any(missing_codes)) {
    cli::cli_alert_warning(
      paste(
        "argument to \"crosswalk_data\" is missing the following",
        sum(missing_codes),
        "habitat classification codes:",
        paste(
          paste0(
            "\"",
            stringi::stri_sort(habitat_codes[missing_codes], numeric = TRUE),
            "\""
          ),
          collapse = ", "
        )
      )
    )
  }
  assertthat::assert_that(
    any(habitat_codes %in% crosswalk_data$code),
    msg = paste(
      "argument to \"crosswalk_data\" does not contain",
      "IUCN habitat classification codes that are suitable for any species"
    )
  )
  assertthat::assert_that(
    length(habitat_codes) >= 1,
    msg = paste(
      "none of the species have any suitable habitat classes -",
      "perhaps the argument to \"spp_habitat_data\" is missing",
      "some information or the argument to \"omit_habitat_codes\"",
      "contains codes that should not be omitted?"
    )
  )
  ## remove missing codes
  habitat_codes <- habitat_codes[!missing_codes]

  # prepare data
  ## create aoh_id column
  x$aoh_id <- paste0(x$id_no, "_", x$seasonal)
  ## split habitat codes by | to obtain list
  x$full_habitat_code <- strsplit(x$full_habitat_code, "|", fixed = TRUE)
  ## subset habitat codes to those that are available
  x$habitat_code <- lapply(x$full_habitat_code, function(x) {
    base::intersect(x, habitat_codes)
  })
  ## collate spatial extent information
  x <- collate_spp_extent_data(x, habitat_data)
  ## add column with output file paths
  if (is.null(res)) {
    x$path <- file.path(output_dir, paste0(x$aoh_id, ".tif"))
  } else {
    x$path <- file.path(
      output_dir, paste0("FRC_", x$id_no, "_", x$seasonal, ".tif")
    )
  }
  ## normalize file paths
  x$path <- vapply(
    x$path, normalize_path, character(1), mustWork = FALSE, USE.NAMES = FALSE
  )
  ## set paths to NA if the species won't be processed
  ## species won't be processed if:
  ##   they don't overlap with the template,
  ##   they have no habitat layers at all,
  ##   none of their habitat layers are available
  x$path[is.na(x$xmin)] <- NA_character_
  x$path[vapply(x$habitat_code, length, integer(1)) == 0] <- NA_character_
  ## verify that habitat data encompasses that species range data
  not_contained <- sf::st_contains(
    sf::st_as_sfc(terra_st_bbox(habitat_data)),
    sf::st_as_sfc(sf::st_bbox(x)),
    sparse = FALSE
  )[[1]]
  if (!isTRUE(not_contained)) {
    cli::cli_alert_warning(
      paste(
        "arguments to \"habitat_data\" and \"elevation_data\" do not fully",
        "contain the ranges for all the species"
      )
    )
  }

  # main processing
  ## display message
  if (verbose) {
    cli::cli_progress_step("generating Area of Habitat data")
  }
  ## processing
  ## use local host for processing
  result <- process_spp_data_on_local(
    x = x,
    habitat_data = habitat_data,
    elevation_data = elevation_data,
    crosswalk_data = crosswalk_data,
    cache_dir = cache_dir,
    engine = engine,
    force = force,
    frc_template_data = frc_template,
    n_threads = n_threads,
    cache_limit = cache_limit,
    verbose = verbose
  )

  # prepare table with metadata
  ## display message
  if (verbose) {
    cli::cli_progress_step("post-processing results")
  }
  ## processing
  x <- dplyr::select(
    x, "id_no", "binomial", "category",
    "migratory", "seasonal",
    "full_habitat_code", "habitat_code",
    "elevation_lower", "elevation_upper",
    "xmin", "xmax", "ymin", "ymax",
    "path"
  )
  ## convert list-column to "|" delimited character-column
  x$habitat_code <- vapply(
    x$habitat_code, paste, character(1), collapse = "|"
  )
  x$full_habitat_code <- vapply(
    x$full_habitat_code, paste, character(1), collapse = "|"
  )
  ## overwrite spatial extent data if fractional coverage computed
  if (!is.null(res)) {
    idx <- !is.na(x$path)
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
  }
  ## overwrite spatial extent data with NAs for distributions not processed
  x$xmin[is.na(x$path)] <- NA_real_
  x$xmax[is.na(x$path)] <- NA_real_
  x$ymin[is.na(x$path)] <- NA_real_
  x$ymax[is.na(x$path)] <- NA_real_

  # return result
  ## display message
  if (verbose) {
    cli::cli_progress_done()
    cli::cli_alert_success("finished")
  }
  ## return
  x
}
