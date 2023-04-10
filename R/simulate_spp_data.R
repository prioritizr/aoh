#' @include internal.R simulate_random_field_data.R convert_seasonal.R
NULL

#' Simulate species data
#'
#' Simulate species data for creating Area of Habitat data
#' (Brooks *et al.* 2019). Specifically, data are simulated to define
#' species geographic ranges, summary information, and habitat preferences.
#'
#' @inheritParams create_spp_aoh_data
#' @inheritParams create_spp_info_data
#'
#' @param n `integer` Number of species to simulate.
#'
#' @param boundary_data [sf::st_sf()] Spatial object delineating the
#'   spatial extent and boundary for simulating species ranges.
#'
#' @param rf_scale_min `numeric` Minimum scaling parameter used
#'   to control the smallest possible level of spatial auto-correlation for
#'   simulated species ranges.
#'   Defaults to 0.5.
#'
#' @param rf_scale_max `numeric` Minimum scaling parameter used
#'   to control the largest possible level of spatial auto-correlation for
#'   simulated species ranges.
#'   Defaults to `0.7`.
#'
#' @return
#' A `list` object containing simulated data that are formatted following
#' conventions used by the
#' [International Union for Conservation of Nature (IUCN) Red List of
#' Threatened Species](https://www.iucnredlist.org/).
#' It contains the following elements:
#' \describe{
#' \item{spp_range_data}{A [sf::st_sf()] object containing the species'
#'   geographic range data.}
#' \item{spp_summary_data}{A [tibble::tibble()] object containing summary
#'  information about the species (including elevational limit information.}
#' \item{spp_habitat_data}{A [tibble::tibble()] object containing habitat
#'  preferences for the species.}
#' }
#'
#' @seealso
#' See [create_spp_aoh_data()] for creating Area of Habitat maps using
#' data for real or simulated species.
#'
#' @examples
#' # please ensure that the fields and smoothr packages are installed
#' # to run these examples
#'
#' @examplesIf require(fields) && require(smoothr)
#' \dontrun{
#' # define persistent storage location
#' download_dir <- rappdirs::user_data_dir("aoh")
#'
#' # create download directory if needed
#' if (!file.exists(download_dir)) {
#'   dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
#' }
#'
#' # specify file path for boundary data
#' boundary_path <- system.file("shape/nc.shp", package = "sf")
#'
#' # import boundary data to simulate species data
#' boundary_data <- sf::st_union(sf::read_sf(boundary_path))
#'
#' # set random number generator seeds for consistency
#' set.seed(500)
#'
#' # simulate data for 5 species
#' x <- simulate_spp_data(
#'   n = 5, boundary_data = boundary_data, cache_dir = download_dir
#' )
#'
#' # preview species range data
#' print(x$spp_range_data)
#'
#' # preview species habitat preference data
#' print(x$spp_habitat_data)
#'
#' # preview species summary data
#' print(x$spp_summary_data)
#' }
#' @references
#' Brooks TM, Pimm SL, Akçakaya HR, Buchanan GM, Butchart SHM, Foden W,
#' Hilton-Taylor C, Hoffmann M, Jenkins CN, Joppa L, Li BV, Menon V,
#' Ocampo-Peñuela N, Rondinini C (2019) Measuring terrestrial Area of Habitat
#' (AOH) and its utility for the IUCN Red List. *Trends in Ecology & Evolution*,
#' 34, 977--986. \doi{10.1016/j.tree.2019.06.009}
#'
#' @export
simulate_spp_data <- function(n,
                              boundary_data,
                              habitat_data = NULL,
                              elevation_data = NULL,
                              crosswalk_data = NULL,
                              rf_scale_min = 0.5,
                              rf_scale_max = 0.7,
                              cache_dir = tempdir(),
                              habitat_version = "latest",
                              force = FALSE,
                              omit_habitat_codes = iucn_habitat_codes_marine(),
                              verbose = TRUE) {
  # assert dependencies available
  assertthat::assert_that(
    requireNamespace("smoothr", quietly = TRUE),
    msg = "the \"smoothr\" package must be installed to simulate data"
  )
  assertthat::assert_that(
    requireNamespace("fields", quietly = TRUE),
    msg = "the \"fields\" package must be installed to simulate data"
  )

  # assert that arguments are valid
  ## initial validation
  assertthat::assert_that(
    assertthat::is.count(n),
    assertthat::noNA(n),
    is.character(omit_habitat_codes)
  )
  ## elevation data
  if (is.null(elevation_data)) {
    ### display message
    if (verbose) {
      cli::cli_progress_step("importing global elevation data")
    }
    ### processing
    elevation_data <- get_global_elevation_data(
      dir = cache_dir, force = force, verbose = verbose
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
    habitat_data <- get_jung_lvl2_habitat_data(
      dir = cache_dir,
      version = habitat_version,
      force = force,
      verbose = verbose
    )
    ### get crosswalk data if needed
    if (is.null(crosswalk_data)) {
      crosswalk_data <- crosswalk_jung_lvl2_data
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
      elevation_data, habitat_data, res = TRUE, stopOnError = FALSE
    ),
    msg = paste(
      "arguments to \"elevation_data\" and \"habitat_data\" don't have the",
      "same spatial properties (e.g., coordinate system, extent, resolution)"
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

  # simulate species ids
  spp_id_no <- sort(sample.int(1e+4, n))

  # determine resolution for simulations
  boundary_data_proj <- sf::st_transform(
    boundary_data, sf::st_crs("ESRI:54017")
  )
  bb <- sf_terra_ext(boundary_data_proj)

  # crop rasters to extent
  habitat_data <- terra::crop(
    x = habitat_data,
    y = bb,
    snap = "out"
  )
  elevation_data <- terra::crop(
    x = elevation_data,
    y = bb,
    snap = "out"
  )

  # aggregate rasters to reduce run time
  bb <- as.list(bb)
  res <- min(bb$xmax - bb$xmin, bb$ymax - bb$ymin) / 250
  res <- max(1000, round(res / 1000) * 1000)
  fact <- floor(res / terra::res(habitat_data))
  fact <- pmax(fact, 1)
  habitat_data <- terra::aggregate(
    habitat_data, fact = fact, fun = "modal"
  )
  elevation_data <- terra::aggregate(
    elevation_data, fact = fact, fun = "median"
  )

  # simulate species range maps
  sim_eoo <- lapply(
    stats::runif(n, min = rf_scale_min, max = rf_scale_max),
    function(x) {
      simulate_random_field_data(
        x = elevation_data,
        n = 1,
        scale = x,
        transform = function(x) round(stats::plogis(x))
      )
    }
  )

  sim_range_data <- lapply(seq_len(n), function(i) {
    x <- sim_eoo[[i]]
    x[x < 0.5] <- NA_real_
    x <- sf::st_as_sf(terra::as.polygons(x))
    x <- smoothr::fill_holes(x, threshold = Inf)
    sf::st_as_sf(
      geometry = sf::st_geometry(x),
      tibble::tibble(id_no = spp_id_no[i])
    )
  })

  # split range data into discrete distributions
  sim_range_data <- lapply(sim_range_data, function(x) {
    # split into separate polygons
    x <- suppressWarnings(sf::st_cast(x, "POLYGON"))
    # extract only valid polygons
    x <- sf::st_make_valid(x)
    x <- x[sf::st_is_valid(x), , drop = FALSE]
    if (nrow(x) == 0) {
      stop("failed to simulate data") # nocov
    }
    rownames(x) <- NULL
    # remove overlapping areas
    x <- x[order(sf::st_area(x)), , drop = FALSE]
    x <- sf::st_difference(x)
    x <- x[!sf::st_is_empty(x), , drop = FALSE]
    # determine if species has seasonal distributions or not
    if (isTRUE(stats::runif(1) > 0.7) && (nrow(x) >= 4)) {
      # migratory
      migrant <- TRUE
      s <- seq(ifelse(stats::runif(1) > 0.5, 1, 2), 4)
      dist_idx <- sample(order(sf::st_area(x), decreasing = TRUE)[seq_along(s)])
      d1 <- x[dist_idx, , drop = FALSE]
      # add in seasonal metadata
      ## simulate some migratory species with only
      ## (i) breeding/non-breeding/passage, and others with
      ## (ii) resident/breeding/non-breeding/passage
      d1$seasonal <- s
      d1$presence <- 1
      d1$origin <- 1
    } else {
      # not migratory
      migrant <- FALSE
      dist_idx <- sample.int(nrow(x), 1, prob = as.numeric(sf::st_area(x)))
      d1 <- x[dist_idx, , drop = FALSE]
      # add in seasonal metadata
      d1$seasonal <- 1
      d1$presence <- 1
      d1$origin <- 1
    }
    # simulate additional data
    extra_idx <- setdiff(seq_len(nrow(x)), dist_idx)
    if (length(extra_idx) > 0) {
      ## simulate additional distributions
      extra_idx <- sample(x = extra_idx, size = min(length(extra_idx), 3))
      d2 <- x[extra_idx, , drop = FALSE]
      d2$presence <- sample(
        x = c(1, seq(3, 6)), size = length(extra_idx), replace = TRUE
      )
      d2$origin <- sample(
        x = seq(2, 6), size = length(extra_idx), replace = TRUE
      )
      if (migrant) {
        d2$seasonal <- sample(
          x = seq(2, 5), size = length(extra_idx), replace = TRUE
        )
      } else {
        d2$seasonal <- 1
      }
      out <- dplyr::bind_rows(d1, d2)
    } else {
      out <- d1
    }
    # return result
    rownames(out) <- NULL
    out
  })

  # smooth the distributions
  sim_range_data <- lapply(sim_range_data, function(x) {
    # smooth data
    x <- smoothr::smooth(x, method = "spline")
    # repair any geometry issues
    x <- sf::st_set_precision(x, 1500)
    x <- sf::st_make_valid(x)
    x <- dplyr::filter(x, !sf::st_is_empty(x))
    x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
    # run intersection
    x <- suppressWarnings(sf::st_intersection(x, boundary_data_proj))
    # fix geometry issues
    x <- sf::st_set_precision(x, 1500)
    x <- sf::st_make_valid(x)
    x <- dplyr::filter(x, !sf::st_is_empty(x))
    x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
    # return result
    x
  })

  # combine data
  sim_range_data <- do.call(dplyr::bind_rows, sim_range_data)
  rownames(sim_range_data) <- NULL

  # add metadata
  sim_range_data <- dplyr::mutate(
    sim_range_data,
    binomial = paste("Simulus", "spp.", .data$id_no),
    compiler = "Simulation",
    yrcompiled = NA_real_,
    citation = NA_character_,
    subspecies = NA_character_,
    subpop = NA_character_,
    source = NA_character_,
    island = NA_character_,
    tax_comm = NA_character_,
    dist_comm = NA_character_,
    generalisd = NA_integer_,
    legend = NA_character_,
    kingdom = NA_character_,
    phylum = NA_character_,
    class = NA_character_,
    `order_` = NA_character_,
    family = NA_character_,
    genus = "Simulus",
    category = NA_character_,
    marine = "false",
    terrestial = "true",
    freshwater = "false"
  )
  if ("id" %in% names(sim_range_data)) {
    sim_range_data <- dplyr::select(sim_range_data, -"id")
  }

  # assign IUCN categories
  spp_category <- sample(
    x = c("LC", "NT", "VU", "EN"),
    size = length(spp_id_no),
    replace = TRUE
  )
  sim_range_data$category <- rep(
    x = spp_category,
    times = rle(sim_range_data$binomial)$lengths
  )

  # simulate habitat preference data
  sim_habitat_data <- simulate_habitat_data(
    x = sim_range_data,
    habitat_data = habitat_data,
    crosswalk_data = crosswalk_data,
    omit_habitat_codes = omit_habitat_codes
  )

  # simulate summary data
  sim_summary_data <- simulate_summary_data(
    x = sim_range_data,
    elevation_data = elevation_data
  )

  # return result
  list(
    spp_range_data = sf::st_transform(sim_range_data, sf::st_crs(4326)),
    spp_habitat_data = sim_habitat_data,
    spp_summary_data = sim_summary_data
  )
}

simulate_summary_data <- function(x, elevation_data) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, "sf"),
    assertthat::has_name(x, "id_no"),
    inherits(elevation_data, "SpatRaster")
  )

  # preliminary processing
  ## unique combinations id_no and seasonal
  x_distinct <- sf::st_drop_geometry(x)
  x_distinct <- dplyr::distinct(x_distinct, .data$id_no, .keep_all = TRUE)

  # main processing
  # create habitat data
  result <- plyr::ldply(seq_len(nrow(x_distinct)), function(i) {
    ## initialization
    curr_id_no <- x_distinct$id_no[[i]]

    ## subset data
    idx <- which(
      (x$id_no == curr_id_no) &
      (x$seasonal %in% seq_len(4)) &
      (x$presence == 1) &
      (x$origin == 1)
    )
    xi <- x[idx, , drop = FALSE]
    xi <- sf::st_as_sf(sf::st_union(xi))

    ## find elevation ranges
    elev_range <- stats::quantile(
      terra::values(terra::mask(x = elevation_data, mask = terra::vect(xi))),
      probs = c(0.05, 0.8),
      na.rm = TRUE,
      names = FALSE
    )

    ## return result
    tibble::tibble(
      id_no = curr_id_no,
      taxonid = curr_id_no,
      scientific_name = x_distinct$binomial[[i]],
      kingdom = NA_character_,
      phylum = NA_character_,
      class = NA_character_,
      order = NA_character_,
      family = NA_character_,
      genus = x_distinct$genus[[i]],
      main_common_name = NA_character_,
      authority = NA_character_,
      published_year = NA_character_,
      assessment_date = NA_character_,
      category = x_distinct$category[[i]],
      criteria = NA_character_,
      population_trend = NA_character_,
      marine_system = x_distinct$marine[[i]],
      freshwater_system = x_distinct$freshwater[[i]],
      terrestrial_system = x_distinct$terrestial[[i]],
      assessor = NA_character_,
      reviewer = NA_character_,
      aoo_km2 = NA_character_,
      eoo_km2 = NA_character_,
      elevation_upper = ceiling(elev_range[[2]]),
      elevation_lower = floor(elev_range[[1]]),
      depth_upper = NA_real_,
      depth_lower = NA_real_,
      errata_flag = NA_character_,
      errata_reason = NA_character_,
      amended_flag = NA_character_,
      amended_reason = NA_character_
    )
  })

  # return result
  tibble::as_tibble(result)
}

simulate_habitat_data <- function(x, habitat_data, crosswalk_data,
                                  omit_habitat_codes) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, "sf"),
    assertthat::has_name(x, "id_no"),
    inherits(habitat_data, "SpatRaster"),
    inherits(crosswalk_data, "data.frame"),
    is.character(omit_habitat_codes),
    assertthat::noNA(omit_habitat_codes)
  )

  # preliminary processing
  ## unique combinations id_no and seasonal
  x_distinct <- sf::st_drop_geometry(x)
  x_distinct <- dplyr::distinct(
    dplyr::select(x_distinct, "id_no", "seasonal")
  )
  x_distinct <- x_distinct[x_distinct$seasonal <= 4, , drop = FALSE]
  x_distinct$seasonal_name <- convert_to_seasonal_name(x_distinct$seasonal)

  # create habitat data
  result <- plyr::ldply(seq_len(nrow(x_distinct)), function(i) {
    ## initialization
    curr_id_no <- x_distinct$id_no[[i]]
    curr_seasonal <- x_distinct$seasonal[[i]]
    curr_seasonal_name <- x_distinct$seasonal_name[[i]]

    ## subset data
    idx <- which(
      (x$id_no == curr_id_no) &
      (x$seasonal == curr_seasonal) &
      (x$presence == 1) &
      (x$origin == 1)
    )
    xi <- x[idx, , drop = FALSE]

    ## calculate frequency of different habitat classes inside species' range
    xi_habitat_freq <- tibble::as_tibble(
      terra::freq(
        terra::mask(
          x = habitat_data,
          mask = terra::vect(xi)
        )
      )
    )

    ## add in IUCN codes
    xi_habitat_freq$iucn_code <- crosswalk_data$code[
      match(xi_habitat_freq$value, crosswalk_data$value)
    ]

    ## extract suitable habitats
    xi_habitat_freq <- xi_habitat_freq[
      !is.na(xi_habitat_freq$value), , drop = FALSE
    ]
    xi_habitat_freq <- xi_habitat_freq[
      !is.na(xi_habitat_freq$iucn_code), , drop = FALSE
    ]
    xi_habitat_freq <- xi_habitat_freq[
      !xi_habitat_freq$iucn_code %in% omit_habitat_codes, , drop = FALSE
    ]

    ## sort by frequency
    xi_habitat_freq <- dplyr::arrange(
      xi_habitat_freq,  dplyr::desc(.data$count)
    )

    ## sample suitable codes
    ## ensure a dominant habitat class selected
    n_dom <- min(nrow(xi_habitat_freq), 5)
    dom_suitable_codes <- sample(
      xi_habitat_freq$iucn_code[n_dom],
      size = 1
    )
    ## add in some non-dominant classes
    nondom_suitable_codes <- sample(
      x = xi_habitat_freq$iucn_code,
      size = min(nrow(xi_habitat_freq), 2),
      replace = FALSE
    )
    ## ensure classes are unique
    suitable_codes <- unique(c(dom_suitable_codes, nondom_suitable_codes))

    ## sample marginal codes
    if (
      (stats::runif(1) > 0.8) &&
      (length(suitable_codes) < nrow(xi_habitat_freq))
    ) {
      potential_codes <- setdiff(suitable_codes, xi_habitat_freq$iucn_code)
      marginal_codes <- sample(
        x = potential_codes,
        size = min(length(potential_codes), 2)
      )
    } else {
      marginal_codes <- c()
    }

    ## extract habitat names
    all_codes <- c(suitable_codes, marginal_codes)
    habitat_names <- iucn_habitat_data$name[
      match(all_codes, iucn_habitat_data$code)
    ]

    ## return data for species' seasonal distribution
    tibble::tibble(
      id_no = curr_id_no,
      code = all_codes,
      habitat = habitat_names,
      suitability = c(
        rep("Suitable", length(suitable_codes)),
        rep("Marginal", length(marginal_codes))
      ),
      season = curr_seasonal_name,
      majorimportance = NA_character_
    )
  })

  # return result
  tibble::as_tibble(result)
}
