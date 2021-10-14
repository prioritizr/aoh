#' @include internal.R misc_randomfields.R convert_seasonal.R
NULL

#' Simulate species data
#'
#' Simulate species data for creating Area of Habitat data
#' (Brooks *et al.* 2019). Specifically, data are simulated to define
#' species geographic ranges, summary information, and habitat preferences.
#'
#' @inheritParams create_spp_aoh_data
#'
#' @param n `integer` Number of species to simulate.
#'
#' @param boundary_data [sf::st_sf()] Spatial object delineating the
#'   spatial extent and boundary for simulating species ranges.
#'
#' @param rf_model_scale_min `numeric` Minimum scaling parameter used
#'   to control the smallest possible size of simulated species ranges.
#'   See the documentation for the `scale` parameter in
#'   `RandomFields::RMgauss()` for more information.
#'   Defaults to `1e+5`.
#'
#' @param rf_model_scale_max `numeric` Maximum scaling parameter used
#'   to control the largest possible size of simulated species ranges.
#'   See the documentation for the `scale` parameter in
#'   `RandomFields::RMgauss()` for more information.
#'   Defaults to `2e+5`.
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
#'  information about the species (including altitudinal limit information.}
#' \item{spp_habitat_data}{A [tibble::tibble()] object containing habitat
#'  preferences for the species.}
#' }
#'
#' @seealso
#' See [create_spp_aoh_data()] for creating Area of Habitat maps using
#' data for real or simulated species.
#'
#' @examples
#' # please ensure that the RandomFields and smoothr packages are installed
#' # to run these examples
#'
#' @examplesIf require(RandomFields) && require(smoothr)
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
#' # specify
#' # simulate data for 5 species
#' x <- simulate_spp_data(
#'   n = 5, boundary_data = boundary_data, cache_dir = download_dir
#' )
#'
#' # preview species range data
#' print(x$spp_range_data)
#'
#' # preview species habitat affiliation data
#' print(x$spp_habitat_data)
#'
#' # preview species summary data
#' print(x$spp_summary_data)
#' }
#' @references
#' Brooks TM, Pimm SL, Akçakaya HR, Buchanan GM, Butchart SHM, Foden W,
#' Hilton-Taylor C, Hoffmann M, Jenkins CN, Joppa L, Li BV, Menon V,
#' Ocampo-Peñuela N, Rondinini C (2019) Measuring terrestrial Area of Habitat
#' (AOH) and its Utility for the IUCN Red List. Trends in Ecology & Evolution.
#' 34:977--986. <doi:10.1016/j.tree.2019.06.009>
#'
#' @export
simulate_spp_data <- function(n,
                              boundary_data,
                              habitat_data = NULL,
                              elevation_data = NULL,
                              rf_model_scale_min = 1e+5,
                              rf_model_scale_max = 2e+5,
                              cache_dir = tempdir(),
                              habitat_version = "latest",
                              force = FALSE,
                              omit_habitat_codes =
                                default_omit_iucn_habitat_codes(),
                              verbose = TRUE) {
  # assert dependencies available
  if (!requireNamespace("RandomFields", quietly = TRUE))
    stop("the \"RandomFields\" package must be installed to simulate data")
  if (!requireNamespace("smoothr", quietly = TRUE))
    stop("the \"smoothr\" package must be installed to simulate data")

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
      cli::cli_process_start("importing global elevation data")
    }
    ### processing
    elevation_data <- get_global_elevation_data(
      dir = cache_dir, force = force, verbose = verbose
    )
    ## update message
    if (verbose) {
      cli::cli_process_done()
    }
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
      cli::cli_process_start("importing global habitat data")
    }
    ### processing
    habitat_data <- get_global_habitat_data(
      dir = cache_dir, version = habitat_version, force = force,
      verbose = verbose
    )
    ## update message
    if (verbose) {
      cli::cli_process_done()
    }
  }
  assertthat::assert_that(
    inherits(habitat_data, "SpatRaster"),
    all(terra::hasValues(habitat_data))
  )

  # simulate species ids
  spp_id_no <- sort(sample.int(1e+4, n))

  # determine resolution for simulations
  boundary_data_proj <- sf::st_transform(
    boundary_data, sf::st_crs("ESRI:54017")
  )
  bb <- sf::st_bbox(boundary_data_proj)
  res <- min(bb$xmax - bb$xmin, bb$ymax - bb$ymin) / 500
  res <- max(1000, round(res / 1000) * 1000)

  # create spatial grid for simulations
  sim_rast <- create_template_rast(
    xres = res, yres = res,
    crs = sf::st_crs(boundary_data_proj),
    bbox = bb
  )
  boundary_data_proj_vect <- terra::vect(boundary_data_proj)
  terra::crs(boundary_data_proj_vect) <- sf_terra_crs(
    sf::st_crs(boundary_data_proj)
  )
  sim_rast <- terra::mask(
    x = terra::init(sim_rast, 1),
    mask = boundary_data_proj_vect
  )

  # simulate species range maps
  sim_eoo <- lapply(
    stats::runif(n, min = rf_model_scale_min, max = rf_model_scale_max),
    function(x) {
      simulate_rf_data(
        x = sim_rast,
        model = RandomFields::RMgauss(scale = x),
        transform = function(x) round(stats::plogis(x)),
        n = 1
      )
    }
  )
  sim_range_data <- lapply(seq_len(n), function(i) {
    x <- sim_eoo[[i]]
    x[x < 0.5] <- NA_real_
    x <- sf::st_as_sf(terra::as.polygons(x))
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
    x <- x[sf::st_is_valid(x), , drop = FALSE]
    print(x)
    if (nrow(x) == 0) {
      stop("failed to simulate data")
    }
    rownames(x) <- NULL
    # determine if species has seasonal distributions or not
    if (isTRUE(stats::runif(1) > 0.7) && (nrow(x) >= 3)) {
      # migratory
      migrant <- TRUE
      dist_idx <- sample(order(sf::st_area(x), decreasing = TRUE)[seq_len(3)])
      d1 <- x[dist_idx, , drop = FALSE]
      # add in seasonal metadata
      d1$seasonal <- seq(2, 4)
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
      ## simulate additional distriubtions
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
    x <- smoothr::smooth(x, method = "ksmooth", smoothness = 2)
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
    category = sample(c("LC", "NT", "VU", "EN"), 1),
    marine = "false",
    terrestial = "true",
    freshwater = "false"
  )
  if ("id" %in% names(sim_range_data)) {
    sim_range_data <- dplyr::select(sim_range_data, -.data$id)
  }

  # simulate habitat preference data
  sim_habitat_data <- simulate_habitat_data(
    x = sim_range_data,
    habitat_data = terra::project(
      x = habitat_data,
      y = sim_rast,
    ),
    omit_habitat_codes = omit_habitat_codes
  )

  # simulate summary data
  sim_summary_data <- simulate_summary_data(
    x = sim_range_data,
    elevation_data = terra::project(
      x = elevation_data,
      y = sim_rast
    )
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
  result <- purrr::map_dfr(seq_len(nrow(x_distinct)), function(i) {
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
      probs = c(0.2, 0.8),
      na.rm = TRUE,
      names = FALSE
    )

    ## return result
    tibble::tibble(
      id_no = curr_id_no,
      taxonid = curr_id_no,
      scientific_name = x$binomial[[1]],
      kingdom = NA_character_,
      phylum = NA_character_,
      class = NA_character_,
      order = NA_character_,
      family = NA_character_,
      genus = x$genus[[1]],
      main_common_name = NA_character_,
      authority = NA_character_,
      published_year = NA_character_,
      assessment_date = NA_character_,
      category = x$category[[1]],
      criteria = NA_character_,
      population_trend = NA_character_,
      marine_system = x$marine[[1]],
      freshwater_system = x$freshwater[[1]],
      terrestrial_system = x$terrestial[[1]],
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

simulate_habitat_data <- function(x, habitat_data, omit_habitat_codes) {
  # assert that arguments are valid
  assertthat::assert_that(
    inherits(x, "sf"),
    assertthat::has_name(x, "id_no"),
    inherits(habitat_data, "SpatRaster"),
    is.character(omit_habitat_codes),
    assertthat::noNA(omit_habitat_codes)
  )

  # preliminary processing
  ## unique combinations id_no and seasonal
  x_distinct <- sf::st_drop_geometry(x)
  x_distinct <- dplyr::distinct(
    dplyr::select(x_distinct, .data$id_no, .data$seasonal)
  )
  x_distinct <- x_distinct[x_distinct$seasonal <= 4, , drop = FALSE]
  x_distinct$seasonal_name <- convert_to_seasonal_name(x_distinct$seasonal)

  ## convert habitat codes to names
  code_data <- habitat_code_data()
  if (all(names(habitat_data) %in% code_data$iucn_code)) {
    habitat_names <- code_data$name
    names(habitat_names) <- code_data$iucn_code
  } else {
    habitat_names <- paste0("code ", names(habitat_data))
    names(habitat_names) <- names(habitat_data)
  }

  # create habitat data
  result <- purrr::map_dfr(seq_len(nrow(x_distinct)), function(i) {
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

    ## find available habitat
    xi_habitat_data <- terra::global(
      x = terra::mask(
        x = habitat_data,
        mask = terra::vect(xi)
      ),
      fun = "sum",
      na.rm = TRUE
    )
    idx <- which(xi_habitat_data$sum > 0)
    xi_habitat_data <- xi_habitat_data[idx, , drop = FALSE]
    xi_habitat_data <- dplyr::arrange(xi_habitat_data,  dplyr::desc(sum))

    ## extract suitable habitat
    idx <- !rownames(xi_habitat_data) %in% omit_habitat_codes
    xi_suitable_habitat_data <- xi_habitat_data[idx, , drop = FALSE]

    ## sample suitable codes
    suitable_codes <- sample(
      x = rownames(xi_suitable_habitat_data),
      size = min(nrow(xi_suitable_habitat_data), 3),
      replace = FALSE
    )

    ## sample marginal codes
    if (
      (stats::runif(1) > 0.8) &&
      (length(suitable_codes) < nrow(xi_habitat_data))
    ) {
      potential_codes <- setdiff(suitable_codes, rownames(xi_habitat_data))
      marginal_codes <- sample(
        x = potential_codes,
        size = min(length(potential_codes), 2)
      )
    } else {
      marginal_codes <- c()
    }

    ## return data for species' seasonal distribution
    tibble::tibble(
      id_no = curr_id_no,
      code = c(suitable_codes, marginal_codes),
      habitat = unname(habitat_names[c(suitable_codes, marginal_codes)]),
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
