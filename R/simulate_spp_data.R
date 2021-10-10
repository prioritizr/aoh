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
#' \dontrun{
#' # import data to simulate species data
#' sim_boundary_data <- sf::read_sf(
#'   system.file("extdata", "sim_boundary_data.gpkg", package = "aoh")
#' )
#' sim_habitat_data <- terra::rast(
#'   system.file("extdata", "sim_habitat_data.tif", package = "aoh")
#' )
#' sim_elevation_data <- terra::rast(
#'   system.file("extdata", "sim_elevation_data.tif", package = "aoh")
#' )
#'
#' # simulate data for 5 species
#' x <- simulate_spp_data(
#'   n = 5,
#'   boundary_data = sim_boundary_data,
#'   habitat_data = sim_habitat_data,
#'   elevation_data = sim_elevation_data
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
                              cache_dir = tempdir(),
                              omit_habitat_codes =
                                default_omit_iucn_habitat_codes(),
                              rf_model_scale_min = 1e+5,
                              rf_model_scale_max = 2e+5,
                              verbose = TRUE) {
  # assert that arguments are valid
  if (!requireNamespace("RandomFields", quietly = TRUE))
    stop("the \"RandomFields\" package must be installed to simulate data")
  if (!requireNamespace("smoothr", quietly = TRUE))
    stop("the \"smoothr\" package must be installed to simulate data")
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
  res <- max(1000, plyr::round_any(res, 1000))

  # create spatial grid for simulations
  sim_rast <- create_template_rast(
    xres = res, yres = res,
    crs = sf::st_crs(boundary_data_proj),
    bbox = bb
  )
  sim_rast <- terra::mask(
    terra::init(sim_rast, 1),
    terra::vect(boundary_data_proj)
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
    # split into seperate polygons
    x <- suppressWarnings(sf::st_cast(x, "POLYGON"))
    rownames(x) <- NULL
    # determine if species has seasonal distributions or not
    if (isTRUE(runif(1) > 0.7) && (nrow(x) >= 3)) {
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
      extra_idx <- sample(extra_idx, min(length(extra_idx), 3))
      d2 <- x[extra_idx, , drop = FALSE]
      d2$presence <- sample(c(1, seq(3, 6)), length(extra_idx), replace = TRUE)
      d2$origin <- sample(seq(2, 6), length(extra_idx), replace = TRUE)
      if (migrant) {
        d2$seasonal <- sample(seq(2, 5), length(extra_idx), replace = TRUE)
      } else {
        d2$seasonal <- 1
      }
    }
    # return result
    dplyr::bind_rows(d1, d2)
  })

  # smooth the distributions
  sim_range_data <- lapply(sim_range_data, function(x) {
    suppressWarnings(
      sf::st_intersection(
        smoothr::smooth(x, method = "ksmooth", smoothness = 2),
        boundary_data_proj
      )
    )
  })

  # combine data
  sim_range_data <- do.call(dplyr::bind_rows, sim_range_data)

  # add metadata
  sim_range_data <- dplyr::mutate(
    sim_range_data,
    binomial = paste("Simulus", "spp.", id_no),
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

  # simulate habitat preference data
  sim_habitat_data <- simulate_habitat_data(
    sim_range_data,
    terra::project(
      x = habitat_data,
      y = sim_rast,
      omit_habitat_codes = omit_habitat_codes
    )
  )

  # simulate summary data
  sim_summary_data <- simulate_summary_data(
    sim_range_data,
    terra::project(
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
    inherits(elevation_data, "SpatRaster"),
  )

  # preliminary processing
  ## unique combinations id_no and seasonal
  x_distinct <- sf::st_drop_geometry(x)
  x_distinct <- dplyr::distinct(x_distinct, id_no, .keep_all = TRUE)

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
    elev_range <- terra::quantile(
      x = terra::mask(
        x = elevation_data,
        y = terra::vect(xi)
      ),
      probs = c(0.2, 0.8),
      na.rm = TRUE
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
      elevation_upper = elev_range[[1]],
      elevation_lower = elev_range[[2]],
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
  x_distinct <- dplyr::distinct(dplyr::select(x_distinct, id_no, seasonal))
  x_distinct$seasonal_name <- convert_to_seasonal_name(x_distinct$seasonal)

  ## convert habitat codes to names
  code_data <- system.file("extdata", "habitat-codes.csv", package = "aoh")
  if (all(names(habitat_data) %in% code_data$name)) {
    habitat_names <- code_data$name
    names(habitat_names) <- code_data$code
  } else {
    habitat_names <- paste0("code ", names(habitat_data))
    names(habitat_names) <- names(habitat_data)
  }

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
    ## find available habitat
    xi_habitat_data <- terra::global(
      x = terra::mask(
        x = habitat_data,
        mask = terra::vect(xi)
      ),
      fun = "sum",
      na.rm = TRUE
    )
    xi_habitat_data <- xi_habitat_data[xi_habitat_data$sum > 0, , drop = FALSE]
    xi_habitat_data <- dplyr::arrange(xi_habitat_data,  dplyr::desc(sum))

    ## extract suitable habitat
    idx <- !rownames(xi_habitat_data) %in% omit_habitat_codes
    xi_suitable_habitat_data <- xi_habitat_data[idx, , drop = FALSE]

    ## sample suitable codes
    suitable_codes <- sample(
      rownames(xi_suitable_habitat_data),
      n = min(nrow(xi_suitable_habitat_data), 3),
      replace = FALSE
    )

    ## sample marginal codes
    if (
      (runif() > 0.8) &&
      (length(suitable_codes) < nrow(xi_habitat_data))
    ) {
      potential_codes <- setdiff(suitable_codes, rownames(xi_habitat_data))
      marginal_codes <- sample(
        potential_codes,
        min(length(potential_codes), 2)
      )
    } else {
      marginal_codes <- c()
    }

    ## return data for species' seasonal distribution
    tibble::tibble(
      id_no = curr_id_no,
      code = habitat_codes,
      habitat = unname(habitat_names[c(suitable_codes, marginal_codes)]),
      suitability = c(
        rep("Suitable", length(suitable_codes)),
        rep("Marginal", length(marginal_codes)),
      ),
      season = curr_seasonal_name,
      majorimportance = NA_character_
    )
  })

  # return result
  tibble::as_tibble(result)
}
