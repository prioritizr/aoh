#' @include internal.R
NULL

#' Clean species range data
#'
#' Clean species geographic range (i.e., extent of occurrence) data obtained
#' from the
#' [International Union for Conservation of Nature (IUCN) Red List of Threatened Species](https://www.iucnredlist.org/).
#' Please note that this function should **not** be used to clean data
#' before using the data to create Area of Habitat data
#' (i.e., [create_spp_aoh_data()]). The procedures used the
#' create create Area of Habitat data will automatically clean the data
#' range data.
#'
#' @param x A [sf::sf()] object containing species geographic range data.
#'
#' @param crs `character or `integer` object representing a coordinate
#'   reference system. Defaults to World Behrmann (ESRI:54017).
#'
#' @param snap_tolerance `numeric` tolerance for snapping geometry to a grid
#'   for resolving invalid geometries. Defaults to 1 meter.
#'
#' @inheritParams create_spp_aoh_data
#' @inheritParams st_repair_geometry
#'
#' @details
#' This function applies the following data cleaning procedures:
#'
#' \enumerate{
#'
#' \item Column names are standardized. This involves converting them to lower
#'   case characters and fixing any spelling mistakes.
#'   If a `SISID` column is present, it is renamed to the `id_no` column.
#'   Additionally, if a `sci_name` or `SCINAME` column is present,
#'   it is renamed to the `binomial` column.
#'
#' \item Species ranges are filtered according to criteria specified
#'   for the `presence`, `origin`, and `seasonal` columns
#'   (i.e., per arguments to `keep_iucn_rl_presence`,
#'   `keep_iucn_rl_origin`, and `keep_iucn_rl_seasonal`).
#'
#' \item Species that are not terrestrial are excluded (i.e., filtering based on
#'    where `terrestrial == "true"`).
#'
#' \item Fix any potential geometry issues (using [st_repair_geometry()]).
#'
#' \item Wrap geometries to date line (using [sf::st_wrap_dateline()]).
#'
#' \item Fix any potential geometry issues (using [st_repair_geometry()]).
#'
#' \item Reproject data to specified coordinate system
#'    (using [sf::st_transform()]).
#'
#' \item Fix any potential geometry issues (iusing [sf::st_make_valid()]).
#'
#' \item Snap geometries to spatial grid (using [lwgeom::st_snap_to_grid()]).
#'   Note that this step is only performed if the argument to `snap_tolerance`
#'   is greater than zero.
#'
#' \item Fix any potential geometry issues (using [st_repair_geometry()]).
#'
#' \item Data are spatially dissolved so that each seasonal distribution for
#'   each taxon is represented by a separate geometry.
#'
#' \item Fix any potential geometry issues (using [st_repair_geometry()]).
#'
#' }
#'
#' For more information on criteria used in steps 2 -- 4, please refer to
#' IUCN SSC Red List Technical Working Group (2021).
#'
#' @return A [sf::sf()] object containing the dataset.
#'
#' @references
#' IUCN SSC Red List Technical Working Group (2021)
#' Mapping Standards and Data Quality for the IUCN Red List Spatial Data
#' (Version 1.19). International Union for Conservation of Nature (IUCN).
#' Available at <https://nc.iucnredlist.org/redlist/resources/files/1539098236-Mapping_Standards_Version_1.16_2018.pdf>.
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
#' # clean data
#' spp_cleaned_range_data <- clean_spp_range_data(spp_range_data)
#'
#' # preview data
#' print(spp_range_data)
#'
#' # plot data
#' ## N.B., the cleaned data are very similar to the input data
#' ## because they don't have any issues
#' plot(spp_cleaned_range_data)
#' }
#' @noRd
clean_spp_range_data <- function(x,
                                 keep_iucn_rl_presence = c(1, 2),
                                 keep_iucn_rl_origin = c(1, 2, 6),
                                 keep_iucn_rl_seasonal = c(1, 2, 3, 4),
                                 crs = sf::st_crs("ESRI:54017"),
                                 snap_tolerance = 1,
                                 geometry_precision = 1e5) {
  # assert arguments are valid
  ## initial checks
  assertthat::assert_that(
    inherits(x, "sf"),
    nrow(x) > 0,
    ncol(x) > 0,
    inherits(crs, "crs"),
    is.numeric(keep_iucn_rl_presence),
    assertthat::noNA(keep_iucn_rl_presence),
    is.numeric(keep_iucn_rl_origin),
    assertthat::noNA(keep_iucn_rl_presence),
    is.numeric(keep_iucn_rl_seasonal),
    assertthat::noNA(keep_iucn_rl_seasonal)
  )
  assertthat::assert_that(
    assertthat::has_name(x, "id_no") ||
      assertthat::has_name(x, "SISID") ||
      assertthat::has_name(x, "SISRecID"),
    msg = paste0(
      "argument to \"x\" must have a \"id_no\", \"SISID\", ",
      "or \"SISRecID\" column"
    )
  )
  assertthat::assert_that(
    assertthat::has_name(x, "binomial") ||
      assertthat::has_name(x, "SCINAME") ||
      assertthat::has_name(x, "sci_name"),
    msg = paste0(
      "argument to \"x\" must have a \"binomial\", \"SCINAME\", ",
      "or \"sci_name\" column"
    )
  )
  assertthat::assert_that(
    all(keep_iucn_rl_presence == round(keep_iucn_rl_presence)),
    msg = c(
      "argument to \"keep_iucn_rl_presence\" does not contain integer codes"
    )
  )
  assertthat::assert_that(
    all(keep_iucn_rl_origin == round(keep_iucn_rl_origin)),
    msg = c(
      "argument to \"keep_iucn_rl_origin\" does not contain integer codes"
    )
  )
  assertthat::assert_that(
    all(keep_iucn_rl_seasonal == round(keep_iucn_rl_seasonal)),
    msg = c(
      "argument to \"keep_iucn_rl_seasonal\" does not contain integer codes"
    )
  )

  # step 1: format all column names
  ## re-order columns so that geometry is last
  ### N.B., this is needed to avoid sf internal errors related to agr
  ### when renaming columns or adding in new columns
  x <- dplyr::select(x, -.data$geometry, dplyr::everything())

  ## convert column names to lower case
  x <- dplyr::rename_all(x, tolower)

  # step 1a: rename and format columns
  ## "terrestrial" column
  if (assertthat::has_name(x, "terrestial")) {
    x <- dplyr::rename(x, terrestrial = "terrestial")
  }
  ## "order_" column
  if (assertthat::has_name(x, "order_")) {
    x <- dplyr::rename(x, order = "order_")
  }

  ## "RedListCategory_*" column
  if (!assertthat::has_name(x, "category")) {
    if (any(grepl("category", names(x), fixed = TRUE))) {
      idx <- which(grepl("category", names(x), fixed = TRUE))[[1]]
      names(x)[idx] <- "category"
      # hack to fix st_agr, https://github.com/r-spatial/sf/issues/1472
      x <- dplyr::mutate(x)
    }
  }

  # step 1c: rename and format columns for old BirdLife data format
  if (!assertthat::has_name(x, "id_no")) {
    if (assertthat::has_name(x, "sisid")) {
      x <- dplyr::rename(x, id_no = "sisid")
    } else if (assertthat::has_name(x, "sisrecid")) {
        x <- dplyr::rename(x, id_no = "sisrecid")
    }
  }
  if (!assertthat::has_name(x, "binomial")) {
    if (assertthat::has_name(x, "sci_name")) {
      x <- dplyr::rename(x, binomial = "sci_name")
    }
  }
  if (!assertthat::has_name(x, "binomial")) {
    if (assertthat::has_name(x, "sciname")) {
      x <- dplyr::rename(x, binomial = "sciname")
    }
  }
  if (assertthat::has_name(x, "presenc")) {
    x <- dplyr::rename(x, presence = "presenc")
  }
  if (assertthat::has_name(x, "seasona")) {
    x <- dplyr::rename(x, seasonal = "seasona")
  }
  if (!assertthat::has_name(x, "category")) {
    x$category <- NA_character_
  }
  if (assertthat::has_name(x, "marine_system")) {
    x <- dplyr::rename(x, marine = "marine_system")
  }
  if (assertthat::has_name(x, "terrestrial_system")) {
    x <- dplyr::rename(x, terrestrial = "terrestrial_system")
  }
  if (assertthat::has_name(x, "freshwater_system")) {
    x <- dplyr::rename(x, freshwater = "freshwater_system")
  }

  # step 1d: add in any missing columns
  ## "familyname" column is present
  ## N.B., BirdLife use the "FamilyName" column to store the Latin name for
  ## bird families and the "Family" column to store the English common name for
  ## bird families, so we need to overwrite the "family" column with values
  ## in the "familyname" column
  ## (since we previously converted all column names to lower case)
  if (assertthat::has_name(x, "familyname")) {
    x <- dplyr::mutate(x, family = .data$familyname)
  }
  ## "genus" column is missing
  if (!assertthat::has_name(x, "genus")) {
    x <- dplyr::mutate(x, genus = vapply(
      strsplit(x$binomial, " "), `[[`, character(1), 1
    ))
  }
  ## "kingdom" column is missing
  if (!assertthat::has_name(x, "kingdom")) {
    if (any(grepl("birdlife", tolower(names(x))))) {
      x$kingdom <- "ANIMALIA"
    } else {
      x$kingdom <- NA_character_
    }
  }
  ## "phylum" column is missing
  if (!assertthat::has_name(x, "phylum")) {
    if (any(grepl("birdlife", tolower(names(x))))) {
      x$phylum <- "CHORDATA"
    } else {
      x$phylum <- NA_character_
    }
  }
  ## "class" column is missing
  if (!assertthat::has_name(x, "class")) {
    if (any(grepl("birdlife", tolower(names(x))))) {
      x$class <- "AVES"
    } else {
      x$class <- NA_character_
    }
  }
  ## "subspecies" column is missing
  if (!assertthat::has_name(x, "subspecies")) {
    x$subspecies <- NA_character_
  }
  ## "order" column is missing
  if (!assertthat::has_name(x, "order")) {
    x$order <- NA_character_
  }
  ## "marine_system" column
  if (!assertthat::has_name(x, "marine")) {
    x$marine <- "false"
    if (!any(grepl("birdlife", tolower(names(x))))) {
      cli::cli_alert_warning(
        paste(
          "argument to \"x\" is missing the \"marine\" column,",
          "assuming none of the species are marine-based"
        )
      )
    }
  }
  ## "freshwater_system" column
  if (!assertthat::has_name(x, "freshwater")) {
    x$freshwater <- "false"
    if (!any(grepl("birdlife", tolower(names(x))))) {
      cli::cli_alert_warning(
        paste(
          "argument to \"x\" is missing the \"freshwater\" column,",
          "assuming none of the species are freshwater-based"
        )
      )
    }
  }
  ## "terrestrial" column
  if (!assertthat::has_name(x, "terrestrial")) {
    x$terrestrial <- "true"
    if (!any(grepl("birdlife", tolower(names(x))))) {
      cli::cli_alert_warning(
        paste(
          "argument to \"x\" is missing the \"terrestrial\" column,",
          "assuming all species are terrestrial"
        )
      )
    }
  }

  # step 1e: re-order columns so that geometry is last
  ## N.B., this is needed to avoid sf internal errors related to agr
  ## when renaming columns or adding in new columns
  x <- dplyr::select(x, -.data$geometry, dplyr::everything())

  # validate data
  ## check column names
  assertthat::assert_that(
    assertthat::has_name(x, "id_no"),
    assertthat::has_name(x, "presence"),
    assertthat::has_name(x, "origin"),
    assertthat::has_name(x, "seasonal"),
    assertthat::has_name(x, "freshwater"),
    assertthat::has_name(x, "marine"),
    assertthat::has_name(x, "terrestrial"),
    assertthat::has_name(x, "category"),
    assertthat::has_name(x, "binomial"),
    assertthat::has_name(x, "subspecies"),
    assertthat::has_name(x, "order"),
    assertthat::has_name(x, "kingdom"),
    assertthat::has_name(x, "phylum"),
    assertthat::has_name(x, "class"),
    assertthat::has_name(x, "genus")
  )
  ## check types
  assertthat::assert_that(
    is.numeric(x$id_no),
    is.numeric(x$presence),
    is.numeric(x$origin),
    is.numeric(x$seasonal),
    is.character(x$marine),
    is.character(x$freshwater),
    is.character(x$terrestrial),
    is.character(x$category),
    is.character(x$binomial),
    is.character(x$subspecies),
    is.character(x$kingdom),
    is.character(x$phylum),
    is.character(x$class),
    is.character(x$genus)
  )
  ## check values
  assertthat::assert_that(
    all(x$freshwater[!is.na(x$freshwater)] %in% c("true", "false")),
    msg = "freshwater column should contain \"true\" or \"false\" values"
  )
  assertthat::assert_that(
    all(x$marine[!is.na(x$marine)] %in% c("true", "false")),
    msg = "marine column should contain \"true\" or \"false\" values"
  )
  assertthat::assert_that(
    all(x$terrestrial[!is.na(x$terrestrial)] %in% c("true", "false")),
    msg = "terrestrial column should contain \"true\" or \"false\" values"
  )

  print(paste("after step 1:", nrow(x)))
  print(x, width = Inf)

  # step 2: exclude polygons based on presence code
  x <- x[
    which(round(x$presence) %in% round(keep_iucn_rl_presence)), , drop = FALSE
  ]
  invisible(gc())
  print(paste("after step 2:", nrow(x)))
  print(x, width = Inf)

  # step 3: exclude polygons based on origin code
  x <- x[
    which(round(x$origin) %in% round(keep_iucn_rl_origin)), , drop = FALSE
  ]
  invisible(gc())
  print(paste("after step 3:", nrow(x)))
  print(x, width = Inf)

  # step 4: exclude uncertain seasonality
  x <- x[
    which(round(x$seasonal) %in% round(keep_iucn_rl_seasonal)), , drop = FALSE
  ]
  invisible(gc())
  print(paste("after step 4:", nrow(x)))
  print(x, width = Inf)

  # step 5: exclude non-terrestrial distributions
  idx <- which(x$terrestrial == "true")
  x <- x[idx, , drop = FALSE]
  invisible(gc())
  print(paste("after step 5:", nrow(x)))
  print(x, width = Inf)

  # step 6: convert MULTISURFACE to MULTIPOLYGON
  x <- sf::st_set_precision(x, geometry_precision)
  idx <- which(vapply(sf::st_geometry(x), inherits, logical(1), "MULTISURFACE"))
  if (length(idx) > 0) {
    g <- sf::st_geometry(x)
    g2 <- lapply(g[idx], sf::st_cast, "MULTIPOLYGON")
    g2 <- lapply(g2, sf::st_buffer, 0)
    g2 <- lapply(g2, sf::st_make_valid)
    for (i in seq_along(idx)) {
      g[[idx[[i]]]] <- g2[[i]]
    }
    x <- sf::st_set_geometry(x, g)
    rm(g, g2)
  }
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  invisible(gc())
  print(paste("after step 6:", nrow(x)))
  print(x, width = Inf)

  # step 7: fix any potential geometry issues
  x <- st_repair_geometry(x, geometry_precision)
  invisible(gc())
  print(paste("after step 7:", nrow(x)))
  print(x, width = Inf)


  # step 8: wrap geometries to dateline
  x <- sf::st_set_precision(x, geometry_precision)
  x <- suppressWarnings(sf::st_wrap_dateline(x,
    options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
  )
  invisible(gc())
  print(paste("after step 8:", nrow(x)))
  print(x, width = Inf)


  # step 9: fix any potential geometry issues
  x <- st_repair_geometry(x, geometry_precision)
  invisible(gc())
  print(paste("after step 9:", nrow(x)))
  print(x, width = Inf)


  # step 10: reproject data
  x <- sf::st_set_precision(x, geometry_precision)
  x <- sf::st_transform(x, crs)
  invisible(gc())
  print(paste("after step 10:", nrow(x)))
  print(x, width = Inf)


  # step 11: fix any potential geometry issues
  x <- st_repair_geometry(x, geometry_precision)
  invisible(gc())
  print(paste("after step 11:", nrow(x)))
  print(x, width = Inf)

  # step 12: snap geometries to grid
  if (snap_tolerance > 0) {
    x <- sf::st_set_precision(x, geometry_precision)
    x <- lwgeom::st_snap_to_grid(x, snap_tolerance)
  }
  invisible(gc())
  print(paste("after step 12:", nrow(x)))
  print(x, width = Inf)

  # step 13: fix any potential geometry issues
  x <- st_repair_geometry(x, geometry_precision)
  invisible(gc())
  print(paste("after step 13:", nrow(x)))
  print(x, width = Inf)

  # step 14: dissolve geometries by species, subspecies, seasonal
  ## create id
  if (is.character(x$seasonal)) {
    x$seasonal <- convert_to_seasonal_id(x$seasonal)
  }
  x$seasonal <- as.integer(x$seasonal)
  x$aoh_id <- withr::with_options(
    list(scipen = 1000),
    paste0("AOH_", x$id_no, "_", x$seasonal)
  )
  old_ids <- unique(x$aoh_id)
  ## geoprocessing
  x <-
    x %>%
    dplyr::group_by(.data$aoh_id) %>%
    dplyr::summarize(
      id_no = dplyr::first(.data$id_no),
      category = dplyr::first(.data$category),
      binomial = dplyr::first(.data$binomial),
      subspecies = dplyr::first(.data$subspecies),
      seasonal = dplyr::first(.data$seasonal),
      kingdom = dplyr::first(.data$kingdom),
      phylum = dplyr::first(.data$phylum),
      class = dplyr::first(.data$class),
      order = dplyr::first(.data$order),
      genus = dplyr::first(.data$genus)
    ) %>%
    dplyr::ungroup()
  ## ensure correct order
  assertthat::assert_that(
    setequal(old_ids, x$aoh_id),
    msg = "failed to dissolve data"
  )
  x <- x[na.omit(match(old_ids, x$aoh_id)), , drop = FALSE]
  invisible(gc())

  print(paste("after step 14:", nrow(x)))
  print(x, width = Inf)

  # step 15: fix any potential geometry issues
  x <- st_repair_geometry(x, geometry_precision)
  invisible(gc())

  # re-order columns so that geometry is last
  ## N.B., this is needed to avoid sf internal errors related to agr
  ## when renaming columns or adding in new columns
  x <- dplyr::select(x, -.data$geometry, dplyr::everything())
  invisible(gc())

  # return result
  x
}
