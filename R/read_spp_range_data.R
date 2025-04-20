#' @include internal.R misc_sf.R
NULL

#' Read species range data
#'
#' Import species geographic range (i.e., extent of occurrence) data obtained
#' from the
#' [International Union for Conservation of Nature (IUCN) Red List of
#' Threatened Species](https://www.iucnredlist.org/).
#'
#' @param path `character` File path to the data (zip archive) file.
#'
#' @param n `numeric` Number of features in the dataset to import.
#'  Defaults to `NULL` such that all available data is imported.
#'
#' @details
#' Data for amphibians, reptiles, and mammals can be obtained directly from
#' the International Union for Conservation of Nature (IUCN) Red List website
#' (see https://www.iucnredlist.org/resources/spatial-data-download).
#' Data for birds can be obtained by requesting data from
#' [BirdLife International](https://www.birdlife.org/)
#' (see <https://datazone.birdlife.org/species/requestdis>).
#' To standardize data from the IUCN Red List and BirdLife International,
#' the `"SISID"` and `"SISRecID"` columns are renamed as `"id_no"`.
#'
#' @return A [sf::sf()] object containing the dataset.
#'
#' @examples
#' # find file path for example range data following IUCN Red List data format
#' ## N.B., the range data were not obtained from the IUCN Red List,
#' ## and were instead based on data from GBIF (https://www.gbif.org/)
#' path <- system.file("extdata", "EXAMPLE_SPECIES.zip", package = "aoh")
#'
#' # import data
#' spp_range_data <- read_spp_range_data(path)
#'
#' # preview data
#' print(spp_range_data)
#'
#' # plot data
#' plot(spp_range_data[, "id_no"])
#' @export
read_spp_range_data <- function(path, n = NULL) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(path),
    assertthat::noNA(path),
    assertthat::is.readable(path)
  )
  assertthat::assert_that(
    isTRUE(
      assertthat::has_extension(path, "zip") ||
      assertthat::has_extension(path, "7z")
    ),
    msg = "`path` must have a \".zip\" or \"7z\" file extension"
  )

  # create temporary directory
  temp_dir <- normalize_path(tempfile(), mustWork = FALSE)
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

  # unzip data to temporary directory
  if (endsWith(path, ".zip")) {
    utils::unzip(path, exdir = temp_dir)
  } else{
    assertthat::assert_that(
      requireNamespace("archive", quietly = TRUE),
      msg =  paste(
        "the \"archive\" package needs to be installed to read 7z files, use",
        "`install.packages(\"archive\")`"
      )
    )
    archive::archive_extract(archive = path, dir = temp_dir)
  }

  # find extracted data
  shp_path <- dir(temp_dir, "^.*\\.shp$", full.names = TRUE, recursive = TRUE)
  shp_path <- gsub("\\", "/", shp_path, fixed = TRUE)
  gdb_path <- dir(
    temp_dir, "^.*\\.gdb$", include.dirs = TRUE, full.names = TRUE,
    recursive = TRUE
  )
  gdb_path <- gsub("\\", "/", gdb_path, fixed = TRUE)

  # import data
  if (length(shp_path) == 1) {
    out <- read_sf_n(shp_path, n = n)
  } else if (length(shp_path) > 1) {
    ## load data
    out <- lapply(shp_path, read_sf_n, n = n)
    ## remove uneeded columns
    out <- lapply(
      out,
      function(x) {
        n <- setdiff(names(x), c("OBJECTID", attr(x, "sf_column")))
        x[, n, drop = FALSE]
      }
    )
    ## get column names for each shapefile
    nms <- lapply(out, names)
    ## check that all shapefiles have the same column names
    assertthat::assert_that(
      all(vapply(nms, FUN.VALUE = logical(1), identical, nms[[1]])),
      msg = paste(
        "`path` contains multiple shapefiles that have",
        "different columns names, and so cannot import spatial data"
      )
    )
    ## merge datasets together
    out <- dplyr::bind_rows(out)
  } else if (length(gdb_path) == 1) { # nocov start
    ## inspect geodatabase
    gdb_dir <- sf::st_layers(gdb_path)
    ## find index for spatial data
    sp_idx <- which(
      grepl("species", tolower(gdb_dir$name), fixed = TRUE) &
      vapply(gdb_dir$geomtype, FUN.VALUE = logical(1), function(x) {
        grepl("polygon", tolower(x), fixed = TRUE)
      })
    )
    assertthat::assert_that(
      length(sp_idx) == 1,
      msg = "`path` does not contain species range data"
    )
    ## find index for tabular data
    tbl_idx <- which(
      c(
        grepl("checklist", tolower(gdb_dir$name), fixed = TRUE) |
        grepl("taxonomic", tolower(gdb_dir$name), fixed = TRUE)
      ) &
      vapply(gdb_dir$geomtype, FUN.VALUE = logical(1), function(x) {
        any(is.na(x))
      })
    )
    assertthat::assert_that(
      length(tbl_idx) == 1,
      msg = "`path` does not contain species metadata"
    )
    ## import data
    out <- read_sf_n(gdb_path, gdb_dir$name[sp_idx], n = n)
    md <- sf::read_sf(gdb_path, gdb_dir$name[tbl_idx])
    ## find range data id column
    if (assertthat::has_name(out, "id_no")) {
      id_col <- "id_no"
    } else if (assertthat::has_name(out, "sisid")) {
      id_col <- "sisid"
    } else if (assertthat::has_name(out, "SISID")) {
      id_col <- "SISID"
    } else if (assertthat::has_name(out, "SISRecID")) {
      id_col <- "SISRecID"
    } else {
      stop(
        "range data in `path` does not contain ",
        "a recognized species identifier column (i.e., a column named ",
        "\"id_no\", \"sisid\", \"SISID\", or \"SISRecID\")"
      )
    }
    ## rename column in metadata
    if (!assertthat::has_name(md, id_col)) {
      if (assertthat::has_name(md, "id_no")) {
        names(md)[which(names(md) == "id_no")[[1]]] <- id_col
      } else if (assertthat::has_name(md, "sisid")) {
        names(md)[which(names(md) == "sisid")[[1]]] <- id_col
      } else if (assertthat::has_name(md, "SISID")) {
        names(md)[which(names(md) == "SISID")[[1]]] <- id_col
      } else if (assertthat::has_name(md, "SISRecID")) {
        names(md)[which(names(md) == "SISRecID")[[1]]] <- id_col
      } else {
        stop(
          "species metadata in `path` does not contain ",
          "a recognized species identifier column (i.e., a column named ",
          "\"id_no\", \"sisid\", \"SISID\", or \"SISRecID\")"
        )
      }
    }
    # merge data
    out <- dplyr::left_join(
      out,
      md[, c(setdiff(names(md), names(out)), id_col), drop = FALSE],
      by = id_col
    )
    out <- dplyr::select(out, -"geometry", dplyr::everything())
  } else {
    stop("`path` does not contain spatial data")
  } # nocov end

  # clean up
  unlink(temp_dir, recursive = TRUE, force = TRUE)

  # return result
  out
}
