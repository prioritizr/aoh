#' @include internal.R
NULL

#' Get species data from the IUCN Red List API
#'
#' Import species data obtained from the [International Union
#' for Conservation of Nature (IUCN) Red List of Threatened
#' Species](https://www.iucnredlist.org/).
#' Please note that a token is required to download
#' data from the IUCN Red List (see instructions below to obtain a token).
#'
#' @inheritParams get_spp_habitat_data
#'
#' @param api_function `function` Function for querying species data from
#'   the IUCN Red List. This function should one of the functions available
#'   in the \pkg{rredlist} package, and have the `id` parameter available
#'   for querying data for a specific taxon identifier
#'   (e.g. [rredlist::rl_habitats()]).
#'
#' @param data_prefix `character` Prefix for saving data to disk for caching.
#'
#' @param data_template `data.frame` Table containing the names and correct
#'   classes (e.g. `numeric`, `character`, `logical`) for each column
#'   expected from calling the argument to `api_function` (see Examples below).
#'
#' @inherit get_spp_habitat_data references return
#'
#' @inheritSection aoh Accessing the IUCN Red List API
#'
#' @examples
#' \dontrun{
#' # define species to download data for based on taxon identifiers
#' spp_ids <- c(12392, 22694927)
#'
#' # define persistent storage location
#' download_dir <- rappdirs::app_dir("aoh")
#'
#' # download and import habitat data
#' result <- get_spp_api_data(
#'   x = spp_ids,
#'   api_function = rredlist::rl_habitats,
#'   data_prefix = "summary",
#'   data_template = tibble::tibble(
#'     code = integer(0),
#'     habitat = character(0),
#'     suitability = character(0),
#'     season = character(0),
#'     majorimportance = character(0)
#'   ),
#'   dir = download_dir
#' )
#' }
#'
#' @examplesIf interactive()
#' \dontrun{
#' # preview data
#' print(result)
#' }
#' @noRd
get_spp_api_data <- function(x, api_function, data_prefix, data_template,
                             dir = tempdir(), version = "latest",
                             key = NULL, delay = 2, force = FALSE,
                             verbose = TRUE) {
  # assert arguments are valid
  assertthat::assert_that(
    is.numeric(x),
    assertthat::noNA(x),
    length(x) > 0,
    is.function(api_function),
    assertthat::is.string(data_prefix),
    assertthat::noNA(data_prefix),
    inherits(data_template, "data.frame"),
    assertthat::is.string(dir),
    assertthat::is.writeable(dir),
    assertthat::is.string(version),
    assertthat::noNA(version),
    inherits(key, c("NULL", "character")),
    assertthat::is.number(delay),
    assertthat::noNA(delay),
    assertthat::is.flag(force),
    assertthat::noNA(force),
    assertthat::is.flag(verbose),
    assertthat::noNA(verbose)
  )
  # prepare x
  ## remove duplicates
  x <- unique(x)
  ## convert to integer
  assertthat::assert_that(
    all(abs(round(x) - x) <= 1e-5),
    msg = "x does not contain integer numbers"
  )
  x <- as.integer(x)

  # find version of data
  if (identical(version, "latest")) {
    iucn_rl_version <- rredlist::rl_version()
  } else {
    iucn_rl_version <- version
  }

  # create file path for caching data
  file_path <- file.path(
    dir, paste0("iucn-red-list-", data_prefix, "-", iucn_rl_version, ".csv.gz")
  )
  if (!identical(version, "latest") && isTRUE(!file.exists(file_path))) {
    stop(
      paste0(
        "cannot find previously downloaded data for \"version\" \"", version,
        "\" at argument to \"dir\""
      )
    )
  }

  # access cached data
  if (file.exists(file_path) && !isTRUE(force)) {
    ## import cached data if available
    iucn_rl_data <- readr::read_csv(
      file_path, col_names = TRUE, na = "", progress = FALSE,
      show_col_types = FALSE,
      col_types = readr::as.col_spec(
        dplyr::bind_cols(
          tibble::tibble(id_no = integer(nrow(data_template))),
          data_template
        )
      )
    )
    assertthat::assert_that(
      identical(names(iucn_rl_data), c("id_no", names(data_template))),
      msg = "issue loading cache data"
    )
  } else {
    ## start from
    iucn_rl_data <- data_template
    iucn_rl_data$id_no <- integer(0)
    iucn_rl_data <- {
      iucn_rl_data[, c("id_no", names(data_template)), drop = FALSE]
    }
  }
  # update cache if needed
  if (any(!x %in% iucn_rl_data$id_no)) {
    ## specify ids that need downloaded
    api_ids <- x[!x %in% iucn_rl_data$id_no]
    ## download data
    pb <- progressr::progressor(along = api_ids)
    progressr::with_progress(
      enable = verbose,
      expr = {
        api_results <- purrr::map(
          .x = api_ids,
          .f = function(x) {
            # wait as need
            Sys.sleep(delay)
            # attempt to download data
            out <- api_function(id = x, key = key)$result
            # update progress bar
            pb()
            # return result
            out
        })
      }
    )
    ## determine which api calls were successful
    api_success <- vapply(api_results, inherits, logical(1), "data.frame")
    ## append data to cache
    if (any(api_success)) {
      iucn_rl_data <- dplyr::bind_rows(
        iucn_rl_data,
        purrr::map_dfr(which(api_success), function(i) {
          # format data
          d <- tibble::as_tibble(api_results[[i]])
          for (j in setdiff(names(data_template), "id_no")) {
            if (j %in% names(d)) {
              d[[j]] <- methods::as(d[[j]], class(data_template[[j]]))
            } else {
              d[[j]] <- methods::as(NA, class(data_template[[j]]))
            }
          }
          # assign id_no column with taxon id
          d$id_no <- api_ids[[i]]
          # re-order columns
          d[, names(iucn_rl_data), drop = FALSE]
        })
      )
    }
    ## save cache if any data available
    if (nrow(iucn_rl_data) > 0) {
      readr::write_csv(iucn_rl_data, file_path, progress = FALSE, na = "")
    }
    ## throw errors
    if (any(!api_success)) {
      stop(
        paste(
          "failed to download data for the following taxon identifiers:",
          paste(paste0("\"", api_ids[!api_success], "\""), collapse = ", ")
        )
      )
    }
  }

  # extract data following same order as x
  out <- dplyr::left_join(
    tibble::tibble(id_no = x), iucn_rl_data, by = c("id_no")
  )

  # return result
  out
}
