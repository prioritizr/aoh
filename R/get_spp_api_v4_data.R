#' @include internal.R
NULL

#' Get species data from the IUCN Red List API V4
#'
#' Import species data obtained from the [International Union
#' for Conservation of Nature (IUCN) Red List of Threatened
#' Species](https://www.iucnredlist.org/).
#' Please note that a token is required to download
#' data from the IUCN Red List (see instructions below to obtain a token).
#'
#' @inheritParams get_spp_habitat_data
#'
#' @param data_format `function` for extracting and preparing the data
#' from the result of `rredlist::rl_assesment()`.
#'
#' @param data_template [tibble::tibble()] for validating that the data has been
#' reformatted correctly.
#'
#' @param progress_name `character` Name for progress bar when downloading
#'   data from IUCN Red List API.
#'   Defaults to "`querying"`.
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
#'   data_format = function(x) {
#'     # extract habitat data
#'     x <- x$habitats[, c("code", "en"), drop = FALSE]
#'     # reformat columns
#'     x$habitat <- x$en
#'     x$code <- gsub(".", "_", x$code, fixed = TRUE)
#'     x$code <- as.numeric(x$code)
#'     # return result
#'     x[, c("code", "habitat")]
#'   },
#'   data_template = tibble::tibble(
#'     code = integer(0),
#'     habitat = character(0),
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
get_spp_api_v4_data <- function(
  x,
  data_format,
  data_template,
  dir = tempdir(),
  version = "latest",
  key = NULL,
  delay = 2,
  force = FALSE,
  progress_name = "querying",
  verbose = TRUE
) {
  # assert arguments are valid
  assertthat::assert_that(
    is.numeric(x),
    assertthat::noNA(x),
    length(x) > 0,
    is.function(data_format),
    inherits(data_template, "data.frame"),
    assertthat::is.string(dir),
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
  ## convert to character ids
  x_char_ids <- withr::with_options(
    list(scipen = 9999),
    as.character(x)
  )

  # normalize file paths
  dir <- normalize_path(dir, mustWork = FALSE)
  assertthat::assert_that(assertthat::is.writeable(dir))

  # find version of data
  if (identical(version, "latest")) {
    iucn_rl_version <- rredlist::rl_version()
  } else {
    iucn_rl_version <- version
  }

  # create file path for caching data
  file_path <- file.path(
    dir, paste0("iucn-red-list-data-", iucn_rl_version, ".rds")
  )
  if (!identical(version, "latest")) {
    assertthat::assert_that(
      file.exists(file_path),
      msg = paste0(
        "can't find previously downloaded data for `version = \"", version,
        "\"` at the location `dir`"
      )
    )
  }

  # access cached data
  if (file.exists(file_path) && !isTRUE(force)) {
    iucn_rl_data <- readRDS(file_path)
  } else {
    iucn_rl_data <- list()
  }

  # update cache if needed
  if (any(!x_char_ids %in% names(iucn_rl_data))) {
    ## specify ids that need downloaded
    idx <- which(!x_char_ids %in% names(iucn_rl_data))
    api_ids <- x[idx]
    api_char_ids <- x_char_ids[idx]
    ## download data
    if (verbose) {
      cli::cli_progress_bar(progress_name, total = length(api_ids))
    }
    api_results <- list()
    {for (i in seq_along(api_ids)) {
      ### wait as need
      Sys.sleep(delay)
      ### attempt to download data
      api_results[[i]] <- try(
        rredlist::rl_sis_latest(id = api_ids[[i]], key = key, parse = TRUE),
        silent = TRUE
      )
      ### update progress bar
      if (verbose) cli::cli_progress_update()
    }}
    if (verbose) cli::cli_progress_done()

    ## determine which api calls were successful
    api_success <- !vapply(api_results, inherits, logical(1), "try-error")

    ## save data for successfully completed species
    if (any(api_success)) {
      ### update iucn red list data with new results
      iucn_rl_data[api_char_ids[api_success]] <- api_results[api_success]
      ### save results
      saveRDS(iucn_rl_data, file_path, version = 3, compress = "gzip")
    }

    ## verify that the failed API calls were because no results
    ## were returned for the query and not due to other errors
    ## (e.g., API was down or no internet connection)
    # nocov start
    if (any(!api_success)) {
      ### check to see if any invalid failures
      is_valid_failure <- vapply(
        api_results[!api_success], FUN.VALUE = logical(1), function(x) {
          all(grepl("No results returned for query.", x, fixed = TRUE))
        }
      )
      ### throw error if needed
      if (any(!is_valid_failure)) {
        stop(
          paste(
            "Failed to successfully query IUCN Red List API,",
            "please check that the API is online and your internet connection",
            "and try again."
          ),
          call. = FALSE
        )
      }
    }
    # nocov end

  } else {
    ## set api_success to TRUE
    api_success <- TRUE
  }

  # if there is any IUCN data in cache, format it
  if (length(iucn_rl_data) > 0) {
    ## data from cache
    out <- try(
      Map(
        data_format,
        iucn_rl_data,
        as.numeric(names(iucn_rl_data))
      ),
      silent = TRUE
    )
    ## throw error if failed
    if (inherits(out, "try-error")) {
      # nocov start
      cli::cli_abort(
        "failed to process IUCN Red List data",
        .internal = TRUE
      )
      # nocov end
    }
  } else {
    ## initialize as empty list
    out <- list() # nocov
  }

  ## if there are any id_no with failed calls, then add elements for them
  if (!all(api_success)) {
    out <- append(
      out,
      Map(
        data_format,
        list(list())[rep(1, sum(!api_success))],
        id_no = api_ids[!api_success]
      )
    )
  }

  # format data
  out <- try(
    lapply(out, format_api_data, data_template),
    silent = TRUE
  )
  if (inherits(out, "try-error")) {
    # nocov start
    cli::cli_abort(
      "failed to format IUCN Red List data",
      .internal = TRUE
    )
    # nocov end
  }

  # re-order data to follow the same order as x
  out <- dplyr::left_join(
    tibble::tibble(id_no = x),
    dplyr::bind_rows(out),
    multiple = "all",
    by = c("id_no")
  )

  # return data
  out
}

#' Format API data
#'
#' Format a `data.frame` object to follow the format of a template.
#'
#' @param x `data.frame` object.
#'
#' @param data_template `data.frame` template object.
#'
#' @return A `data.frame` object.
#'
#' @noRd
format_api_data <- function(x, data_template) {
  # assert valid arguments
  assertthat::assert_that(
    is.data.frame(x),
    is.data.frame(data_template)
  )

  # format columns
  for (j in setdiff(names(data_template), "id_no")) {
    if (j %in% names(x)) {
      x[[j]] <- methods::as(x[[j]], class(data_template[[j]]))
    } else {
      x[[j]] <- methods::as(NA, class(data_template[[j]]))
    }
  }

  # re-order columns and return result
  x[, c("id_no", names(data_template)), drop = FALSE]
}
