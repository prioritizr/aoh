#' Get species data from the IUCN Red List API
#'
#' Import species data obtained from the International Union
#' for Conservation of Nature (IUCN) Red List of Threatened
#' Species (<https://www.iucnredlist.org/>).
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
#'
#' # preview result (only if running R in an interactive session)
#' if (interactive()) {
#'   print(result)
#' }
#' }
#' @noRd
get_spp_api_data <- function(x, api_function, data_prefix, data_template,
                         dir = tempdir(), version = "latest",
                         key = NULL, delay = 2, force = FALSE,
                         verbose = TRUE) {
  # assert arguments are valid
  # TODO

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
    iucn_rl_data <- tibble::as_tibble(
      data.table::fread(file_path, data.table = FALSE, sep = ",")
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
    api_results <- plyr::llply(
      .data = api_ids,
      .progress = ifelse(verbose, "text", "none"),
      .fun = function(x) {
        # wait as need
        Sys.sleep(delay)
        # attempt to download data
        api_function(id = x, key = key)$result
    })
    ## determine which api calls were successful
    api_success <- vapply(api_results, length, integer(1)) > 0
    ## append data to cache
    if (any(api_success)) {
      iucn_rl_data <- dplyr::bind_rows(
        iucn_rl_data,
        tibble::as_tibble(
          plyr::ldply(which(api_success), function(i) {
            # format data
            d <- api_results[[i]]$result
            for (j in setdiff(names(data_template), "id_no")) {
              if (j %in% names(d)) {
                d[[j]] <- as(d[[j]], class(data_template[[j]]))
              } else {
                d[[j]] <- as(NA, class(data_template[[j]]))
              }
            }
            # assign id_no column with taxon id
            out$id_no <- api_ids[[i]]
            # return result
            out[, names(iucn_rl_data), drop = FALSE]
          })
        )
      )
    }
    ## save cache if any data available
    if (nrow(iucn_rl_data) > 0) {
      data.table::fwrite(iucn_rl_data, file_path, sep = ",")
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
  assertthat::assert_that(all(x %in% iucn_rl_data$id_no))
  out <- dplyr::left_join(
    tibble::tibble(id_no = x), iucn_rl_data, by = c("id_no")
  )
  # return result
  out
}
