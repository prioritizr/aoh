skip_on_github_workflow <- function(x) {
  testthat::skip_if(
    identical(Sys.getenv("GITHUB_WORKFLOW"), x),
    paste("On", x))
}

skip_on_local <- function(x) {
  testthat::skip_if(!identical(Sys.getenv("CI"), "true"), "On local")
}

skip_if_iucn_key_missing <- function() {
  testthat::skip_if(
    nchar(Sys.getenv("IUCN_REDLIST_KEY")) == 0,
    "IUCN Red List API key not found"
  )
}

skip_if_local_and_slow_internet <- function(x) {
  testthat::skip_if_not_installed("pingr")
  x <- (mean(pingr::ping("www.google.com", count = 10)) > 10) &&
       !identical(Sys.getenv("CI"), "true")
  testthat::skip_if(x, paste("On local and slow internet"))
}

skip_if_iucn_red_list_data_not_available <- function(x) {
  dir <- rappdirs::user_data_dir("iucn-red-list-data")
  path <- file.path(dir, x)
  if (file.exists(path)) {
    return(TRUE)
  }
  if (!requireNamespace("piggyback")) {
    return(testthat::skip_if_not_installed("piggyback"))
  }
  if (!file.exists(dir)) {
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  }
  res <- try(
    piggyback::pb_download(
      file = x,
      dest = dir,
      tag = "latest",
      repo = "jeffreyhanson/iucn-red-list-data"
    ),
    silent = TRUE
  )
  if (inherits(res, "try-error")) {
    return(testthat::skip("Unable to access IUCN Red List data"))
  }
  TRUE
}
