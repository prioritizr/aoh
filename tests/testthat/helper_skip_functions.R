skip_on_github_workflow <- function(x) {
  testthat::skip_if(
    identical(Sys.getenv("GITHUB_WORKFLOW"), x),
    paste("On", x)
  )
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

skip_if_iucn_api_not_available <- function() {
  skip_if_iucn_key_missing()
  testthat::skip_if_not(
    is_iucn_rl_api_available(),
    "IUCN Red List API not available"
  )
}

skip_if_local_and_slow_internet <- function(x) {
  testthat::skip_if_not_installed("pingr")
  x <- (mean(pingr::ping("www.google.com", count = 5)) > 2.5) &&
       !identical(Sys.getenv("CI"), "true")
  testthat::skip_if(x, paste("On local and slow internet"))
}

skip_if_iucn_red_list_data_not_available <- function(x) {
  dir <- rappdirs::user_data_dir("iucn-red-list-data")
  path <- file.path(dir, x)
  testthat::skip_if_not(
    file.exists(path),
    message = paste0("IUCN Red List data not available: \"", x, "\"")
  )
}

skip_if_gdal_calc_not_available <- function() {
  testthat::skip_if_not(
    suppressWarnings(is_gdal_calc_available()),
    message = "gdal_calc.py script not available"
  )
}

skip_if_grass_not_available <- function() {
  testthat::skip_if_not(
    suppressWarnings(is_grass_available()),
    message = "GRASS not available"
  )
}

skip_if_cached_data_not_available <- function() {
  testthat::skip_if_not(
    file.exists(rappdirs::user_data_dir("aoh")),
    message = "cached global data not available"
  )
}

skip_if_zenodo_data_not_available <- function(x) {
  testthat::skip_if(
    inherits(x, "try-error"),
    message = "Zenodo dataset not available"
  )
}

skip_if_zenodo_website_not_available <- function() {
  x <- try(rvest::read_html("https://zenodo.org/"), silent = TRUE)
  testthat::skip_if(
    inherits(x, "try-error"),
    message = "Zenodo website not available"
  )
}
