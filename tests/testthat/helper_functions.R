skip_on_github_workflow <- function(x) {
  testthat::skip_if(
    identical(Sys.getenv("GITHUB_WORKFLOW"), x),
    paste("On", x))
}

skip_if_local_and_slow_internet <- function(x) {
  testthat::skip_if_not_installed("pingr")
  x <- (mean(pingr::ping("www.google.com", count = 10)) > 10) &&
       !identical(Sys.getenv("CI"), "true")
  testthat::skip_if(x, paste("On local and slow internet"))
}
