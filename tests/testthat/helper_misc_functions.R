is_on_local <- function() {
  !identical(Sys.getenv("CI"), "true") &&
  identical(Sys.getenv("NOT_CRAN"), "true")
}

extract_cache_doi <- function(x) {
  if (!file.exists(rappdirs::user_data_dir("aoh"))) {
    stop("cache directory doesn't exist")
  }
  v <- dir(
    rappdirs::user_data_dir("aoh"), x, full.names = TRUE, recursive = TRUE
  )
  if (length(v) == 0) {
    stop("data not found in cache directory")
  }
  v <- basename(dirname(v))
  v <- gsub("_", "/", v, fixed = TRUE)
  sort(v, decreasing = TRUE)[[1]]
}
