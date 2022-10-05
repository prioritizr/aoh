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

new_temp_dir <- function() {
  f <- tempfile()
  dir.create(f, showWarnings = FALSE, recursive = TRUE)
  f
}

standardize_geometry <- function(x) {
  x_crs <- sf::st_crs(x)
  sf::st_crs(x) <- sf::st_crs(NA)
  x <- sf::st_buffer(x, 0)
  sf::st_crs(x) <- x_crs
  x
}
