extract_cache_doi <- function(x) {
  v <- dir(
    rappdirs::user_data_dir("aoh"), x, full.names = TRUE, recursive = TRUE
  )
  if (length(v) == 0) return(NULL)
  v <- basename(dirname(v))
  v <- gsub("_", "/", v, fixed = TRUE)
  v
}
