# alias for roxygen2:::`%||%`
`%||%` <- function(a, b) {
  if (length(a) > 0) {
    return(a)
  } else {
    return(b)
  }
}
