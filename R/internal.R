# alias for roxygen2:::`%||%`
`%||%` <- function(a, b) {
  if (length(a) > 0) {
    return(a)
  } else {
    return(b)
  }
}

#' Normalize file path
#'
#' Normalize a file path to ensure cross-platform compatibility.
#'
#' @param path `character` File path.
#'
#' @param winslash `character` File path separator for use on Windows systems.
#'  Defaults to `"\\"`.

#' @param mustWork `logical` Should the argument to `path` resolve to
#'  a file that already exists?
#'  If `TRUE`, then an error is thrown if the path does not exist.
#'  Defaults to `NA`, such that a warning is thrown if the path does not exist.
#'
#' @details
#' This function is used to normalize file paths to ensure cross-platform
#' consistency. Although [normalizePath()] can help achieve this, it does
#' not fix files paths on Windows systems that contain the text `"~.1"`
#' used to shorten long file paths.
#'
#' @return A `character` file path.
#'
#' @noRd
normalize_path <- function(path, winslash = "\\", mustWork = NA) {
  # assert arguments are valid
  assertthat::assert_that(
    assertthat::is.string(path),
    assertthat::noNA(path),
    assertthat::is.string(winslash),
    assertthat::noNA(winslash),
    assertthat::is.flag(mustWork)
  )

  # normalize the file path
  if (identical(.Platform$OS.type, "unix")) {
    # if on a Unix platform, then we just need to use normalizePath()
    out <- normalizePath(path, winslash = winslash, mustWork = mustWork)
  } else {
    # if on a Windows platform, then things get a bit more complicated...
    if (!grepl("~", path, fixed = TRUE)) {
      # if the file path doesn't contain a "~" symbol,
      # then we only need to use normalizePath()
      out <- normalizePath(path, winslash = winslash, mustWork = mustWork)
    } else {
      # if it does, then we need to normalize the directory and file names
      # separately
      out <- file.path(
        normalizePath(dirname(path), winslash = winslash, mustWork = mustWork),
        basename(path)
      )
      # normalize the path again, in case file.path() uses wrong slash
      out <- normalizePath(out, winslash = winslash, mustWork = mustWork)
    }
  }

  # return result
  out
}
