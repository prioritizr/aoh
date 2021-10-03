#' @include internal.R
NULL

# define column names for IUCN shapefiles
iucn_column_names <- c(
  "id_no", "binomial", "presence", "origin", "seasonal", "compiler",
  "yrcompiled", "citation", "subspecies", "subpop", "source", "island",
  "tax_comm", "dist_comm", "generalisd", "legend", "kingdom", "phylum",
  "class", "order_", "family", "genus", "category", "marine",
  "freshwater"
)

# define assertion function
has_iucn_format_column_names <- function(x) {
  all(iucn_column_names %in% names(x)) &&
  c(("terrestial" %in% names(x)) || ("terrestrial" %in% names(x)))
}

# specify custom error message
assertthat::on_failure(has_iucn_format_column_names) <- function(call, env) {
  missing_names <- iucn_column_names[!iucn_column_names %in% names(env$x)]
  print(missing_names)
  if (!(any("terrestial", "terrestrial") %in% names(x))) {
    missing_names <- c(missing_names, "terrestial")
  }
  paste0(
    deparse(call$x), "is missing the following columns: ",
    paste(paste0("\"", missing_names, "\""), collapse = ", ")
  )
}
