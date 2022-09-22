iucn_names <- c(
  "id_no", "binomial", "presence", "origin", "seasonal", "compiler",
  "yrcompiled", "citation", "subspecies", "subpop", "source", "island",
  "tax_comm", "dist_comm", "generalisd", "legend", "kingdom", "phylum",
  "class", "order_", "family", "genus", "category", "marine", "terrestial",
  "freshwater", "geometry"
)

birdlife_names <- c(
  "SISID", "presence", "origin", "seasonal", "geometry"
)

alt_birdlife_names <- c(
  "id_no", "presence", "origin", "seasonal", "geometry"
)

cleaned_names <- c(
  "aoh_id", "id_no", "category", "binomial", "subspecies",
  "seasonal", "kingdom", "phylum", "class", "order",
  "genus", "geometry"
)

aoh_names <- c(
  "id_no", "binomial", "category", "migratory", "seasonal", 
  "full_habitat_code", "habitat_code",
  "elevation_lower", "elevation_upper",
  "xmin", "xmax", "ymin", "ymax",
  "path", "geometry"
)

info_names <- c(
  "id_no", "binomial", "category", "migratory", "seasonal",
  "full_habitat_code",
  "elevation_lower", "elevation_upper",
  "geometry"
)

latest_jung_version <- try(
  extract_cache_doi("jung-lvl1"), silent = TRUE
)
latest_jung_potential_version <- try(
  extract_cache_doi("jung-plvl1"), silent = TRUE
)
latest_lumb_cgls_version <- try(
  extract_cache_doi("lumbierres-"), silent = TRUE
)
latest_elevation_version <- try(
  extract_cache_doi("dem-"), silent = TRUE
)
