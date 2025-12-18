# Get species summary data

Import species summary data obtained from the [International Union for
Conservation of Nature (IUCN) Red List of Threatened
Species](https://www.iucnredlist.org/). Please note that a token is
required to download data from the [IUCN Red List
API](https://api.iucnredlist.org/) (see instructions below to obtain a
token).

## Usage

``` r
get_spp_summary_data(
  x,
  dir = tempdir(),
  version = "latest",
  key = NULL,
  delay = 2,
  force = FALSE,
  verbose = TRUE
)
```

## Arguments

- x:

  `integer` Taxon identifier for the species on the International Union
  for Conservation of Nature (IUCN) Red List of Threatened Species. For
  example, the taxon identifier for the species *Loxodonta africana* is
  `181008073`.

- dir:

  `character` Folder path where data should be downloaded. By default,
  data are downloaded to a temporary directory (i.e.,
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html)). **To avoid
  downloading the same data multiple times, it is strongly recommended
  to specify a persistent storage location (see Examples below).** If
  data are already available in folder for the specified version, then
  the data are imported and not re-downloaded from the IUCN Red List.

- version:

  `character` Value indicating version of the IUCN Red List for
  obtaining data (e.g., `"2021-2"`). Although it is not possible to
  query past versions of the IUCN Red List, this functionality is useful
  for accessing data previously downloaded from the IUCN Red List.
  Defaults to `"latest"` such that data are downloaded from the most
  recent version of the IUCN Red List.

- key:

  `character` Token for querying the IUCN Red List API. Defaults to
  `NULL` such that the token accessed from the `"IUCN_REDLIST_KEY"`
  environmental variable (which can be specified in the .Renviron file).

- delay:

  `integer` Number of seconds to wait between subsequent calls to the
  IUCN Red List API. Defaults to 2 seconds (as recommended by the
  rredlist package;
  <https://docs.ropensci.org/rredlist/articles/rredlist.html>).

- force:

  `logical` Should the data be downloaded even if the the data are
  already available? Defaults to `FALSE`.

- verbose:

  `logical` Should progress be displayed while downloading data?
  Defaults to `TRUE`.

## Value

A table
([`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html))
object.

## Details

Data are downloaded from the IUCN Red List using the
[`rredlist::rl_search()`](https://docs.ropensci.org/rredlist/reference/rl_search.html).
This function is essentially a wrapper designed to help download data
for multiple species and provide caching for previously downloaded data.

## Accessing the IUCN Red List API

You will need to obtain a token for the [IUCN Red List
API](https://api.iucnredlist.org/) (if you do not have one already). To
achieve this, please visit the IUCN API website
(<https://api.iucnredlist.org/>), click the "Generate a token" link at
the top of the web page, and fill out the form to apply for a token. You
should then receive a token shortly after completing the form (but not
immediately). After receiving a token, please open the `.Renviron` file
on your computer (e.g., using
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html)).
Next, please add the following text to the file (replacing the string
with the token) and save the file:

    IUCN_REDLIST_KEY="your_actual_token_not_this_string"

Please restart your R session. You should now be able to access the IUCN
Red List API. To verify this, please try running the following *R* code
and – assuming everything works correctly – you should see `TRUE` as the
output:

    # verify access to IUCN Red List API
    is_iucn_rl_api_available()

If these instructions did not work, please consult the documentation for
the rredlist package for further details.

## References

Please use the following citation for data obtained from the IUCN Red
List:

IUCN (insert year). IUCN Red List of Threatened Species. Version (insert
version). Available at \<www.iucnredlist.org\>.

To obtain the version number of the latest version, use
[`rredlist::rl_version()`](https://docs.ropensci.org/rredlist/reference/rl_version.html).

## Examples

``` r
# \dontrun{
# define species to download data for based on taxon identifiers
spp_ids <- c(18, 22694927)

# define persistent storage location
download_dir <- rappdirs::user_data_dir("aoh")

# create download directory if needed
if (!file.exists(download_dir)) {
  dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
}

# download and import summary data
result <- get_spp_summary_data(spp_ids, download_dir)
# }

if (FALSE) { # interactive()
# \dontrun{
# preview data
print(result)
# }
}
```
