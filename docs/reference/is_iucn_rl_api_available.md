# Is IUCN Red List API available?

The International Union for Conservation of Nature (IUCN) provides an
[API to access data from Red List of Threatened
Species](https://api.iucnredlist.org/). This function checks whether
data can be accessed from the API. Please note that a token is required
to access the API (see below for instructions to obtain a token).

## Usage

``` r
is_iucn_rl_api_available(key = NULL, n = 5)
```

## Arguments

- key:

  `character` Token for querying the IUCN Red List API. Defaults to
  `NULL` such that the token accessed from the `"IUCN_REDLIST_KEY"`
  environmental variable (which can be specified in the .Renviron file).

- n:

  `integer` Number of times to attempt to access the API.

## Value

A `logical` indicating if the IUCN Red List API can be accessed.

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

## Examples

``` r
# \dontrun{
# check if IUCN Red List API is available?
is_iucn_rl_api_available()
#> [1] TRUE
# }
```
