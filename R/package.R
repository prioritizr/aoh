#' aoh: Create Area of Habitat Data
#'
#' Area of Habitat (AOH) maps aim to delineate the spatial distribution of
#' suitable habitat for a species (Brooks *et al.* 2019). They are used to help
#' understand the impacts of habitat loss on species, and prioritize areas for
#' conservation (e.g., Tracewski *et al.* 2016; Rondinini *et al.* 2005).
#' These maps are generally produced by obtaining geographic range data for a
#' species, and then removing areas that do not contain suitable habitat or
#' occur outside the known elevational limits for the species
#' (Brooks *et al.* 2019). To help make these maps accessible, the package
#' provides routines for automatically creating Area of Habitat data based on
#' the [International Union for Conservation of Nature (IUCN) Red List of
#' Threatened Species](https://www.iucnredlist.org/). After manually downloading
#' species range data from the
#' [IUCN Red List](https://www.iucnredlist.org/resources/spatial-data-download),
#' users can import them (using [read_spp_range_data()]), prepare them and
#' collate additional information for subsequent processing (using
#' [create_spp_info_data()]), and then create Area of Habitat data
#' (using [create_spp_aoh_data()]). Global elevation and habitat classification
#' data (Jung *et al.* 2020a,b; Lumbierres *et al. 2021; Robinson *et al.*
#' 2014) are automatically downloaded, and data on species'
#' habitat preferences and elevational limits are obtained automatically using
#' the [IUCN Red List API](https://api.iucnredlist.org/).
#' Since accessing the IUCN Red List API requires a token, users may need to
#' [obtain a token](https://api.iucnredlist.org/)
#' and update their R configuration to
#' recognize the token (see instructions below for details).
#'
#' @section Accessing the IUCN Red List API:
#'
#' You will need to obtain a token for the
#' [IUCN Red List API](https://api.iucnredlist.org/) (if you do not have one
#' already). To achieve this, please visit the IUCN API website
#' (<https://api.iucnredlist.org/>), click the "Generate a token" link at the
#' top of the web page, and fill out the form to apply for a token.
#' You should then receive a token shortly after completing
#' the form (but not immediately). After receiving a token, please open the
#' `.Renviron` file on your computer (e.g., using `usethis::edit_r_environ()`).
#' Next, please add the following text to the file (replacing the string with
#' the token) and save the file:
#'
#' ```
#' IUCN_REDLIST_KEY="your_actual_token_not_this_string"
#' ```
#'
#' Please restart your R session. You should now be able to access the IUCN Red
#' List API. To verify this, please try running the following _R_ code and --
#' assuming everything works correctly -- you should see `TRUE` as the output:
#'
#' ```
#' # verify access to IUCN Red List API
#' is_iucn_rl_available()
#' ```
#'
#' If these instructions did not work, please consult the documentation for the
#' \pkg{rredlist} package for further details.
#'
#' @references
#' Brooks TM, Pimm SL, Akçakaya HR, Buchanan GM, Butchart SHM, Foden W,
#' Hilton-Taylor C, Hoffmann M, Jenkins CN, Joppa L, Li BV, Menon V,
#' Ocampo-Peñuela N, Rondinini C (2019) Measuring terrestrial Area of Habitat
#' (AOH) and its utility for the IUCN Red List. *Trends in Ecology & Evolution*,
#' 34, 977--986. \doi{10.1016/j.tree.2019.06.009}
#'
#' Jung M, Dahal PR, Butchart SHM, Donald PF, De Lamo X, Lesiv M, Kapos V,
#' Rondinini C, and Visconti P (2020a) A global map of
#' terrestrial habitat types. *Scientific data*, 7, 1--8.
#' \doi{10.1038/s41597-020-00599-8}
#'
#' Jung M, Dahal PR, Butchart SHM, Donald PF, De Lamo X, Lesiv M, Kapos V,
#' Rondinini C, and Visconti P (2020b) A global map of
#' terrestrial habitat types (insert version) \[Data set\].
#' *Zenodo*.
#' \doi{10.5281/zenodo.4058819}
#'
#' Lumbierres M, Dahal PR, Di Marco M, Butchart SHM, Donald PF, and
#' Rondinini C (2021) Translating habitat class to land cover to map area of
#' habitat of terrestrial vertebrates. *Conservation Biology*, 36, e13851.
#' \doi{10.1111/cobi.13851}
#'
#' Robinson N, Regetz J, and Guralnick RP (2014) EarthEnv-DEM90: A nearly-
#' global, void-free, multi-scale smoothed, 90m digital elevation model from
#' fused ASTER and SRTM data.
#' *ISPRS Journal of Photogrammetry and Remote Sensing*, 87:57--67.
#' \doi{10.1016/j.isprsjprs.2013.11.002}
#'
#' Rondinini C, Stuart S, Boitani L (2005) Habitat suitability models and the
#' shortfall in conservation planning for African vertebrates.
#' *Conservation Biology*, 19, 1488--1497.
#' \doi{10.1111/j.1523-1739.2005.00204.x}
#'
#' Tracewski Ł, Butchart SHM, Di Marco M, Ficetola GF, Rondinini C, Symes A,
#' Wheatley H, Beresford AE, Buchanan GM (2016) Toward quantification of the
#' impact of 21st‐century deforestation on the extinction risk of terrestrial
#' vertebrates. *Conservation Biology*, 30, 1070--1079.
#' \doi{10.1111/cobi.12715}
#'
#' @name aoh
#' @docType package
#' @aliases aoh-package
"_PACKAGE"
NULL

#' @import sf
#' @import terra
#' @importFrom rlang .data
#' @importFrom tidyselect any_of
NULL

# ensure package checks pass
#' @importFrom rappdirs app_dir
#' @importFrom parallel detectCores
NULL
