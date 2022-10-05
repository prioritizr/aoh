Dear CRAN volunteers,

Thank you for reviewing this submission. This is the first submission for the aoh package. Briefly, the package provides automated routines for processing Area of Habitat (AOH) data.

Cheers,

Jeff

# Test environments

* [Ubuntu 20.04, R-release](https://github.com/prioritizr/aoh/actions?query=workflow%3AUbuntu)
* [Ubuntu 20.04, R-devel](https://github.com/prioritizr/aoh/actions?query=workflow%3AUbuntu)
* [Mac OSX 10.15, R-release](https://github.com/prioritizr/aoh/actions?query=workflow%3A%22Mac+OSX%22)
* [macOS 11.5.2 (arm64), R-release (macOS builder)](https://mac.r-project.org/macbuilder/submit.html)
* [Windows Server 2019, R-release](https://github.com/prioritizr/aoh/actions?query=workflow%3AWindows)
* [Windows Server 2008 (x64), R-devel (Win-Builder)](https://win-builder.r-project.org/)

# R CMD check results

0 errors | 0 warnings | 3 notes

# Package check notes

* Possibly misspelled words in DESCRIPTION:
    al (6:66, 10:42, 12:19, 13:33)
    et (6:63, 10:39, 12:16, 13:30)
    IUCN (8:73)
    Lumbierres (12:5)

  **I can confirm that these words are not misspelled.**

* Suggests or Enhances not in mainstream repositories:
  prepr

  **The prepr R package is an optional dependency that is available on GitHub (<https://github.com/dickoa/prepr>). Instructions for installing the prepr R package are provided in the DESCRIPTION file and the package README file.**

* Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1111/2041-210X.13427
    From: README.md
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1111/cobi.12715
    From: README.md
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1111/cobi.13851
    From: README.md
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1111/j.1523-1739.2005.00204.x
    From: README.md
    Status: 503
    Message: Service Unavailable

  **I can confirm that all of these URLs are valid. They correspond to the following journal articles: Durán et al. 2020 (https://doi.org/10.1111/2041-210X.13427), Lumbierres et al. 2022 (https://doi.org/10.1111/cobi.13851), and Rondinini et al. 2005 (https://doi.org/10.1111/j.1523-1739.2005.00204.x).**

* Found the following (possibly) invalid DOIs:
    DOI: 10.1111/cobi.13851
      From: DESCRIPTION
      Status: Service Unavailable
      Message: 503

  **I can confirm that this DOI is valid. It corresponds to Lumbierres et al. (2022).**

# Downstream dependencies

There are no existing packages that depend on this package.
