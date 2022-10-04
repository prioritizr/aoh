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

* Found the following (possibly) invalid DOIs:
    DOI: 10.1111/cobi.13851
      From: DESCRIPTION
      Status: Service Unavailable
      Message: 503

  **I can confirm that this DOI is valid. It corresponds to Lumbierres et al. (2022).**

# Downstream dependencies

There are no existing packages that depend on this package.
