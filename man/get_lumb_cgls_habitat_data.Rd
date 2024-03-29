% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_lumb_cgls_habitat_data.R
\name{get_lumb_cgls_habitat_data}
\alias{get_lumb_cgls_habitat_data}
\title{Get Lumbierres \emph{et al.} (2021) CGLS habitat classification data}
\usage{
get_lumb_cgls_habitat_data(
  dir = tempdir(),
  version = "latest",
  force = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{dir}{\code{character} Folder path for downloading and caching data.
By default, data are downloaded to a temporary directory (i.e.,
\code{tempdir()}). \strong{To avoid downloading the same data multiple times, it is
strongly recommended to specify a persistent storage location (see Examples
below).}}

\item{version}{\code{character} Value indicating the specific version of the
dataset
that should be downloaded. The version should be indicated using the
Digital Object Identifier of the specific version required (e.g.
\code{"10.5281/zenodo.3673586"}).
Defaults to \code{"latest"} such that the latest
release of the dataset with available habitat data is used.}

\item{force}{\code{logical} Should the data be downloaded even if the
the data are already available?
Defaults to \code{FALSE}.}

\item{verbose}{\code{logical} Should progress be displayed while downloading
data?
Defaults to \code{TRUE}.}
}
\value{
A \code{\link[terra:rast]{terra::rast()}} object containing the habitat data
(100 m resolution). Pixel values indicate the habitat classification codes.
}
\description{
Import habitat classification data derived from the
Copernicus Global Land Service Land Cover
(CGLS-LC100) dataset (Buchhorn \emph{et al.}, 2019; Buchhorn \emph{et al.}, 20200)
following Lumbierres \emph{et al.} (2021).
If data are not available locally, they are downloaded from
a Zenodo repository (\doi{10.5281/zenodo.6622059}).
}
\details{
The data were produced by obtaining the level 1 habitat
classification data (derived from Lumbierres \emph{et al.} 2021),
and resampling the data (using nearest neighbor
interpolation) to the World Behrmannn coordinate reference
system (ESRI:54017).
}
\examples{
\dontrun{
# define persistent storage location
download_dir <- rappdirs::user_data_dir("aoh")

# create download directory if needed
if (!file.exists(download_dir)) {
  dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
}

# download and import habitat data
habitat_data <- get_lumb_cgls_habitat_data(download_dir, version = "latest")

# preview data
print(habitat_data)

# plot data
plot(habitat_data)
}
}
\references{
Buchhorn M, Smets B, Bertels L, Lesiv M, Tsendbazar N-E, Herold M, and Fritz
SA (2019) Copernicus Global Land Service: Land Cover 100m:
Epoch 2015: Globe. \emph{Dataset of the global component of the Copernicus Land
Monitoring Service}.
\doi{10.5281/zenodo.3243508}

Buchhorn M, Smets B, Bertels L, de Roo B, Lesiv M, Tsendbazar N-E, Linlin
L, and Tarko A (2020) \emph{Copernicus Global Land Service: Land Cover
100m: Version 3 Globe 2015–2019: Product User Manual}. Geneve: Zenodo.
\doi{10.5281/zenodo.3606295}

Lumbierres M, Dahal PR, Di Marco M, Butchart SHM, Donald PF, and
Rondinini C (2021) Translating habitat class to land cover to map area of
habitat of terrestrial vertebrates. \emph{Conservation Biology}, 36, e13851.
\doi{10.1111/cobi.13851}
}
\seealso{
See \code{\link[=crosswalk_lumb_cgls_data]{crosswalk_lumb_cgls_data()}} for details on which grid values correspond
to which habitat classification codes.
}
