% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iucn_habitat_codes.R
\name{iucn_habitat_codes_introduced}
\alias{iucn_habitat_codes_introduced}
\title{IUCN Red List codes for introduced habitats}
\usage{
iucn_habitat_codes_introduced()
}
\value{
A \code{character} vector of codes.
}
\description{
The \href{https://www.iucnredlist.org/}{International Union for Conservation of Nature (IUCN) Red List of Threatened Species}
provides a
\href{https://www.iucnredlist.org/resources/habitat-classification-scheme}{habitat classification scheme}
for different habitat types worldwide.
This function returns codes for habitat classes that correspond to
\emph{introduced vegetation}.
}
\examples{
# print codes
print(iucn_habitat_codes_introduced())

}
\seealso{
Other codes: 
\code{\link{iucn_habitat_codes_artificial}()},
\code{\link{iucn_habitat_codes_marine}()},
\code{\link{iucn_habitat_codes_terrestrial}()}
}
\concept{codes}
