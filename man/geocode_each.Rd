% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geocode_each.R
\name{geocode_each}
\alias{geocode_each}
\title{Geocodes one location through the Google API}
\usage{
geocode_each(stringlocation, info = c("latlng"))
}
\arguments{
\item{stringlocation}{a location string}

\item{info}{the variables to inform: by default, latlng. Information can be e.g. "locality","country","country_code","formatted_address". See https://developers.google.com/maps/documentation/geocoding/intro#Types for the list of all possibilities supported by the Google Map geocoding API.}
}
\value{
tibble with additional columns corresponding to the info required
}
\description{
Geocodes one location through the Google API
}
\examples{
register_google(key="[your key]")
geocode_each("la Guillotière, Lyon")
}