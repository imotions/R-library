% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imotionsApi.R
\name{privateCalculateAreaAOI}
\alias{privateCalculateAreaAOI}
\title{Private function to compute the area of an AOI (adapted from pracma::polyarea).}
\usage{
privateCalculateAreaAOI(x, y)
}
\arguments{
\item{x}{The X coordinates of the polygon.}

\item{AOIsUrl}{The path/url to the AOIs for this study/respondent/stimulus.}

\item{endpoint}{A string indicating which endpoint is being targeted.}
}
\value{
A data.table with AOIs.
}
\description{
Private function to compute the area of an AOI (adapted from pracma::polyarea).
}
\keyword{internal}
