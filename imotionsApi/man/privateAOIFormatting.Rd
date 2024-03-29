% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imotionsApi.R
\name{privateAOIFormatting}
\alias{privateAOIFormatting}
\title{Private function to return the AOIs for a specific path/url.}
\usage{
privateAOIFormatting(study, AOIsUrl, endpoint)
}
\arguments{
\item{study}{An imStudy object as returned from \code{\link{imStudy}}.}

\item{AOIsUrl}{The path/url to the AOIs for this study/respondent/stimulus.}

\item{endpoint}{A string indicating which endpoint is being targeted.}
}
\value{
A data.table with AOIs.
}
\description{
Private function to return the AOIs for a specific path/url.
}
\keyword{internal}
