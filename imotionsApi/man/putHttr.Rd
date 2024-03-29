% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imotionsApi.R
\name{putHttr}
\alias{putHttr}
\title{Perform a PUT HTTP request with authentication (if needed).}
\usage{
putHttr(connection, url, fileName = NULL, reqBody = NULL, message = NULL)
}
\arguments{
\item{connection}{An imConnection object as returned from \code{\link{imConnection}}.}

\item{url}{The url/path where the PUT request need to be send.}

\item{fileName}{Optional - the name of the file to upload if any.}

\item{reqBody}{Optional - the body in case of AOI metrics for example.}

\item{message}{Optional - a short message indicating which steps are getting performed to get a more indicative
error message.}
}
\description{
The request will be retried up to 3 times if an error is encountered.
}
\keyword{internal}
