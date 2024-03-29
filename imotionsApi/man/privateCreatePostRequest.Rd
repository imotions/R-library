% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imotionsApi.R
\name{privateCreatePostRequest}
\alias{privateCreatePostRequest}
\title{Create headers specific to the data format (signals, events, exports).}
\usage{
privateCreatePostRequest(params, study, sampleName, fileName)
}
\arguments{
\item{params}{The list of parameters provided to the script - specific parameters value will be stored as metadata.}

\item{study}{An imStudy object as returned from \code{\link{imStudy}}.}

\item{sampleName}{The name of the new sensor/metrics/events/export you would like to create. In case of export,
this should correspond to the name of the file created (a path can also be provided).}
}
\description{
Create headers specific to the data format (signals, events, exports).
}
\keyword{internal}
