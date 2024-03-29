% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imotionsApi.R
\name{privateCreateMetadata}
\alias{privateCreateMetadata}
\title{Create metadata for the file that needs to be uploaded.}
\usage{
privateCreateMetadata(data, metadata = NULL)
}
\arguments{
\item{data}{A data.table containing the signals to upload (imData object are also accepted).}

\item{metadata}{Optional - a data.table with metadata information. Column names will be converted to metadata headers
and there must be a row corresponding to each data column.}
}
\description{
Create metadata for the file that needs to be uploaded.
}
\keyword{internal}
