% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imotionsApi.R
\name{getSensorsMetadata}
\alias{getSensorsMetadata}
\title{Get sensors specific metadata.}
\usage{
getSensorsMetadata(sensors)
}
\arguments{
\item{sensors}{An imSensorList object as returned from \code{\link{getSensors}}.}
}
\value{
A data.table with sensors metadata (one row by sensor).
}
\description{
Available sensors in your study can be listed using the \code{\link{getSensors}}.
}
\examples{
\dontrun{
connection <- imotionsApi::imConnection("xxxxxxxx")
studies <- imotionsApi::listStudies(connection)
study <- imotionsApi::imStudy(connection, studies$id[1])
respondents <- imotionsApi::getRespondents(study)
sensors <- imotionsApi::getSensors(study, respondents[1, ])
metadata <- imotionsApi::getSensorsMetadata(study, sensors)
}
}
