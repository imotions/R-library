% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imotionsApi.R
\name{getUploadAoiMetricsUrl}
\alias{getUploadAoiMetricsUrl}
\title{Generic getUploadAoiMetricsUrl function that takes as parameter a study object, a respondent/segment object and
an AOI
.
Return the path/url to upload AOI metrics to this respondent/segment object.}
\usage{
getUploadAoiMetricsUrl(study, imObject, AOI)
}
\arguments{
\item{study}{An imStudy object as returned from \code{\link{imStudy}}.}

\item{imObject}{An imRespondent or imSegment object of interest.}

\item{AOI}{An imAOI object as returned from \code{\link{getAOIs}}.}
}
\description{
Generic getUploadAoiMetricsUrl function that takes as parameter a study object, a respondent/segment object and
an AOI
.
Return the path/url to upload AOI metrics to this respondent/segment object.
}
\keyword{internal}
