% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imotionsApi.R
\name{privateGetIntervalsForAnnotations}
\alias{privateGetIntervalsForAnnotations}
\title{Get time intervals for annotations.}
\usage{
privateGetIntervalsForAnnotations(study, respondent, stimuli)
}
\arguments{
\item{study}{An imStudy object as returned from \code{\link{imStudy}}.}

\item{respondent}{An imRespondent object as returned from \code{\link{getRespondents}}.}

\item{stimuli}{An imStimulusList object as returned from \code{\link{getStimuli}}.}
}
\value{
A data.table composed of the start, end, duration, parent stimulus id/name, id, name and comment of each
        annotation.
}
\description{
Return the start, end, duration, parent stimulus id/name, id, name and comment of each annotation.
Annotation with multiple fragments will be return as multiple intervals (start, end, duration) with same id and name.
}
\keyword{internal}
