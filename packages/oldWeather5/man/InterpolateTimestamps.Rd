% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/utils.R
\name{InterpolateTimestamps}
\alias{InterpolateTimestamps}
\title{Interpolate timestamps where needed}
\usage{
InterpolateTimestamps(classifications)
}
\arguments{
\item{classifications}{list from \code{\link{ReadClassifications}}.}
}
\value{
modified list with timestamps for all annotations.
}
\description{
Some annotations are missing a timestamp - provide them by interpolation
}
\details{
Gap between timestamps is smaller of 2 seconds and interpolation between
previous and next.
}

