% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bluehill_functions.R
\name{bh_label_samples}
\alias{bh_label_samples}
\title{Add header information to samples}
\usage{
bh_label_samples(samples, headers)
}
\arguments{
\item{samples}{A \code{data.table} of samples.}

\item{headers}{A \code{data.table} of sample headers.}
}
\value{
\code{samples} with added columns for \code{blank_row} and \code{label} for each specimen.
}
\description{
Copies the \code{blank_row} and \code{Specimen Label} parts from a \code{data.table} of specimen headers
and adds them to a \code{data.table} of samples. Will give NA if either (but not both) headers are missing.
It's probably better to do this explicitly in the main code, but using specimen_labels
which returns .(blank, label) as that is more readable than .(V1, V2).
}
