% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{h_recal_from_results}
\alias{h_recal_from_results}
\title{Recalibrate results given their OD and calibrator values}
\usage{
h_recal_from_results(result, od, target_old, target_new)
}
\arguments{
\item{result}{vector of results}

\item{od}{vector of od (same length as results)}

\item{target_old}{vector of old calibrator values}

\item{target_new}{vector of new calibrator values}
}
\value{
a vector of recalibrated results
}
\description{
Given a vector of results and a vector of their respective OD, a vector of
old calibrator values and a vector of new calibrator values, calculate new
results by recalibrating with the new calibrator values. Only valid for
straight line (OLS regression) calibration.
}
