% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findOptimum.R
\name{findOptimum}
\alias{findOptimum}
\title{Find the optimum performance and the temperature at which it occurs
within a given interval for a list of fits (using their AIC-weighted
average)}
\usage{
findOptimum(fitList, interval)
}
\arguments{
\item{fitList}{List of fits, output from fitModels()}

\item{interval}{Vector determining the range in which optimum is searched
for (e.g. c(15, 40))}
}
\value{
Named vector with optimum performance and temperature
}
\description{
Find the optimum performance and the temperature at which it occurs
within a given interval for a list of fits (using their AIC-weighted
average)
}
