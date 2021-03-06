% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_functions.R
\name{estimate_sd}
\alias{estimate_sd}
\title{Estimate standard deviation from mean, min, and max}
\usage{
estimate_sd(mean, min, max, num_sds = 3)
}
\arguments{
\item{mean}{The mean}

\item{min}{The minimum}

\item{max}{The standard deviation}

\item{num_sds}{Number of standard deviations before reaching min/max}
}
\value{

}
\description{
Estimate standard deviation from mean, min, and max
}
\examples{
# estimate sd when a distribution has a mean of 10, min of 5, max of 10.
# this will use 3 stanard deivations to cover the range
estimate_sd(10, 5, 15)
# this will use 2 stanard deivations to cover the range
estimate_sd(10, 5, 15, 2)
}
