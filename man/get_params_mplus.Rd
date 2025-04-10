% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MplusHelpers.R
\name{get_params_mplus}
\alias{get_params_mplus}
\title{Extracts parameters from Bayesian models produces by Mplus where 'readModels' cannot help}
\usage{
get_params_mplus(file)
}
\arguments{
\item{file}{Name of the Mplus .out file.}
}
\value{
A data.frame with all parameters.
}
\description{
Extracts parameters from Bayesian models produces by Mplus where 'readModels' cannot help. Currently for latent class models only.
}
