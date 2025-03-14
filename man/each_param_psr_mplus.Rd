% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MplusHelpers.R
\name{each_param_psr_mplus}
\alias{each_param_psr_mplus}
\title{Various versions of PSR}
\usage{
each_param_psr_mplus(parameters, id.parameter, iterations.range = NULL)
}
\arguments{
\item{parameters}{A 3-dimensional array (typically taken from Mplus-produced gh5 file in gh5$bayesian_data$parameters_autocorr$parameters or from any other), where first dimension is parameters, second dimension is iterations, and third dimension is chains}

\item{id.parameter}{Integer id of parameter}

\item{iterations.range}{Range of iterations to use. All available are used by default (NULL).}
}
\description{
Various versions of PSR
}
\details{
Returns a vector of different versions of PSR:
\itemize{
    \item  "Rstan" the most up-to-date, provided by the  \code{\link[Rstan]{Rhat}} function (Vehtari et al., 2019).
    \item  "Gelman" (Gelman et al. (2004)).
    \item  "Mplus" (Asparouhov and  Muthen, 2010),
    \item  "Naive" which is a raw ratio of sum of a between- and within-chain variances to a within-chain variance.
}
}
\examples{

\dontrun{
m <- MplusAutomation::readModels("mybayes.out")
eachParamPSRMplus(a$gh5$bayesian_data$parameters_autocorr$parameters, 1)
}

}
