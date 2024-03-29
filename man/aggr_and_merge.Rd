% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multilevel_helpers.R
\name{aggr_and_merge}
\alias{aggr_and_merge}
\title{Creates country level variables by aggregating individual level variables}
\usage{
aggr_and_merge(
  variables,
  group,
  data,
  FUN = "mean",
  prefix = paste0(group, ".")
)
}
\arguments{
\item{variables}{Variable to be aggregated.}

\item{group}{Grouping variable.}

\item{data}{A dataset with individual-level data.}

\item{FUN}{Aggregation function, mean by default.}

\item{prefix}{Character string, what should be added to the  names of aggregated variables.}
}
\description{
Creates country level variables by aggregating individual level variables
}
