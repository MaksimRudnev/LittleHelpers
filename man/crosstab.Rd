% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general_helpers.R
\name{crosstab}
\alias{crosstab}
\title{Easy cross-tabulation with labels}
\usage{
crosstab(rows, cols, data, margin = "row", useNA = "always", drop.empty = T)
}
\arguments{
\item{rows}{Character, variable name to put in rows}

\item{cols}{Character, variable name to put in columns}

\item{data}{Data.frame containing variables}

\item{margin}{If any proportions should be computed, might be `row`, `col`, or `none`.}

\item{useNA}{How to deal with NAs, passed to `table`.}

\item{drop.empty}{Remove empty categories?}
}
\description{
Easy cross-tabulation with labels
}
\examples{
\dontrun{
   crosstab("country", "frequency", wvs6, "row")
}


}
