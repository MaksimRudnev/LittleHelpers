% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphs.R
\name{random_interaction}
\alias{random_interaction}
\title{Plot cross-level interactions for mer objects}
\usage{
random_interaction(
  x,
  real = TRUE,
  z.levels = c("1SD", "2SD", "1.2SD", "all"),
  scatter = TRUE,
  labs = NULL,
  x.levels = NULL,
  silent = T,
  ...
)
}
\arguments{
\item{x}{A mer object produced by \code{lmer} function}

\item{real}{Logical. Whether real groups should be used to predict random effects, or extrapolate using means and standard deviations?}

\item{z.levels}{String, indicating what levels of moderating group-level variable should be computed. Can take (only) the following values:
\itemize{
\item "1SD". Default. Computes group-level mean and standard deviation of moderating variable and finds groups with similar values, so prediction of slopes and values is made based on three real  groups.
\item "1SD" Computes group-level mean and standard deviation of moderating variable.
\item "2SD" Computes group-level mean and doubled standard deviation of moderating variable.
\item "12SD" Computes group-level mean and plots both standard deviation and doubled standard deviation of moderating variable.
}}

\item{scatter}{Logical. Should scatterplot be created in addition?}

\item{labs}{List of length equal to a number of cross-level interactions, each element should contain sublist with names "x", "y", "line1", "line2", "line3", optionally "line4", "line5", "caption".}

\item{x.levels}{desired x-axis coordinates, individual-level term. If NULL (default) are defined automatically.}

\item{...}{Arguments passed to \code{effect} function of \code{effects} package.}
}
\value{
Returns one or several ggplots. In case one plot is returned it can be appended with \code{+theme()}, \code{+geom_()}, etc.
}
\description{
Takes object produced by lme4::lmer() function, and returns interaction plot(s).
}
\details{
It somehow repeats functionality of 'sjPlot::sjp.int', but differs in being able to select real groups close to +/- 1 sd and mean of moderating variable; makes prettier and customizable plots.
}
\examples{
random_interaction(lmr, "model.1SD.2SD")
}
\seealso{
\link{random_plot} \link{good_table}
}
