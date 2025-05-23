% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lavaan_helpers.R
\name{lav_to_graph}
\alias{lav_to_graph}
\title{Create a Graphviz path diagram using lavaan syntax}
\usage{
lav_to_graph(
  m,
  layout = "dot",
  adds = NULL,
  file = NA,
  rmarkdown = FALSE,
  code.only = F,
  try.labels = T,
  label.wrap = 15,
  std = F,
  elements = c("obs", "lat.covs", "resid.covs", "resid", "slopes"),
  ...
)
}
\arguments{
\item{m}{character, following lavaan syntax model conventions (see examples), or fitted lavaan object.}

\item{layout}{Can be 'dot', 'neato', 'twopi', 'circo', or 'fdp'}

\item{adds}{Any graphviz code to be added to the graph.}

\item{file}{character, file name to save svg code, usually with 'svg' extension.}

\item{rmarkdown}{Logical. If the function used in the context of Rmarkdown.}

\item{try.labels}{Try extracting labels from the data.}

\item{label.wrap}{Number of character to wrap a label}

\item{std}{Whether include standardized or unstandardized estimates.}

\item{elements}{Character vector. What to include in the diagram, possible values "obs", "lat.covs", "resid.covs", "resid", "slopes", and "intercepts/thresholds" (currently not supported).}

\item{...}{arguments passed to DiagrammeR::grViz function.}
}
\value{
The function invisibly returns the dot code which can be edited to make more customized diagrams. You can use package \pkg{DiagrammeR} or any other graphviz engine, e.g. \url{http://graphviz.it}. It will most likely to be useful with large and complex models.
}
\description{
Converts lavaan syntax or object to graphviz code. Requires 'DiagrammeR' package.
}
\examples{
lav_to_graph("F =~ a1 + a2 + a3 + a4")
lav_to_graph("F =~ a1 + a2 + a3 + a4; a1 ~~ a2; d ~ F ", engine = "neato")
}
