% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sigma_attrs.R
\name{add_edge_size}
\alias{add_edge_size}
\title{Modify the edge size of a sgraph object.}
\usage{
add_edge_size(sigma_obj, one_size = NULL)
}
\arguments{
\item{sigma_obj}{sgraph object}

\item{one_size}{A single size to use for all edges}
}
\value{
sgraph with modified edge sizes
}
\description{
Modify the edge size of a sgraph by providing a single size
}
\examples{
library(igraph)
library(sgraph)

data(lesMis)

sig <- sigma_from_igraph(igraph = lesMis) \%>\%
  add_edge_size(one_size = 5)
sig

}
