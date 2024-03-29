% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mully_import.R
\name{importGraphCSV}
\alias{importGraphCSV}
\title{Import a mully graph from CSV files}
\usage{
importGraphCSV(name = NA, direct = FALSE, layers, nodes, edges)
}
\arguments{
\item{name}{The name of the graph}

\item{direct}{A boolean to indicate if the graph is directed or not}

\item{layers}{The path to the CSV file containing the layers' information}

\item{nodes}{The path to the CSV file containing the nodes' information}

\item{edges}{The path to the CSV file containing the edges' information}
}
\value{
A new mully graph
}
\description{
Import a mully graph from CSV files
}
