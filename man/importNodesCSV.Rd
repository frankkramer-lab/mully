% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mully_import.R
\name{importNodesCSV}
\alias{importNodesCSV}
\title{Import Nodes to a mully graph from a CSV file}
\usage{
importNodesCSV(g, file, name = "name")
}
\arguments{
\item{g}{The mully graph to which the nodes will be added. The graph should already have the layers.}

\item{file}{The path to the CSV file containing the nodes' information}

\item{name}{The name of the column containing the names of the nodes}
}
\value{
The mully graph with the added nodes
}
\description{
Import Nodes to a mully graph from a CSV file
}
