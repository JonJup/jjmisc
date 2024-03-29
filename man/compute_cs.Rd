% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_cs.R
\name{compute_cs}
\alias{compute_cs}
\title{Compute classification strength}
\usage{
compute_cs(dist, grouping, season, typology)
}
\arguments{
\item{dist}{A distance matrix}

\item{grouping}{character vector with group membership of sites}

\item{season}{character, for which season is the run? The value is supplied as value for the output table.}

\item{typology}{character, for this typology is this test run? The value is supplied as value for the output table.}
}
\value{
data.table
}
\description{
Compute classification strength
}
