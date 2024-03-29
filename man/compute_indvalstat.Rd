% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_indvalstat.R
\name{compute_indvalstat}
\alias{compute_indvalstat}
\title{Compute indval statitics}
\usage{
compute_indvalstat(community, grouping)
}
\arguments{
\item{community}{data.frame with sites as rows and taxa as columns}

\item{grouping}{character vector with group membership of sites}
}
\value{
data.table with number of statistically significant indicator taxa and the mean indicator value of statistically significant indicator taxa.
}
\description{
Internal function used to compute test statistics based on the indicator value.
Indicator values are computed with indicspecies::multipatt and 999 permutations.
}
