% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/temporal_aggregation.R
\name{temporal_aggregation}
\alias{temporal_aggregation}
\title{Aggregate samples from the same sampling site and season}
\usage{
temporal_aggregation(data, sites, id_append, debug = FALSE)
}
\arguments{
\item{data}{data.table}

\item{sites}{data.table}

\item{id_append}{character}

\item{debug}{boolean}
}
\value{
data.table
}
\description{
Debug mode currently requires tmap package to be loaded which is technically no dependency, i.e. not automatically loaded.
}
\examples{

}
