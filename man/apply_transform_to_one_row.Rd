% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reading_from_mplus.R
\name{apply_transform_to_one_row}
\alias{apply_transform_to_one_row}
\alias{transform_to_one_row}
\title{Reading results from estimated Mplus models}
\usage{
apply_transform_to_one_row(x, what)

transform_to_one_row(x, what)
}
\arguments{
\item{x}{a data frame or a list of data frames}

\item{what}{a character vector indicating which statistics - to be chosen
from the set: \code{"est"}, \code{"se"},  \code{"est_se"} and \code{"pval"} -
should be included in the resulting data frames}
}
\value{
A data frame with only one row and often many columns. Name of
columns are created by concatenating names of columns that do not store
statistics using a dot (".") as a separator and then adding statistic name
separated by "__".
}
\description{
Transforms data frames with model parameters in \emph{long}
format to a format with only one row and each parameter-statistic put into
a separate column.
}
