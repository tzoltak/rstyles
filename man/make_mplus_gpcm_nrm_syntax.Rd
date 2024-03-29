% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resp_styles_to_mplus.R
\name{make_mplus_gpcm_nrm_syntax}
\alias{make_mplus_gpcm_nrm_syntax}
\title{Preparing Mplus code to estimate RS models}
\usage{
make_mplus_gpcm_nrm_syntax(
  scoringColumn,
  latentTraitName,
  items,
  reverseCoded,
  itemCategories,
  fixSlopes
)
}
\arguments{
\item{scoringColumn}{a one-column matrix (column of a scoring matrix)}

\item{latentTraitName}{a string with latent variable name}

\item{items}{a character vector with item names}

\item{reverseCoded}{a character vector with names of reverse-coded items}

\item{itemCategories}{a data frame with columns named \emph{item} and
\emph{value} storing unique (non-NA) values of items that occur in the data}

\item{fixSlopes}{a logical value indicating whether slopes of the latent
trait should be fixed to be the same}
}
\value{
A character vector
}
\description{
Prepares Mplus model syntax describing how items are loaded
by latent traits in a GPCM specification of a NRM.
}
