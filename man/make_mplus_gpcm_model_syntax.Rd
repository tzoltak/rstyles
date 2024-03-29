% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resp_styles_to_mplus.R
\name{make_mplus_gpcm_model_syntax}
\alias{make_mplus_gpcm_model_syntax}
\title{Preparing Mplus code to estimate RS models}
\usage{
make_mplus_gpcm_model_syntax(
  data,
  items,
  scoringMatrix,
  observedExogenous = vector(mode = "character", length = 0L),
  observedDependent = vector(mode = "character", length = 0L),
  fixSlopes = vector(mode = "character", length = 0L),
  reverseCoded = vector(mode = "list", length = 0L),
  orthogonal = vector(mode = "character", length = 0L),
  weight = NA_character_,
  output = "STDYX",
  savedata = NA_character_,
  analysis = list(ESTIMATOR = "MLR", ALGORITHM = "INTEGRATION", INTEGRATION = "STANDARD",
    PROCESSORS = "4"),
  title = "Some GPCM model with custom scoring matrix"
)
}
\arguments{
\item{data}{a data frame}

\item{items}{a character vector of item names or a list describing items
matching to latent traits in a between-item multidimensional model; in the
latter case list elements must be named after column names of the
\code{scoringMatrix} with each element being a list describing different
latent traits that should be scored using a given column of the
\code{scoringMatrix}; names of these \emph{second-level} elements specify
names of latent traits and elements themselves are character vectors of item
names that are assigned to (loaded) a given latent trait}

\item{scoringMatrix}{a matrix describing how responses (described in rownames
of the matrix) map on \emph{scores} of latent traits (described in columns of
the matrix)}

\item{observedExogenous}{either:
\itemize{
  \item{a character vector with names of observed exogenous predictors that
        should be used to predict latent variables in the model}
  \item{a matrix with latent traits in columns and observed exogenous
        predictors in rows specifying which of the exogenous predictors should
        be used to predict which latent traits (matrix should contain only
        0 and 1 or \code{TRUE} and \code{FALSE})}
}}

\item{observedDependent}{either:
\itemize{
  \item{a character vector with names of observed dependent variables that
        should be predicted using latent variables in the model}
  \item{a matrix with latent traits in columns and observed dependent
        variables in rows specifying which of the dependent variables should
        be predicted by which latent traits (matrix should contain only
        0 and 1 or \code{TRUE} and \code{FALSE})}
}}

\item{fixSlopes}{optionally a character vector of latent trait names
for which item slopes parameters should be fixed across items
(these names need to occur in column names of \code{scoringMatrix})}

\item{reverseCoded}{optionally a named list of character vectors with names
of list elements specifying latent trait names and elements giving names of
items that are \emph{reverse coded} with respect to this latent trait;
please note, that these don't need to be given if slopes of the no-RS
trait(s) are not fixed across items}

\item{orthogonal}{optionally a character vector of latent trait names
indicating which latent traits should be specified as orthogonal to each
other (all the mentioned latent traits will be specified as orthogonal to each
other and all the other latent traits)}

\item{weight}{optionally a string with a name of the variable storing weights}

\item{output}{optionally a character vector of Mplus options defining what
type of results should be included in the output}

\item{savedata}{optionally a string with a name of the file in which factor
scores should be saved}

\item{analysis}{a list with elements \code{ESTIMATOR}, \code{ALGORITHM},
\code{INTEGRATION} and \code{PROCESSORS} containing Mplus \emph{ANALYSIS}
options (provided as strings)}

\item{title}{string with a title for the analysis}
}
\value{
A list with elements named \code{TITLE}, \code{VARIABLE},
\code{ANALYSIS}, \code{MODEL}, \code{MODELCONSTRAINT}, \code{SAVEDATA},
\code{rdata}, \code{usevariables} that can be used as arguments to the
\code{mplusObject} function from the package \emph{MplusAutomation} using
\code{\link{do.call}}
}
\description{
Prepares components of Mplus model description syntax for a
GPCM (NRM).
}
\details{
For the description of model specification see \emph{Details}
section in \code{\link{make_mplus_gpcm_vmmc_syntax}}
}
\section{Limitations}{

At the moment there is no possibility to prepare models with many
no-RS latent traits loading different sets of items.
}

