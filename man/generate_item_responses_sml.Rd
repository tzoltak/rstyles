% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_responses.R
\name{generate_item_responses_sml}
\alias{generate_item_responses_sml}
\title{Internals: simulating responding to item in the GPCM way}
\usage{
generate_item_responses_sml(theta, scoringMatrix, slopes, intercepts)
}
\arguments{
\item{theta}{a matrix of latent traits' values}

\item{scoringMatrix}{a matrix describing scoring patterns on each latent trait}

\item{slopes}{a vector of slope parameters of each trait}

\item{intercepts}{a vector of intercept parameters}
}
\value{
a vector of responses on item
}
\description{
Function generates responses in the GPCM way with a whole
scoring matrix used at once. Only (G)PCM/NRM approach is suitable in such
a case, because with complicated scoring matrices there is no guarantee that
probabilities of responses are increasing along with order of responses
(rows) in a scoring matrix. Consequently, no normal ogive models can be used.
}
\seealso{
\code{link{generate_test_responses}},
\code{\link{generate_item_responses_sqn}},
\code{\link{generate_item_responeses_gpcm}}
}
