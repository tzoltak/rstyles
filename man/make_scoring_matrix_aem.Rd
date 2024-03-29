% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scoring_matrices.R
\name{make_scoring_matrix_aem}
\alias{make_scoring_matrix_aem}
\title{Make scoring matrix}
\usage{
make_scoring_matrix_aem(
  responses,
  sequence = c("mae", "mea", "aem", "ame", "ema", "eam", "gpcm"),
  nMiddle = 2L,
  nExtreme = 1L,
  nAcquiescence = floor(length(responses)/2),
  reversed = FALSE,
  aType = c("separate", "common"),
  iType = c("separate", "common")
)
}
\arguments{
\item{responses}{a vector of available responses (\emph{categories}) - can be
a character vector or positive integer describing number of responses}

\item{sequence}{a string: "gpcm" or a three-letters sequence describing
the order of nodes in the IRTree:
\itemize{
  \item{'m' stands for choosing between middle \emph{category} and some other
        \emph{category}}
  \item{'a' stands for choosing between \emph{acquiescence} response (i.e.
        located \emph{after/below} a \emph{middle} one) and some other
        response}
  \item{'e' stands for choosing between \emph{extreme} category and some
        other \emph{category}}
}}

\item{nMiddle}{the (maximum) number of \emph{middle} \emph{categories}}

\item{nExtreme}{(half of) the number of \emph{extreme} \emph{categories}}

\item{nAcquiescence}{the number of \emph{acquiescence} \emph{categories}}

\item{reversed}{a logical value - is item a reversed one? (see details)}

\item{aType}{determines a way in which scoring pattern for acquiescence is
generated when it appears in different branches of the IRTree (whether to
create separate columns allowing for different discrimination of the
acquiescence in different nodes of the tree or to create only a single column
holding discrimination in different nodes of the tree constant)}

\item{iType}{determines a way in which scoring pattern for additional (see
the description of the `aType` parameter above) \emph{intensity} trait will
be generated (see details)}
}
\value{
a matrix of integers
}
\description{
Makes response matrix, i.e. matrix describing how each latent
trait (represented in columns) affects (or not) chances to choose each
response category (represented in rows) assuming effects of
\emph{acquiescence}, \emph{extreme} and \emph{middle} response styles.
}
\details{
\strong{\code{sequence} other than "gpcm":}

For important remarks on the possibilities and limitations of interpretation
of IRTree models, that are represented by this type of scoring matrices,
see Plieninger (2020).

For number of responses between 5 and 6 function generates scoring
matrix in a way mimicking Böckenholt's approach (2017) to describe
response to the item as a sequence of binary decisions involving choosing
of the middle, extreme and acquiescence categories - this decisions may be
made in different order, what is controlled by argument \code{sequence}.

Please note that following Böckenholt \emph{acquiescence} trait is managed in
a little different way that the other two. If choice involving
\emph{acquiescence} may be made in different nodes of IRTree (i.e. for
different combinations of values in previous columns of the scoring matrix),
separate column describing decision in each node (for each combination) is
created by default (and names of these columns are \emph{a} followed by
integer index). That allows for specifying different IRT parameters for each
node. Setting argument \code{aType = "common"} allows to collapse these
column into one if you want to constrain model parameters between nodes in
a convenient way.

With less than 5 possible responses functions apply the same logic, but not
all of the three aforementioned styles can be involved because lack of
variability in possible responses.

With more than 6 possible responses there must be additional trait added to
scoringMatrix to describe process of choice between all the possible
responses. In such a case function adds additional columns to a scoring
matrix that names are \emph{i} (standing for intensity) followed by an index
and are filled up with scores for such combinations of values in previous
columns of the scoring matrix that occur more than once. Scores in these
columns are sequences of non-negative integers either increasing
(\code{reversed=FALSE}) or decreasing (\code{reversed=TRUE}) that are
generated independent for each unique combination of values in the previous
columns and by default each of such combinations is described by a separate
column (allowing for specification of different model parameters).
Analogously to \emph{acquiescence} trait these columns can be collapsed into
one by setting \code{iType = "common"}.

\strong{\code{sequence} is "gpcm":}

In this case a GPCM scoring matrix is generated mimicking approach of
Plieninger (2016), i.e. assuming that response process is
a \emph{gpcm} and four factors: intensity of the trait that
is \strong{not} a response style (column \emph{i}), tendency to choose middle
\emph{categories} (column \emph{m}) tendency to choose extreme
\emph{categories} (column \emph{e}) and tendency to choose acquiescence
\emph{categories} (column \emph{a}) contribute altogether to propensity
of choosing each response.
}
\examples{
# Bockenholt 2017: 73
(bockenholtMAE5 <- make_scoring_matrix_aem(5, "mae"))
# Bockenholt 2017: 76
(bockenholtMAE6 <- make_scoring_matrix_aem(6, "mae"))
# Bockenholt 2017: 77
(bockenholtAEM6 <- make_scoring_matrix_aem(6, "aem"))
# Plieninger 2016: 39
(plieninger5 <- make_scoring_matrix_aem(5, "gpcm"))
(plieninger5r <- make_scoring_matrix_aem(5, "gpcm", reversed = TRUE))

# some more complicated cases:
make_scoring_matrix_aem(10, "ema", nMiddle = 3, nExtreme = 2)
make_scoring_matrix_aem(10, "ema", nMiddle = 3, nExtreme = 2,
                        aType = "common", iType = "common")
make_scoring_matrix_aem(9, "mae", nMiddle = 3, nExtreme = 2, reversed = TRUE)
}
