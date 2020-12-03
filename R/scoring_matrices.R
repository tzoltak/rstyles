#' @title Make scoring matrix
#' @param responses vector of available responses (\emph{categories}) - can be
#' a character vector or positive integer describing number of responses
#' @param sequence sequence of decisions made by a \emph{respondent}, determined
#' by order of codes in a string:
#' \itemize{
#'   \item{'m' stands for choosing between middle \emph{category} and some other
#'         \emph{category}}
#'   \item{'a' stands for choosing between \emph{acquiescence} response (i.e.
#'         located \emph{after/below} a \emph{middle} one) and some other
#'         response}
#'   \item{'e' stands for choosing between \emph{extreme} category and some
#'         other \emph{category}}
#' }
#' @param nMiddle (maximum) number of \emph{middle} \emph{categories}
#' @param nExtreme (half of the) number of \emph{extreme} \emph{categories}
#' @param nAcquiescence number of \emph{acquiescence} \emph{categories}
#' @param reversed logical value - is item a reversed one? (see details)
#' @param iType determines a way in which scoring pattern for additional
#' \emph{intensity} trait will be generated (see details)
#' @details \strong{\code{sequence} other than "simultaneous"}
#'
#' For number of responses between 5 and 6 function generates scoring
#' matrix in a way mimicking Böckenholt's approach (2017) to describe
#' response to the item as a sequence of binary decissions involving choosing
#' of the middle, extreme and acquiescence categories - this decisions may be
#' made in diferent order, what is controlled by parameter \code{sequence}.
#'
#' With less than 5 possible responses functions apply the same logic, but not
#' all of the three aformentioned styles can be invloved because lack of
#' variablity in possible responses.
#'
#' With more than 6 possible responses there must be odditional trait added to
#' scoringMatrix to describe process of choice between all the possible
#' responses. In such a case function adds 4th column to a scoring matrix that
#' is named \emph{i} (standing for intensity) and is filled up with scores for
#' such rows of a scoring matrix that occurs more than once. Scores in this
#' column are sequences of positive integers either increasing
#' (\code{reversed=FALSE}) or decreasing (\code{reversed=TRUE}) that are
#' generted either as a one sequence for all the rows to be filled up
#' (\code{iType="between"}) or as independent sequences for each unique
#' combination of values in the precious columns (\code{iType="within"}).)
#'
#' \strong{\code{sequence} is "simultaneous"}
#'
#' In this case a GPCM scoring matrix is generated mimicking approach of
#' Plieninger (2016), i.e. assuming that response process is
#' a \emph{simultaneous} and four factors: intensity of the trait that
#' is \strong{not} a response style (column \emph{i}), tendency to choose middle
#' \emph{categories} (column \emph{m}) tendency to choose extreme
#' \emph{categories} (column \emph{e}) and tendency to choose cquiescence
#' \emph{categories} (column \emph{a}) contribute alltogether to propensity
#' of choosing each response.
#' @return matrix of integers
#' @export
make_scoring_matrix_aem <- function(
  responses, sequence = c("mae", "mea", "aem", "ame", "ema", "eam",
                          "simultaneous"),
  nMiddle = 2L, nExtreme = 1L,
  nAcquiescence = floor(length(responses) / 2),
  reversed = FALSE, iType = c("within", "between"))
{
  sequence = match.arg(sequence)
  iType = match.arg(iType)
  stopifnot("Paramater `responses` must be a vector." = is.vector(responses),
            "Paramater `responses` can't contain duplicated values." =
              all(!duplicated(responses)),
            "Paramater `responses` must have at least two values or be a positive integer." =
              length(responses) > 1 | is.numeric(responses),
            "Paramater `responses` can't contain NAs." = !anyNA(responses))
  if (length(responses) <= 1) {
    stopifnot("Paramater `responses` must have at least two values or be a positive integer." =
                length(responses) == 1,
              "Paramater `responses` must have at least two values or be a positive integer." =
                as.integer(responses) == responses,
              "Paramater `responses` must have at least two values or be a positive integer larger than 1." =
                responses > 1)
    responses = 1:responses
  }
  stopifnot("Paramater `nMiddle` must be a non-negative integer." =
              is.numeric(nMiddle),
            "Paramater `nMiddle` must be a non-negative integer." =
              length(nMiddle) == 1,
            "Paramater `nMiddle` can't contain NAs." =
              !is.na(nMiddle),
            "Paramater `nMiddle` must be a non-negative integer." =
              as.integer(nMiddle) == nMiddle,
            "Paramater `nMiddle` must be a non-negative integer." =
              nMiddle >= 0,
            "Paramater `nExtreme` must be a non-negative integer." =
              is.numeric(nExtreme),
            "Paramater `nExtreme` must be a non-negative integer." =
              length(nExtreme) == 1,
            "Paramater `nExtreme` can't contain NAs." =
              !is.na(nExtreme),
            "Paramater `nExtreme` must be a non-negative integer." =
              as.integer(nExtreme) == nExtreme,
            "Paramater `nExtreme` must be a non-negative integer." =
              nExtreme >= 0)
  if (length(responses) %% 2 != nMiddle %% 2) {
    nMiddle <- nMiddle - 1
  }
  stopifnot("Paramater `nAcquiescence` must be a non-negative integer." =
              is.numeric(nAcquiescence),
            "Paramater `nAcquiescence` must be a non-negative integer." =
              length(nAcquiescence) == 1,
            "Paramater `nAcquiescence` can't contain NAs." =
              !is.na(nAcquiescence),
            "Paramater `nAcquiescence` must be a non-negative integer." =
              as.integer(nAcquiescence) == nAcquiescence,
            "Paramater `nAcquiescence` must be a non-negative integer." =
              nAcquiescence >= 0,
            "Parameter `reversed` must be TRUE or FALSE." =
              is.logical(reversed),
            "Parameter `reversed` must be TRUE or FALSE." =
              length(reversed) == 1,
            "Parameter `reversed` must be TRUE or FALSE." =
              reversed %in% c(FALSE, TRUE))
  stopifnot("There are fewer responses than the number of responses that is supposed to be either middle (`nMiddle`) or extreme (`2*nExtreme`)." =
              nMiddle + 2*nExtreme <= length(responses),
            "Number of responses that is supposed to be acquiescence (`nAcquiescence`) must be no more than a half of number of available responses." =
              nAcquiescence <= floor(length(responses) / 2))

  if (sequence == "simultaneous") {
    colNames <- c("i", "m", "e", "a")
  } else {
    colNames <- c(strsplit(sequence, "")[[1]], "i")
  }
  scoringMatrix <- matrix(NA_integer_, nrow = length(responses), ncol = 4,
                          dimnames = list(responses, colNames))
  # initial fill-in
  for (i in 1L:ncol(scoringMatrix)) {
    if (colnames(scoringMatrix)[i] == "m") {
      scoringMatrix[, i] <- c(rep(0L, (nrow(scoringMatrix) - nMiddle) / 2),
                              rep(1L, nMiddle),
                              rep(0L, (nrow(scoringMatrix) - nMiddle) / 2))
    } else if (colnames(scoringMatrix)[i] == "e") {
      scoringMatrix[, i] <- c(rep(1L, nExtreme),
                              rep(0L, nrow(scoringMatrix) - 2*nExtreme),
                              rep(1L, nExtreme))
    } else if (colnames(scoringMatrix)[i] == "a") {
      scoringMatrix[, i] <- c(rep(0L, nrow(scoringMatrix) - nAcquiescence),
                              rep(1L, nAcquiescence))
    } else if (sequence == "simultaneous") {
      scoringMatrix[, i] <- sort(1L:nrow(scoringMatrix), decreasing = reversed)
    }
  }
  if (sequence != "simultaneous") {
    # inserting NAs in rows that describe paths that ends up earlier
    for (i in 1L:(ncol(scoringMatrix) - 1)) {
      patterns <- scoringMatrix[!duplicated(scoringMatrix[, 1L:i]), 1L:i,
                                drop = FALSE]
      for (j in 1L:nrow(patterns)) {
        whichRows <- apply(unname(scoringMatrix[, 1L:i, drop = FALSE]),
                           1L, identical, y = unname(patterns[j, ]))
        if (sum(whichRows) == 1L) {
          scoringMatrix[whichRows, (i + 1L):ncol(scoringMatrix)] <- NA
        }
      }
    }
    # generating scores on 'intensity' trait, if needed
    patterns <-
      scoringMatrix[!duplicated(scoringMatrix[, 1L:(ncol(scoringMatrix) - 1L)]),
                    1L:(ncol(scoringMatrix) - 1L), drop = FALSE]
    for (j in 1L:nrow(patterns)) {
      whichRows <- apply(unname(scoringMatrix[, 1L:(ncol(scoringMatrix) - 1L),
                                              drop = FALSE]),
                         1L, identical, y = unname(patterns[j, ]))
      if (sum(whichRows) > 1L) {
        if (iType == "between") {
          scoringMatrix[whichRows, ncol(scoringMatrix)] <- 1L
        } else {
          scoringMatrix[whichRows, ncol(scoringMatrix)] <-
            sort(1L:sum(whichRows), decreasing = reversed)
        }
      }
    }
    if (iType == "between") {
      scoringMatrix[!is.na(scoringMatrix[, ncol(scoringMatrix)]),
                    ncol(scoringMatrix)] <-
        sort(1L:sum(!is.na(scoringMatrix[, ncol(scoringMatrix)])),
             decreasing = reversed)
    }
  }

  columnsToRemove <- apply(scoringMatrix, 2, function(x) {
    return(length(setdiff(unique(x), NA)) < 2)
  })
  scoringMatrix <- scoringMatrix[, !columnsToRemove, drop = FALSE]

  return(scoringMatrix)
}
