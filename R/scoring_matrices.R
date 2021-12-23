#' @title Make scoring matrix
#' @description Makes response matrix, i.e. matrix describing how each latent
#' trait (represented in columns) affects (or not) chances to choose each
#' response category (represented in rows) assuming effects of
#' \emph{acquiescence}, \emph{extreme} and \emph{middle} response styles.
#' @param responses vector of available responses (\emph{categories}) - can be
#' a character vector or positive integer describing number of responses
#' @param sequence text: "simultaneous" or a three-letters sequence describing
#' the order of decisions made by a \emph{respondent}:
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
#' @param aType determines a way in which scoring pattern for acquiescence is
#' generated when it appears in different branches of the IRTree (whether to
#' create separate columns allowing for different discrimination of the
#' acquiescence in different nodes of the tree or to create only a single column
#' holding discrimination in different nodes of the tree constant)
#' @param iType determines a way in which scoring pattern for additional (see
#' the description of the `aType` parameter above)
#' \emph{intensity} trait will be generated (see details)
#' @details \strong{\code{sequence} other than "simultaneous":}
#'
#' For number of responses between 5 and 6 function generates scoring
#' matrix in a way mimicking Böckenholt's approach (2017) to describe
#' response to the item as a sequence of binary decisions involving choosing
#' of the middle, extreme and acquiescence categories - this decisions may be
#' made in different order, what is controlled by argument \code{sequence}.
#'
#' Please note that following Böckenholt \emph{acquiescence} trait is managed in
#' a little different way that the other two. If choice involving
#' \emph{acquiescence} may be made in different nodes of IRTree (i.e. for
#' different combinations of values in previous columns of the scoring matrix),
#' separate column describing decision in each node (for each combination) is
#' created by default (and names of these columns are \emph{a} followed by
#' integer index). That allows for specifying different IRT parameters for each
#' node. Setting argument \code{aType = "common"} allows to collapse these
#' column into one if you want to constrain model parameters between nodes in
#' a convenient way.
#'
#' With less than 5 possible responses functions apply the same logic, but not
#' all of the three aforementioned styles can be involved because lack of
#' variability in possible responses.
#'
#' With more than 6 possible responses there must be additional trait added to
#' scoringMatrix to describe process of choice between all the possible
#' responses. In such a case function adds additional columns to a scoring
#' matrix that names are \emph{i} (standing for intensity) followed by an index
#' and are filled up with scores for such combinations of values in previous
#' columns of the scoring matrix that occur more than once. Scores in these
#' columns are sequences of non-negative integers either increasing
#' (\code{reversed=FALSE}) or decreasing (\code{reversed=TRUE}) that are
#' generated independent for each unique combination of values in the previous
#' columns and by default each of such combinations is described by a separate
#' column (allowing for specification of different model parameters).
#' Analogously to \emph{acquiescence} trait these columns can be collapsed into
#' one by setting \code{iType = "common"}.
#'
#' \strong{\code{sequence} is "simultaneous":}
#'
#' In this case a GPCM scoring matrix is generated mimicking approach of
#' Plieninger (2016), i.e. assuming that response process is
#' a \emph{simultaneous} and four factors: intensity of the trait that
#' is \strong{not} a response style (column \emph{i}), tendency to choose middle
#' \emph{categories} (column \emph{m}) tendency to choose extreme
#' \emph{categories} (column \emph{e}) and tendency to choose acquiescence
#' \emph{categories} (column \emph{a}) contribute altogether to propensity
#' of choosing each response.
#' @return matrix of integers
#' @examples
#' # Bockenholt 2017: 73
#' (bockenholtMAE5 <- make_scoring_matrix_aem(5, "mae"))
#' # Bockenholt 2017: 76
#' (bockenholtMAE6 <- make_scoring_matrix_aem(6, "mae"))
#' # Bockenholt 2017: 77
#' (bockenholtAEM6 <- make_scoring_matrix_aem(6, "aem"))
#' # Plieninger 2016: 39
#' (plieninger5 <- make_scoring_matrix_aem(5, "simultaneous"))
#' (plieninger5r <- make_scoring_matrix_aem(5, "simultaneous", reversed = TRUE))
#'
#' # some more complicated cases:
#' make_scoring_matrix_aem(10, "ema", nMiddle = 3, nExtreme = 2)
#' make_scoring_matrix_aem(10, "ema", nMiddle = 3, nExtreme = 2,
#'                         aType = "common", iType = "common")
#' make_scoring_matrix_aem(9, "mae", nMiddle = 3, nExtreme = 2, reversed = TRUE)
#' @export
make_scoring_matrix_aem <- function(
  responses, sequence = c("mae", "mea", "aem", "ame", "ema", "eam",
                          "simultaneous"),
  nMiddle = 2L, nExtreme = 1L, nAcquiescence = floor(length(responses) / 2),
  reversed = FALSE,
  aType = c("separate", "common"), iType = c("separate", "common"))
{
  sequence = match.arg(sequence)
  aType = match.arg(aType)
  iType = match.arg(iType)
  responses <- assert_responses(responses)
  if (inherits(responses, "try-error")) {
    stop(sub("^.*: \\n +", "", responses))
  }
  stopifnot("Argument `nMiddle` must be a non-negative integer." =
              is.numeric(nMiddle),
            "Argument `nMiddle` must be a non-negative integer." =
              length(nMiddle) == 1L,
            "Argument `nMiddle` can't contain NAs." =
              !is.na(nMiddle),
            "Argument `nMiddle` must be a non-negative integer." =
              as.integer(nMiddle) == nMiddle,
            "Argument `nMiddle` must be a non-negative integer." =
              nMiddle >= 0L,
            "Argument `nExtreme` must be a non-negative integer." =
              is.numeric(nExtreme),
            "Argument `nExtreme` must be a non-negative integer." =
              length(nExtreme) == 1L,
            "Argument `nExtreme` can't contain NAs." =
              !is.na(nExtreme),
            "Argument `nExtreme` must be a non-negative integer." =
              as.integer(nExtreme) == nExtreme,
            "Argument `nExtreme` must be a non-negative integer." =
              nExtreme >= 0L)
  if (length(responses) %% 2L != nMiddle %% 2L) {
    nMiddle <- nMiddle - 1L
  }
  stopifnot("Argument `nAcquiescence` must be a non-negative integer." =
              is.numeric(nAcquiescence),
            "Argument `nAcquiescence` must be a non-negative integer." =
              length(nAcquiescence) == 1L,
            "Argument `nAcquiescence` can't contain NAs." =
              !is.na(nAcquiescence),
            "Argument `nAcquiescence` must be a non-negative integer." =
              as.integer(nAcquiescence) == nAcquiescence,
            "Argument `nAcquiescence` must be a non-negative integer." =
              nAcquiescence >= 0L,
            "Argument `reversed` must be TRUE or FALSE." =
              is.logical(reversed),
            "Argument `reversed` must be TRUE or FALSE." =
              length(reversed) == 1L,
            "Argument `reversed` must be TRUE or FALSE." =
              reversed %in% c(FALSE, TRUE))
  stopifnot("There are fewer responses than the number of responses that is supposed to be either middle (`nMiddle`) or extreme (`2*nExtreme`)." =
              nMiddle + 2L*nExtreme <= length(responses),
            "Number of responses that is supposed to be acquiescence (`nAcquiescence`) must be no more than a half of number of available responses." =
              nAcquiescence <= floor(length(responses) / 2))

  if (sequence == "simultaneous") {
    colNames <- c("i", "m", "e", "a")
  } else {
    colNames <- c(strsplit(sequence, "")[[1L]], "i")
  }
  scoringSubMatrices <- vector(mode = "list", length = 4)
  names(scoringSubMatrices) <- colNames
  # initial fill-in
  for (i in 1L:length(scoringSubMatrices)) {
    if (names(scoringSubMatrices)[i] == "m") {
      scoringSubMatrices[[i]] <-
        matrix(c(rep(0L, (length(responses) - nMiddle) / 2),
                 rep(1L, nMiddle),
                 rep(0L, (length(responses) - nMiddle) / 2)),
               ncol = 1, dimnames = list(responses, "m"))
    } else if (names(scoringSubMatrices)[i] == "e") {
      scoringSubMatrices[[i]] <-
        matrix(c(rep(1L, nExtreme),
                 rep(0L, length(responses) - 2*nExtreme),
                 rep(1L, nExtreme)),
               ncol = 1, dimnames = list(responses, "e"))
    } else if (names(scoringSubMatrices)[i] == "a") {
      if (sequence != "simultaneous" && aType == "separate" && i > 1L) {
        scoringMatrix <- cbind(scoringSubMatrices[[1]],
                               scoringSubMatrices[[2]],
                               scoringSubMatrices[[3]],
                               scoringSubMatrices[[4]])
        patterns <- apply(scoringMatrix, 1L, paste, collapse = "")
        uniquePatterns <- table(patterns)
        uniquePatterns <- names(uniquePatterns)[uniquePatterns > 1L]
        nColAcquiescence <- length(uniquePatterns)
        if (nColAcquiescence > 1L) {
          scoringSubMatrices[[i]] <-
            matrix(rep(c(rep(0L, length(responses) - nAcquiescence),
                         rep(1L, nAcquiescence)),
                       nColAcquiescence),
                   ncol = nColAcquiescence,
                   dimnames = list(responses, paste0("a", 1L:nColAcquiescence)))
          for (p in 1L:length(uniquePatterns)) {
            scoringSubMatrices[[i]][patterns != uniquePatterns[p], p] <-
              NA_integer_
          }
        } else if (nColAcquiescence > 0L) {
          scoringSubMatrices[[i]] <-
            matrix(c(rep(0L, length(responses) - nAcquiescence),
                     rep(1L, nAcquiescence)),
                   ncol = 1, dimnames = list(responses, "a"))
        }
      } else {
        scoringSubMatrices[[i]] <-
          matrix(c(rep(0L, length(responses) - nAcquiescence),
                   rep(1L, nAcquiescence)),
                 ncol = 1, dimnames = list(responses, "a"))
      }
    } else if (sequence == "simultaneous") {
      scoringSubMatrices[[i]] <-
        matrix(sort(0L:(length(responses) - 1), decreasing = reversed),
               ncol = 1, dimnames = list(responses, "i"))
    } else {
      scoringMatrix <- cbind(scoringSubMatrices[[1]],
                             scoringSubMatrices[[2]],
                             scoringSubMatrices[[3]],
                             scoringSubMatrices[[4]])
      patterns <- apply(scoringMatrix, 1L, paste, collapse = "")
      uniquePatterns <- table(patterns)
      uniquePatterns <- names(uniquePatterns)[uniquePatterns > 1L]
      nColIntensity <- length(uniquePatterns)
      if (nColIntensity > 0L) {
        if (nColIntensity > 1L && iType == "separate") {
          scoringSubMatrices[[i]] <-
            matrix(rep(NA_integer_, length(responses)*nColIntensity),
                   ncol = nColIntensity,
                   dimnames = list(responses, paste0("i", 1L:nColIntensity)))
        } else {
          scoringSubMatrices[[i]] <-
            matrix(rep(NA_integer_, length(responses)), ncol = 1,
                   dimnames = list(responses, "i"))
        }
        for (p in 1L:length(uniquePatterns)) {
          scoringSubMatrices[[i]][patterns == uniquePatterns[p],
                                  ifelse(iType == "separate", p, 1L)] <-
            sort(0L:(sum(patterns == uniquePatterns[p]) - 1L),
                 decreasing = reversed)
        }
      }
    }
  }
  scoringMatrix <- cbind(scoringSubMatrices[[1]],
                         scoringSubMatrices[[2]],
                         scoringSubMatrices[[3]],
                         scoringSubMatrices[[4]])
  if (sequence != "simultaneous") {
    # inserting NAs in rows that describe paths that ends up earlier
    for (i in 1L:(ncol(scoringMatrix) - 1L)) {
      patterns <- scoringMatrix[!duplicated(scoringMatrix[, 1L:i]), 1L:i,
                                drop = FALSE]
      for (j in 1L:nrow(patterns)) {
        whichRows <- apply(unname(scoringMatrix[, 1L:i, drop = FALSE]),
                           1L, identical, y = unname(patterns[j, ]))
        if (sum(whichRows) == 1L) {
          scoringMatrix[whichRows, (i + 1L):ncol(scoringMatrix)] <- NA_integer_
        }
        if (length(unique(scoringMatrix[whichRows, (i + 1L)])) == 1L) {
          scoringMatrix[whichRows, (i + 1L)] <- NA_integer_
        }
      }
    }
  }

  columnsToRemove <- apply(scoringMatrix, 2L, function(x) {
    return(length(setdiff(unique(x), NA)) < 2L)
  })
  scoringMatrix <- scoringMatrix[, !columnsToRemove, drop = FALSE]

  return(scoringMatrix)
}
#' @title Make scoring matrix
#' @description Makes trivial response matrix, corresponding to the most simple,
#' the same for each trait GPCM scoring scheme. This function may be useful if
#' one wants to use \code{\link{generate_slopes}} and
#' \code{\link{generate_intercepts}} functions to generate items' parameters
#' with no reference to response styles.
#' @param responses vector of available responses (\emph{categories}) - can be
#' a character vector or positive integer describing number of responses
#' @param nTraits optionally number of traits affecting the item response;
#' disregarded if \code{traitsNames} are provided
#' @param traitsNames optionally character vector containing names of the traits
#' @return matrix of integers
#' @examples
#' make_scoring_matrix_trivial(5, 2)
#' make_scoring_matrix_trivial(5, traitsNames = c("A", "B"))
#' @export
make_scoring_matrix_trivial <- function(responses, nTraits = 1L,
                                        traitsNames = paste0("F", 1L:nTraits)) {
  responses <- assert_responses(responses)
  if (inherits(responses, "try-error")) {
    stop(sub("^.*: \\n +", "", responses))
  }
  stopifnot("Argument `nTraits` must be a non-negative integer." =
              is.numeric(nTraits),
            "Argument `nTraits` must be a non-negative integer." =
              length(nTraits) == 1L,
            "Argument `nTraits` can't contain NAs." =
              !is.na(nTraits),
            "Argument `nTraits` must be a non-negative integer." =
              as.integer(nTraits) == nTraits,
            "Argument `nTraits` must be a non-negative integer." =
              nTraits >= 0L,
            "Argument `traitsNames` must be a character vector." =
              is.character(traitsNames),
            "Argument `traitsNames` must be a character vector containing at least one element." =
              length(traitsNames) > 0,
            "Argument `traitsNames` can't contain missing values." =
              !any(is.na(traitsNames)),
            "Argument `traitsNames` can't contain duplicates." =
              !any(duplicated(traitsNames)))
  return(matrix(rep((1L:length(responses)) - 1, length(traitsNames)),
                nrow = length(responses),
                dimnames = list(responses, traitsNames)))
}
#' @title Make scoring matrix
#' @description Makes response matrix using \emph{random thresholds} approach.
#' @param responses vector of available responses (\emph{categories}) - can be
#' a character vector or positive integer describing number of responses
#' @details Be aware that while using this kind of response matrix latent
#' traits must be set orthogonal to assure model identifiability.
#' @return matrix of integers
#' @examples
#' make_scoring_matrix_rt(5)
#' @export
make_scoring_matrix_rt <- function(responses) {
  responses <- assert_responses(responses)
  if (inherits(responses, "try-error")) {
    stop(sub("^.*: \\n +", "", responses))
  }
  sM <- matrix(0L, nrow = length(responses), ncol = length(responses),
               dimnames = list(responses,
                               c("i", paste0("rt", 1L:(length(responses) - 1L)))))
  sM[lower.tri(sM, diag = TRUE)] <- 1L
  sM[, 1L] <- 0L:(nrow(sM) - 1L)
  return(sM)
}
#' @title Make scoring matrix
#' @description Makes response matrix using \emph{sum to zero} approach.
#' @param responses vector of available responses (\emph{categories}) - can be
#' a character vector or positive integer describing number of responses
#' @return matrix of integers
#' @examples
#' make_scoring_matrix_stz(5)
#' @export
make_scoring_matrix_stz <- function(responses) {
  responses <- assert_responses(responses)
  if (inherits(responses, "try-error")) {
    stop(sub("^.*: \\n +", "", responses))
  }
  sM <- matrix(0L, nrow = length(responses), ncol = length(responses),
               dimnames = list(responses,
                               c("i", paste0("stz", 1L:(length(responses) - 1L)))))
  diag(sM) <- 1L
  sM[1L, ] <- -1L
  sM[, 1L] <- 0L:(nrow(sM) - 1L)
  return(sM)
}
# common assertions
assert_responses <- function(responses) {
  e = try(stopifnot("Argument `responses` must be a vector." =
                      is.vector(responses),
                    "Argument `responses` can't contain duplicated values." =
                      all(!duplicated(responses)),
                    "Argument `responses` must have at least two values or be a positive integer." =
                      length(responses) > 1L | is.numeric(responses),
                    "Argument `responses` can't contain NAs." =
                      !anyNA(responses)),
          silent = TRUE)
  if (inherits(e, "try-error")) {
    return(e)
  }
  if (length(responses) <= 1L) {
    e <- try(stopifnot("Argument `responses` must have at least two values or be a positive integer." =
                         length(responses) == 1L,
                       "Argument `responses` must have at least two values or be a positive integer." =
                         as.integer(responses) == responses,
                       "Argument `responses` must have at least two values or be a positive integer larger than 1." =
                         responses > 1L))
    if (inherits(e, "try-error")) {
      return(e)
    }
    responses = 1L:responses
  }
  return(responses)
}

