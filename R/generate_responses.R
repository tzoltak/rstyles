#' @title Generate simulated data
#' @param traitCov covariance matrix of traits
#' @param items list with test items' specification
#' @param n number of observations
#' @param traitMeans expected values of traits
#' @param performAssertions logical value indicating whether function should
#' perform assertions of the other parameters (\code{TRUE} by default, may be
#' changed to \code{FALSE} for a little performance gain)
#' @return matrix with responses on items
#' \code{\link{generate_item_responses_sqn}},
#' \code{\link{generate_item_responses_sml}}
#' @export
generate_test_responses <- function(traitCov, items, n,
                                    traitMeans = rep(0, ncol(traitCov)),
                                    performAssertions = TRUE) {
  if (performAssertions) {
    stopifnot("Parameter `traitCov` must be a numeric matrix." =
                is.matrix(traitCov),
              "Parameter `traitCov` must be a numeric matrix." =
                is.numeric(traitCov),
              "Parameter `traitCov` must be a symmetrix matrix (with the same names of rows and columns)." =
                isSymmetric(traitCov),
              "Parameter `traitCov` can't contain NAs." = all(!is.na(traitCov)),
              "Parameter `items` must be a list of class 'itemList'" =
                is.list(items),
              "Each element of `items` must be of class 'rstylesItem'" =
                all(sapply(items, inherits, what = "rstylesItem")),
              "Parameter `n` must be a numeric vector." = is.numeric(n),
              "Parameter `n` must be of length one." = length(n) == 1,
              "Parameter `n` must be an integer." = as.integer(n) == n,
              "Parameter `traitMeans` must be a numeric vector." =
                is.numeric(traitMeans),
              "Parameter `traitMeans` must have the same number of elements as the number of columns in `traitCov`." =
                length(traitMeans) %in% c(1, ncol(traitCov)),
              "Parameter `traitMeans` can't contain NAs." =
                all(!is.na(traitMeans)))
  }

  if (length(traitMeans) == 1) {
    traitMeans <- rep(traitMeans, ncol(traitCov))
  }
  theta = mvtnorm::rmvnorm(n, traitMeans, traitCov)
  colnames(theta) <- colnames(traitCov)

  responses = matrix(NA_integer_, nrow = n, ncol = length(items))
  for (i in 1:ncol(responses)) {
    scoringMatrix <- items[[i]]$scoringMatrix
    if (!is.null(items[[i]]$scoringOnPreviousResponses) && i > 1) {
      for (j in which(!duplicated(responses[, 1:(i - 1), drop = FALSE]))) {
        scoringMatrixTemp <-
          cbind(do.call(items[[i]]$scoringOnPreviousResponses,
                        list(previousResponses = responses[j, 1:(i - 1)],
                             scoringMatrix = scoringMatrix)),
                scoringMatrix)
        whichRows <- apply(responses, 1, identical, y = responses[j, ])
        if (items[[i]]$mode == 'sequential') {
          responses[whichRows, i] <-
            generate_item_responses_sqn(theta[whichRows, , drop = FALSE],
                                        scoringMatrixTemp,
                                        items[[i]]$slopes,
                                        items[[i]]$intercepts,
                                        items[[i]]$editResponse)
        } else {#simultaneous
          responses[whichRows, i] <-
            generate_item_responses_sml(theta[whichRows, , drop = FALSE],
                                        scoringMatrixTemp,
                                        items[[i]]$slopes,
                                        items[[i]]$intercepts)
        }
      }
    } else {# if there is no scoring on previous responses, there is no need for looping through combinations of previuos responses
      if (items[[i]]$mode == 'sequential') {
        responses[, i] <- generate_item_responses_sqn(theta, scoringMatrix,
                                                      items[[i]]$slopes,
                                                      items[[i]]$intercepts,
                                                      items[[i]]$editResponse)
      } else {#simultaneous
        responses[, i] <- generate_item_responses_sml(theta, scoringMatrix,
                                                      items[[i]]$slopes,
                                                      items[[i]]$intercepts)
      }
    }
  }
  return(responses)
}
#' @title Internals: simulating responding to item in a 'sequential' (IRTree) way
#' @param theta matrix of latent traits' values
#' @param scoringMatrix matrix describing scoring patterns on each latent trait
#' @param slopes vector of slope parameters of each trait
#' @param intercepts intercept parameters
#' @param editResponse optional function returning scoring matrix that should be
#' used instead that provided by \code{scoringMatrix}; this should be function
#' accepting two parameters: \code{response} - generated response (by the model
#' described with the first column of the \code{scoringMatrix}) that is supposed
#' to be \emph{edited} and \code{scoringMatrix} - current scoring matrix (to be
#' replaced)
#' @description Function generates responses 'sequentially`, i.e. using IRTree
#' approach. It goes through consecutive columns of a scoring matrix,
#' calling \code{\link{generate_item_responeses_gpcm}} to get responses at
#' a given node of a tree and the recursively calls itself on subsets of
#' observations with a given response with reduced scoring matrix.
#'
#' \strong{Limitations:} Because function internally relies on calling
#' \code{\link{generate_item_responeses_gpcm}}, no normal ogive models can be
#' used (this may be changed in the future versions).
#' @return vector of responses on item
#' @seealso \code{link{generate_test_responses}},
#' \code{\link{generate_item_responses_sml}},
#' \code{\link{generate_item_responeses_gpcm}}
generate_item_responses_sqn <- function(theta, scoringMatrix, slopes,
                                        intercepts, editResponse = NULL) {
  thetaAtNode <- theta[, which(colnames(theta) == colnames(scoringMatrix)[1]),
                       drop = FALSE]
  slopeAtNode <- slopes[which(names(slopes) == colnames(scoringMatrix)[1])]
  scoringMatrixAtNode <-
    matrix(scoringMatrix[!duplicated(scoringMatrix[, 1]), 1], ncol = 1,
           dimnames = list(NULL, colnames(scoringMatrix)[1]))
  rownames(scoringMatrixAtNode) <- scoringMatrixAtNode
  responses <- generate_item_responeses_gpcm(
    thetaAtNode,
    scoringMatrixAtNode * slopeAtNode,
    intercepts)
  if (ncol(scoringMatrix) > 1 || is.function(editResponse)) {
    responsesBeforeUpdate <- responses
    for (r in unique(responses)) {
      if (is.function(editResponse)) {
        scoringMatrixNextNode <- do.call(editResponse,
                                         list(response = r,
                                              scoringMatrix = scoringMatrix))
      } else {
        scoringMatrixNextNode <- scoringMatrix[scoringMatrix[, 1] == r,
                                               -1, drop = FALSE]
      }
      nPossibleResponsesNextNode <-
        length(setdiff(unique(scoringMatrixNextNode[, 1]), NA))
      if (nPossibleResponsesNextNode > 1) {
        responses[responsesBeforeUpdate == r] <-
          generate_item_responses_sqn(theta[responsesBeforeUpdate == r,
                                            drop = FALSE],
                                      scoringMatrixNextNode,
                                      slopes, intercepts)
      } else if (nPossibleResponsesNextNode == 1) {
        # there is no check whether this is vector of length one!
        responses[responsesBeforeUpdate == r] <-
          rownames(scoringMatrixNextNode)[!is.na(scoringMatrixNextNode[, 1])]
      } else {
        # there is no check whether this is vector of length one!
        responses[responsesBeforeUpdate == r] <-
          rownames(scoringMatrix)[scoringMatrix[, 1] == r]
      }
    }
  }

  return(responses)
}
#' @title Internals: simulating responding to item in a 'simultaneous' way
#' @param theta matrix of latent traits' values
#' @param scoringMatrix matrix describing scoring patterns on each latent trait
#' @param slopes vector of slope parameters of each trait
#' @param intercepts intercept parameters
#' @description Function generates responses 'simultaneously` with a whole
#' scoring matrix used at once. Only (G)PCM approach is suitable in such a case,
#' because with complicated scoring matrices there is no guarantee that
#' probabilities of responses are increasing along with order of responses
#' (rows) in a scoring matrix. Consequently, no normal ogive models can be used.
#' @return vector of responses on item
#' @seealso \code{link{generate_test_responses}},
#' \code{\link{generate_item_responses_sqn}},
#' \code{\link{generate_item_responeses_gpcm}}
generate_item_responses_sml <- function(theta, scoringMatrix, slopes,
                                        intercepts) {
  theta <- theta[, sapply(colnames(scoringMatrix), match,
                          table = colnames(theta)), drop = FALSE]
  slopes <- slopes[sapply(colnames(scoringMatrix), match,
                          table = names(slopes)), drop = FALSE]
  responses <- generate_item_responeses_gpcm(
    theta,
    scoringMatrix * rep(slopes, each = nrow(scoringMatrix)),
    intercepts)
  return(responses)
}
#' @title Internals: calculation of (G)PCM probabilities and drawing responses
#' @param theta matrix of latent traits' values
#' @param weightsMatrix matrix of discrimination parameters (being
#' multiplication of a design matrix by discriminations of factors)
#' @param intercepts intercept parameters
#' @description Computes probabilities of each response using (G)PCM and draws
#' responses. Be awawre that for efficiency reasons it assumes  that
#' \strong{\code{theta} and \code{weightsMatrix} have the same number and order
#' of columns} (unlike \code{\link{generate_item_responses_sqn}} and
#' \code{\link{generate_item_responses_sml}} that are used to match theirs
#' parameters  \code{theta}, \code{scoringMatrix} and \code{slopes} before
#' passing them to \code{generate_item_responeses_gpcm}).
#' @return vector of responses on item
generate_item_responeses_gpcm <- function(theta, weightsMatrix, intercepts) {
  probs <- matrix(NA_real_,
                  nrow = nrow(theta), ncol = nrow(weightsMatrix),
                  dimnames = list(NULL, rownames(weightsMatrix)))

  for (i in 1:nrow(weightsMatrix)) {
    probs[, i] <-
      exp(intercepts[i] + theta %*% t(weightsMatrix[i, , drop = FALSE]))
  }
  probs <- probs / rowSums(probs)
  for (i in 1:nrow(probs)) {
    probs[i, ] <- cumsum(probs[i, ])
  }
  # contrary to common opinion loop (above) can be faster (than commented code below)
  # probs <- t(apply(probs, 1, cumsum))
  probs[, ncol(probs)] = stats::runif(nrow(probs), 0, 1)

  responses <- vector(mode = "character", length = nrow(probs))
  for (i in 1:nrow(probs)) {
    responses[i] <- colnames(probs)[probs[i, ] >= probs[i, ncol(probs)]][1]
  }
  # contrary to common opinion loop (above) can be faster (than commented code below)
  # responses <- apply(probs, 1,
  #                    function(x) {return(
  #                      names(x)[x >= x[length(x)]][1]
  #                    )})
  return(responses)
}
