#' @title Generate simulated data
#' @description Generates simulated responses to a test given informations
#' about items values of latent traits.
#' @param theta matrix (or data frame) of already generated latent traits'
#' values
#' @param items list with test items' specification
#' @param performAssertions logical value indicating whether function should
#' perform assertions of the other arguments (\code{TRUE} by default, may be
#' changed to \code{FALSE} for a little performance gain)
#' @return matrix with responses on items
#' \code{\link{generate_item_responses_sqn}},
#' \code{\link{generate_item_responses_sml}}
#' @export
generate_test_responses <- function(theta, items, performAssertions = TRUE) {
  if (performAssertions) {
    if (is.data.frame(theta)) {
      theta <- as.matrix(theta)
    }
    stopifnot("Argument `theta` must be a numeric matrix." =
                is.matrix(theta),
              "Argument `theta` must be a numeric matrix." =
                is.numeric(theta),
              "Argument `theta` must have at least one row." = nrow(theta) > 0,
              "Argumentr `theta` can't contain NAs." = all(!is.na(theta)),
              "Argument `items` must be a list of class 'itemList'" =
                is.list(items),
              "Each element of `items` must be of class 'rstylesItem'" =
                all(sapply(items, inherits, what = "rstylesItem")))
    traitNames <- sapply(items, function(x) {return(names(x$slopes))})
    traitNames <- unique(as.vector(traitNames))
    traitNames <- sub("[[:digit:]]+$", "", traitNames)
    if (!all(traitNames %in% colnames(theta))) {
      stop("Items refer to latent traits that don't appear in the provided covariance matrix of latent traits: '",
           paste(setdiff(traitNames, colnames(theta)), collapse = "', '"),
           "'.")
    }
  }

  responses = matrix(NA_integer_, nrow = nrow(theta), ncol = length(items))
  for (i in 1L:ncol(responses)) {
    scoringMatrix <- items[[i]]$scoringMatrix
    # dispatch for speed
    if (!is.null(items[[i]]$scoringOnPreviousResponses) && i > 1L) {
      for (j in which(!duplicated(responses[, 1L:(i - 1L), drop = FALSE]))) {
        scoringMatrixTemp <-
          cbind(do.call(items[[i]]$scoringOnPreviousResponses,
                        list(previousResponses = responses[j, 1L:(i - 1L)],
                             scoringMatrix = scoringMatrix)),
                scoringMatrix)
        # line below is an order of magnitude faster than using apply
        whichRows <-
          colSums(t(responses[, 1L:(i - 1L), drop = FALSE]) ==
                    responses[j, 1L:(i - 1L)]) == (i - 1L)
        if (items[[i]]$mode == 'sequential') {
          responses[whichRows, i] <-
            generate_item_responses_sqn(theta[whichRows, , drop = FALSE],
                                        scoringMatrixTemp,
                                        items[[i]]$slopes,
                                        items[[i]]$intercepts,
                                        items[[i]]$editResponse,
                                        TRUE)
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
#' @description Function generates responses 'sequentially`, i.e. using IRTree
#' approach. It goes through consecutive columns of a scoring matrix,
#' calling \code{\link{generate_item_responeses_gpcm}} to get responses at
#' a given node of a tree and the recursively calls itself on subsets of
#' observations with a given response with reduced scoring matrix.
#' @section Limitations:
#' Because function internally relies on calling
#' \code{\link{generate_item_responeses_gpcm}}, no normal ogive models can be
#' used (this may be changed in the future versions).
#' @param theta matrix of latent traits' values
#' @param scoringMatrix matrix describing scoring patterns on each latent trait
#' @param slopes vector of slope parameters of each trait
#' @param intercepts intercept parameters
#' @param editResponse optional function returning scoring matrix that should be
#' used instead that provided by \code{scoringMatrix}; this should be function
#' accepting two arguments: \code{response} - generated response (by the model
#' described with the first column of the \code{scoringMatrix}) that is supposed
#' to be \emph{edited} and \code{scoringMatrix} - current scoring matrix (to be
#' replaced)
#' @param decidingOnPreviousResponse logical value indicating whether first
#' column of provided scoring matrix describes making decision whether to
#' respond on the basis of responses to previous items or not (in this first
#' case \emph{negative} choice shouldn't reduce number of rows in a response
#' matrix)
#' @return vector of responses on item
#' @seealso \code{link{generate_test_responses}},
#' \code{\link{generate_item_responses_sml}},
#' \code{\link{generate_item_responeses_gpcm}}
generate_item_responses_sqn <- function(theta, scoringMatrix, slopes,
                                        intercepts, editResponse = NULL,
                                        decidingOnPreviousResponse = FALSE) {
  slopeAtNode <- slopes[which(names(slopes) == colnames(scoringMatrix)[1L])]
  interceptsAtNode <-
    intercepts[c(1L, grep(paste0("^", names(slopeAtNode),
                                 "_?[[:digit:]]+"), names(intercepts)))]
  thetaAtNode <- theta[, sapply(colnames(theta),
                                function(x, y) {substr(y, 1, nchar(x)) == x},
                                y = names(slopeAtNode)),
                       drop = FALSE]
  scoringMatrixAtNode <-
    matrix(scoringMatrix[!duplicated(scoringMatrix[, 1L]), 1L], ncol = 1,
           dimnames = list(NULL, colnames(scoringMatrix)[1L]))
  scoringMatrixAtNode <-
    scoringMatrixAtNode[order(scoringMatrixAtNode[, 1L]), , drop = FALSE]
  rownames(scoringMatrixAtNode) <- scoringMatrixAtNode
  responses <- generate_item_responeses_gpcm(
    thetaAtNode,
    scoringMatrixAtNode * slopeAtNode,
    interceptsAtNode)
  if (ncol(scoringMatrix) > 1L || is.function(editResponse)) {
    responsesBeforeUpdate <- responses
    for (r in unique(responses)) {
      if (is.function(editResponse)) {
        scoringMatrixNextNode <- do.call(editResponse,
                                         list(response = r,
                                              scoringMatrix = scoringMatrix))
      } else if (decidingOnPreviousResponse && r == "0") {
        scoringMatrixNextNode <- scoringMatrix[, -1L, drop = FALSE]
      } else {
        scoringMatrixNextNode <- scoringMatrix[scoringMatrix[, 1L] == r,
                                               -1L, drop = FALSE]
      }
      # dropping columns describing other branches of a tree
      scoringMatrixNextNode <-
        scoringMatrixNextNode[, c(
          !apply(is.na(scoringMatrixNextNode[, -ncol(scoringMatrixNextNode),
                                             drop = FALSE]),
                 2L, all),
          TRUE), drop = FALSE]
      interceptsNextNode <-
        intercepts[-grep(paste0("^", names(slopeAtNode),
                                "_?[[:digit:]]+"), names(intercepts))]
      nPossibleResponsesNextNode <-
        length(setdiff(unique(scoringMatrixNextNode[, 1L]), NA))
      if (nPossibleResponsesNextNode > 1L) {
        responses[responsesBeforeUpdate == r] <-
          generate_item_responses_sqn(theta[responsesBeforeUpdate == r, ,
                                            drop = FALSE],
                                      scoringMatrixNextNode,
                                      slopes[-1L], interceptsNextNode)
      } else if (nPossibleResponsesNextNode == 1L) {
        # there is no check whether this is vector of length one!
        responses[responsesBeforeUpdate == r] <-
          rownames(scoringMatrixNextNode)[!is.na(scoringMatrixNextNode[, 1L])]
      } else {
        # there is no check whether this is vector of length one!
        responses[responsesBeforeUpdate == r] <-
          rownames(scoringMatrix)[scoringMatrix[, 1L] == r]
      }
    }
  } else {
    responsesBeforeUpdate <- responses
    for (i in unique(responses)) {
      responses[responsesBeforeUpdate == i] <-
        rownames(scoringMatrix)[as.character(scoringMatrix[, 1L]) == i]
    }
  }
  return(responses)
}
#' @title Internals: simulating responding to item in a 'simultaneous' way
#' @description Function generates responses 'simultaneously` with a whole
#' scoring matrix used at once. Only (G)PCM approach is suitable in such a case,
#' because with complicated scoring matrices there is no guarantee that
#' probabilities of responses are increasing along with order of responses
#' (rows) in a scoring matrix. Consequently, no normal ogive models can be used.
#' @param theta matrix of latent traits' values
#' @param scoringMatrix matrix describing scoring patterns on each latent trait
#' @param slopes vector of slope parameters of each trait
#' @param intercepts intercept parameters
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
#' @description Computes probabilities of each response using (G)PCM and draws
#' responses. Be aware that for efficiency reasons it assumes  that
#' \strong{\code{theta} and \code{weightsMatrix} have the same number and order
#' of columns} (unlike \code{\link{generate_item_responses_sqn}} and
#' \code{\link{generate_item_responses_sml}} that are used to match theirs
#' arguments \code{theta}, \code{scoringMatrix} and \code{slopes} before
#' passing them to \code{generate_item_responeses_gpcm}).
#' @param theta matrix of latent traits' values
#' @param weightsMatrix matrix of discrimination parameters (being
#' multiplication of a design matrix by discriminations of factors)
#' @param intercepts intercept parameters
#' @return vector of responses on item
generate_item_responeses_gpcm <- function(theta, weightsMatrix, intercepts) {
  probs <- matrix(NA_real_,
                  nrow = nrow(theta), ncol = nrow(weightsMatrix),
                  dimnames = list(NULL, rownames(weightsMatrix)))

  for (i in 1L:nrow(weightsMatrix)) {
    probs[, i] <-
      exp(intercepts[i] + theta %*% t(weightsMatrix[i, , drop = FALSE]))
  }
  probs <- probs / rowSums(probs)
  for (i in 1L:nrow(probs)) {
    probs[i, ] <- cumsum(probs[i, ])
  }
  # contrary to common opinion loop (above) can be faster (than commented code below)
  # probs <- t(apply(probs, 1, cumsum))
  probs[, ncol(probs)] = stats::runif(nrow(probs), 0, 1)

  responses <- vector(mode = "character", length = nrow(probs))
  for (i in 1L:nrow(probs)) {
    responses[i] <- colnames(probs)[probs[i, ] >= probs[i, ncol(probs)]][1L]
  }
  # contrary to common opinion loop (above) can be faster (than commented code below)
  # responses <- apply(probs, 1,
  #                    function(x) {return(
  #                      names(x)[x >= x[length(x)]][1]
  #                    )})
  return(responses)
}
