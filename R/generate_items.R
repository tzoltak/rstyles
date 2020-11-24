#' @title Internals: making object representing an item
#' @param scoringMatrix matrix describing how responses (described in rownames
#' of the matrix) map on \emph{scores} of latent traits (described in columns of
#' the matrix)
#' @param slopes \strong{named} numeric vector of slope parameters with names describing
#' latent variables matchng each slope (must contain at least all the names
#' occuring in column names of the \code{scoringMatrix} but may also contain
#' additional slopes matching latent traits scoring patterns on which will be
#' returned by functions provided with parameters
#' \code{scoringOnPreviousResponses} or \code{editResponse})
#' @param intercepts numeric vector of intercept parameters (must be shorter
#' of one than number of rows in the \\code{scoringMatrix} or the same length
#' but with the first element being 0)
#' @param mode a way the item should be answered - see
#' \code{\link{generate_item_responses_sqn}},
#' \code{\link{generate_item_responses_sml}}
#' @param scoringOnPreviousResponses optional function returning a column vector
#' that will be put before first column of the \code{scoringMatrix}
#' @param editResponse only if \code{mode='sequential'}: optional function
#' returning scoring matrix that should replace that provided by
#' \code{scoringMatrix} after \emph{response is made} at the first \emph{node};
#' this should be function accepting two parameters: \code{response} - generated
#' response (by the model described with the first column of the
#' \code{scoringMatrix}) that is supposed to be \emph{edited} and
#' \code{scoringMatrix} - current scoring matrix (to be replaced)
#' @description Function mostly performs checks whether provided parameters are
#' reasonable and match each other.
#' @export
make_item <- function(scoringMatrix, slopes, intercepts,
                      mode = c('sequential', 'simultaneous'),
                      scoringOnPreviousResponses = NULL, editResponse = NULL) {
  stopifnot(is.matrix(scoringMatrix),
            ncol(scoringMatrix) > 0,
            nrow(scoringMatrix) > 1,
            length(unique(rownames(scoringMatrix))) == nrow(scoringMatrix),
            !anyNA(scoringMatrix[, 1]),
            all(!duplicated(scoringMatrix)),
            all(apply(scoringMatrix, 1, function(x) {
              return(max(which(!is.na(x))) < min(which(is.na(x))))})),
            is.numeric(slopes),
            !anyNA(slopes),
            all(colnames(scoringMatrix) %in% names(slopes)),
            !is.null(scoringOnPreviousResponses) | !is.null(editResponse) |
              all(names(slopes) %in% colnames(scoringMatrix)),
            is.numeric(intercepts),
            length(intercepts) %in% nrow(scoringMatrix) + c(0, 1),
            !anyNA(intercepts),
            length(intercepts) == (nrow(scoringMatrix) - 1) | intercepts[1] == 0)
  if (!is.null(scoringOnPreviousResponses)) {
    stopifnot(is.function(scoringOnPreviousResponses),
              all(c("previousResponses", "scoringMatrix") %in%
                    names(formals(editResponse))))
  }
  if (!is.null(editResponse)) {
    stopifnot(is.function(editResponse),
              all(c("response", "scoringMatrix") %in%
                    names(formals(editResponse))))
  }
  if (length(intercepts) < nrow(scoringMatrix)) {
    intercepts <- c(0, intercepts)
  }
  return(structure(list(scoringMatrix = scoringMatrix,
                        slopes = slopes,
                        intercepts = intercepts,
                        mode = mode,
                        scoringOnPreviousResponses = scoringOnPreviousResponses,
                        editResponse = editResponse),
                   class = c("rstylesItem", "list")))
}
