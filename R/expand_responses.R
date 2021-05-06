#' @title Helper functions for manipulating generated responses
#' @description Using scoring matrix provided as its second argument function
#' \emph{expands} (recodes) responses to each item (column) in a provided matrix
#' of responses into a set of variables that may be put into model estimation.
#' @param responses matrix of responses with items in columns and observations
#' in rows
#' @param scoringMatrix matrix describing scoring patterns on each latent trait
#' @return matrix (of integers)
#' @examples
#' sM <- make_scoring_matrix_aem(5, "mae")
#' responses <- matrix(sample(1L:5L, 20, replace = TRUE), ncol = 4,
#'                     dimnames = list(NULL, paste0("item", 1:4)))
#' responses
#' sM
#' expand_responses(responses, sM)
#' @export
expand_responses <- function(responses, scoringMatrix) {
  stopifnot(is.matrix(responses) | is.data.frame(responses),
            !is.null(colnames(responses)),
            !duplicated(colnames(responses)),
            is.matrix(scoringMatrix),
            !is.null(rownames(scoringMatrix)),
            !duplicated(rownames(scoringMatrix)),
            !is.null(colnames(scoringMatrix)),
            !duplicated(colnames(scoringMatrix)),
            all(unique(unlist(sapply(responses, unique))) %in%
                  rownames(scoringMatrix)))

  respWide <- matrix(NA, nrow = nrow(responses),
                     ncol = ncol(scoringMatrix) * ncol(responses),
                     dimnames = list(NULL,
                                     paste0(rep(colnames(scoringMatrix),
                                                each = ncol(responses)),
                                            "_", rep(colnames(responses),
                                                     ncol(scoringMatrix)))))
  for (i in 1L:ncol(responses)) {
    for (j in 1L:ncol(scoringMatrix)) {
      for (r in rownames(scoringMatrix)) {
        respWide[responses[, i] %in% r, (j - 1)*ncol(responses) + i] <-
          scoringMatrix[rownames(scoringMatrix) == r, j]
      }
    }
  }
  return(respWide)
}
