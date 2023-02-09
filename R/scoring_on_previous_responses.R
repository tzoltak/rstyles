#' @title Graphical patterns in careless/inattentive responding
#' @description Functions that allows to model careless/inattentive responding
#' in a form of creating various \emph{graphical patterns} of responses:
#' \itemize{
#'   \item{\code{score_on_last_answer_straight} - \emph{straightlining}, i.e.
#'         tendency to choose the same answer as in the previous item;}
#'   \item{\code{score_on_last_answer_next} - tendency to choose answer that
#'         is \emph{directly to the right side/below} of the answer to the
#'         previous item; if there are no answers \emph{farther right/below},
#'         than turn to the first (\emph{most left/top}) available answer;}
#'   \item{\code{score_on_last_answer_previous} - tendency to choose answer that
#'         is \emph{directly to the left side/up} of the answer to the
#'         previous item; if there are no answers \emph{farther left/up},
#'         than turn to the last (\emph{most right/bottom}) available answer;}
#'   \item{\code{score_on_previous_answers_bounce} - tendency to choose answer
#'         that is either \emph{directly to the right side/below} or
#'         \emph{directly to the left side/up} of the answer to the previous
#'         item in such a way to \emph{continue a pattern} of the last two
#'         responses; if there are no more answers \emph{in a given direction},
#'         than change direction (\emph{bounce}).}
#' }
#' @param previousResponses a character vector of previous responses
#' @param scoringMatrix a matrix describing how responses (described in rownames
#' of the matrix) map on \emph{scores} of latent traits (described in columns of
#' the matrix)
#' @return a one-column matrix of 0 and 1 with the same number of rows
#' (and rownames) as \code{scoringMatrix} with column name \emph{ci}
#' @examples
#' sM <- make_scoring_matrix_aem(1:5, "gpcm")
#'
#'
#' answersStraigth <- t(sapply(c(1:5, rep(3, 5)), score_on_last_answer_straight,
#'                             scoringMatrix = sM))
#' dimnames(answersStraigth) <- list(paste0("previous answer: '",
#'                                          c(1:5, rep(3, 5)), "'"),
#'                                   paste0("'", 1:ncol(answersStraigth), "'"))
#' answersStraigth
#'
#' answersNext <- t(sapply(rep(1:5, 2), score_on_last_answer_next,
#'                         scoringMatrix = sM))
#' dimnames(answersNext) <- list(paste0("previous answer: '",
#'                                      1:nrow(answersNext), "'"),
#'                               paste0("'", 1:ncol(answersNext), "'"))
#' answersNext
#'
#' answersPrev <- t(sapply(rep(1:5, 2), score_on_last_answer_previous,
#'                         scoringMatrix = sM))
#' dimnames(answersPrev) <- list(paste0("previous answer: '",
#'                                      1:nrow(answersPrev), "'"),
#'                               paste0("'", 1:ncol(answersNext), "'"))
#' answersPrev
#'
#' bounceMatrix <- matrix(c(3:1, 2:5, 4:2, 2:1, 2:5, 4:1), ncol = 2)
#' answersBounce <- t(apply(bounceMatrix, 1, score_on_previous_answers_bounce,
#'                          scoringMatrix = sM))
#' dimnames(answersBounce) <- list(paste0("previous answers: '",
#'                                        apply(bounceMatrix, 1, paste,
#'                                              collapse = "', '"), "'"),
#'                                 paste0("'", 1:ncol(answersBounce), "'"))
#' answersBounce
#' @rdname score_on_previous_answers
#' @export
score_on_last_answer_straight <- function(previousResponses, scoringMatrix) {
  answers <- rownames(scoringMatrix)
  previousResponse <- previousResponses[length(previousResponses)]
  return(matrix(as.integer(previousResponse == rownames(scoringMatrix)),
                ncol = 1, dimnames = list(answers, "ci")))
}
#' @rdname score_on_previous_answers
#' @export
score_on_last_answer_next <- function(previousResponses, scoringMatrix) {
  answers <- rownames(scoringMatrix)
  previousResponse <- previousResponses[length(previousResponses)]
  response <-
    (which(previousResponse == rownames(scoringMatrix)) + 1L)
  if (response > length(answers)) response = 1L
  scoringMatrix <- matrix(0L, nrow = length(answers), ncol = 1,
                          dimnames = list(answers, "ci"))
  scoringMatrix[response, 1L] <- 1L
  return(scoringMatrix)
}
#' @rdname score_on_previous_answers
#' @export
score_on_last_answer_previous <- function(previousResponses, scoringMatrix) {
  answers <- rownames(scoringMatrix)
  previousResponse <- previousResponses[length(previousResponses)]
  response <-
    (which(previousResponse == rownames(scoringMatrix)) - 1L)
  if (response == 0L) response = length(answers)
  scoringMatrix <- matrix(0L, nrow = length(answers), ncol = 1,
                          dimnames = list(answers, "ci"))
  scoringMatrix[response, 1L] <- 1L
  return(scoringMatrix)
}
#' @rdname score_on_previous_answers
#' @export
score_on_previous_answers_bounce <- function(previousResponses, scoringMatrix) {
  answers <- rownames(scoringMatrix)
  if (length(previousResponses) < 2) {
    direction <- 1L
  } else if (which(previousResponses[length(previousResponses)] ==
            rownames(scoringMatrix)) >
      which(previousResponses[length(previousResponses) - 1L] ==
            rownames(scoringMatrix))) {
    direction <- 1L
  } else {
    direction <- -1L
  }
  previousResponse <- previousResponses[length(previousResponses)]
  response <-
    (which(previousResponse == rownames(scoringMatrix)) + direction)
  if (response == 0L) {
    response = 2L
  } else if (response > length(answers)) {
    response = length(answers) - 1L
  }
  scoringMatrix <- matrix(0L, nrow = length(answers), ncol = 1,
                          dimnames = list(answers, "ci"))
  scoringMatrix[response, 1L] <- 1L
  return(scoringMatrix)
}
