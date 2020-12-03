#' @export
score_on_last_answer_straight <- function(previousResponses, scoringMatrix) {
  answers <- rownames(scoringMatrix)
  previousResponse <- previousResponses[length(previousResponses)]
  return(matrix(as.numeric(previousResponse == rownames(scoringMatrix)),
                ncol = 1, dimnames = list(answers, "spa")))
}
#' @export
score_on_last_answer_next <- function(previousResponses, scoringMatrix) {

}
#' @export
score_on_last_answer_previous <- function(previousResponses, scoringMatrix) {

}
