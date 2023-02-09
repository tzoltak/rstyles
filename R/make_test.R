#' @title Creating object representing a test
#' @description Function makes a list object representing test items given
#' matrices of slopes and intercepts/thresholds parameters. Result may be used
#' within a call to \code{\link{generate_test_responses}}.
#' @param scoringMatrix a \emph{scoring matrix} that should be used for items,
#' especially one generated with \code{\link{make_scoring_matrix_aem}}
#' @param slopes a matrix of slope parameters (items in rows, traits in cols),
#' especially generated with \code{\link{generate_slopes}}
#' @param intercepts a matrix of intercept parameters (items in rows,
#' intercepts/thresholds in cols), especially generated with
#' \code{\link{generate_intercepts}}
#' @param mode the way the item should be answered - see
#' \code{\link{generate_item_responses_sqn}},
#' \code{\link{generate_item_responses_sml}}
#' @param scoringOnPreviousResponses an optional function returning a column
#' vector that will be put before first column of the \code{scoringMatrix}
#' @param editResponse only if \code{mode='irtree'}: an optional function
#' returning scoring matrix that should replace that provided by
#' \code{scoringMatrix} after \emph{response is made} at the first \emph{node};
#' this should be function accepting two arguments: \code{response} - generated
#' response (by the model described with the first column of the
#' \code{scoringMatrix}) that is supposed to be \emph{edited} and
#' \code{scoringMatrix} - current scoring matrix (to be replaced)
#' @param names an optional character vector providing names of the items (by
#' default names will be created as concatenation of the letter "i" - like
#' "item" - and consecutive integers)
#' @details Function is actually a simple wrapper around \code{\link{make_item}}
#' - see documentation of this function for further details.
#'
#' \strong{Column names of the \code{intercepts} matrix:}
#'
#' \itemize{
#'   \item{If \code{mode = "gpcm"} names should be of the form:
#'         \emph{dN} where \emph{d} stands for itself and \emph{N} are
#'         consecutive integers from 1 to one less than the number of rows of
#'         the the \emph{scoring matrix}.}
#'   \item{If \code{mode = "irtree"} names should be of the form:
#'         \emph{traitN} where \emph{trait} are names of traits - the same as
#'         those in columns of the \emph{scoring matrix} - and \emph{N} are
#'         integers describing consecutive thresholds of a \emph{pseudo-item}
#'         representing the given trait. In the most typical case with binary
#'         \emph{pseudo-items} this should be simply "1" (with in one column for
#'         each trait) but polytomous \emph{pseudo-items} need to be represented
#'         by the number of columns less by one than the number of possible
#'         responses, with \emph{N} being consecutive integers.}
#' }
#' @return A list of objects of the \emph{rstylesItem} class.
#' @examples
#' ################################################################################
#' # responses to 10 items using 5-point Likert scale
#' # with respect to the Bockenholt's IRTree "MAE" model
#' # 1) make scoring matrix
#' sM <- make_scoring_matrix_aem(5, "mae")
#' # 2) generate items' slopes:
#' # slopes on the 'middle" and "extreme" latent traits set to 1 for all items
#' # and slopes on the "acquiescence" latent trait generated from a log-normal
#' # distribution with expected value of about 1.02 and standard deviation of
#' # about 0.21
#' slopes <-  cbind(generate_slopes(10, sM[, 1, drop = FALSE], 1),
#'                  generate_slopes(10, sM[, 2, drop = FALSE], FUN = rlnorm,
#'                                  meanlog = 0,
#'                                  sdlog = 0.2),
#'                  generate_slopes(10, sM[, 3, drop = FALSE], 1))
#' # 3) generate items' intercepts:
#' # intercepts generated from the uniform distribution with limits set to
#' # -1.5 and 1.5
#' intercepts <- generate_intercepts(10, sM, runif,
#'                                   list(min = -1.5,
#'                                        max = 1.5))
#' # 4) call `make_test()`
#' # (for IRTree mode must be set accordingly)
#' test <- make_test(sM, slopes, intercepts, "irtree")
#'
#' ################################################################################
#' # responses to 20 items using 5-point Likert scale
#' # with respect to the Plieninger's GPCM (partialy-compensatory) model
#' # 1) make scoring matrix
#' sM <- make_scoring_matrix_aem(5, "gpcm")
#' # 2) generate items' slopes:
#' # slopes on the 'middle", "extreme" and "acquiescence" latent traits
#' # set to 1 for all items and slopes on the "intensity" latent trait generated
#' # from a normal distribution with expected value of 1 and standard deviation
#' # of 0.3
#' slopes <-  cbind(generate_slopes(20, sM[, 1, drop = FALSE], FUN = rnorm,
#'                                  mean = 1, sd = 0.3),
#'                  generate_slopes(20, sM[, -1], 1))
#' # 3) generate items' thresholds:
#' # thresholds generated from the normal distributon of items' (general)
#' # diificulty wit expected value of 0 and standardo deviation of 1.5 and the
#' # uniform distribution with limits -1 and 1 of categories' thresholds
#' # relative to item's difficulty
#' intercepts <- generate_intercepts(20, sM, FUNd = rnorm, FUNt = runif,
#'                                   argsd = list(mean = 0, sd = 1.5),
#'                                   argst = list(min = -1, max = 1))
#' # 4) call `make_test()`
#' test <- make_test(sM, slopes, intercepts, "gpcm")
#' @export
make_test <- function(scoringMatrix, slopes, intercepts,
                      mode = c('irtree', 'gpcm'),
                      scoringOnPreviousResponses = NULL, editResponse = NULL,
                      names = paste0("i", 1:nrow(slopes))) {
  # make_item() performs detailed assertions, so here there are only the basic ones
  mode <- match.arg(mode)
  stopifnot("Argument `slopes` must be a numeric matrix." = is.matrix(slopes),
            "Argument `slopes` must be a numeric matrix." = is.numeric(slopes),
            "Argument `intercepts` must be a numeric matrix." =
              is.matrix(intercepts),
            "Argument `intercepts` must be a numeric matrix." =
              is.numeric(intercepts),
            "Matrices provide by `slopes` and `intercepts` arguments must have the same number of rows." =
              nrow(slopes) == nrow(intercepts),
            is.function(scoringOnPreviousResponses) |
              is.null(scoringOnPreviousResponses),
            is.function(editResponse) | is.null(editResponse),
            "Argument `names` must be a character vector." =
              is.character(names),
            "Length of the `names` must be equal to number of rows of `slopes` and `intercepts`." =
              length(names) == nrow(slopes))
  items <- vector(mode = "list", length = nrow(slopes))
  names(items) <- names
  for (i in 1L:length(items)) {
    items[[i]] <- make_item(scoringMatrix = scoringMatrix,
                            slopes = slopes[i, ],
                            intercepts = intercepts[i, ],
                            mode = mode,
                            scoringOnPreviousResponses =
                              scoringOnPreviousResponses,
                            editResponse = editResponse)
  }
  class(items) <- c("rstylesTest", class(items))
  return(items)
}
