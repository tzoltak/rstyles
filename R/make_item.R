#' @title Creating object representing an item
#' @description Function mostly performs checks whether provided arguments are
#' reasonable and match each other.
#' @param scoringMatrix a matrix describing how responses (described in rownames
#' of the matrix) map on \emph{scores} of latent traits (described in columns of
#' the matrix)
#' @param slopes a \strong{named} numeric vector of slope (discrimination)
#' parameters with names describing latent variables matching each slope
#' (must contain at least all the names occurring in column \emph{names} of the
#' \code{scoringMatrix} but may also contain additional slopes matching latent
#' traits scoring patterns that will be returned by functions provided with
#' arguments \code{scoringOnPreviousResponses} or \code{editResponse})
#' @param intercepts a numeric vector of intercept parameters (must be shorter
#' of one than number of rows in the \\code{scoringMatrix} or the same length
#' but with the first element being 0)
#' @param mode the way the item should be answered - see
#' \code{\link{generate_item_responses_sqn}} for "irtree" and
#' \code{\link{generate_item_responses_sml}} for "gpcm"
#' @param scoringOnPreviousResponses an optional function returning a column vector
#' that will be put before first column of the \code{scoringMatrix}
#' @param editResponse only if \code{mode='irtree'}: an optional function
#' returning scoring matrix that should replace that provided by
#' \code{scoringMatrix} after \emph{response is made} at the first \emph{node};
#' this should be function accepting two arguments: \code{response} - generated
#' response (by the model described with the first column of the
#' \code{scoringMatrix}) that is supposed to be \emph{edited} and
#' \code{scoringMatrix} - current scoring matrix (to be replaced)
#' @return An object of class \emph{rstylesItem} representing an item. List of
#' such objects is passed as a test specification to
#' \code{\link{generate_test_responses}}.
#' @export
make_item <- function(scoringMatrix, slopes, intercepts,
                      mode = c('irtree', 'gpcm'),
                      scoringOnPreviousResponses = NULL, editResponse = NULL) {
  mode <- match.arg(mode)
  stopifnot("Argument `scoringMatrix` must be a numeric matrix." =
              is.matrix(scoringMatrix),
            "Argument `scoringMatrix` must be a numeric matrix." =
              is.numeric(scoringMatrix),
            "Argument `scoringMatrix` must have at least one column." =
              ncol(scoringMatrix) > 0L,
            "Argument `scoringMatrix` must have at least two rows." =
              nrow(scoringMatrix) > 1L,
            "Argument `scoringMatrix` must have unique rownames." =
              length(unique(rownames(scoringMatrix))) == nrow(scoringMatrix),
            "With argument `mode` set to 'gpcm` argument `scoringMatrix` can't contain NAs." =
              mode == 'irtree' || !anyNA(scoringMatrix),
            "Argument `scoringMatrix` can't contain NAs in the first column." =
              !anyNA(scoringMatrix[, 1L]),
            "Argument 'scoringMatrix` can't contain duplicated rows." =
              all(!duplicated(scoringMatrix)),
            "Argument `slopes` must be a numeric vector." =
              is.vector(slopes),
            "Argument `slopes` must be a numeric vector." =
              is.numeric(slopes),
            "Argument `slopes` can't contain NAs." =
              !anyNA(slopes),
            "Elements of argument `slopes` must have unique names." =
              all(!duplicated(names(slopes))),
            "All the colnames of `scoringMatrix` must appear in names of `slopes`." =
              all(colnames(scoringMatrix) %in% names(slopes)),
            "With no arguments `scoringOnPreviousResponses` or `editResponse` being provided, all the names of `slopes` must appear in names of `scoringMatrix`." =
              !is.null(scoringOnPreviousResponses) | !is.null(editResponse) |
              all(names(slopes) %in% colnames(scoringMatrix)),
            "Argument `intercepts` must be a numeric vector." =
              is.numeric(intercepts),
            "Argument `intercepts` must be a numeric vector." =
              is.vector(intercepts),
            "Argument `intercepts` can't contain NAs." =
              !anyNA(intercepts))
  if (mode == "irtree") {
    for (i in 1L:ncol(scoringMatrix)) {
      if (length(setdiff(unique(scoringMatrix[, i]), NA_integer_)) !=
          (length(grep(paste0("^", colnames(scoringMatrix)[i],
                              "_?[[:digit:]]+$"), names(intercepts))) + 1)) {
        stop("Number of intercepts provided for trait '",
             colnames(scoringMatrix)[i],
             "' doesn't match number of distinct scores for this trait in the scoring matrix (there should be one more distinct scores than number of intercepts).")
      }
    }
    intercepts <- c(d0 = 0, intercepts)
  } else {
    stopifnot("Argument `intercepts` must have the same length or be shorter by one than a number of rows of `scoringMatrix`." =
                length(intercepts) %in% (nrow(scoringMatrix) - c(0L, 1L)))
    if (length(intercepts) == nrow(scoringMatrix) & intercepts[1L] != 0) {
      warning("It is atypical to provide the same number of `intercepts` as the number of rows of `scoringMatrix` and not setting the first intercept to 0.")
    }
    if (length(intercepts) < nrow(scoringMatrix)) {
      intercepts <- c(d0 = 0, intercepts)
    }
  }
  if (!is.null(scoringOnPreviousResponses)) {
    stopifnot("Argument `scoringOnPreviousResponses` must be a function." =
                is.function(scoringOnPreviousResponses),
              "Function provided by argument `scoringOnPreviousResponses` must accept arguments `previousResponses` and `scoringMatrix`." =
                all(c("previousResponses", "scoringMatrix") %in%
                      names(formals(scoringOnPreviousResponses))))
    traitNames <- lapply(rownames(scoringMatrix),
                         scoringOnPreviousResponses,
                         scoringMatrix)
    traitNames <- unique(sapply(traitNames, colnames))
    if (!all(traitNames %in% names(slopes))) {
      stop("Function provided by argument `scoringOnPreviousResponses` returns scoring pattern regarding trait(s): '",
           paste(setdiff(traitNames, names(slopes)), collapse = "', '"),
           "' but there is/are no slope(s) provided for this trait(s).")
    }
  }
  if (!is.null(editResponse)) {
    stopifnot("Argument `editResponse` must be a function." =
                is.function(editResponse),
              "Function provided by argument `editResponse` must accept arguments `response` and `scoringMatrix`." =
                all(c("response", "scoringMatrix") %in%
                    names(formals(editResponse))))
    traitNames <- lapply(rownames(scoringMatrix),
                         editResponse,
                         scoringMatrix)
    traitNames <- unique(sapply(traitNames, colnames))
    if (!all(traitNames %in% names(slopes))) {
      stop("Function provided by argument `editResponse` returns scoring pattern regarding trait(s): '",
           paste(setdiff(traitNames, names(slopes)), collapse = "', '"),
           "' but there is/are no slope(s) provided for this trait(s).")
    }
  }
  return(structure(list(scoringMatrix = scoringMatrix,
                        slopes = slopes,
                        intercepts = intercepts,
                        mode = mode,
                        scoringOnPreviousResponses = scoringOnPreviousResponses,
                        editResponse = editResponse),
                   class = c("rstylesItem", "list")))
}
