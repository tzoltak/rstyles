#' @title Internals: making object representing an item
#' @description Function mostly performs checks whether provided parameters are
#' reasonable and match each other.
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
#' @export
make_item <- function(scoringMatrix, slopes, intercepts,
                      mode = c('sequential', 'simultaneous'),
                      scoringOnPreviousResponses = NULL, editResponse = NULL) {
  stopifnot("Parameter `scoringMatrix` must be a numeric matrix." =
              is.matrix(scoringMatrix),
            "Parameter `scoringMatrix` must be a numeric matrix." =
              is.numeric(scoringMatrix),
            "Parameter `scoringMatrix` must have at least one column." =
              ncol(scoringMatrix) > 0L,
            "Parameter `scoringMatrix` must have at least two rows." =
              nrow(scoringMatrix) > 1L,
            "Parameter `scoringMatrix` must have unique rownames." =
              length(unique(rownames(scoringMatrix))) == nrow(scoringMatrix),
            "With parameter `mode` set to 'simultaneous` parameter `scoringMatrix` can't contain NAs." =
              mode == 'sequential' || !anyNA(scoringMatrix),
            "Parameter `scoringMatrix` can't contain NAs in the first column." =
              !anyNA(scoringMatrix[, 1L]),
            "Parameter 'scoringMatrix` can't contain duplicated rows." =
              all(!duplicated(scoringMatrix)),
            "Parameter `slopes` must be a numeric vector." =
              is.vector(slopes),
            "Parameter `slopes` must be a numeric vector." =
              is.numeric(slopes),
            "Parameter `slopes` can't contain NAs." =
              !anyNA(slopes),
            "Elements of parameter `slopes` must have unique names." =
              all(!duplicated(names(slopes))),
            "All the colnames of `scoringMatrix` must appear in names of `slopes`." =
              all(colnames(scoringMatrix) %in% names(slopes)),
            "With no parameters `scoringOnPreviousResponses` or `editResponse` being provided, all the names of `slopes` must appear in names of `scoringMatrix`." =
              !is.null(scoringOnPreviousResponses) | !is.null(editResponse) |
              all(names(slopes) %in% colnames(scoringMatrix)),
            "Parameter `intercepts` must be a numeric vector." =
              is.numeric(intercepts),
            "Parameter `intercepts` must be a numeric vector." =
              is.vector(intercepts),
            "Parameter `intercepts` can't contain NAs." =
              !anyNA(intercepts))
  if (mode == "sequential") {
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
    stopifnot("Parameter `intercepts` must have the same length or be shorter by one than a number of rows of `scoringMatrix`." =
                length(intercepts) %in% (nrow(scoringMatrix) - c(0L, 1L)),
              "If length of `intercepts` is the same as number of rows of `scoringMatrix`, the first element of `intercepts` must be 0." =
                length(intercepts) == (nrow(scoringMatrix) - 1L) | intercepts[1L] == 0)
    if (length(intercepts) < nrow(scoringMatrix)) {
      intercepts <- c(d0 = 0, intercepts)
    }
  }
  if (!is.null(scoringOnPreviousResponses)) {
    stopifnot("Parameter `scoringOnPreviousResponses` must be a function." =
                is.function(scoringOnPreviousResponses),
              "Function provided by parameter `scoringOnPreviousResponses` must accept parameters `previousResponses` and `scoringMatrix`." =
                all(c("previousResponses", "scoringMatrix") %in%
                      names(formals(scoringOnPreviousResponses))))
    traitNames <- lapply(rownames(scoringMatrix),
                         scoringOnPreviousResponses,
                         scoringMatrix)
    traitNames <- unique(sapply(traitNames, colnames))
    if (!all(traitNames %in% names(slopes))) {
      stop("Function provided by parameter `scoringOnPreviousResponses` returns scoring pattern regarding trait(s): '",
           paste(setdiff(traitNames, names(slopes)), collapse = "', '"),
           "' but there is/are no slope(s) provided for this trait(s).")
    }
  }
  if (!is.null(editResponse)) {
    stopifnot("Parameter `editResponse` must be a function." =
                is.function(editResponse),
              "Function provided by parameter `editResponse` must accept parameters `response` and `scoringMatrix`." =
                all(c("response", "scoringMatrix") %in%
                    names(formals(editResponse))))
    traitNames <- lapply(rownames(scoringMatrix),
                         editResponse,
                         scoringMatrix)
    traitNames <- unique(sapply(traitNames, colnames))
    if (!all(traitNames %in% names(slopes))) {
      stop("Function provided by parameter `editResponse` returns scoring pattern regarding trait(s): '",
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
