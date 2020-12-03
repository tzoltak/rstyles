# generate_test <- function(nItems, a, d, b = NULL) {
#   stopifnot(is.numeric(nItems),
#             length(nItem) == 1,
#             !is.na(nItems),
#             is.list(a),
#             is.list(d))
#   if (!is.null(b)) {
#     stopifnot(is.numeric(b),
#               length(b) == nItems)
#   }
# }
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
  stopifnot("Parameter `scoringMatrix` must be a numeric matrix." =
              is.matrix(scoringMatrix),
            "Parameter `scoringMatrix` must be a numeric matrix." =
              is.numeric(scoringMatrix),
            "Parameter `scoringMatrix` must have at least one column." =
              ncol(scoringMatrix) > 0,
            "Parameter `scoringMatrix` must have at least two rows." =
              nrow(scoringMatrix) > 1,
            "Parameter `scoringMatrix` must have unique rownames." =
              length(unique(rownames(scoringMatrix))) == nrow(scoringMatrix),
            "With parameter `mode` set to 'sequential` parameter `scoringMatrix` can't contain NAs." =
              mode == 'sequential' | !anyNA(scoringMatrix),
            "Parameter `scoringMatrix` can't contain NAs in the first column." =
              !anyNA(scoringMatrix[, 1]),
            "In each row of `scoringMatrix` NA element can't be followed by a non-NA element." =
              all(apply(scoringMatrix, 1, function(x) {
                return(max(which(!is.na(x))) < ifelse(anyNA(x),
                                                      min(which(is.na(x))),
                                                      length(x) + 1))})),
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
            "Parameter `intercepts` must have the same length or be shorter by one than a number of rows of `scoringMatrix`." =
              length(intercepts) %in% (nrow(scoringMatrix) - c(0, 1)),
            "If length of `intercepts` is the same as number of rows of `scoringMatrix`, the first element of `intercepts` must be 0." =
              length(intercepts) == (nrow(scoringMatrix) - 1) | intercepts[1] == 0,
            "Parameter `intercepts` can't contain NAs." =
              !anyNA(intercepts))
  if (!is.null(scoringOnPreviousResponses)) {
    stopifnot("Parameter `scoringOnPreviousResponses` must be a function." =
                is.function(scoringOnPreviousResponses),
              "Function provided by parameter `scoringOnPreviousResponses` must accept parameters `previousResponses` and `scoringMatrix`." =
                all(c("previousResponses", "scoringMatrix") %in%
                      names(formals(scoringOnPreviousResponses))))
  }
  if (!is.null(editResponse)) {
    stopifnot("Parameter `editResponse` must be a function." =
                is.function(editResponse),
              "Function provided by parameter `editResponse` must accept parameters `response` and `scoringMatrix`." =
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
