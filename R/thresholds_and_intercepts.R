#' @title Conversion between thresholds and intercepts parametrisations
#' @description Utility functions allowing to covert \emph{thresholds} (i.e.
#' parameterisation convienient to think of item steps difficulty) to
#' \emph{intercepts} (i.e. parameterisation used internally by packages
#' \emph{rstyles} and \emph{mirt}) or back forth.
#' @param thresholds vector or matrix (thresholds in cols, items in rows) of
#' item thresholds
#' @param intercepts vector or matrix (intercepts in cols, items in rows) of
#' item intercepts
#' @seealso \code{\link{generate_intercepts}}
#' @returns vector or matrix of thresholds or intercepts6
#' @examples
#' # 5 items with (general) difficulty evenly spanned between -2 and 2
#' # and for each item thresholds eveny spanned betwenn -1.5 and 1.5 relatively
#' # to item difficulty
#' thresholds <- matrix(rep(seq(-2, 2, by = 1), 4) +
#'                      rep(seq(-1.5, 1.5, by = 0.75), each = 4),
#'                      ncol = 4)
#' (intercepts <- thresholds2intercepts(thresholds))
#' intercepts2thresholds(intercepts)
#' # works also for vectors
#' thresholds2intercepts(thresholds[1, ])
#' intercepts2thresholds(intercepts[1, ])
#' @name thresholds_and_intercepts
#' @export
thresholds2intercepts <- function(thresholds) {
  UseMethod("thresholds2intercepts", thresholds)
}
#' @rdname thresholds_and_intercepts
#' @export
thresholds2intercepts.data.frame <- function(thresholds) {
  return(thresholds2intercepts(as.matrix(thresholds)))
}
#' @rdname thresholds_and_intercepts
#' @export
thresholds2intercepts.matrix <- function(thresholds) {
  stopifnot(is.numeric(thresholds) | is.integer(thresholds))
  colnames(thresholds) <- paste0("d", 1L:ncol(thresholds))
  return(t(apply(cbind(d0 = rep(0, nrow(thresholds)), thresholds), 1, cumsum)))
}
#' @rdname thresholds_and_intercepts
#' @export
thresholds2intercepts.default <- function(thresholds) {
  stopifnot("Only methods for matrices, data frames and numeric or integer vectors are implemented." =
              is.vector(thresholds),
            "Only methods for matrices, data frames and numeric or integer vectors are implemented." =
              is.numeric(thresholds) | is.integer(thresholds))
  names(thresholds) <- paste0("d", 1L:length(thresholds))
  return(c(d0 = 0, cumsum(thresholds)))
}
#' @rdname thresholds_and_intercepts
#' @export
intercepts2thresholds <- function(intercepts) {
  UseMethod("intercepts2thresholds", intercepts)
}
#' @rdname thresholds_and_intercepts
#' @export
intercepts2thresholds.data.frame <- function(intercepts) {
  return(intercepts2thresholds(as.matrix(intercepts)))
}
#' @rdname thresholds_and_intercepts
#' @export
intercepts2thresholds.matrix <- function(intercepts) {
  stopifnot(is.numeric(intercepts) | is.integer(intercepts))
  if (!all(intercepts[, 1L] == 0)) {
    warning("Adding 'd0' intercept equal to 0 (as the first element).")
    intercepts <- cbind(rep(0, nrow(intercepts)), intercepts)
  }
  colnames(intercepts) <- paste0("t", 0L:(ncol(intercepts) - 1L))
  return(intercepts[, -1L, drop = FALSE] -
           intercepts[, -ncol(intercepts), drop = FALSE])
}
#' @rdname thresholds_and_intercepts
#' @export
intercepts2thresholds.default <- function(intercepts) {
  stopifnot("Only methods for matrices, data frames and numeric or integer vectors are implemented." =
              is.vector(intercepts),
            "Only methods for matrices, data frames and numeric or integer vectors are implemented." =
              is.numeric(intercepts) | is.integer(intercepts))
  if (intercepts[1L] != 0) {
    warning("Adding 'd0' intercept equal to 0 (as the first element).")
    intercepts <- c(0, intercepts)
  }
  names(intercepts) <- paste0("t", 0L:(length(intercepts) - 1L))
  return(intercepts[-1L] - intercepts[-length(intercepts)])
}
