#' @title Generating parameters of items - slopes (discrimination)
#' @description Function generates a matrix of items' slope (discrimination)
#' parameters.
#' @param nItems the number of items for which slopes will be generated
#' @param scoringMatrix a \emph{scoring matrix} that will be used for the
#' generated items, specifically generated with
#' \code{\link{make_scoring_matrix_aem}} or
#' \code{\link{make_scoring_matrix_trivial}}
#' @param ... arguments that will be passed to \code{FUN}; should rather be
#' named and in typical applications should be numeric vectors of the length of
#' one or of the number of columns of the \code{scoringMatrix}
#' @param FUN a function that will be used to generate slopes, typically
#' @param nReversed the number of \emph{reversed} (\emph{revers-keyed}) items
#' @param reverseTraits a character vector containing names of traits for which
#' items should be \emph{reversed} (if \code{nReversed > 0}); default value is
#' "i", that is the name assigned to the trait that is assumed to describe
#' \emph{the trait that scale is supposed to measure} (i.e. trait that is not
#' related with response styles) by \code{\link{make_scoring_matrix_aem}} when
#' called with argument \code{sequence = "gpcm"}
#' @param reverseIndep a logical value indicating whether sampling items that are
#' \emph{reversed} should be performed independently for each trait (given by
#' \code{reverseTraits}) or for all the traits simultaneously (this argument
#' is only considered if \code{nReversed > 0} and there is more than one trait
#' given by \code{reverseTraits})
#' \code{\link[stats]{Normal}}, \code{\link[stats]{Lognormal}},
#' \code{\link[stats]{Uniform}} or \code{\link[truncnorm]{rtruncnorm}}
#' - see details
#' @details There are two typical ways of using this function (compare examples
#' below):
#' \itemize{
#'   \item{If one wants to generate constant slopes for all the items, one
#'         should simply provide a vector which elements describe slope for
#'         each of the consecutive traits. You may name elements of this vector
#'         to get this names assigned to columns of the returned matrix.}
#'   \item{If one wants to sample slopes using specific distribution, one
#'         must provide a sampling function using \code{FUN} argument.
#'         Arguments that should be passed to this function one should provide
#'         as additional arguments to a \code{generate_slopes} call. If one
#'         wants to generate slopes for more than one trait, one needs to
#'         provide at least one of these arguments as a vector with as many
#'         elements, as there are traits (even if all of its elements would be
#'         equal). Moreover one may name elements of this vector, to get these
#'         names assigned to columns of the returned matrix. Other arguments may
#'         be provided as single values if one wants to hold them constant
#'         across traits.}
#' }
#' In a case slopes are sampled, this is done independently for each of the
#' traits. The same sampling function is used for each trait and there is no
#' way to change this (but one may still use \code{\link{cbind}} to collapse
#' results of several separate \code{generate_slopes} calls to achieve this
#' goal).
#' @return A matrix of \code{nItems} rows and number of columns equal to the
#' length of vectors provided by \code{...}.
#' @seealso \code{\link{generate_intercepts}}, \code{\link{make_test}}
#' @examples
#' # 5 items with slopes equals 1 on all the latent traits
#' sM <- make_scoring_matrix_aem(5, sequence = "mae")
#' generate_slopes(5, sM, 1)
#' # 10 items with slopes on all the latent traits generated from a uniform
#' # distribution with limits 0.5 and 3
#' sM <- make_scoring_matrix_aem(5, sequence = "mae")
#' generate_slopes(10, sM, FUN = runif, min = 0.5, max = 3)
#' # 10 items with slopes generated from a normal distributions with parameters set to:
#' # trait 'm' (i.e. the first column in the scoring matrix): mean 1, sd 0.2
#' # trait 'a' (i.e. the second column in the scoring matrix): mean 1.2, sd 0.3
#' # trait 'e' (i.e. the third column in the scoring matrix): mean 1.5, sd 0.5
#' sM <- make_scoring_matrix_aem(5, sequence = "mae")
#' generate_slopes(10, sM, FUN = rnorm,
#'                 mean = c(1, 1.2, 1.5),
#'                 sd = c(0.2, 0.3, 0.5))
#' # 10 items with slopes generated from a truncated-normal distributions with
#' # parameters set to:
#' # trait 'm' (i.e. the first column in the scoring matrix): mean 1, sd 0.5
#' # trait 'a' (i.e. the second column in the scoring matrix): mean 1.2, sd 0.7
#' # trait 'e' (i.e. the third column in the scoring matrix): mean 1.5, sd 1
#' # and bounds equals to 0.5 (lower) and 2.5 (upper) for all the traits
#' sM <- make_scoring_matrix_aem(5, sequence = "mae")
#' require(truncnorm)
#' generate_slopes(10, sM, FUN = rtruncnorm,
#'                 mean = c(1, 1.2, 1.5),
#'                 sd = c(0.5, 0.7, 1),
#'                 a = 0.5,
#'                 b = 2.5)
#' # 10 items with slopes generated from a normal distributions with mean of 1
#' # and standard deviation of 0.2 with half of the items "reverse-keyed" on
#' # the trait "i"
#' sM <- make_scoring_matrix_aem(5, sequence = "gpcm")
#' generate_slopes(10, sM, FUN = rnorm, mean = 1, sd = 0.2,
#'                 nReversed = 5, reverseTraits = "i")
#' @export
generate_slopes <- function(nItems, scoringMatrix, ..., FUN = identity,
                            nReversed = 0L, reverseTraits = "i",
                            reverseIndep = FALSE){
  stopifnot("Argument `nItems` must be a positive integer." =
              is.numeric(nItems),
            "Argument `nItems` must be a positive integer." =
              length(nItems) == 1L,
            "Argument `nItems` must be a positive integer." =
              as.integer(nItems) == nItems,
            "Argument `nItems` must be a positive integer." =
              !is.na(nItems),
            "Argument `scoringMatrix` must be a numeric matrix of at least two rows (and one column)." =
              is.matrix(scoringMatrix),
            "Argument `scoringMatrix` must be a numeric matrix of at least two rows (and one column)." =
              nrow(scoringMatrix) > 1L,
            "Argument `scoringMatrixm` must be a numeric matrix of at least two rows (and one column)." =
              ncol(scoringMatrix) > 0L,
            "Argument `FUN` must be a function." =
              is.function(FUN),
            "Argument `nReversed` must be a positive integer." =
              is.numeric(nReversed),
            "Argument `nReversed` must be a positive integer." =
              length(nReversed) == 1L,
            "Argument `nReversed` must be a positive integer." =
              as.integer(nReversed) == nReversed,
            "Argument `nReversed` must be a positive integer." =
              !is.na(nReversed),
            "Number of reversed items can not exceed number of items." =
              nReversed <= nItems,
            "Argument `reverseTraits` must be a character vector." =
              is.character(reverseTraits),
            "All the names provided by `reverseTraits` argument must occur in colmn names of the `scoringMatrix`." =
              nReversed == 0L | all(reverseTraits %in% colnames(scoringMatrix)),
            "Argument `reverseIndep` must be a logical value (TRUE or FALSE)." =
              is.logical(reverseIndep),
            "Argument `reverseIndep` must be a logical value (TRUE or FALSE)." =
              length(reverseIndep) == 1L,
            "Argument `reverseIndep` must be a logical value (TRUE or FALSE)." =
              reverseIndep %in% c(TRUE, FALSE))
  answers <- scoringMatrix[1L, ]
  names(answers) <- colnames(scoringMatrix)
  dots <- c(list(answers),
            list(...))
  colNames <- names(dots)
  dots <- tryCatch(as.data.frame(dots),
                   error = function(e) {
                     stop("Argument that will be passed to the function generating slopes ('",
                          paste(names(dots), collapse = "', "),
                          "') must be vectors of the same length as the number of columns of the matrix provided by argument `scoringMatrix` (or of the length of one).",
                          call. = FALSE)
                   })
  names(dots) <- colNames
  dots <- dots[, -1L, drop = FALSE]
  slopes <- matrix(NA_real_, nrow = nItems, ncol = nrow(dots))
  colnames(slopes) <- rownames(dots)
  if ("n" %in% names(formals(FUN))) {
    dots$n <- nItems
  }
  badArgs <- setdiff(names(dots), c(names(formals(FUN)), ""))
  if (length(badArgs) > 0L & !("..." %in% names(formals(FUN)))) {
    stop("Function provided by FUN doesn't use argument",
         ifelse(length(badArgs) > 1L, "s", ""),
         " `", paste(badArgs, collapse = "`, `"), "`.")
  }
  for (i in 1L:nrow(dots)) {
    slopes[, i] <- do.call(FUN, dots[i, , drop = FALSE])
  }
  if (nReversed > 0L) {
    reverseTraits <- which(colnames(slopes) %in% reverseTraits)
    if (reverseIndep) {
      for (i in reverseTraits) {
        slopes[, i] <- slopes[, i] * sample(c(rep(-1, nReversed),
                                              rep(1, nItems - nReversed)),
                                            nItems, replace = FALSE)
      }
    } else {
      slopes[, reverseTraits] <-
        slopes[, reverseTraits] * rep(sample(c(rep(-1, nReversed),
                                               rep(1, nItems - nReversed)),
                                             nItems, replace = FALSE),
                                      length(reverseTraits))
    }
  }
  return(slopes)
}
#' @title Generating parameters of items - intercepts (thresholds/difficulties)
#' @description Function generates a matrix of items' intercept
#' (thresholds/difficulties) parameters.
#' @param nItems the number of items for which intercepts will be generated
#' @param scoringMatrix a \emph{scoring matrix} that will be used for the
#' generated items, specifically generated with
#' \code{\link{make_scoring_matrix_aem}} or
#' \code{\link{make_scoring_matrix_trivial}}
#' @param FUNd a function that will be used to generate item difficulties,h
#' typically \code{\link[stats]{Uniform}}, \code{\link[stats]{Normal}} or
#' \code{\link[truncnorm]{rtruncnorm}}
#' @param argsd a list of arguments to be passed to \code{FUNd}
#' @param FUNt optionally a function that will be used to generate item
#' thresholds (i.e. difficulties of categories relative to difficulty of the
#' whole item), assuming \emph{gpcm} item responding process;
#' typically \code{\link[stats]{Uniform}}, \code{\link[stats]{Normal}} or
#' \code{\link[truncnorm]{rtruncnorm}}; if not set item responding process is
#' assumed to be a \emph{irtree} one;
#' @param argst optionally a list of arguments to be passed to \code{FUNt}
#' @details \strong{Assuming \emph{irtree} response process:}
#'
#' Assuming \emph{irtree} response process test item must be characterized
#' by a set of intercept parameters describing individual thresholds of binary
#' \emph{pseudo-items} modeling consecutive decisions in the assumed sequence
#' of responding process. In such a case:
#' \itemize{
#'   \item{argument \code{FUNd} provides a function that will be used to
#'         generate thresholds for each of the \code{pseudo-items};}
#'   \item{argument \code{argsd} provides arguments that will be passed to the
#'         function provided by \code{FUNd}; elements of this list should rather
#'         be named; each element of the list should be a vector (typically
#'         a numeric one) with only one element or as many elements, as the
#'         number of columns in the \code{scoringMatrix} - in this latter case
#'         consecutive elements of (each) vector will be passed to separate
#'         calls of the \code{FUNd} generating thresholds for respective
#'         \emph{pseudo-items} across all the items;}
#'   \item{arguments \code{FUNt} and \code{argst} are not used.}
#' }
#' \strong{Assuming \emph{GPCM} response process:}
#'
#' Assuming \emph{GPCM} response process test item must be characterized
#' by a set of \emph{intercept} parameters describing relative frequency of
#' specific categories of the response scale. However, it is convenient to
#' define model parameters in another parameterisation, in which item
#' \emph{thresholds} describe {difficulty} of switching between consecutive
#' categories of the response scale, and only then transform these
#' \emph{thresholds} to \emph{intercepts}. Function follows this approach and:
#' 1) first generates general \emph{difficulties} of the whole items, then
#' 2) for each item generates values of thresholds relatively to the item
#' \code{difficulty}, imposing identifiability assumption that these values must
#' sum up to 0, and finally 3) computes intercepts given values of parameters
#' generated in the previous steps. Consequently in such a case:
#' \itemize{
#'   \item{argument \code{FUNd} provides a function that will be used to
#'         generate general \code{difficulties} of items;}
#'   \item{argument \code{argsd} provides arguments that will be passed to the
#'         function provided by \code{FUNd}; elements of this list should rather
#'         be named; each element of the list should be a vector of length of
#'         one;}
#'   \item{argument \code{FUNt} provides a function that will be used to
#'         generate values of thresholds - generated values will be then sorted
#'         (descending - due to the parameterization that is used within
#'         \code{\link{generate_test_responses}}) and centered - consequently it
#'         is assumed that they are interpreted relatively to the general item
#'         \emph{difficulty};}
#'   \item{argument \code{argst} provides arguments that should be passed to the
#'         function provided by \code{FUNt}; elements of this list should rather
#'         be named; each element of the list should be a vector of length of
#'         one or of the length of \code{nItems}; in this latter case
#'         consecutive elements of (each) vector will be passed to separate
#'         calls of the \code{FUNt} generating thresholds for consecutive items;}
#' }
#'
#' Returned \emph{intercepts} are computed by summing general item
#' \emph{difficulty} and values of the relative thresholds generated for
#' a given item and then computing cumulative sum of this vector.
#' @return A matrix of \code{nItems} rows and number of columns equal to the
#' number of intercepts. \strong{Be aware that these are \emph{intercepts} and
#' not \emph{thresholds} what are returned} (and intercept for a category
#' \emph{g} is a sum of \emph{thresholds} from the first category up to the
#' category \emph{g} minus minus item difficulty).
#' @seealso \code{\link{generate_slopes}}, \code{\link{make_test}},
#' \code{\link{thresholds2intercepts}}, \code{\link{intercepts2thresholds}}
#' @examples
#' # 5 items with 5-point response scale assuming IRTree item response
#' # process with "pseudo-items" intercepts sampled from a uniform distribution
#' # with limits +-1.5
#' sM <- make_scoring_matrix_aem(5, sequence = "mae")
#' generate_intercepts(5, sM, runif, list(min = -1.5, max = 1.5))
#' # 10 items with 5-point response scale assuming IRTree item response
#' # process with "pseudo-items" intercepts sampled from a normal distribution
#' # with the mean of 0 and the standard deviation of 1.5
#' sM <- make_scoring_matrix_aem(5, sequence = "mae")
#' generate_intercepts(5, sM, rnorm, list(mean = 0, sd = 1))
#' # 10 items with 5-point response scale assuming IRTree item response
#' # process with "pseudo-items" intercepts sampled from a uniform distribution
#' # with limits set to:
#' # trait 'm' (i.e. the first column in the scoring matrix): from -3 to -1
#' # trait 'a' (i.e. the second column in the scoring matrix): from -1 to 1
#' # trait 'e' (i.e. the third column in the scoring matrix): from 1 to 3
#' sM <- make_scoring_matrix_aem(5, sequence = "mae")
#' generate_intercepts(10, sM, runif,
#'                     list(min = c(-3, -1, 1),
#'                          max = c(-1, 1, 3)))
#'
#' # 10 items with 6-point response scale assuming GPCM item response
#' # process with items difficulties sampled from a normal distribution with
#' # the mean of 0 and the standard deviation of 1.5 and thresholds relative
#' # to the items difficulties sampled from a uniform distribution with
#' # the limits of +-2
#' sM <- make_scoring_matrix_aem(6, sequence = "gpcm")
#' generate_intercepts(10, sM,
#'                     FUNd = rnorm, argsd = list(mean = 0, sd = 1.5),
#'                     FUNt = runif, argst = list(min = -2, max = 2))
#' # 5 items with 6-point response scale assuming GPCM item response
#' # process with items difficulties sampled from a uniform distribution with
#' # the limits of +-2 and thresholds relative to the items difficulties sampled
#' # from a normal distribution with the mean of 0 and the standard deviation
#' # defined individually for each item
#' # the limits of +-2
#' sM <- make_scoring_matrix_aem(6, sequence = "gpcm")
#' generate_intercepts(5, sM,
#'                     FUNd = runif, argsd = list(min = -2, max = 2),
#'                     FUNt = rnorm, argst = list(mean = 0,
#'                                                sd = c(1, 1.2, 1.4, 1.6, 1.9)))
#' # 20 items with 5-point response scale assuming GPCM item response
#' # process with items difficulties sampled from a uniform distribution with
#' # the limits of +-2 and thresholds relative to the items difficulties
#' # generated deterministically as a sequence of 4 regularly spaced values
#' # from 0.9 to -0.9
#' sM <- make_scoring_matrix_aem(5, sequence = "gpcm")
#' generate_intercepts(20, sM,
#'                     FUNd = runif, argsd = list(min = -2, max = 2),
#'                     FUNt = seq, argst = list(from = 0.9,
#'                                              to = -0.9,
#'                                              length.out = 4))
#' @export
generate_intercepts <- function(nItems, scoringMatrix, FUNd, argsd = NULL,
                                FUNt = NULL, argst = NULL) {
  stopifnot("Argument `nItems` must be a positive integer." =
              is.numeric(nItems),
            "Argument `nItems` must be a positive integer." =
              length(nItems) == 1L,
            "Argument `nItems` must be a positive integer." =
              as.integer(nItems) == nItems,
            "Argument `nItems` must be a positive integer." =
              !is.na(nItems),
            "Argument `scoringMatrix` must be a numeric matrix of at least two rows (and one column)." =
              is.matrix(scoringMatrix),
            "Argument `scoringMatrix` must be a numeric matrix of at least two rows (and one column)." =
              nrow(scoringMatrix) > 1L,
            "Argument `scoringMatrix` must be a numeric matrix of at least two rows (and one column)." =
              ncol(scoringMatrix) > 0L,
            "Argument `FUNs` must be a function." =
              is.function(FUNd),
            "Argument `argsd` must be a list (or NULL)." =
              is.list(argsd) | is.null(argsd),
            "Argument `FUNs` must be a function (or NULL)." =
              is.function(FUNt) | is.null(FUNt),
            "Argument `argst` must be a list (or NULL)." =
              is.list(argst) | is.null(argst))
  if (!is.null(FUNt)) {
    stopifnot("If argument `FUNt` is provided elements of the list provided by argument `argsd` must be vectors of length one." =
                all(sapply(argsd, length) == 1L))
    return(generate_intercepts_sml(nItems = nItems,
                                   scoringMatrix = scoringMatrix,
                                   FUNd = FUNd, argsd = argsd,
                                   FUNt = FUNt, argst = argst))
  } else {
    return(generate_intercepts_sqn(nItems = nItems,
                                   scoringMatrix = scoringMatrix,
                                   FUNd = FUNd, argsd = argsd))
  }
}
generate_intercepts_sqn <- function(nItems, scoringMatrix, FUNd, argsd) {
  argsd <- c(list(scoringMatrix[1, ]), argsd)
  colNames <- names(argsd)
  argsd <- tryCatch(as.data.frame(argsd),
                    error = function(e) {
                      stop("Argument that will be passed to the function generating intercepts ('",
                           paste(names(argsd), collapse = "', "),
                           "') must be vectors of the same length as the number of columns of the matrix provided by argument `scoringMatrix` (or of the length of one).",
                           call. = FALSE)
                    })
  names(argsd) <- colNames
  argsd <- argsd[, -1L, drop = FALSE]
  intercepts <- matrix(NA_real_, nrow = nItems, ncol = nrow(argsd))
  colnames(intercepts) <- paste0(rownames(argsd), "1")
  if ("n" %in% names(formals(FUNd))) {
    argsd$n <- nItems
  }
  badArgs <- setdiff(names(argsd), c(names(formals(FUNd)), ""))
  if (length(badArgs) > 0L & !("..." %in% names(formals(FUNd)))) {
    stop("Function provided by FUNd doesn't use argument",
         ifelse(length(badArgs) > 1L, "s", ""),
         " `", paste(badArgs, collapse = "`, `"), "`.")
  }
  for (i in 1L:nrow(argsd)) {
    intercepts[, i] <- do.call(FUNd, argsd[i, , drop = FALSE])
  }
  return(intercepts)
}
generate_intercepts_sml <- function(nItems, scoringMatrix, FUNd, argsd,
                                    FUNt, argst) {
  badArgs <- setdiff(names(argsd), c(names(formals(FUNd)), ""))
  if (length(badArgs) > 0L & !("..." %in% names(formals(FUNd)))) {
    stop("Function provided by FUNd doesn't use argument",
         ifelse(length(badArgs) > 1L, "s", ""),
         " `", paste(badArgs, collapse = "`, `"), "`.")
  }

  if ("n" %in% names(formals(FUNd))) {
    argsd$n <- nItems
  }
  difficulties <- do.call(FUNd, argsd)
  if (length(difficulties) == 1L) {
    difficulties <- rep(difficulties, nItems)
  } else if (length(difficulties) != nItems) {
    stop("Function provided by argument `FUNd` generated number of values that do not equal to `nItems` (or to 1).")
  }
  if (nrow(scoringMatrix) < 3) {
    return(matrix(difficulties, ncol = 1, dimnames = list(NULL, "d1")))
  }

  argst <- c(list(1L:nItems), argst)
  colNames <- names(argst)
  argst <- tryCatch(as.data.frame(argst),
                    error = function(e) {
                      stop("Argument that will be passed to the function generating thresholds ('",
                           paste(names(argst), collapse = "', "),
                           "') must be vectors of the same length as value of `nItems` (or of the length of one).",
                           call. = FALSE)
                    })
  names(argst) <- colNames
  argst <- argst[, -1L, drop = FALSE]
  if ("n" %in% names(formals(FUNt))) {
    argst$n <- nrow(scoringMatrix) - 1L
  }
  badArgs <- setdiff(names(argst), c(names(formals(FUNt)), ""))
  if (length(badArgs) > 0L & !("..." %in% names(formals(FUNt)))) {
    stop("Function provided by FUNt doesn't use argument",
         ifelse(length(badArgs) > 1L, "s", ""),
         " `", paste(badArgs, collapse = "`, `"), "`.")
  }

  intercepts <- vector(mode = "list", length = nItems)
  for (i in 1L:nItems) {
    intercepts[[i]] <- do.call(FUNt, argst[i, , drop = FALSE])
    if (length(intercepts[[i]]) != nrow(scoringMatrix) - 1L) {
      stop("Function provided by argument `FUNt` generated number of values that do not equal to the number of rows of the scoring matrix minus 1.")
    }
    intercepts[[i]] <-
      sort(intercepts[[i]], decreasing = TRUE) - mean(intercepts[[i]])
    intercepts[[i]] <- intercepts[[i]] - difficulties[i]
  }
  return(thresholds2intercepts(matrix(unlist(intercepts),
                                      nrow = nItems, byrow = TRUE)))
}
