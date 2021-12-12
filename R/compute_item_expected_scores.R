#' @title Generating expected distribution of item scores
#' @description Function generates expected distribution of item scores given
#' item object constructed using \code{\link{make_item}} or test (i.e. list of
#' items) object constructed using \code{\link{make_test}} and covariance matrix
#' of latent traits, assuming multivariate-normal distribution of latent traits.
#' @param x object of class \emph{rstylesItem} or \emph{rstylesTest}
#' @param vcov covariance matrix of latent traits (in line with item's scoring
#' matrix); if not provided uncorrelated standard normal is used
#' @returns table
#' @seealso \code{\link{make_item}}
#' @examples
#' itemGPCM <- make_item(scoringMatrix = make_scoring_matrix_aem(1:5, "simultaneous")[, -4],
#'                       slopes = c(i = 1, m = 2, e = 3),
#'                       intercepts = cumsum(c(0, seq(-0.5, 0.5, length.out = 4))),
#'                       mode = "simultaneous")
#' itemIRTree <- make_item(scoringMatrix = make_scoring_matrix_aem(1:5, "mae"),
#'                         slopes = c(m = 1, a = 1, e = 1),
#'                         intercepts = c(m1 = 0, a1 = 0, e1 = 0),
#'                         mode = "sequential")
#' vcov <- matrix(c( 1,    0.5, -0.5,
#'                   0.5,  1,   -0.25,
#'                  -0.5, -0.25, 1),
#'                nrow = 3, dimnames = list(c("i", "m", "e"), c("i", "m", "e")))
#' vcovIRTree <- vcov
#' colnames(vcovIRTree) <- rownames(vcovIRTree) <- c("a", "m", "e")
#' compute_item_expected_scores(itemGPCM) # orthogonal, standard-normal latent traits
#' compute_item_expected_scores(itemGPCM, vcov)
#' compute_item_expected_scores(itemIRTree) # orthogonal, standard-normal latent traits
#' compute_item_expected_scores(itemIRTree, vcovIRTree)
#'
#' test <- make_test(sM,
#'                   generate_slopes(11, sM, c(1, 2, 3)),
#'                   generate_intercepts(11, sM,
#'                                       FUNd = seq,
#'                                       argsd = list(from = -1.5, to = 1.5,
#'                                       length.out = 11),
#'                                       FUNt = seq,
#'                                       argst = list(from = -1.5, to = 1.5,
#'                                       length.out = 4)),
#'                   "simultaneous")
#' sapply(compute_item_expected_scores(test, vcov), identity)
#' @name compute_item_expected_scores
#' @export
compute_item_expected_scores = function(x, vcov) {
  UseMethod("compute_item_expected_scores", x)
}
#' @rdname compute_item_expected_scores
#' @export
compute_item_expected_scores.rstylesTest = function(x, vcov) {
  return(lapply(x, compute_item_expected_scores, vcov = vcov))
}
#' @rdname compute_item_expected_scores
#' @export
compute_item_expected_scores.rstylesItem <- function(x,
                                                     vcov = diag(ncol(x$scoringMatrix))) {
  stopifnot("Argument `vcov` must be a symmetric numeric matrix." =
              is.matrix(vcov),
            "Argument `vcov` must be a symmetric numeric matrix." =
              is.numeric(vcov),
            "Argument `vcov` must be a symmetric numeric matrix." =
              isSymmetric(vcov),
            "Argument `vcov` must have the same rows (and columns) as columns of the item's scoring matrix." =
              ncol(vcov) == ncol(x$scoringMatrix),
            all(colnames(vcov) %in% colnames(x$scoringMatrix)) ||
              identical(vcov, diag(ncol(x$scoringMatrix))),
            "All elements on the diagonal of `vcov` argument (i.e. latent trait variances) must be positive." =
              all(diag(vcov) >= 0))
  if (!is.null(x$scoringOnPreviousResponses)) {
    warning("Item's scoring on previous responses rule is not taken into account.")
  }
  if (!is.null(x$editResponse)) {
    warning("Item's editing response rule is not taken into account.")
  }

  limits <- c(-6, 6)
  if (ncol(x$scoringMatrix) > 6) {
    n <- 5
  } else {
    n <- c(61, 31, 25, 11, 7, 5)[ncol(x$scoringMatrix)]
  }
  message("Using quadrature with ", n, " points regulary spanned between ",
          limits[1], " and ", limits[2], " on each of ",
          ncol(x$scoringMatrix), " dimensions (overall ",
          n^ncol(x$scoringMatrix), " quadrature points).")
  theta <- lapply(diag(vcov)^0.5,
                  function(x, limits, n) {
                    limits = x*limits
                    return(seq(limits[1], limits[2], length.out = n))
                  }, limits = limits, n = n)
  theta <- as.matrix(expand.grid(theta))

  w <- mvtnorm::dmvnorm(theta, sigma = vcov)
  w <- w / sum(w)

  if (x$mode == "simultaneous") {
    x$slopes <- x$slopes[sapply(colnames(x$scoringMatrix), match,
                                table = names(x$slopes)), drop = FALSE]
    probs <- compute_item_expected_scores_gpcm(
      theta,
      x$scoringMatrix * rep(x$slopes,
                            each = nrow(x$scoringMatrix)),
      x$intercepts)
  } else if (x$mode == "sequential") {
    nodes <- vector(mode = "list", length = ncol(x$scoringMatrix))
    names(nodes) <- colnames(x$scoringMatrix)
    for (i in 1L:length(nodes)) {
      scoresAtNode <- sort(unique(x$scoringMatrix[, i]))
      nodes[[i]] <- make_item(
        matrix(scoresAtNode, ncol = 1,
               dimnames = list(scoresAtNode, names(nodes)[i])),
        x$slopes[which(names(x$slopes) == names(nodes)[i])],
        x$intercepts[grep(paste0("^", names(nodes)[i],
                                 "_?[[:digit:]]+"), names(x$intercepts))],
        "simultaneous")
      nodes[[i]] <- compute_item_expected_scores_gpcm(
        theta[, names(nodes)[i], drop = FALSE],
        nodes[[i]]$scoringMatrix * rep(nodes[[i]]$slopes,
                                       each = nrow(nodes[[i]]$scoringMatrix)),
        nodes[[i]]$intercepts)
    }
    probs <- matrix(NA_real_, nrow = nrow(theta),
                    ncol = nrow(x$scoringMatrix),
                    dimnames = list(NULL, rownames(x$scoringMatrix)))
    for (p in 1:nrow(x$scoringMatrix)) {
      probsAtNodes <- mapply(
        function(probs, result) {
          if (is.na(result)) {
            return(rep(1, nrow(probs)))
          } else {
            return(probs[, colnames(probs) == result])
          }
        },
        nodes,
        x$scoringMatrix[p, ])
      probs[, p] <- apply(probsAtNodes, 1, Reduce, f = "*")
    }
  }
  return(colSums(probs*w))
}
compute_item_expected_scores_gpcm <- function(theta, weightsMatrix, intercepts) {
  probs <- matrix(NA_real_,
                  nrow = nrow(theta), ncol = nrow(weightsMatrix),
                  dimnames = list(NULL, rownames(weightsMatrix)))
  for (i in 1L:nrow(weightsMatrix)) {
    probs[, i] <-
      exp(intercepts[i] + theta %*% t(weightsMatrix[i, , drop = FALSE]))
  }
  return(probs / rowSums(probs))
}
