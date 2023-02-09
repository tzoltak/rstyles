#' @title Reading results from estimated Mplus models
#' @description Transforms results included in estimated \code{mplusObject}
#' into list of (lists of) data frames storing different sets of parameters.
#' @param model an (already estimated) \code{mplusObject}
#' @param wide optionally a character vector indicating that resulting data
#' frames should be transformed to \emph{wide} format with only one row and
#' all the statistics in separate columns; argument gives names of the
#' statistics - to be chosen from the set: \code{"est"}, \code{"se"},
#' \code{"est_se"}, \code{"pval"} - that should be included in the resulting
#' data frames
#' @param deleteDataFile a logical value indicating whether to remove from disk
#' the text file storing the data from which the model was estimated
#' @return A list with elements:
#' \itemize{
#'   \item{\code{fitIndices} - one-row data frame containing model fit indices
#'         and possibly warnings an errors,}
#'   \item{\code{pathCoefs} -  a list with element \code{unstandardized} and
#'         possibly other elements storing standardized results; each of the
#'         elements is a data frame storing slopes and intercepts from the
#'         structural part of the model and also possibly latent trait means
#'         (if freely estimated in the model),}
#'   \item{\code{variances} - a list with element \code{unstandardized} and
#'         possibly other elements storing standardized results; each of the
#'         elements is a data frame storing variable variances or residual
#'         variances,}
#'   \item{\code{covariances} - a list with element \code{unstandardized} and
#'         possibly other elements storing standardized results; each of the
#'         elements is a data frame storing variable covariances,}
#'   \item{\code{slopes} - a list with element \code{unstandardized} and
#'         possibly other elements storing standardized results; each of the
#'         elements is a data frame storing slope/discrimination parameters
#'         linking latent traits with their items (observed indicators),}
#'   \item{\code{thresholds} - a list with element \code{unstandardized} and
#'         possibly other elements storing standardized results; each of the
#'         elements is a data frame storing threshold/step/intercept item
#'         (observed indicators of latent traits) parameters,}
#'   \item{\code{rSquared} - R-squared statistics for observed dependent
#'         variables (if reported in the \emph{Mplus} output),}
#'   \item{\code{reliabilities} - factor scores reliabilities, available only
#'         if factor scores and their standard errors were estimated.}
#' }
#' @export
read_mplus_object_results <- function(model,
                                      wide = vector(mode = "character", length = 0L),
                                      deleteDataFile = TRUE) {
  stopifnot("Argument 'model' must be an object of the 'mplusObject' class." =
              inherits(model, "mplusObject"),
            "Model has not been estimated yet." = "results" %in% names(model),
            "Argument 'wide' must be a character vector with elements chosen from the set: {'est', 'se', 'est_se', 'pval'}." =
              is.character(wide),
            "Argument 'wide' must be a character vector with elements chosen from the set: {'est', 'se', 'est_se', 'pval'}." =
              all(wide %in% c("est", "se", "est_se", "pval")),
            "Argument 'deleteDataFile' must be either `TRUE` or `FALSE`." =
              is.logical(deleteDataFile),
            "Argument 'deleteDataFile' must be either `TRUE` or `FALSE`." =
              length(deleteDataFile) == 1L,
            "Argument 'deleteDataFile' must be either `TRUE` or `FALSE`." =
              deleteDataFile %in% c(TRUE, FALSE))

  if (deleteDataFile) {
    deleteDataFile <- try(unlink(model$results$input$data$file))
  }
  if (inherits(deleteDataFile, "try-error")) {
    warning("Failed to remove the file '", model$results$input$data$file, "'.")
  }

  coefs <- lapply(model$results$parameters, as.data.frame)
  fitIndices = cbind(as.data.frame(
    model$results$summaries[, names(model$results$summaries) %in%
                              c("Estimator", "Observations", "Parameters",
                                "LL", "LLCorrectionFactor",
                                "AIC", "BIC", "aBIC", "AICC")]),
    timeElapsed = get_elapsed_time(model),
    errors = paste(unlist(model$results$errors), collapse = "\n"),
    warnings = paste(unlist(model$results$warnings), collapse = "\n"),
    converged = "se" %in% names(coefs$unstandardized))
  if (!"se" %in% names(coefs$unstandardized)) {
    return(list(fitIndices = fitIndices,
                pathCoefs = NULL,
                variances = NULL,
                covariances = NULL,
                slopes = NULL,
                thresholds = NULL,
                rSquared = NULL,
                reliabilities = NULL))
  }
  coefs <- lapply(coefs, check_coefs_too_large)
  if (any(is.infinite(coefs$unstandardized$est)) |
      any(is.infinite(coefs$unstandardized$se))) {
    fitIndices$warnings <-
      paste0(fitIndices$warnings,
             "\nValues of some parameter estimates or standard errors were too large to be expressed in Mplus output.")
  }
  latentTraits <- unique(sub("\\.BY", "",
                             grep("\\.BY", coefs$unstandardized$paramHeader,
                                  value = TRUE)))
  observedExogenous <-
    coefs$unstandardized[grepl(paste0("^(", paste(latentTraits, collapse = "|"),
                                      ")\\.ON$"),
                               coefs$unstandardized$paramHeader), ]
  observedExogenous <- unique(observedExogenous$param)
  observedDependent <-
    coefs$unstandardized[coefs$unstandardized$param %in% latentTraits &
                           grepl("\\.ON$", coefs$unstandardized$paramHeader), ]
  observedDependent <- sub("\\.ON$", "", unique(observedDependent$paramHeader))

  rSquared <- coefs$r2
  if (!is.null(rSquared)) {
    names(rSquared) <- sub("^param$", "variable", names(rSquared))
  }
  coefs <- coefs[grepl("standardized", names(coefs))]
  covariances <- lapply(coefs,
                        function(x) {
                          x <- x[grepl("\\.WITH$", x$paramHeader), ]
                          x$paramHeader <- sub("\\.WITH$", "", x$paramHeader)
                          names(x) <- sub("^paramHeader", "variable1", names(x))
                          names(x) <- sub("^param", "variable2", names(x))
                          return(x)})
  variances <- lapply(coefs,
                      function(x) {
                        x <- x[x$paramHeader %in% c("Variances",
                                                    "Residual.Variances"), ]
                        names(x) <- sub("^param$", "variable", names(x))
                        names(x) <- sub("^paramHeader$", "type", names(x))
                        x$type <- sub("\\.", " ", sub("s$", "", x$type))
                        return(x)
                      })
  pathCoefs <- lapply(coefs,
                      function(x) {
                        x <- x[grepl("\\.ON$", x$paramHeader) |
                                 (x$paramHeader %in% c("Intercepts", "Means") &
                                    !grepl("#[[:digit:]]+$", x$param)), ]
                        x$variable <- ifelse(x$paramHeader %in% c("Intercepts",
                                                                  "Means"),
                                             x$param,
                                             sub("\\.ON$", "", x$paramHeader))
                        x$predictor <- ifelse(x$paramHeader %in% c("Intercepts",
                                                                   "Means"),
                                              paste0("(", sub("s$", "",
                                                              x$paramHeader), ")"),
                                              x$param)
                        return(x[, c("variable", "predictor",
                                     setdiff(names(x), c("paramHeader", "param",
                                                         "variable", "predictor")))])
                      })
  slopes <- lapply(coefs,
                   function(x) {
                     x <- x[grepl("\\.BY$", x$paramHeader), ]
                     x$paramHeader <- sub("\\.BY$", "", x$paramHeader)
                     names(x) <- sub("^paramHeader$", "latentTrait", names(x))
                     names(x) <- sub("^param$", "item", names(x))
                     return(x)
                   })
  thresholds <- lapply(coefs,
                       function(x) {
                         x <- x[x$paramHeader %in% c("Thresholds", "Steps") |
                                  (x$paramHeader %in% "Intercepts" &
                                     grepl("#", x$param)), ]
                         x$paramHeader <- sub("s$", "", x$paramHeader)
                         names(x) <- sub("^paramHeader$", "type", names(x))
                         names(x) <- sub("^param$", "item", names(x))
                         return(x[, c("item", "type",
                                      setdiff(names(x), c("item", "type")))])
                       })
  results <- list(
    fitIndices = fitIndices,
    pathCoefs = pathCoefs,
    variances = variances,
    covariances = covariances,
    slopes = slopes,
    thresholds = thresholds,
    rSquared = rSquared,
    reliabilities = get_fs_reliabilities(model, latentTraits))
  if (length(wide) > 0L) {
    results[-1] <- lapply(results[-1], apply_transform_to_one_row, what = wide)
  }
  return(results)
}
#' @title Reading results from estimated Mplus models
#' @description Extracts model estimation time in seconds.
#' @param model an (already estimated) \code{mplusObject}
#' @return A one-element numeric vector.
get_elapsed_time <- function(model) {
  stopifnot("Argument 'model' must be an object of the 'mplusObject' class." =
              inherits(model, "mplusObject"),
            "Model has not been estimated yet." = "results" %in% names(model))
  elapsed <- sub("^.*Elapsed Time: +", "",
                 grep("Elapsed Time", model$results$output, value = TRUE))
  elapsed <- as.numeric(strsplit(elapsed, ":", fixed = TRUE)[[1]])
  return(elapsed[3] + elapsed[2]*60 + elapsed[1]*60^2)
}
#' @title Reading results from estimated Mplus models
#' @description Checks if some of the columns storing the statistics
#' in the provided data frame contain values indicating that the statistics
#' value was to large to be written to the \emph{Mplus} output (making such
#' a column a character vector) and if so recodes such values to \code{Inf} and
#' transforms a column to numeric vector. Also, recodes values of \code{est},
#' \code{se}, \code{est_se} and \code{pval} for parameters that were not
#' estimated in the model (i.e. they were fixed) to \code{NaNs}.
#' @param x a data frame storing model parameters
#' @return A data frame.
check_coefs_too_large <- function(x) {
  for (col in intersect(names(x), c("est", "se", "est_se", "pval"))) {
    if (is.character(x[, col])) {
      x[, col] <- ifelse(x[, col] %in% "*********",
                         Inf,
                         suppressWarnings(as.numeric(x[, col])))
    }
  }
  if (all(c("se", "est_se", "pval") %in% names(x))) {
    notEstimated = x$se == 0 & x$est_se == 999 & x$pval == 999
    x$se[notEstimated] <- NaN
    x$est_se[notEstimated] <- NaN
    x$pval[notEstimated] <- NaN
  }
  return(x)
}
#' @title Reading results from estimated Mplus models
#' @description Computes factor scores reliabilities (if available).
#' @param model an (already estimated) \code{mplusObject}
#' @param latentTraits a character vector with latent trait names
#' @details Reliability is estimated as a ratio between factor scores variance
#' and the sum of factor scores variance and mean squared factor scores standard
#' errors.
#' @return A data frame with columns:
#' \itemize{
#'   \item{\code{latentTrait} - name of a trait,}
#'   \item{\code{est} - estimated reliability.}
#' }
#' @importFrom stats var
get_fs_reliabilities <- function(model, latentTraits) {
  stopifnot("Argument 'model' must be an object of the 'mplusObject' class." =
              inherits(model, "mplusObject"),
            "Model has not been estimated yet." = "results" %in% names(model),
            "Argument 'latentTrait' must be a character vector." =
              is.character(latentTraits),
            "Argument 'latentTrait' must not contain missing values." =
              all(!is.na(latentTraits)))
  if (!("savedata" %in% names(model$results))) return(NULL)

  fscores <- model$results$savedata
  stdErrors <- fscores[, grep(paste0("^(", paste(latentTraits, collapse = "|"),")_SE$"),
                              names(fscores)),
                       drop = FALSE]
  if (ncol(stdErrors) == 0L) return(NULL)

  fscores <- fscores[, grep(paste0("^(", paste(latentTraits, collapse = "|"),")$"),
                            names(fscores)),
                     drop = FALSE]
  results <- data.frame(latentTrait = names(fscores),
                        var_fs = unname(sapply(fscores, var, na.rm = TRUE)),
                        av_fsse2 = unname(
                          sapply(stdErrors,
                                 function(x) mean(x^2, na.rm = TRUE))),
                        stringsAsFactors = FALSE)
  results$est <- results$var_fs / (results$var_fs + results$av_fsse2)
  return(results[, c("latentTrait", "est")])
}
#' @title Reading results from estimated Mplus models
#' @description Transforms data frames with model parameters in \emph{long}
#' format to a format with only one row and each parameter-statistic put into
#' a separate column.
#' @param x a data frame or a list of data frames
#' @param what a character vector indicating which statistics - to be chosen
#' from the set: \code{"est"}, \code{"se"},  \code{"est_se"} and \code{"pval"} -
#' should be included in the resulting data frames
#' @return A data frame with only one row and often many columns. Name of
#' columns are created by concatenating names of columns that do not store
#' statistics using a dot (".") as a separator and then adding statistic name
#' separated by "__".
apply_transform_to_one_row <- function(x, what) {
  if (is.data.frame(x)) {
    return(transform_to_one_row(x, what))
  } else {
    return(lapply(x, transform_to_one_row, what = what))
  }
}
#' @rdname apply_transform_to_one_row
transform_to_one_row <- function(x, what) {
  allStatColumnNames <- c("est", "se", "est_se", "pval")
  idColumns <- setdiff(names(x), allStatColumnNames)
  what <- intersect(what, names(x))
  x <- x[, c(idColumns, what), drop = FALSE]
  colNames <- do.call(paste,
                      append(as.list(x[, idColumns, drop = FALSE]),
                             list(sep = ".")))
  colNames <- paste0(rep(colNames, length(what)),
                     "__", rep(what, each = length(colNames)))
  results <- unlist(lapply(x[, what, drop = FALSE], as.list),
                    recursive = FALSE)
  names(results) <- colNames
  return(as.data.frame(results))
}
