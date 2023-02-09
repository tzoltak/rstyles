#' @title Preparing Mplus code to estimate RS models
#' @description Prepares components of Mplus model description syntax for
#' IRTree model.
#' @param data a data frame
#' @inheritParams make_mplus_gpcm_vmmc_syntax
#' @param output optionally a character vector of Mplus options defining what
#' type of results should be included in the output
#' @param savedata optionally a string with a name of the file in which factor
#' scores should be saved
#' @param analysis a list with elements \code{ESTIMATOR}, \code{ALGORITHM},
#' \code{INTEGRATION} and \code{PROCESSORS} containing Mplus \emph{ANALYSIS}
#' options (provided as strings)
#' @param title string with a title for the analysis
#' @details For the description of model specification see \emph{Details}
#' section in \code{\link{make_mplus_irtree_vmmc_syntax}}
#' @section Limitations:
#' At the moment there is no possibility to prepare models with many
#' no-RS latent traits loading different sets of items.
#' @return A list with elements named \code{TITLE}, \code{VARIABLE},
#' \code{ANALYSIS}, \code{MODEL}, \code{MODELCONSTRAINT}, \code{SAVEDATA},
#' \code{rdata}, \code{usevariables} that can be used as arguments to the
#' \code{mplusObject} function from the package \emph{MplusAutomation} using
#' \code{\link{do.call}}
#' @importFrom stats na.omit setNames
#' @export
make_mplus_irtree_model_syntax <- function(data, items, scoringMatrix,
                                           observedExogenous = vector(mode = "character", length = 0L),
                                           observedDependent = vector(mode = "character", length = 0L),
                                           fixSlopes = vector(mode = "character", length = 0L),
                                           reverseCoded = vector(mode = "list", length = 0L),
                                           orthogonal = vector(mode = "character", length = 0L),
                                           weight = NA_character_,
                                           output = "STDYX",
                                           savedata = NA_character_,
                                           analysis = list(ESTIMATOR = "MLR",
                                                           ALGORITHM = "INTEGRATION",
                                                           INTEGRATION = "STANDARD",
                                                           PROCESSORS = "4"),
                                           title = "Some GPCM model with custom scoring matrix") {
  stopifnot(is.data.frame(data),
            all(sapply(data, function(x) !all(is.na(x)))),
            is.character(items) | is.list(items),
            all(unlist(items) %in% names(data)),
            is.matrix(scoringMatrix), is.numeric(scoringMatrix),
            all(sapply(data[, unique(unlist(items)), drop = FALSE],
                       function(x, values) {all(x %in% values)},
                       values = c(rownames(scoringMatrix), NA_character_))),
            is.list(reverseCoded),
            is.character(savedata), length(savedata) == 1L,
            is.character(output), all(!is.na(output)),
            is.vector(analysis),
            is.character(title), length(title) == 1L, nchar(title) < 78L)
  if (is.character(items)) {
    stopifnot(all(items %in% names(data)))
    itemNames <- items
    items <- setNames(rep(list(list(items)), ncol(scoringMatrix)),
                      colnames(scoringMatrix))
    items <- mapply(setNames, items, names(items), SIMPLIFY = FALSE)
  } else { # a list
    stopifnot(all(names(items) %in% colnames(scoringMatrix)),
              all(colnames(scoringMatrix) %in% names(items)),
              all(unlist(items) %in% names(data)))
    itemNames <- unique(unlist(items))
  }
  if (is.character(observedExogenous)) {
    stopifnot(all(observedExogenous %in% names(data)))
  } else {
    stopifnot(is.matrix(observedExogenous),
              all(rownames(observedExogenous) %in% names(data)))
  }
  if (is.character(observedDependent)) {
    stopifnot(all(observedDependent %in% names(data)))
  } else {
    stopifnot(is.matrix(observedDependent),
              all(rownames(observedDependent) %in% names(data)))
  }
  addToAnalysis <- formals(make_mplus_gpcm_model_syntax)$analysis[
    setdiff(names(formals(make_mplus_gpcm_model_syntax)$analysis),
            c(names(analysis), ""))]
  if (!is.null(addToAnalysis)) analysis <- append(analysis, addToAnalysis)
  dataIRTree <- expand_responses(responses = data[, itemNames],
                                 scoringMatrix = scoringMatrix)
  onlyNAs <- apply(is.na(dataIRTree), 2L, all)
  if (any(onlyNAs)) warning("There are pseudoitems with only NAs in the data: '",
                            paste(colnames(dataIRTree)[onlyNAs], collapse = "', '"),
                            "'. They will be excluded from the modeling.")
  onlyOneValue <- apply(dataIRTree, 2L,
                        function(x) {return(length(unique(na.omit(x))))}) < 2L
  if (any(onlyOneValue)) warning("There are pseudoitems that have only one value in the data: '",
                                 paste(colnames(dataIRTree)[onlyNAs], collapse = "', '"),
                                 "'. They will be excluded from the modeling.")
  dataIRTree <- dataIRTree[, !onlyNAs & !onlyOneValue, drop = FALSE]
  # changing names of items in data structures to the corresponding pseudo-items
  itemNames <- colnames(dataIRTree)
  items <- mapply(function(x, nm) lapply(x, function(x, nm) paste0(nm, "_", x), nm = nm),
                  items, names(items), SIMPLIFY = FALSE)
  if (length(reverseCoded) > 0L) {
    stopifnot(!is.null(names(reverseCoded)))
    for (i in seq_along(items)) {
      for (j in seq_along(items[[i]])) {
        if (names(items[[i]])[j] %in% names(reverseCoded)) {
          reverseCoded[[names(items[[i]])[j]]] <-
            paste0(names(items)[i], "_", reverseCoded[[names(items[[i]])[j]]])
        }
      }

    }
  }
  # end of changing names
  data <- data.frame(dataIRTree, data[, na.omit(c(observedExogenous,
                                                  observedDependent, weight)),
                                      drop = FALSE])
  syntax <- make_mplus_irtree_vmmc_syntax(items =  items,
                                          scoringMatrix = scoringMatrix,
                                          observedExogenous = observedExogenous,
                                          observedDependent = observedDependent,
                                          fixSlopes = fixSlopes,
                                          reverseCoded = reverseCoded,
                                          orthogonal = orthogonal,
                                          weight = weight)
  analysis <- paste(mapply(
    function(nm, val) {
      return(paste0(nm, " IS ", val, ";"))
    },
    names(analysis), analysis, SIMPLIFY = TRUE))
  if (length(output) > 0L) {
    output <- paste0(paste(output, collapse = " "), ";")
  } else {
    output <- NULL
  }
  if (savedata %in% c(NA_character_, "")) {
    savedata <- NULL
  } else {
    savedata <- paste0("FILE IS ", savedata, ";\n",
                       "SAVE IS FSCORES;")
  }
  return(as.MplusSyntaxElements(list(TITLE = title,
                                     VARIABLE = syntax$VARIABLE,
                                     ANALYSIS = analysis,
                                     MODEL = syntax$MODEL,
                                     MODELCONSTRAINT = syntax$MODELCONSTRAINT,
                                     OUTPUT = output,
                                     SAVEDATA = savedata,
                                     rdata = as.data.frame(data),
                                     usevariables = colnames(data))))
}
#' @title Preparing Mplus code to estimate RS models
#' @description Prepares components of Mplus model description syntax for
#' IRTree model.
#' @param items a character vector of item names or a list describing items
#' matching to latent traits in a between-item multidimensional model; in the
#' latter case list elements must be named after column names of the
#' \code{scoringMatrix} with each element being a list describing different
#' latent traits that should be scored using a given column of the
#' \code{scoringMatrix}; names of these \emph{second-level} elements specify
#' names of latent traits and elements themselves are character vectors of item
#' names that are assigned to (loaded) a given latent trait
#' @param scoringMatrix a matrix describing how responses (described in rownames
#' of the matrix) map on \emph{scores} of latent traits (described in columns of
#' the matrix)
#' @inheritParams make_mplus_structural_model
#' @param fixSlopes optionally a character vector of latent trait names
#' for which item slopes parameters should be fixed across items
#' (these names need to occur in column names of \code{scoringMatrix})
#' @param reverseCoded optionally a named list of character vectors with names
#' of list elements specifying latent trait names and elements giving names of
#' items that are \emph{reverse coded} with respect to this latent trait;
#' please note, that these don't need to be given if slopes of the no-RS
#' trait(s) are not fixed across items
#' @param orthogonal optionally a character vector of latent trait names
#' indicating which latent traits should be specified as orthogonal to each
#' other (all the mentioned latent traits will be specified as orthogonal to each
#' other and all the other latent traits)
#' @param weight optionally a string with a name of the variable storing weights
#' @details Models are identified by fixing variances of the the latent
#' variables \strong{that are not mentioned in \code{fixSlopes}} to 1 and
#' by fixing \emph{slope} parameters to 1 (or -1 in a case of reverse coded
#' items) and freeing latent trait variances that are mentioned in
#' \code{fixSlopes}.
#' @return A list of strings with elements named \code{VARIABLE}, \code{MODEL}
#' and \code{MODELCONSTRAINT} (the last one can be \code{NULL})
#' @importFrom stats setNames
make_mplus_irtree_vmmc_syntax <- function(items, scoringMatrix,
                                          observedExogenous = vector(mode = "character", length = 0L),
                                          observedDependent = vector(mode = "character", length = 0L),
                                          fixSlopes = vector(mode = "character", length = 0L),
                                          reverseCoded = vector(mode = "list", length = 0L),
                                          orthogonal = vector(mode = "character", length = 0L),
                                          weight = NA_character_) {
  stopifnot(is.character(items) | is.list(items),
            is.matrix(scoringMatrix), is.numeric(scoringMatrix),
            is.character(weight), length(weight) == 1L)
  if (is.character(items)) {
    stopifnot(!anyNA(items), length(items) > 0L,
              all(!duplicated(items)))
    latentTraits <- colnames(scoringMatrix)
    names(latentTraits) <- latentTraits
    items <- rep(list(items), ncol(scoringMatrix))
    items <- mapply(setNames, items, colnames(scoringMatrix), SIMPLIFY = FALSE)
    names(items) <- colnames(scoringMatrix)
  } else { # a list
    stopifnot(all(names(items) %in% colnames(scoringMatrix)),
              all(colnames(scoringMatrix) %in% names(items)),
              !anyNA(unlist(items)),
              all(sapply(items, is.list)),
              all(sapply(items, function(x) all(sapply(x, is.character)))),
              all(!sapply(items, function(x) duplicated(unlist(x)))),
              all(sapply(items, function(x) all(sapply(x, length) > 0))))
    latentTraits <- c(unlist(lapply(items, names)),
                      setdiff(colnames(scoringMatrix), names(items)))
    names(latentTraits) <- c(unlist(mapply(function(x, nm) rep(nm, length(x)),
                                           items, names(items), SIMPLIFY = FALSE)),
                             setdiff(colnames(scoringMatrix), names(items)))
  }
  stopifnot(is.character(fixSlopes), all(fixSlopes %in% latentTraits),
            is.list(reverseCoded), all(names(reverseCoded) %in% latentTraits),
            all(!duplicated(names(reverseCoded))), all(unlist(reverseCoded) %in% unlist(items)),
            is.character(orthogonal), all(orthogonal %in% latentTraits))
  if (length(reverseCoded) > 0L & length(fixSlopes) == 0L) {
    message("With no slopes being fixed argument 'reverseCoded' won't affect the model specification.")
  }
  modelStructural <- make_mplus_structural_model(latent = latentTraits,
                                                 observedExogenous = observedExogenous,
                                                 observedDependent = observedDependent)
  observedExogenous <- attributes(modelStructural)$observedExogenous
  observedDependent <- attributes(modelStructural)$observedDependent

  model <- constraints <- vector(mode = "character", length = 0L)
  for (lt in seq_along(latentTraits)) {
    i <- which(colnames(scoringMatrix) == names(latentTraits)[lt])
    traitItems <- items[[colnames(scoringMatrix)[i]]][[latentTraits[lt]]]
    if (latentTraits[lt] %in% names(reverseCoded)) {
      traitItemsReversed <- reverseCoded[[latentTraits[lt]]]
    } else {
      traitItemsReversed <- vector(mode = "character", length = 0L)
    }

    if (latentTraits[lt] %in% fixSlopes) {
      traitItems <- setdiff(traitItems, traitItemsReversed)
      if (length(traitItems) > 0L) {
        model <- c(model,
                   paste0(latentTraits[lt], " BY ",
                          paste(traitItems, collapse = "@1 "), "@1;"))
      }
      if (length(traitItemsReversed) > 0L) {
        model <- c(model,
                   paste0(latentTraits[lt], " BY ",
                          paste(traitItemsReversed, collapse = "@-1 "), "@-1;"))
      }
      model <- c(model,
                 paste0(latentTraits[lt], "*;"))
    } else {
      model <- c(model,
                 paste0(latentTraits[lt], " BY ", traitItems[1L], "* ",
                        paste(traitItems[-1L], collapse = " "), ";"))
      model <- c(model,
                 paste0(latentTraits[lt], "@1;"))
    }
  }
  model <- c(model,
             make_mplus_latent_traits_orthogonal_syntax(orthogonal))
  if (!is.na(weight)) {
    weight <- paste0("WEIGHT = ", weight, ";")
  } else {
    weight  <- vector(mode = "character", length = 0L)
  }
  if (length(constraints) == 0L) constraints = NULL

  results <- list(VARIABLE =
                    paste(c(strwrap(paste0("CATEGORICAL = ",
                                           paste(unique(unlist(items)),
                                                 collapse = " "), ";"),
                                    width = 80L, exdent = 14L),
                            weight),
                          collapse = "\n"),
                  MODEL = paste(c(modelStructural, model),
                                collapse = "\n"),
                  MODELCONSTRAINT = paste(constraints, collapse = "\n"))
  return(as.MplusSyntaxElements(results))
}
#' @title Preparing Mplus code to estimate RS models
#' @description Prepares components of Mplus model description syntax for a
#' GPCM (NRM).
#' @inheritParams make_mplus_irtree_model_syntax
#' @details For the description of model specification see \emph{Details}
#' section in \code{\link{make_mplus_gpcm_vmmc_syntax}}
#' @section Limitations:
#' At the moment there is no possibility to prepare models with many
#' no-RS latent traits loading different sets of items.
#' @return A list with elements named \code{TITLE}, \code{VARIABLE},
#' \code{ANALYSIS}, \code{MODEL}, \code{MODELCONSTRAINT}, \code{SAVEDATA},
#' \code{rdata}, \code{usevariables} that can be used as arguments to the
#' \code{mplusObject} function from the package \emph{MplusAutomation} using
#' \code{\link{do.call}}
#' @importFrom stats na.omit
#' @export
make_mplus_gpcm_model_syntax <- function(data, items, scoringMatrix,
                                         observedExogenous = vector(mode = "character", length = 0L),
                                         observedDependent = vector(mode = "character", length = 0L),
                                         fixSlopes = vector(mode = "character", length = 0L),
                                         reverseCoded = vector(mode = "list", length = 0L),
                                         orthogonal = vector(mode = "character", length = 0L),
                                         weight = NA_character_,
                                         output = "STDYX",
                                         savedata = NA_character_,
                                         analysis = list(ESTIMATOR = "MLR",
                                                         ALGORITHM = "INTEGRATION",
                                                         INTEGRATION = "STANDARD",
                                                         PROCESSORS = "4"),
                                         title = "Some GPCM model with custom scoring matrix") {
  stopifnot(is.data.frame(data),
            all(sapply(data, function(x) !all(is.na(x)))),
            is.character(items) | is.list(items),
            all(unlist(items) %in% names(data)),
            is.matrix(scoringMatrix), is.numeric(scoringMatrix), !anyNA(scoringMatrix),
            all(sapply(data[, unique(unlist(items)), drop = FALSE],
                       function(x, values) {all(x %in% values)},
                       values = c(rownames(scoringMatrix), NA_character_))),
            is.list(reverseCoded),
            is.character(savedata), length(savedata) == 1L,
            is.vector(analysis),
            is.character(title), length(title) == 1L, nchar(title) < 78L)
  if (is.character(items)) {
    stopifnot(all(items %in% names(data)))
    itemNames <- items
  } else { # a list
    stopifnot(all(names(items) %in% colnames(scoringMatrix)),
              all(colnames(scoringMatrix) %in% names(items)),
              all(unlist(items) %in% names(data)))
    itemNames <- unique(unlist(items))
  }
  if (is.character(observedExogenous)) {
    stopifnot(all(observedExogenous %in% names(data)))
  } else {
    stopifnot(is.matrix(observedExogenous),
              all(rownames(observedExogenous) %in% names(data)))
  }
  if (is.character(observedDependent)) {
    stopifnot(all(observedDependent %in% names(data)))
  } else {
    stopifnot(is.matrix(observedDependent),
              all(rownames(observedDependent) %in% names(data)))
  }
  addToAnalysis <- formals(make_mplus_gpcm_model_syntax)$analysis[
    setdiff(names(formals(make_mplus_gpcm_model_syntax)$analysis),
            c(names(analysis), ""))]
  if (!is.null(addToAnalysis)) analysis <- append(analysis, addToAnalysis)
  itemCategories <- lapply(data[, itemNames, drop = FALSE],
                           function(x) {return(setdiff(unique(x), NA))})
  syntax <- make_mplus_gpcm_vmmc_syntax(items =  items,
                                        scoringMatrix = scoringMatrix,
                                        observedExogenous = observedExogenous,
                                        observedDependent = observedDependent,
                                        fixSlopes = fixSlopes,
                                        reverseCoded = reverseCoded,
                                        orthogonal = orthogonal,
                                        weight = weight,
                                        itemCategories = itemCategories,
                                        trySimpleGPCM = !any(grepl("STD",
                                                                   output)))
  if (is.na(weight)) weight = vector(mode = "character", length = 0L)
  data <- data[, c(unique(unlist(items)), observedExogenous,
                   observedDependent, weight), drop = FALSE]
  analysis <- paste(mapply(
    function(nm, val) {
      return(paste0(nm, " IS ", val, ";"))
    },
    names(analysis), analysis, SIMPLIFY = TRUE))
  if (length(output) > 0L) {
    output <- paste0(paste(output, collapse = " "), ";")
  } else {
    output <- NULL
  }
  if (savedata %in% c(NA_character_, "")) {
    savedata = NULL
  } else {
    savedata <- paste0("FILE IS ", savedata, ";\n",
                       "SAVE IS FSCORES;")
  }
  return(as.MplusSyntaxElements(list(TITLE = title,
                                     VARIABLE = syntax$VARIABLE,
                                     ANALYSIS = analysis,
                                     MODEL = syntax$MODEL,
                                     MODELCONSTRAINT = syntax$MODELCONSTRAINT,
                                     OUTPUT = output,
                                     SAVEDATA = savedata,
                                     rdata = as.data.frame(data),
                                     usevariables = colnames(data))))
}
#' @title Preparing Mplus code to estimate RS models
#' @description Prepares components of Mplus model description syntax.
#' @inheritParams make_mplus_irtree_vmmc_syntax
#' @param itemCategories a list of values that a given item takes in the data
#' @param trySimpleGPCM a logical value indicating whether to try to use
#' a simple Mplus GPCM specification instead of the NRM when
#' \code{scoringMatrix} has only one column
#' @details Models are identified by fixing variances of the the latent
#' variables \strong{that are not mentioned in \code{fixSlopes}} to 1 and
#' by fixing \emph{slope} parameters to 1 (or -1 in a case of reverse coded
#' items) and freeing latent trait variances that are mentioned in
#' \code{fixSlopes}.
#'
#' Please note that Mplus assumes that the last category is always
#' scored 0, so if \code{scoringMatrix} contains some non-zero elements in its
#' last row function will automatically adjust the coding scheme for latent
#' traits (columns of the \code{scoringMatrix}) where last cell is non-zero by
#' subtracting value in this cell from the whole column. Typically this will
#' introduce negative scores to this column, but this is something Mplus can
#' carry and it doesn't affect estimates of slope parameters. However,
#' \strong{this will make estimated intercept parameters incomparable with the
#' specification using the original scoring scheme}. Also, this will make slope
#' parameters for a given latent trait negative (while preserving the origin -
#' for the purpose of interpretation - of the latent trait itself).
#' @return A list of strings with elements named \code{VARIABLE}, \code{MODEL}
#' and \code{MODELCONSTRAINT} (the last one can be \code{NULL})
#' @importFrom stats na.omit setNames
make_mplus_gpcm_vmmc_syntax <- function(items, scoringMatrix,
                                        observedExogenous = vector(mode = "character", length = 0L),
                                        observedDependent = vector(mode = "character", length = 0L),
                                        fixSlopes = vector(mode = "character", length = 0L),
                                        reverseCoded = vector(mode = "list", length = 0L),
                                        orthogonal = vector(mode = "character", length = 0L),
                                        weight = NA_character_,
                                        itemCategories = rep(list(rownames(scoringMatrix)),
                                                             length(items)),
                                        trySimpleGPCM = TRUE) {
  stopifnot(is.character(items) | is.list(items),
            is.matrix(scoringMatrix), is.numeric(scoringMatrix),
            is.character(weight), length(weight) == 1L,
            is.logical(trySimpleGPCM), length(trySimpleGPCM) == 1L,
            trySimpleGPCM %in% c(TRUE, FALSE))
  if (is.character(items)) {
    stopifnot(!anyNA(items), length(items) > 0L,
              all(!duplicated(items)))
    latentTraits <- colnames(scoringMatrix)
    names(latentTraits) <- latentTraits
    items <- setNames(rep(list(list(items)), ncol(scoringMatrix)),
                      colnames(scoringMatrix))
    items <- mapply(setNames, items, names(items), SIMPLIFY = FALSE)
  } else { # a list
    stopifnot(all(names(items) %in% colnames(scoringMatrix)),
              all(colnames(scoringMatrix) %in% names(items)),
              !anyNA(unlist(items)),
              all(sapply(items, is.list)),
              all(sapply(items, function(x) all(sapply(x, is.character)))),
              all(!sapply(items, function(x) duplicated(unlist(x)))),
              all(sapply(items, function(x) all(sapply(x, length) > 0))))
    latentTraits <- c(unlist(lapply(items, names)),
                      setdiff(colnames(scoringMatrix), names(items)))
    names(latentTraits) <- c(unlist(mapply(function(x, nm) rep(nm, length(x)),
                                           items, names(items), SIMPLIFY = FALSE)),
                             setdiff(colnames(scoringMatrix), names(items)))
  }
  stopifnot(is.character(fixSlopes), all(fixSlopes %in% latentTraits),
            is.list(reverseCoded), all(names(reverseCoded) %in% latentTraits),
            all(!duplicated(names(reverseCoded))), all(unlist(reverseCoded) %in% unlist(items)),
            is.character(orthogonal), all(orthogonal %in% latentTraits))
  if (length(reverseCoded) > 0L & length(fixSlopes) == 0L) {
    message("With no slopes being fixed argument 'reverseCoded' won't affect the model specification.")
  }
  itemCategories <- lapply(itemCategories, na.omit)
  itemCategories <- mapply(function(v, nm) data.frame(item = nm,
                                                      value = v,
                                                      stringsAsFactors = FALSE),
                           itemCategories, names(itemCategories),
                           SIMPLIFY = FALSE)
  modelStructural <- make_mplus_structural_model(latent = latentTraits,
                                                 observedExogenous = observedExogenous,
                                                 observedDependent = observedDependent)
  observedExogenous <- attributes(modelStructural)$observedExogenous
  observedDependent <- attributes(modelStructural)$observedDependent
  model <- constraints <- vector(mode = "character", length = 0L)

  # simple GPCM specification
  if (ncol(scoringMatrix) == 1L & trySimpleGPCM &
      all(sapply(itemCategories, nrow) == nrow(scoringMatrix))) {
    variable <- paste0("CATEGORICAL = ",
                       paste(unique(unlist(items)), collapse = " "), " (gpcm);")
    for (lt in seq_along(latentTraits)) {
      i <- which(colnames(scoringMatrix) == names(latentTraits)[lt])
      traitItemsReversed <- reverseCoded[[latentTraits[lt]]]
      traitItems <- items[[colnames(scoringMatrix)[i]]][[latentTraits[lt]]]
      stopifnot(all(traitItemsReversed %in% traitItems))
      traitItemsStraight <- setdiff(traitItems, traitItemsReversed)

      if (latentTraits[lt] %in% fixSlopes) {
        if (length(traitItemsStraight) > 0L) {
          model <- c(model,
                     paste0(latentTraits[lt], " BY ",
                            paste(traitItemsStraight, collapse = "@1 "), "@1;"))
        }
        if (length(traitItemsReversed) > 0L) {
          model <- c(model,
                     paste0(latentTraits[lt], " BY ",
                            paste(traitItemsReversed, collapse = "@-1 "), "@-1;"))
        }
        model <- c(model,
                   paste0(latentTraits[lt], "*;"))

      } else {
        model <- c(model,
                   paste0(latentTraits[lt], " BY ", traitItems[1L], "* ",
                          paste(traitItems[-1L], collapse = " "), ";"))
        model <- c(model,
                   paste0(latentTraits[lt], "@1;"))
      }
    }
  # GPCM with a custom scoring matrix
  } else {
    itemCategories <- do.call(rbind, itemCategories)
    variable <- paste0("NOMINAL = ",
                       paste(unique(unlist(items)), collapse = " "), ";")
    for (lt in seq_along(latentTraits)) {
      i <- which(colnames(scoringMatrix) == names(latentTraits)[lt])
      if (latentTraits[lt] %in% names(reverseCoded)) {
        traitItemsReversed <- reverseCoded[[latentTraits[lt]]]
      } else {
        traitItemsReversed <- vector(mode = "character", length = 0L)
      }
      traitItems <- items[[colnames(scoringMatrix)[i]]][[latentTraits[lt]]]
      stopifnot(all(traitItemsReversed %in% traitItems))
      # Mplus assumes that last category is always codded by 0
      # if scoring matrix has another value there, the codding scheme must be changed
      if (scoringMatrix[nrow(scoringMatrix), i] != 0) {
        scoringMatrix[, i] <- scoringMatrix[, i] - scoringMatrix[nrow(scoringMatrix), i]
      }

      syntax <- make_mplus_gpcm_nrm_syntax(scoringColumn = scoringMatrix[, i, drop = FALSE],
                                           latentTraitName = latentTraits[lt],
                                           items = traitItems,
                                           reverseCoded = traitItemsReversed,
                                           itemCategories = itemCategories,
                                           fixSlopes = latentTraits[lt] %in% fixSlopes)
      model <- c(model,
                 syntax$loadings,
                 syntax$latentTrait,
                 "")
      constraints <- c(constraints,
                       syntax$constraints)
    }
    model <- c(model,
               make_mplus_latent_traits_orthogonal_syntax(orthogonal))
  }
  if (!is.na(weight)) {
    weight  <- paste0("WEIGHT = ", weight, ";")
  } else {
    weight <- vector(mode = "character", length = 0L)
  }
  if (length(constraints) == 0L) constraints = NULL

  results <- list(VARIABLE =
                    paste(c(strwrap(variable,
                                    width = 80L, exdent = 10L),
                            weight),
                          collapse = "\n"),
                  MODEL = paste(c(modelStructural, model),
                                collapse = "\n"),
                  MODELCONSTRAINT = paste(constraints, collapse = "\n"))
  return(as.MplusSyntaxElements(results))
}
#' @title Preparing Mplus code to estimate RS models
#' @description Prepares Mplus model description syntax for the structural part
#' of the model given latent traits, observed exogenous predictors and observed
#' dependent variables.
#' @param latent a character vector of latent variable names
#' @param observedExogenous either:
#' \itemize{
#'   \item{a character vector with names of observed exogenous predictors that
#'         should be used to predict latent variables in the model}
#'   \item{a matrix with latent traits in columns and observed exogenous
#'         predictors in rows specifying which of the exogenous predictors should
#'         be used to predict which latent traits (matrix should contain only
#'         0 and 1 or \code{TRUE} and \code{FALSE})}
#' }
#' @param observedDependent either:
#' \itemize{
#'   \item{a character vector with names of observed dependent variables that
#'         should be predicted using latent variables in the model}
#'   \item{a matrix with latent traits in columns and observed dependent
#'         variables in rows specifying which of the dependent variables should
#'         be predicted by which latent traits (matrix should contain only
#'         0 and 1 or \code{TRUE} and \code{FALSE})}
#' }
#' @return A character vector
make_mplus_structural_model <- function(latent, observedExogenous,
                                        observedDependent) {
  stopifnot(is.character(latent), !anyNA(latent), length(latent) > 0L, all(!duplicated(latent)),
            is.character(observedExogenous) | is.numeric(observedExogenous) | is.logical(observedExogenous),
            is.character(observedDependent) | is.numeric(observedDependent) | is.logical(observedDependent))
  if (is.character(observedExogenous)) {
    stopifnot(is.vector(observedExogenous),
              all(!is.na(observedExogenous)),
              all(!duplicated(observedExogenous)))
    observedExogenous <- matrix(TRUE, ncol = length(latent),
                                nrow = length(observedExogenous),
                                dimnames = list(observedExogenous, latent))
  } else {
    stopifnot(all(observedExogenous %in% c(0, 1)),
              all(colnames(observedExogenous) %in% latent),
              !anyNA(rownames(observedExogenous)),
              all(!duplicated(rownames(observedExogenous))))
    observedExogenous <- matrix(as.logical(observedExogenous),
                                nrow = nrow(observedExogenous),
                                dimnames = dimnames(observedExogenous))
  }
  predicting <- apply(observedExogenous, 1L, any)
  if (any(!predicting)) {
    warning("There are some observed exogenous variables that do not predict any latent variable: '",
            paste(rownames(observedExogenous)[!predicting], collapse = "', '"), "'",
            "\nThese variables won't be included in the model.")
  }
  observedExogenous <- observedExogenous[predicting, , drop = FALSE]
  if (is.character(observedDependent)) {
    stopifnot(is.vector(observedDependent),
              all(!is.na(observedDependent)),
              all(!duplicated(observedDependent)))
    observedDependent <- matrix(TRUE, ncol = length(latent),
                                nrow = length(observedDependent),
                                dimnames = list(observedDependent, latent))
  } else {
    stopifnot(all(observedDependent %in% c(0, 1)),
              all(colnames(observedDependent) %in% latent),
              !anyNA(rownames(observedDependent)),
              all(!duplicated(rownames(observedDependent))))
    observedDependent <- matrix(as.logical(observedDependent),
                                nrow = nrow(observedDependent),
                                dimnames = dimnames(observedDependent))
  }
  predicted <- apply(observedDependent, 1L, any)
  if (any(!predicted)) {
    warning("There are some observed dependent variables that are not predicted by any latent variable: '",
            paste(rownames(observedDependent)[!predicted], collapse = "', '"), "'",
            "\nThese variables won't be included in the model.")
  }
  observedDependent <- observedDependent[predicted, , drop = FALSE]

  if (nrow(observedExogenous) > 0L) {
    latentPredicted <- apply(observedExogenous, 2L, any)
    observedExogenous <-
      paste0(latent[latentPredicted], " ON ",
             apply(observedExogenous[, latentPredicted, drop = FALSE], 2L,
                   function(x) {return(paste(names(x)[x], collapse = " "))}),
             ";")
    observedExogenous <- strwrap(observedExogenous, width = 80L, exdent = 5L)
  } else {
    observedExogenous <- vector(mode = "character", length = 0L)
  }
  if (nrow(observedDependent) > 0L) {
    observedDependent <-
      paste0(rownames(observedDependent), " ON ",
             apply(observedDependent, 1L,
                   function(x) {return(paste(names(x)[x], collapse = " "))}),
             ";")
    observedDependent <- strwrap(observedDependent, width = 80L, exdent = 5L)
  } else {
    observedDependent <- vector(mode = "character", length = 0L)
  }
  if (length(observedExogenous) > 0L & length(observedDependent) > 0L) {
    space <- ""
  } else {
    space <- vector(mode = "character", length = 0L)
  }
  if (length(observedExogenous) > 0L | length(observedDependent) > 0L) {
    results <- c(MODEL = paste(c(observedExogenous, space, observedDependent, ""),
                               collapse = "\n"))
  } else {
    results <- vector(mode = "character", length = 0L)
  }
  attributes(results)$observedExogenous = rownames(observedExogenous)
  attributes(results)$observedDependent = rownames(observedDependent)
  return(as.MplusSyntaxElements(results))
}
#' @title Preparing Mplus code to estimate RS models
#' @description Prepares Mplus model syntax describing how items are loaded
#' by latent traits in a GPCM specification of a NRM.
#' @param scoringColumn a one-column matrix (column of a scoring matrix)
#' @param latentTraitName a string with latent variable name
#' @param items a character vector with item names
#' @param reverseCoded a character vector with names of reverse-coded items
#' @param itemCategories a data frame with columns named \emph{item} and
#' \emph{value} storing unique (non-NA) values of items that occur in the data
#' @param fixSlopes a logical value indicating whether slopes of the latent
#' trait should be fixed to be the same
#' @return A character vector
make_mplus_gpcm_nrm_syntax <- function(scoringColumn, latentTraitName, items,
                                       reverseCoded, itemCategories, fixSlopes) {
  stopifnot(is.matrix(scoringColumn), ncol(scoringColumn) == 1L,
            is.character(latentTraitName), length(latentTraitName) == 1L,
            !is.na(latentTraitName),
            is.character(items), length(items) > 0L, !anyNA(items),
            is.character(reverseCoded), !anyNA(reverseCoded),
            is.data.frame(itemCategories),
            "item" %in% names(itemCategories),
            "value" %in% names(itemCategories),
            is.logical(fixSlopes), length(fixSlopes) == 1L,
            fixSlopes %in% c(TRUE, FALSE))
  results <- data.frame(lt = unname(latentTraitName),
                        item = rep(items, each = nrow(scoringColumn)),
                        value = rep(rownames(scoringColumn), length(items)),
                        wt = rep(scoringColumn[, 1L], length(items)),
                        absWt = rep(abs(scoringColumn[, 1L]), length(items)),
                        stringsAsFactors = FALSE)
  results$fix = paste0(ifelse(rep(fixSlopes, nrow(results)),
                              ifelse(results$item %in% reverseCoded, "@-", "@"),
                              "*"),
                       results$wt)
  results$rev = ifelse(results$item %in% reverseCoded, "n", "")
  results <- merge(results, itemCategories,
                   by = c("item", "value"))
  results <- lapply(split(results, results["item"]),
                    function(x) cbind(x, cat = c(seq(1L, nrow(x) - 1L), 0L)))
  results <- do.call(rbind, results)
  results <- results[results$cat != 0L & results$wt != 0, ]
  results$label <- paste0(results$lt, results$item, "_", results$absWt, results$rev)
  constraints <- unique(cbind(results[, c("item", "wt", "absWt", "rev", "label")]))
  results <- paste0(results$lt, " BY ", results$item, "#", results$cat,
                    results$fix, " (", results$label, ");")
  results <- sub("([*@])--([[:digit:]])", "\\1\\2", results)
  constraints <- unlist(lapply(
    split(constraints, constraints$item),
    function(x) {
      if (nrow(x) == 1L) return(vector(mode = "character", length = 0L))
      c(paste0(x$label[-nrow(x)], " = ",
               ifelse(x$rev[-nrow(x)] == "n", "-", ""), x$wt[-nrow(x)],
               "/", ifelse(x$rev[-1L] == "n", "-", ""), x$wt[-1L],
               "*", x$label[-1L], ";"),
        "")
    }))
  constraints <- gsub("--([[:digit:]]+\\*)", "\\1", constraints)
  if (fixSlopes) constraints <- vector(mode = "character", length = 0L)
  return(list(loadings = results,
              latentTrait = paste0(latentTraitName,
                                   ifelse(fixSlopes, "*;", "@1;")),
              constraints = constraints))
}
#' @title Preparing Mplus code to estimate RS models
#' @description Prepares Mplus model description syntax that fixes covariances
#' between given latent traits to 0.
#' @param latentTraits a character vector of latent traits names
#' @return A character vector
make_mplus_latent_traits_orthogonal_syntax <- function(latentTraits) {
  stopifnot(is.character(latentTraits), !anyNA(latentTraits))
  if (length(latentTraits) == 0L) return(vector(mode = "character", length = 0L))

  covariancess <- expand.grid(lt1 = latentTraits, lt2 = latentTraits,
                              stringsAsFactors = FALSE)
  covariancess <-
    covariancess[as.vector(lower.tri(matrix(1L:nrow(covariancess),
                                            nrow = nrow(covariancess)^0.5))), ]
  covariancess <-
    sapply(split(covariancess,
                 droplevels(factor(covariancess$lt1,
                                   unique(covariancess$lt1)))),
           function(x) {
             return(strwrap(paste0(x$lt1[1L], " WITH ",
                                   paste(x$lt2, collapse = "@0 "), "@0;"),
                            width = 80L, exdent = nchar(x$lt2[1L]) + 7L))})
  return(covariancess)
}
#' @title Preparing Mplus code to estimate RS models
#' @description Print method for objects containing Mplus syntaxes
#' @param x an object of class \emph{MplusSyntaxElements}
#' @param ... optional arguments to \code{print.data.frame} methods (used only
#' if \code{x} has an element that is a data frame)
#' @param n an integer passed to \code{\link{head}} (used only if \code{x}
#' has an element that is a data frame)
#' @return invisibly \code{x}
#' @importFrom utils head
#' @export
print.MplusSyntaxElements <- function(x, ..., n = 10L) {
  for (i in seq_along(x)) {
    cat("----------------------------------------\n", names(x)[i], "\n")
    if (is.character(x[[i]]) & names(x)[i] == toupper(names(x)[i])) {
      cat(paste(x[[i]], collapse = "\n"), "\n\n", sep = "")
    } else if (is.data.frame(x[[i]])) {
      print(head(x[[i]], ..., n = n))
    } else {
      print(x[[i]])
    }
  }
  invisible(x)
}
#' @title Preparing Mplus code to estimate RS models
#' @description Assigns \emph{MplusSyntaxElements} to an object
#' @param x an object
#' @return \code{x} with \emph{MplusSyntaxElements} class assigned
as.MplusSyntaxElements <- function(x) {
  class(x) <- c("MplusSyntaxElements", class(x))
  return(x)
}
