################################################################################
# setup
set.seed(26112020)
# generating items' parameters - "simultaenous" mode
nItems <- 20
sMSml <- make_scoring_matrix_aem(1:5, "simultaneous")
slopesSml <- cbind(ci = rep(10, nItems),
                   generate_slopes(nItems, sMSml[, 1, drop = FALSE],
                                   FUN = rlnorm, meanlog = 0, sdlog = 0.3),
                   generate_slopes(nItems, sMSml[, -1], 1))
interceptsSml <- generate_intercepts(nItems, sMSml,
                                     FUNd = runif, argsd = list(min = -2,
                                                                max = 2),
                                     FUNt = seq, argst = list(from = 0.9,
                                                              to = -0.9,
                                                              length.out = 4))
# generating "subjects" - uncorrelated traits
vcovTraitsSml <- matrix(0, nrow = ncol(sMSml) + 1, ncol = ncol(sMSml) + 1,
                        dimnames = list(c("ci", colnames(sMSml)),
                                        c("ci", colnames(sMSml))))
# very low variance to don't have "negative careless/inattentive"
diag(vcovTraitsSml) <- 0.1
expectedTraitsSml <- setNames(c(2, rep(0, ncol(vcovTraitsSml) - 1)),
                              colnames(vcovTraitsSml))
thetaSml = mnormt::rmnorm(50, mean = expectedTraitsSml, varcov = vcovTraitsSml)

# generating items' parameters - "sequentaial" mode
sMSqt <- make_scoring_matrix_aem(1:5, "mae")
slopesSqt <- cbind(ci = rep(10, nItems),
                   generate_slopes(nItems, sMSqt,
                                   FUN = rlnorm, meanlog = 0, sdlog = 0.2))
interceptsSqt <- cbind(ci1 = rep(0, nItems),
                       generate_intercepts(nItems, sMSqt,
                                           FUNd = rnorm, argsd = list(mean = 0,
                                                                      sd = 1.5)))
# generating "subjects" - uncorrelated traits
vcovTraitsSqt <- matrix(0, nrow = ncol(sMSqt) + 1, ncol = ncol(sMSqt) + 1,
                        dimnames = list(c("ci", colnames(sMSqt)),
                                        c("ci", colnames(sMSqt))))
# very low variance to don't have "negative careless/inattentive"
diag(vcovTraitsSqt) <- 0.1
expectedTraitsSqt <- setNames(c(2, rep(0, ncol(vcovTraitsSqt) - 1)),
                              colnames(vcovTraitsSqt))
thetaSqt = mnormt::rmnorm(50, mean = expectedTraitsSqt, varcov = vcovTraitsSqt)
################################################################################
# very intense straightlining with aem - simultaneous
itemsSSml <- make_test(sMSml, slopesSml, interceptsSml, "simultaneous",
                       scoringOnPreviousResponses =
                         score_on_last_answer_straight)
set.seed(26112020)
respSSml <- generate_test_responses(thetaSml, itemsSSml)
respSSml <- apply(respSSml, 1:2, as.numeric)
colnames(respSSml) <- paste0("i", 1:ncol(respSSml))

test_that("Straightlining can be generated with simultaneous A, M, E RS", {
  expect_true(all(apply(respSSml, 1,
                        function(x) {return(length(unique(x)))}) == 1))
})
################################################################################
# very intense straightlining with mae - sequential
itemsSSqt <- make_test(sMSqt, slopesSqt, interceptsSqt,  "sequential",
                       scoringOnPreviousResponses =
                         score_on_last_answer_straight)
set.seed(26112020)
respSSqt <- generate_test_responses(thetaSqt, itemsSSqt)
respSSqt <- apply(respSSqt, 1:2, as.numeric)
colnames(respSSqt) <- paste0("i", 1:ncol(respSSqt))

test_that("Straightlining can be generated with sequential A, M, E RS", {
  expect_true(all(apply(respSSqt, 1,
                        function(x) {return(length(unique(x)))}) == 1))
})
################################################################################
# very "bouncing" C/IRS with aem - simultaneous
itemsBSml <- make_test(sMSml, slopesSml, interceptsSml, "simultaneous",
                       scoringOnPreviousResponses =
                         score_on_previous_answers_bounce)
set.seed(26112020)
respBSml <- generate_test_responses(thetaSml, itemsBSml)
respBSml <- apply(respBSml, 1:2, as.numeric)
colnames(respBSml) <- paste0("i", 1:ncol(respBSml))

test_that("'Bouncing' pattern can be generated with simultaneous A, M, E RS", {
  expect_true(all(apply(respBSml, 1, function(x) {
    pattern = rep(c(1:5, 4:2), 3)
    return(all(x == pattern[x[1]:(x[1] + 19)]))
  })))
})
################################################################################
# very "bouncing" C/IRS with mae - sequential
itemsBSqt <- make_test(sMSqt, slopesSqt, interceptsSqt, "sequential",
                       scoringOnPreviousResponses =
                         score_on_previous_answers_bounce)
set.seed(26112020)
respBSqt <- generate_test_responses(thetaSqt, itemsBSqt)
respBSqt <- apply(respBSqt, 1:2, as.numeric)
colnames(respBSqt) <- paste0("i", 1:ncol(respBSqt))

test_that("'Bouncing' pattern can be generated with sequential A, M, E RS", {
  expect_true(all(apply(respBSqt, 1, function(x) {
    pattern = rep(c(1:5, 4:2), 3)
    return(all(x == pattern[x[1]:(x[1] + 19)]))
  })))
})
################################################################################
# very  'next answer' C/IRS with aem - simultaneous
itemsNSml <- make_test(sMSml, slopesSml, interceptsSml, "simultaneous",
                       scoringOnPreviousResponses =
                         score_on_last_answer_next)
set.seed(26112020)
respNSml <- generate_test_responses(thetaSml, itemsNSml)
respNSml <- apply(respNSml, 1:2, as.numeric)
colnames(respNSml) <- paste0("i", 1:ncol(respNSml))

test_that("'Next answer' pattern can be generated with simultaneous A, M, E RS", {
  expect_true(all(apply(respNSml, 1, function(x) {
    pattern = rep(1:5, 5)
    return(all(x == pattern[x[1]:(x[1] + 19)]))
  })))
})
################################################################################
# very 'next answer' C/IRS with mae - sequential
itemsNSqt <- make_test(sMSqt, slopesSqt, interceptsSqt, "sequential",
                       scoringOnPreviousResponses =
                         score_on_last_answer_next)
set.seed(26112020)
respNSqt <- generate_test_responses(thetaSqt, itemsNSqt)
respNSqt <- apply(respNSqt, 1:2, as.numeric)
colnames(respNSqt) <- paste0("i", 1:ncol(respNSqt))

test_that("'Next answer' pattern can be generated with sequential A, M, E RS", {
  expect_true(all(apply(respNSqt, 1, function(x) {
    pattern = rep(1:5, 5)
    return(all(x == pattern[x[1]:(x[1] + 19)]))
  })))
})
################################################################################
# very 'previous answer' C/IRS with aem - simultaneous
itemsPSml <- make_test(sMSml, slopesSml, interceptsSml, "simultaneous",
                       scoringOnPreviousResponses =
                         score_on_last_answer_previous)
set.seed(26112020)
respPSml <- generate_test_responses(thetaSml, itemsPSml)
respPSml <- apply(respPSml, 1:2, as.numeric)
colnames(respPSml) <- paste0("i", 1:ncol(respPSml))

test_that("'Previous answer' pattern can be generated with simultaneous A, M, E RS", {
  expect_true(all(apply(respPSml, 1, function(x) {
    pattern = rep(5:1, 5)
    return(all(x == pattern[(6 - x[1]):(6 - x[1] + 19)]))
  })))
})
################################################################################
# very "'previous answer' C/IRS with mae - sequential
itemsPSqt <- make_test(sMSqt, slopesSqt, interceptsSqt, "sequential",
                       scoringOnPreviousResponses =
                         score_on_last_answer_previous)
set.seed(26112020)
respPSqt <- generate_test_responses(thetaSqt, itemsPSqt)
respPSqt <- apply(respPSqt, 1:2, as.numeric)
colnames(respPSqt) <- paste0("i", 1:ncol(respPSqt))

test_that("'Previous answer' pattern can be generated with sequential A, M, E RS", {
  expect_true(all(apply(respPSqt, 1, function(x) {
    pattern = rep(5:1, 5)
    return(all(x == pattern[(6 - x[1]):(6 - x[1] + 19)]))
  })))
})
################################################################################
# end
