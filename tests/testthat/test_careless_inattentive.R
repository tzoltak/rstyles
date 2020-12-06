################################################################################
# setup
set.seed(26112020)
nItems <- 20
sMSml <- make_scoring_matrix_aem(1:5, "simultaneous")
slopesSml <- matrix(c(rep(10, nItems),
                      rlnorm(nItems, 0, 0.3),
                      rep(1, nItems),
                      rep(1, nItems),
                      rep(1, nItems)),
                    ncol = 5, dimnames = list(NULL, c("ci", colnames(sMSml))))
interceptsSml <- runif(nItems, -2, 2)
interceptsSml <- t(sapply(interceptsSml,
                          function(x) x + seq(0.9, -0.9, length.out = 4)))
colnames(interceptsSml) <- paste0("d", 1:ncol(interceptsSml))
# uncorrelated traits
vcovTraitsSml <- matrix(0, nrow = ncol(sMSml) + 1, ncol = ncol(sMSml) + 1,
                        dimnames = list(c("ci", colnames(sMSml)),
                                        c("ci", colnames(sMSml))))
# very low variance to don't have "negative careless/inattentive"
diag(vcovTraitsSml) <- 0.1
expectedTraitsSml <- setNames(c(2, rep(0, ncol(vcovTraitsSml) - 1)),
                              colnames(vcovTraitsSml))

sMSqt <- make_scoring_matrix_aem(1:5, "mae")
slopesSqt <- matrix(c(rep(10, nItems),
                      rlnorm(nItems, 0, 0.2),
                      rlnorm(nItems, 0, 0.2),
                      rlnorm(nItems, 0, 0.2)),
                    ncol = 4, dimnames = list(NULL, c("ci", colnames(sMSqt))))
interceptsSqt <- matrix(c(rep(0, nItems),
                          rnorm(nItems, 0, 1.5),
                          rnorm(nItems, 0, 1.5),
                          rnorm(nItems, 0, 1.5)),
                        ncol = ncol(sMSqt) + 1,
                        dimnames = list(NULL,
                                        paste0(c("ci", colnames(sMSqt)), "1")))

# uncorrelated traits
vcovTraitsSqt <- matrix(0, nrow = ncol(sMSqt) + 1, ncol = ncol(sMSqt) + 1,
                        dimnames = list(c("ci", colnames(sMSqt)),
                                        c("ci", colnames(sMSqt))))
# very low variance to don't have "negative careless/inattentive"
diag(vcovTraitsSqt) <- 0.1
expectedTraitsSqt <- setNames(c(2, rep(0, ncol(vcovTraitsSqt) - 1)),
                              colnames(vcovTraitsSqt))
################################################################################
# very intense straightlining with aem - simultaneous
itemsSSml <- vector(mode = "list", length = nItems)
for (i in 1:nItems) {
  itemsSSml[[i]] <- make_item(sMSml, slopesSml[i, ], interceptsSml[i, ],
                              "simultaneous",
                              scoringOnPreviousResponses =
                                score_on_last_answer_straight)
}
set.seed(26112020)
respSSml <- generate_test_responses(vcovTraitsSml, itemsSSml, n = 50,
                                    traitMeans = expectedTraitsSml)
respSSml <- apply(respSSml, 1:2, as.numeric)
colnames(respSSml) <- paste0("i", 1:ncol(respSSml))

test_that("Straightlining can be generated with simultaneous A, M, E RS", {
  expect_true(all(apply(respSSml, 1,
                        function(x) {return(length(unique(x)))}) == 1))
})
################################################################################
# very intense straightlining with mae - sequential
itemsSSqt <- vector(mode = "list", length = nItems)
for (i in 1:nItems) {
  itemsSSqt[[i]] <- make_item(sMSqt, slopesSqt[i, ], interceptsSqt[i, ],
                              "sequential",
                              scoringOnPreviousResponses =
                                score_on_last_answer_straight)
}
set.seed(26112020)
respSSqt <- generate_test_responses(vcovTraitsSqt, itemsSSqt, n = 50,
                                    traitMeans = expectedTraitsSqt)
respSSqt <- apply(respSSqt, 1:2, as.numeric)
colnames(respSSqt) <- paste0("i", 1:ncol(respSSqt))

test_that("Straightlining can be generated with sequential A, M, E RS", {
  expect_true(all(apply(respSSqt, 1,
                        function(x) {return(length(unique(x)))}) == 1))
})
################################################################################
# very "bouncing" C/IRS with aem - simultaneous
itemsBSml <- vector(mode = "list", length = nItems)
for (i in 1:nItems) {
  itemsBSml[[i]] <- make_item(sMSml, slopesSml[i, ], interceptsSml[i, ],
                              "simultaneous",
                              scoringOnPreviousResponses =
                                score_on_previous_answers_bounce)
}
set.seed(26112020)
respBSml <- generate_test_responses(vcovTraitsSml, itemsBSml, n = 50,
                                 traitMeans = expectedTraitsSml)
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
itemsBSqt <- vector(mode = "list", length = nItems)
for (i in 1:nItems) {
  itemsBSqt[[i]] <- make_item(sMSqt, slopesSqt[i, ], interceptsSqt[i, ],
                              "sequential",
                              scoringOnPreviousResponses =
                                score_on_previous_answers_bounce)
}
set.seed(26112020)
respBSqt <- generate_test_responses(vcovTraitsSqt, itemsBSqt, n = 50,
                                 traitMeans = expectedTraitsSqt)
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
itemsNSml <- vector(mode = "list", length = nItems)
for (i in 1:nItems) {
  itemsNSml[[i]] <- make_item(sMSml, slopesSml[i, ], interceptsSml[i, ],
                              "simultaneous",
                              scoringOnPreviousResponses =
                                score_on_last_answer_next)
}
set.seed(26112020)
respNSml <- generate_test_responses(vcovTraitsSml, itemsNSml, n = 50,
                                 traitMeans = expectedTraitsSml)
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
itemsNSqt <- vector(mode = "list", length = nItems)
for (i in 1:nItems) {
  itemsNSqt[[i]] <- make_item(sMSqt, slopesSqt[i, ], interceptsSqt[i, ],
                              "sequential",
                              scoringOnPreviousResponses =
                                score_on_last_answer_next)
}
set.seed(26112020)
respNSqt <- generate_test_responses(vcovTraitsSqt, itemsNSqt, n = 50,
                                 traitMeans = expectedTraitsSqt)
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
itemsPSml <- vector(mode = "list", length = nItems)
for (i in 1:nItems) {
  itemsPSml[[i]] <- make_item(sMSml, slopesSml[i, ], interceptsSml[i, ],
                              "simultaneous",
                              scoringOnPreviousResponses =
                                score_on_last_answer_previous)
}
set.seed(26112020)
respPSml <- generate_test_responses(vcovTraitsSml, itemsPSml, n = 50,
                                 traitMeans = expectedTraitsSml)
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
itemsPSqt <- vector(mode = "list", length = nItems)
for (i in 1:nItems) {
  itemsPSqt[[i]] <- make_item(sMSqt, slopesSqt[i, ], interceptsSqt[i, ],
                              "sequential",
                              scoringOnPreviousResponses =
                                score_on_last_answer_previous)
}
set.seed(26112020)
respPSqt <- generate_test_responses(vcovTraitsSqt, itemsPSqt, n = 50,
                                 traitMeans = expectedTraitsSqt)
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
rm(list = ls())
