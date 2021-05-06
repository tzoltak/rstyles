set.seed(26112020)
# generating test
nItems <- 20
sM <- make_scoring_matrix_aem(1:5, "simultaneous")
slopes <- cbind(generate_slopes(nItems, sM[, 1L, drop = FALSE],
                                FUN = rlnorm, meanlog = 0, sdlog = 0.3,
                                nReversed = floor(nItems / 2)),
                generate_slopes(nItems, sM[, 2:4], 1))
intercepts <- generate_intercepts(nItems, sM,
                                  FUNd = runif, argsd = list(min = -2, max = 2),
                                  FUNt = seq, argst = list(from = 0.9,
                                                           to = -0.9,
                                                           length.out = 4))
items <- make_test(sM, slopes, intercepts, "simultaneous")

# generating "subjects" - uncorrelated traits
vcovTraits <- matrix(0, nrow = ncol(sM), ncol = ncol(sM),
                     dimnames = list(colnames(sM), colnames(sM)))
diag(vcovTraits) <- 1
theta = mnormt::rmnorm(1000, varcov = vcovTraits)
colnames(theta) <- colnames(vcovTraits)

# generating responses
resp <- generate_test_responses(theta, items)
resp <- apply(resp, 1:2, as.numeric)
colnames(resp) <- paste0("i", 1:ncol(resp))

# scaling
mSml <- suppressMessages(mirt(resp,
                              mirt.model("i = 1-20
                                          m = 1-20
                                          e = 1-20
                                          a = 1-20"),
                         'gpcm',
                         gpcm_mats = lapply(1:ncol(resp), function(x) sM),
                         method = "EM", TOL = 0.1, verbose = FALSE))
estItemPars <- coef(mSml, simplify = TRUE)$items
test_that("Item parameters of simultaneous A, E, M RS (with 5-point scale) recovers in estimation with reasonable MSEs.", {
  expect_lt(mean((slopes[, 1] - estItemPars[, 1])^2), 0.2)
  expect_lt(mean((slopes[, 2] - estItemPars[, 2])^2), 0.02)
  expect_lt(mean((slopes[, 3] - estItemPars[, 3])^2), 0.02)
  expect_lt(mean((slopes[, 4] - estItemPars[, 4])^2), 0.4)
  expect_lt(mean((intercepts[, 1] - estItemPars[, 26])^2), 0.06)
  expect_lt(mean((intercepts[, 2] - estItemPars[, 27])^2), 0.12)
  expect_lt(mean((intercepts[, 3] - estItemPars[, 28])^2), 0.09)
  expect_lt(mean((intercepts[, 4] - estItemPars[, 29])^2), 0.08)
})

# cat("Generating model:\n")
# round(cbind(slopes, d0 = 0, intercepts), 2)
# cat("Estimated parameters:\n")
# round(estItemPars[, -grep("^ak", colnames(estItemPars))], 2)
# cat("Correlations:\n")
# cor(slopes[, 1], estItemPars[, 1])
# diag(cor(intercepts, estItemPars[, grep("^d[1-4]", colnames(estItemPars))]))
# cat("MSE:\n")
# round(c(i = mean((slopes[, 1] - estItemPars[, 1])^2),
#         m = mean((slopes[, 2] - estItemPars[, 2])^2),
#         e = mean((slopes[, 3] - estItemPars[, 3])^2),
#         a = mean((slopes[, 4] - estItemPars[, 4])^2),
#         d1 = mean((intercepts[, 1] - estItemPars[, 26])^2),
#         d2 = mean((intercepts[, 2] - estItemPars[, 27])^2),
#         d3 = mean((intercepts[, 3] - estItemPars[, 28])^2),
#         d4 = mean((intercepts[, 4] - estItemPars[, 29])^2)), 3)

rm(list = ls())
