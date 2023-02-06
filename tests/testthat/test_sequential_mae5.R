set.seed(26112020)
# generating test
nItems <- 20
sM <- make_scoring_matrix_aem(1:5, "mae")
slopes <- generate_slopes(nItems, sM, FUN = rlnorm, meanlog = 0, sdlog = 0.2)
intercepts <- generate_intercepts(nItems, sM,
                                  FUNd = rnorm, argsd = list(mean = 0, sd = 1.5))
items <- make_test(sM, slopes, intercepts, "irtree")

# generating "subjects" - uncorrelated traits
vcovTraits <- matrix(0, nrow = ncol(sM), ncol = ncol(sM),
                     dimnames = list(colnames(sM), colnames(sM)))
diag(vcovTraits) <- 1
theta = mnormt::rmnorm(1000, varcov = vcovTraits)
colnames(theta) <- colnames(vcovTraits)

# generating responses
resp <- generate_test_responses(theta, items)

# scaling
respWide <- expand_responses(resp, sM)
mSqt <- mirt(respWide,
             mirt.model("m = 1-20
                         a = 21-40
                         e = 41-60"),
             '2PL', TOL = 0.1, verbose = FALSE)
estItemPars <- coef(mSqt, simplify = TRUE)$items
estItemPars <- cbind(m = estItemPars[1:nItems, 1],
                     a = estItemPars[(nItems + 1):(2*nItems), 2],
                     e = estItemPars[(2*nItems + 1):(3*nItems), 3],
                     dm = estItemPars[1:nItems, 4],
                     da = estItemPars[(nItems + 1):(2*nItems), 4],
                     de = estItemPars[(2*nItems + 1):(3*nItems), 4])
test_that("Item parameters of IRTrees M, A, E RS (with 5-point scale) recovers in estimation with reasonable MSEs.", {
  expect_lt(mean((slopes[, 1] - estItemPars[, 1])^2), 0.02)
  expect_lt(mean((slopes[, 2] - estItemPars[, 2])^2), 0.09)
  expect_lt(mean((slopes[, 3] - estItemPars[, 3])^2), 0.05)
  expect_lt(mean((intercepts[, 1] - estItemPars[, 4])^2), 0.02)
  expect_lt(mean((intercepts[, 2] - estItemPars[, 5])^2), 0.05)
  expect_lt(mean((intercepts[, 3] - estItemPars[, 6])^2), 0.06)
})

# cat("Generating model:\n")
# round(cbind(slopes, intercepts), 2)
# cat("Estimated parameters:\n")
# round(estItemPars, 2)
# cat("Correlations:\n")
# diag(cor(slopes, estItemPars[, 1:3]))
# diag(cor(intercepts, estItemPars[, 4:6]))
# cat("MSE:\n")
# round(c(m = mean((slopes[, 1] - estItemPars[, 1])^2),
#         a = mean((slopes[, 2] - estItemPars[, 2])^2),
#         e = mean((slopes[, 3] - estItemPars[, 3])^2),
#         dm = mean((intercepts[, 1] - estItemPars[, 4])^2),
#         da = mean((intercepts[, 2] - estItemPars[, 5])^2),
#         de = mean((intercepts[, 3] - estItemPars[, 6])^2)), 3)
