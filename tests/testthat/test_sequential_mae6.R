set.seed(26112020)
# generating test
nItems <- 20
sM <- make_scoring_matrix_aem(1:6, "mae")
slopes <- generate_slopes(nItems, sM, FUN = rlnorm, meanlog = 0, sdlog = 0.2)
intercepts <- generate_intercepts(nItems, sM,
                                  FUNd = rnorm, argsd = list(mean = 0, sd = 1.5))
items <- make_test(sM, slopes, intercepts, "sequential")

# generating "subjects" - uncorrelated traits
vcovTraits <- matrix(0, nrow = 3, ncol = 3,
                     dimnames = list(c("m", "a", "e"), c("m", "a", "e")))
diag(vcovTraits) <- 1
theta = mnormt::rmnorm(1000, varcov = vcovTraits)
colnames(theta) <- colnames(vcovTraits)

# generating responses
resp <- generate_test_responses(theta, items)
resp <- apply(resp, 1:2, as.numeric)
colnames(resp) <- paste0("i", 1:ncol(resp))

# scaling
respWide <- expand_responses(resp, sM)
mSqt <- suppressMessages(mirt(respWide,
                              mirt.model("m = 1-20
                         a1 = 21-40
                         a2 = 41-60
                         e = 61-80"),
                         '2PL', TOL = 0.1, verbose = FALSE))
estItemPars <- coef(mSqt, simplify = TRUE)$items
estItemPars <- cbind(m = estItemPars[1:nItems, 1],
                     a1 = estItemPars[(nItems + 1):(2*nItems), 2],
                     a2 = estItemPars[(2*nItems + 1):(3*nItems), 3],
                     e = estItemPars[(3*nItems + 1):(4*nItems), 4],
                     dm = estItemPars[1:nItems, 5],
                     da1 = estItemPars[(nItems + 1):(2*nItems), 5],
                     da2 = estItemPars[(2*nItems + 1):(3*nItems), 5],
                     de = estItemPars[(3*nItems + 1):(4*nItems), 5])
test_that("Item parameters of sequential M, A, E RS (with 6-point scale) recovers in estimation with reasonable MSEs.", {
  expect_lt(mean((slopes[, 1] - estItemPars[, 1])^2), 0.02)
  expect_lt(mean((slopes[, 2] - estItemPars[, 2])^2), 0.05)
  expect_lt(mean((slopes[, 3] - estItemPars[, 3])^2), 0.05)
  expect_lt(mean((slopes[, 4] - estItemPars[, 4])^2), 0.15)
  expect_lt(mean((intercepts[, 1] - estItemPars[, 5])^2), 0.01)
  expect_lt(mean((intercepts[, 2] - estItemPars[, 6])^2), 0.03)
  expect_lt(mean((intercepts[, 3] - estItemPars[, 7])^2), 0.02)
  expect_lt(mean((intercepts[, 4] - estItemPars[, 8])^2), 0.35)
})

# cat("Generating model:\n")
# round(cbind(slopes, intercepts), 2)
# cat("Estimated parameters:\n")
# round(estItemPars, 2)
# cat("Correlations:\n")
# diag(cor(slopes, estItemPars[, 1:4]))
# diag(cor(intercepts, estItemPars[, 5:8]))
# cat("MSE:\n")
# round(c(m = mean((slopes[, 1] - estItemPars[, 1])^2),
#         a1 = mean((slopes[, 2] - estItemPars[, 2])^2),
#         a2 = mean((slopes[, 3] - estItemPars[, 3])^2),
#         e = mean((slopes[, 4] - estItemPars[, 4])^2),
#         dm = mean((intercepts[, 1] - estItemPars[, 5])^2),
#         da1 = mean((intercepts[, 2] - estItemPars[, 6])^2),
#         da2 = mean((intercepts[, 3] - estItemPars[, 7])^2),
#         de = mean((intercepts[, 4] - estItemPars[, 8])^2)), 3)
