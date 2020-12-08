set.seed(26112020)
nItems <- 20
sM <- make_scoring_matrix_aem(1:6, "mae")
slopes <- matrix(c(rlnorm(nItems, 0, 0.2),
                   rlnorm(nItems, 0, 0.2),
                   rlnorm(nItems, 0, 0.2),
                   rlnorm(nItems, 0, 0.2)),
                 ncol = ncol(sM), dimnames = list(NULL, colnames(sM)))
intercepts <- matrix(c(rnorm(nItems, 0, 1.5),
                       rnorm(nItems, 0, 1.5),
                       rnorm(nItems, 0, 1.5),
                       rnorm(nItems, 0, 1.5)),
                     ncol = ncol(sM),
                     dimnames = list(NULL, paste0(colnames(sM), "1")))

items <- vector(mode = "list", length = nItems)
for (i in 1:nItems) {
  items[[i]] <- make_item(sM, slopes[i, ], intercepts[i, ], "sequential")
}
# uncorrelated traits
vcovTraits <- matrix(0, nrow = 3, ncol = 3,
                     dimnames = list(c("m", "a", "e"), c("m", "a", "e")))
diag(vcovTraits) <- 1
theta = mvtnorm::rmvnorm(1000, sigma = vcovTraits, )
colnames(theta) <- colnames(vcovTraits)

resp <- generate_test_responses(theta, items)
resp <- apply(resp, 1:2, as.numeric)
colnames(resp) <- paste0("i", 1:ncol(resp))

respWide <- expand_responses(resp, sM)

mSqt <- mirt(respWide,
             mirt.model("m = 1-20
                         a1 = 21-40
                         a2 = 41-60
                         e = 61-80"),
             '2PL', TOL = 0.1, verbose = FALSE)
estItemPars <- coef(mSqt, simplify = TRUE)$items
estItemPars <- cbind(m = estItemPars[1:nItems, 1],
                     a1 = estItemPars[(nItems + 1):(2*nItems), 2],
                     a2 = estItemPars[(2*nItems + 1):(3*nItems), 3],
                     e = estItemPars[(3*nItems + 1):(4*nItems), 4],
                     dm = estItemPars[1:nItems, 5],
                     da = estItemPars[(nItems + 1):(2*nItems), 5],
                     de = estItemPars[(2*nItems + 1):(3*nItems), 5])
# test_that("Item parameters of sequential M, A, E RS (with 5-point scale) recovers in estimation with reasonable MSEs.", {
#   expect_lt(mean((slopes[, 1] - estItemPars[, 1])^2), 0.02)
#   expect_lt(mean((slopes[, 2] - estItemPars[, 2])^2), 0.09)
#   expect_lt(mean((slopes[, 3] - estItemPars[, 3])^2), 0.05)
#   expect_lt(mean((intercepts[, 1] - estItemPars[, 4])^2), 0.02)
#   expect_lt(mean((intercepts[, 2] - estItemPars[, 5])^2), 0.05)
#   expect_lt(mean((intercepts[, 3] - estItemPars[, 6])^2), 0.06)
# })

# cat("Generating model:\n")
# round(cbind(slopes, intercepts), 2)
# cat("Estimated parameters:\n")
# round(estItemPars, 2)
# cat("Correlations:\n")
# diag(cor(slopes, estItemPars[, 1:3]))
# diag(cor(intercepts, estItemPars[, 4:6]))
# cat("MSE:\n")
# round(c(i = mean((slopes[, 1] - estItemPars[, 1])^2),
#         m = mean((slopes[, 2] - estItemPars[, 2])^2),
#         e = mean((slopes[, 3] - estItemPars[, 3])^2),
#         d1 = mean((intercepts[, 1] - estItemPars[, 4])^2),
#         d2 = mean((intercepts[, 2] - estItemPars[, 5])^2),
#         d3 = mean((intercepts[, 3] - estItemPars[, 6])^2)), 3)

rm(list = ls())
