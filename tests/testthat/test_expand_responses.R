test_that("", {
  set.seed(5122020)
  sM <- make_scoring_matrix_aem(5, "mae")
  responses <- matrix(sample(1L:5L, 20, replace = TRUE), ncol = 4,
                      dimnames = list(NULL, paste0("item", 1:4)))
  respExp <- expand_responses(responses, sM)
  pattern <- matrix(c(0, 1, 1, 0, 1,NA,NA, 0, 1,NA,NA, 0,
                      0, 0, 0 ,0, 0, 1, 0, 0, 1, 1, 0, 1,
                      0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1,
                      0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1),
                    byrow = TRUE, nrow = nrow(responses),
                    dimnames = list(NULL, c(paste0("m_", colnames(responses)),
                                            paste0("a_", colnames(responses)),
                                            paste0("e_", colnames(responses)))))
  expect_identical(respExp, pattern)
})
