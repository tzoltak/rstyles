bockenholtMAE5 <- matrix(c(0L, 0L, 1L,
                           0L, 0L, 0L,
                           1L, NA, NA,
                           0L, 1L, 0L,
                           0L, 1L, 1L),
                         byrow = TRUE, ncol = 3,
                         dimnames = list(1:5, c("m", "a", "e")))
bockenholtAEM6 <- matrix(c(0L, 1L, NA,
                           0L, 0L, 0L,
                           0L, 0L, 1L,
                           1L, 0L, 1L,
                           1L, 0L, 0L,
                           1L, 1L, NA),
                         byrow = TRUE, ncol = 3,
                         dimnames = list(1:6, c("a", "e", "m")))
plieninger5 <- matrix(c(0L, 0L, 1L, 0L,
                        1L, 0L, 0L, 0L,
                        2L, 1L, 0L, 0L,
                        3L, 0L, 0L, 1L,
                        4L, 0L, 1L, 1L),
                      byrow = TRUE, ncol = 4,
                      dimnames = list(1:5, c("i", "m", "e", "a")))
ema10m3e2 <- matrix(c(1L, NA, NA, NA, 0L, NA, NA, 0L, NA,
                      1L, NA, NA, NA, 0L, NA, NA, 1L, NA,
                      0L, 0L, 0L, NA, NA, 0L, NA, NA, NA,
                      0L, 0L, 0L, NA, NA, 1L, NA, NA, NA,
                      0L, 1L, NA, 0L, NA, NA, NA, NA, NA,
                      0L, 1L, NA, 1L, NA, NA, NA, NA, NA,
                      0L, 0L, 1L, NA, NA, NA, 0L, NA, NA,
                      0L, 0L, 1L, NA, NA, NA, 1L, NA, NA,
                      1L, NA, NA, NA, 1L, NA, NA, NA, 0L,
                      1L, NA, NA, NA, 1L, NA, NA, NA, 1L),
                    byrow = TRUE, ncol = 9,
                    dimnames = list(1:10, c("e", "m", "a1", "a2", "a3",
                                            "i1", "i2", "i3", "i4")))
ema10m3e2common <- matrix(c(1L, NA, 0L, 0L,
                            1L, NA, 0L, 1L,
                            0L, 0L, 0L, 0L,
                            0L, 0L, 0L, 1L,
                            0L, 1L, 0L, NA,
                            0L, 1L, 1L, NA,
                            0L, 0L, 1L, 0L,
                            0L, 0L, 1L, 1L,
                            1L, NA, 1L, 0L,
                            1L, NA, 1L, 1L),
                    byrow = TRUE, ncol = 4,
                    dimnames = list(1:10, c("e", "m", "a", "i")))
mae9rm3e2 <- matrix(c(0L, 0L, NA, 1L, 1L, NA, NA,
                      0L, 0L, NA, 1L, 0L, NA, NA,
                      0L, 0L, NA, 0L, NA, NA, NA,
                      1L, NA, 0L, NA, NA, NA, 1L,
                      1L, NA, 0L, NA, NA, NA, 0L,
                      1L, NA, 1L, NA, NA, NA, NA,
                      0L, 1L, NA, 0L, NA, NA, NA,
                      0L, 1L, NA, 1L, NA, 1L, NA,
                      0L, 1L, NA, 1L, NA, 0L, NA),
                    byrow = TRUE, ncol = 7,
                    dimnames = list(1:9, c("m", "a1", "a2", "e",
                                           "i1", "i2", "i3")))
test_that("", {
  expect_identical(make_scoring_matrix_aem(5, "mae"), bockenholtMAE5)
  expect_identical(make_scoring_matrix_aem(6, "aem"), bockenholtAEM6)
  expect_identical(make_scoring_matrix_aem(5, "gpcm"), plieninger5)
  expect_identical(make_scoring_matrix_aem(10, "ema", nMiddle = 3,
                                           nExtreme = 2), ema10m3e2)
  expect_identical(make_scoring_matrix_aem(10, "ema", nMiddle = 3,
                                           nExtreme = 2, aType = "common",
                                           iType = "common"), ema10m3e2common)
  expect_identical(make_scoring_matrix_aem(9, "mae", nMiddle = 3, nExtreme = 2,
                                           reversed = TRUE), mae9rm3e2)
})
