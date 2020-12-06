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
plieninger5 <- matrix(c(1L, 0L, 1L, 0L,
                        2L, 0L, 0L, 0L,
                        3L, 1L, 0L, 0L,
                        4L, 0L, 0L, 1L,
                        5L, 0L, 1L, 1L),
                      byrow = TRUE, ncol = 4,
                      dimnames = list(1:5, c("i", "m", "e", "a")))
ema10m3e2 <- matrix(c(1L, 0L, 0L, 1L,
                      1L, 0L, 0L, 2L,
                      0L, 0L, 0L, 1L,
                      0L, 0L, 0L, 2L,
                      0L, 1L, 0L, NA,
                      0L, 1L, 1L, NA,
                      0L, 0L, 1L, 1L,
                      0L, 0L, 1L, 2L,
                      1L, 0L, 1L, 1L,
                      1L, 0L, 1L, 2L),
                    byrow = TRUE, ncol = 4,
                    dimnames = list(1:10, c("e", "m", "a", "i")))
ema10m3e2between <- matrix(c(1L, 0L, 0L, 1L,
                             1L, 0L, 0L, 2L,
                             0L, 0L, 0L, 3L,
                             0L, 0L, 0L, 4L,
                             0L, 1L, 0L, NA,
                             0L, 1L, 1L, NA,
                             0L, 0L, 1L, 5L,
                             0L, 0L, 1L, 6L,
                             1L, 0L, 1L, 7L,
                             1L, 0L, 1L, 8L),
                           byrow = TRUE, ncol = 4,
                           dimnames = list(1:10, c("e", "m", "a", "i")))
mae9rm3e2 <- matrix(c(0L, 0L, 1L, 2L,
                      0L, 0L, 1L, 1L,
                      0L, 0L, 0L, NA,
                      1L, 0L, 0L, 2L,
                      1L, 0L, 0L, 1L,
                      1L, 1L, NA, NA,
                      0L, 1L, 0L, NA,
                      0L, 1L, 1L, 2L,
                      0L, 1L, 1L, 1L),
                    byrow = TRUE, ncol = 4,
                    dimnames = list(1:9, c("m", "a", "e", "i")))
test_that("", {
  expect_identical(make_scoring_matrix_aem(5, "mae"), bockenholtMAE5)
  expect_identical(make_scoring_matrix_aem(6, "aem"), bockenholtAEM6)
  expect_identical(make_scoring_matrix_aem(5, "simultaneous"), plieninger5)
  expect_identical(make_scoring_matrix_aem(10, "ema", nMiddle = 3,
                                           nExtreme = 2), ema10m3e2)
  expect_identical(make_scoring_matrix_aem(10, "ema", nMiddle = 3,
                                           nExtreme = 2, iType = "between"),
                   ema10m3e2between)
  expect_identical(make_scoring_matrix_aem(9, "mae", nMiddle = 3, nExtreme = 2,
                                           reversed = TRUE), mae9rm3e2)
})
