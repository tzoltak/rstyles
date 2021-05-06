#' @title Log-normal distribution parameters
#' @rdname log-normal
#' @description Functions return parameters of a log-normal distribution on the
#' exponentiated scale (\code{lnorm_mean}, \code{lnorm_sd}) or values of
#' parameters on the log scale that will result in given values of parameters
#' on the exponentiated scale (\code{find_pars_lnorm}).
#' @param meanlog mean (expected value)of the distribution on the log scale
#' @param sdlog standard deviation of the distribution on the log scale
#' @param m mean (expected value) of the distribution on the exponential scale
#' @param sd standard deviation of the distribution on the exponential scale
#' @examples
#' # actual expected value of the log-normal distribution with the log scale
#' # parameters of 3 and 1.5 respectively
#' lnorm_mean(3, 1.5)
#' # check:
#' mean(rlnorm(10^6, 3, 1.5))
#' # actual standard deviation of the log-normal distribution with the log scale
#' # parameters of 3 and 1.5 respectively
#' lnorm_sd(3, 1.5)
#' # check:
#' sd(rlnorm(10^6, 3, 1.5))
#' # what log scale parameters one should use to get actual (exponentiated scale)
#' # expected value and standard deviaton of the geneareted data of 1 and 0.2
#' # respectively?
#' (p <- find_pars_lnorm(1, 0.2))
#' # check:
#' lnorm_mean(p[1], p[2])
#' lnorm_sd(p[1], p[2])
#' @export
lnorm_mean <- function(meanlog, sdlog) {
  return(exp(meanlog + sdlog^2/2))
}
#' @rdname log-normal
#' @export
lnorm_sd <- function(meanlog, sdlog) {
  return((exp(sdlog^2) - 1)^0.5 * exp(2 * meanlog + sdlog^2)^0.5)
}
#' @rdname log-normal
#' @export
find_pars_lnorm <- function(m, sd) {
  vlog <- log(sd^2/m^2 + 1)
  meanlog <- log(m) - vlog/2
  return(c(meanlog = meanlog, sdlog = vlog^0.5))
}
