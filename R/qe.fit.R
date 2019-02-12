#' Quantile estimation fitting
#'
#' @description
#'
#' \emph{Note}: As of release 0.1.2, this function is deprecated; \code{\link[estmeansd]{qe.fit}} (in the \sQuote{estmeansd} package) should be used instead.
#'
#' This function fits several parametric families of distributions from summary data in the following forms: \itemize{
#' \item S1: median, minimum and maximum values, and sample size
#' \item S2: median, first and third quartiles, and sample size
#' \item S3: median, minimum and maximum values, first and third quartiles, and sample size
#'  }
#'
#' @details
#'
#' Distributions are fit by minimizing the distance between observed and distribution quantiles in the L2-norm. If data are positive, the normal, log-normal, gamma, and Weibull distributions are fit. Otherwise, only the normal distribution is fit. The limited-memory Broyden-Fletcher-Goldfarb-Shanno (L-BFGS-M) algorithm implemented in the \code{\link[stats]{optim}} function is used for minimization. Default box constraints are described in McGrath et al. (2018).
#'
#' @param min.val numeric value giving the sample minimum.
#' @param q1.val numeric value giving the sample first quartile.
#' @param med.val numeric value giving the sample median.
#' @param q3.val numeric value giving the sample third quartile.
#' @param max.val numeric value giving the sample maximum.
#' @param n numeric value giving the sample size.
#' @param qe.fit.control optional list of control parameters for the minimization algorithm. Default values are defined in the function.
#' \tabular{ll}{
#' \code{norm.mu.start} \tab numeric value giving the starting value for the \eqn{\mu} parameter of the normal distribution. \cr
#' \code{norm.sigma.start} \tab numeric value giving the starting value for the \eqn{\sigma} parameter of the normal distribution. \cr
#' \code{lnorm.mu.start} \tab numeric value giving the starting value for the \eqn{\mu} parameter of the log-normal distribution. \cr
#' \code{lnorm.sigma.start} \tab numeric value giving the starting value for the \eqn{\sigma} parameter of the log-normal distribution. \cr
#' \code{gamma.shape.start} \tab numeric value giving the starting value for the shape parameter of the gamma distribution. \cr
#' \code{gamma.rate.start} \tab numeric value giving the starting value for the rate parameter of the gamma distribution. \cr
#' \code{weibull.shape.start} \tab numeric value giving the starting value for the shape parameter of the Weibull distribution. \cr
#' \code{weibull.scale.start} \tab numeric value giving the starting value for the scale parameter of the Weibull distribution. \cr
#' \code{norm.mu.bounds} \tab vector giving the bounds on the \eqn{\mu} parameter of the normal distribution. \cr
#' \code{norm.sigma.bounds} \tab vector giving the bounds on the \eqn{\sigma} parameter of the normal distribution. \cr
#' \code{lnorm.mu.bounds} \tab vector giving the bounds on the \eqn{\mu} parameter of the the log-normal distribution. \cr
#' \code{lnorm.sigma.bounds} \tab vector giving the bounds on the \eqn{\sigma} parameter of the log-normal distribution. \cr
#' \code{gamma.shape.bounds} \tab vector giving the bounds on the shape parameter of the gamma distribution. \cr
#' \code{gamma.rate.bounds} \tab vector giving the bounds on the rate parameter of the gamma distribution. \cr
#' \code{weibull.shape.bounds} \tab vector giving the bounds on the shape parameter of the Weibull distribution. \cr
#' \code{weibull.scale.bounds} \tab vector giving the bounds on the scale parameter of the Weibull distribution. \cr}
#'
#' @return A object of class \code{qe.fit}. The object is a list with the following components:
#' \item{norm.par}{Estimated parameters of the normal distribution.}
#' \item{lnorm.par}{Estimated parameters of the log-normal distribution.}
#' \item{gamma.par}{Estimated parameters of the gamma distribution.}
#' \item{weibull.par}{Estimated parameters of Weibull distribution.}
#' \item{values}{Values of the objective functions evaluated at the estimated paramters of each candidate distribution.}
#'
#' The results are printed with the \code{\link{print.qe.fit}} function.

#' @references McGrath S., Sohn H., Steele R., and Benedetti A. (2018). Two-sample aggregate data meta-analysis of medians. \emph{ArXiv e-prints}. \url{https://arxiv.org/abs/1809.01278}.
#'
#' @seealso \code{\link{metamedian-deprecated}}
#' @examples
#' \donttest{
#' ## Generate S2 summary data
#' set.seed(1)
#' n <- 100
#' x <- stats::rlnorm(n, 2.5, 1)
#' quants <- stats::quantile(x, probs = c(0.25, 0.5, 0.75))
#'
#' ## Fit distributions
#' qe.fit(q1.val = quants[1], med.val = quants[2], q3.val = quants[3], n = n)
#' }
#'
#' @export

qe.fit <- function(min.val, q1.val, med.val, q3.val, max.val, n,
                   qe.fit.control = list()) {

  .Deprecated(msg = "'qe.fit' is deprecated.\nUse 'qe.fit' in the 'estmeansd' package instead.")

  scenario <- get.scenario(min.val, q1.val, med.val, q3.val, max.val)
  if (missing(n) & (scenario %in% c("S1", "S3"))) {
    stop("Need to specify n in S1 or S3")
  }

  if (scenario == "S1") {
    probs <- c(1 / n, 0.5, 1 - 1 / n)
    quants <- c(min.val, med.val, max.val)
  }
  if (scenario == "S2") {
    probs <- c(0.25, 0.5, 0.75)
    quants <- c(q1.val, med.val, q3.val)
  }
  if (scenario == "S3") {
    probs <- c(1 / n, 0.25, 0.5, 0.75, 1 - 1 / n)
    quants <- c(min.val, q1.val, med.val, q3.val, max.val)
  }

  if (min(quants == 0)) {
    quants[quants == 0] <- 10^(-2)
  }

  con <- list(
    norm.mu.start = med.val, norm.sigma.start = 1,
    norm.sigma.bounds = c(10^(-3), 50), lnorm.sigma.start = 1,
    lnorm.sigma.bounds = c(10^(-3), 10), gamma.shape.start = 1,
    gamma.rate.start = 1, gamma.shape.bounds = c(10^(-3), 40),
    gamma.rate.bounds = c(10^(-3), 40), weibull.shape.start = 1,
    weibull.scale.start = 1, weibull.shape.bounds = c(10^(-3), 50),
    weibull.scale.bounds = c(10^(-3), 50)
  )

  if (scenario == "S1" | scenario == "S2") {
    con$norm.mu.bounds <- c(quants[1], quants[3])
    if (min(quants) > 0) {
      con$lnorm.mu.start <- log(quants[2])
      con$lnorm.mu.bounds <- c(log(quants[1]), log(quants[3]))
    }
  }
  if (scenario == "S3") {
    con$norm.mu.bounds <- c(quants[2], quants[4])
    if (min(quants) > 0) {
      con$lnorm.mu.start <- log(quants[3])
      con$lnorm.mu.bounds <- c(log(quants[2]), log(quants[4]))
    }
  }

  con[names(qe.fit.control)] <- qe.fit.control

  S.theta.norm <- function(theta) {
    summand <- sum((stats::qnorm(p = probs, mean = theta[1],
                                 sd = theta[2]) - quants)^2)
  }
  S.theta.lnorm <- function(theta) {
    summand <- sum((stats::qlnorm(p = probs, meanlog = theta[1],
                                  sdlog = theta[2]) - quants)^2)
  }
  S.theta.gamma <- function(theta) {
    summand <- sum((stats::qgamma(p = probs, shape = theta[1],
                                  rate = theta[2]) - quants)^2)
  }
  S.theta.weibull <- function(theta) {
    summand <- sum((stats::qweibull(p = probs, shape = theta[1],
                                    scale = theta[2]) - quants)^2)
  }

  no.fit <- function(e) {
    return(list(par = NA, value = NA))
  }

  fit.norm <- tryCatch({
    stats::optim(par = c(con$norm.mu.start, con$norm.sigma.start),
                 S.theta.norm, method = "L-BFGS-B",
                 lower = c(con$norm.mu.bound[1], con$norm.sigma.bound[1]),
                 upper = c(con$norm.mu.bound[2], con$norm.sigma.bound[2]))
  },
  error = no.fit
  )

  if (min(quants) < 0) {
    message("Only fit the normal distribution because of negative quantiles.")
    fit.lnorm <- fit.gamma <- fit.weibull <- no.fit()
  } else {
    fit.lnorm <- tryCatch({
      stats::optim(par = c(con$lnorm.mu.start, con$lnorm.sigma.start),
                   S.theta.lnorm, method = "L-BFGS-B",
                   lower = c(con$lnorm.mu.bound[1], con$lnorm.sigma.bound[1]),
                   upper = c(con$lnorm.mu.bound[2], con$lnorm.sigma.bound[2]))
    },
    error = no.fit
    )
    fit.gamma <- tryCatch({
      stats::optim(par = c(con$gamma.shape.start, con$gamma.rate.start),
                   S.theta.gamma, method = "L-BFGS-B",
                   lower = c(con$gamma.shape.bound[1], con$gamma.rate.bound[1]),
                   upper = c(con$gamma.shape.bound[2], con$gamma.rate.bound[2]))
    },
    error = no.fit
    )
    fit.weibull <- tryCatch({
      stats::optim(par = c(con$weibull.shape.start, con$weibull.scale.start),
                   S.theta.weibull, method = "L-BFGS-B",
                   lower = c(con$weibull.shape.bound[1],
                             con$weibull.scale.bound[1]),
                   upper = c(con$weibull.shape.bound[2],
                             con$weibull.scale.bound[2]))
    },
    error = no.fit
    )
  }

  values <- c(fit.norm$value, fit.lnorm$value, fit.gamma$value,
              fit.weibull$value)
  names(values) <- c("normal", "log-normal", "gamma", "weibull")
  norm.par <- fit.norm$par
  lnorm.par <- fit.lnorm$par
  gamma.par <- fit.gamma$par
  weibull.par <- fit.weibull$par
  if (length(norm.par) != 1) {
    names(norm.par) <- c("mu", "sigma")
  }
  if (length(lnorm.par) != 1) {
    names(lnorm.par) <- c("mu", "sigma")
  }
  if (length(gamma.par) != 1) {
    names(gamma.par) <- c("shape", "rate")
  }
  if (length(weibull.par) != 1) {
    names(weibull.par) <- c("shape", "scale")
  }

  output <- list(norm.par = norm.par, lnorm.par = lnorm.par,
                 gamma.par = gamma.par, weibull.par = weibull.par,
                 values = values)
  class(output) <- "qe.fit"
  return(output)
}


