#' Print method for objects of class "qe.fit"
#'
#' @description
#'
#' \emph{Note}: As of release 0.1.2, this function is deprecated; \code{\link[estmeansd]{print.qe.fit}} (in the \sQuote{estmeansd} package) should be used instead.
#'
#' Print method for objects of class "qe.fit".
#'
#' @param x object of class "qe.fit".
#' @param ... other arguments.
#' @return No value is returned.
#' @seealso \code{\link{metamedian-deprecated}}, \code{\link{qe.fit}}
#' @examples
#' \donttest{
#' ## Generate S2 summary data
#' set.seed(1)
#' n <- 100
#' x <- stats::rlnorm(n, 2.5, 1)
#' quants <- stats::quantile(x, probs = c(0.25, 0.5, 0.75))
#'
#' ## Fit distributions
#' res <- qe.fit(q1.val = quants[1], med.val = quants[2], q3.val = quants[3],
#'        n = n)
#' print(res)
#' }
#'
#' @export

## S3 method for class 'qe.fit'
print.qe.fit <- function(x, ...) {
  if (length(x$norm.par) != 1) {
    cat("\nEstimated parameters of normal distribution:\n")
    print(x$norm.par)
  } else {
    cat("\nNormal distribution not fit\n")
  }
  if (length(x$lnorm.par) != 1) {
    cat("\nEstimated parameters of log-normal distribution:\n")
    print(x$lnorm.par)
  } else {
    cat("\nLog-normal distribution not fit\n")
  }
  if (length(x$gamma.par) != 1) {
    cat("\nEstimated parameters of gamma distribution:\n")
    print(x$gamma.par)
  } else {
    cat("\nGamma distribution not fit\n")
  }
  if (length(x$weibull.par) != 1) {
    cat("\nEstimated parameters of Weibull distribution:\n")
    print(x$weibull.par)
  } else {
    cat("\nWeibull distribution not fit\n")
  }
  cat("\nValues of objective functions corresponding to estimated
      parameters:\n")
  print(x$values)
}
