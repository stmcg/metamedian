#' Meta-Analysis via median of (the difference of) medians method
#'
#' This function meta-analyzes the study-specific effect sizes by applying the (weighted) median of medians method (McGrath et al., 2018a) in one-sample contexts and the (weighted) median of the difference of median method (McGrath et al., 2018b) in two-sample contexts.
#'
#' For one-group studies, authors may report the sample median or mean. If these measures are supplied for \code{yi} and weights are not provided for \code{wi}, the function implements the median of medians (MM) method (McGrath et al., 2018a).
#'
#' For two-group studies, authors may report the difference of medians or the difference of means across both groups. If these measures are supplied for \code{yi} and weights are not provided for \code{wi}, the function implements the median of the difference of medians (MDM) method (McGrath et al., 2018b).
#'
#' Analogous weighted versions of the MM and MDM methods can be applied when study-specific sample sizes are provided for \code{wi}.
#'
#' The confidence interval around the pooled estimate is constructed by inverting the sign test.
#'
#' @param yi vector of the study-specific effect sizes (e.g., the medians or the difference of medians)
#' @param wi optional vector of positive, study-specific weights (e.g., sample sizes)
#' @param norm.approx optional logical scalar indicating whether normality approximation of the binomial should be used to construct an approximate 95\% confidence interval (the default is \code{TRUE}).
#' @return A list with components
#' \item{pooled.est}{Pooled estimate}
#' \item{ci.lb}{Lower bound of confidence interval}
#' \item{ci.ub}{Upper bound of confidence interval}
#' \item{cov.level}{Theoretical coverage of the confidence interval around the pooled estimate. When \code{norm.approx} is set to \code{TRUE}, the theoretical coverage is 95\%. When \code{norm.approx} is set to \code{FALSE}, the theoretical coverage is set to the smallest possible value greater than 95\%.}
#'
#' @references McGrath S., Zhao X., Qin Z.Z., Steele R., and Benedetti A. (2018). One-sample aggregate data meta-analysis of medians. \emph{Statistics in Medicine}. 1–16.
#' @references McGrath S., Sohn H., Steele R., and Benedetti A. (2018). Two-sample aggregate data meta-analysis of medians. \emph{ArXiv e-prints}. \url{https://arxiv.org/abs/1809.01278}.
#'
#' @examples
#' ## Storing data (study-specific difference of medians)
#' yi <- c(5.23, 3.10, 0.50, 0.78, 3.48, 0.59, 2.20, 5.06, 4.00)
#'
#' ## Meta-analysis of the difference of medians
#' pool.med(yi)
#'
#' @export

pool.med <- function(yi, wi, norm.approx = TRUE) {
  if (missing(yi)) {
    stop("Need to specify yi argument")
  }
  if (!missing(wi)) {
    if (any(wi <= 0)) {
      stop("wi argument must only have positive elements")
    }
    if (length(yi) != length(wi)) {
      stop("Lengths of yi and wi arguments do not match")
    }
  }
  n <- length(yi)
  if (norm.approx) {
    cov.level <- 0.95
    prob <- min(0.5, stats::qnorm(0.975) / (2 * sqrt(n)))
    prob.vec <- c(0.5 - prob, 0.5, 0.5 + prob)
    if (missing(wi)) {
      quantiles <- unname(stats::quantile(yi, probs = prob.vec))
    } else {
      quantiles <- unname(Hmisc::wtd.quantile(yi, weights = wi,
                                              probs = prob.vec))
    }
  } else {
    if (n < 6) {
      stop("Not enough studies for exact coverage >= 95% CI.")
    }
    T.val.all <- 0:floor(n / 2)
    coverages <- 1 - 2 * stats::pbinom(T.val.all, n, 0.5)
    cov.level <- min(coverages[coverages > 0.95])
    ind <- which(coverages == cov.level)
    if (missing(wi)) {
      yi.sorted <- sort(yi)
      quantiles <- c(yi.sorted[ind], unname(stats::quantile(yi, probs = 0.5)),
                     yi.sorted[n - ind])
    } else {
      prob.vec <- c(ind / n, 0.5, (n - ind + 1) / n)
      quantiles <- unname(Hmisc::wtd.quantile(yi, weights = wi,
                                              probs = prob.vec))
    }
  }
  return(list(pooled.est = quantiles[2], ci.lb = quantiles[1],
              ci.ub = quantiles[3], cov.level = cov.level))
}
