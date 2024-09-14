#' Study-Level application of quantile estimation method
#'
#' This function estimates the asymptotic sampling variance of either the (estimated) median or the (estimated) difference of medians for a primary study that reports one of the following summary measures: \itemize{
#' \item S1: median, minimum and maximum values, and sample size
#' \item S2: median, first and third quartiles, and sample size
#' \item S3: median, minimum and maximum values, first and third quartiles, and sample size
#' \item S4: mean, standard deivation, and sample size.
#'  }.
#'
#' In order to estimate the asymptotic sampling variance of the median (in S1, S2, or S3), one must have an estimate of the probability density function of the outcome evaluated at the population median. The \code{\link[estmeansd]{qe.fit}} function is applied to estimate the outcome distribution.
#'
#' For two-group studies studies, one may assume that the outcome in both groups follows the same parametric family of distributions. In this case, distribution selection for the QE method is applied as follows. The \code{\link[estmeansd]{qe.fit}} function is applied to fit the candidate distributions of each group separately. However, for each candidate distribution, the objective function evaluated at the fitting parameters are summed over the two groups. The parametric family of distributions with the smallest sum is used as the underlying distribution of the both groups. If \code{single.family} is \code{TRUE}, then \code{selected.dist} is a character string indicating the selected parametric family. If \code{single.family} is \code{FALSE}, then \code{selected.dist} is a vector of length 2 where elements 1 and 2 are character strings of the selected parametric families in groups 1 and 2, respectively.
#'
#' One may also assume for two-group studies that the outcome distributions in the two groups only differ by a location shfit. In this case, a weighted mean (weighted by sample size) of the estimated probability density functions evaluated at the population medians is used to estimate the asymptotic sampling variance of the difference of medians. See McGrath et al. (2020) for further details.
#'
#' When a study provides S4 summary measures, the outcome distribution is assumed to be normal. The sample median is estimated by the sample mean, and its variance is estimated by the sample variance divided by the sample size. In this case, the \code{single.family} and \code{loc.shift} arguments are not used.
#'
#' @param min.g1 numeric value giving the sample minimum (first group for two-group studies).
#' @param q1.g1 numeric value giving the first quartile (first group for two-group studies).
#' @param med.g1 numeric value giving the sample median (first group for two-group studies).
#' @param q3.g1 numeric value giving the sample third quartile (first group for two-group studies).
#' @param max.g1 numeric value giving the sample maximum (first group for two-group studies).
#' @param n.g1 numeric value giving the sample size (first group for two-group studies).
#' @param mean.g1 numeric value giving the sample mean (first group for two-group studies).
#' @param sd.g1 numeric value giving the sample standard deviation (first group for two-group studies).
#' @param min.g2 numeric value giving the sample minimum of the second group for two-group studies.
#' @param q1.g2 numeric value giving the sample first quartile of the second group for two-group studies.
#' @param med.g2 numeric value giving the sample median of the second group for two-group studies.
#' @param q3.g2 numeric value giving the sample third quartile of the second group for two-group studies.
#' @param max.g2 numeric value giving the sample maximum of the second group for two-group studies.
#' @param n.g2 numeric value giving the sample size of the second group for two-group studies.
#' @param mean.g2 numeric value giving the sample mean of the second group for two-group studies.
#' @param sd.g2 numeric value giving the sample standard deviation of the second group for two-group studies.
#' @param single.family logical scalar indicating that for two-group studies, the parametric family of distributions is assumed to be the same across both groups (the default is \code{FALSE}). See 'Details'.
#' @param loc.shift logical scalar indicating that for two-group studies, distributions are assumed to only differ by a location shift (the default is \code{FALSE}). See 'Details'.
#' @param qe.fit.control.g1 optional list of control parameters for \code{\link[estmeansd]{qe.fit}} (first group for two-group studies).
#' @param qe.fit.control.g2 optional list of control parameters for \code{\link[estmeansd]{qe.fit}} of the second group for two-group studies.
#'
#' @return A list with the following components:
#' \item{var}{Estimated sampling variance of the effect size.}
#' \item{effect.size}{Effect size of study.}
#' \item{selected.dist}{Selected outcome distribution(s). See 'Details'.}
#' \item{study.type}{Character string specifying whether one-group or two-group summary data was provided.}
#'
#' @references McGrath S., Sohn H., Steele R., and Benedetti A. (2020). Meta-analysis of the difference of medians. \emph{Biometrical Journal}, \strong{62}, 69-98.
#'
#' @examples
#' ## Generate S2 summary data
#' set.seed(1)
#' n <- 100
#' x <- stats::rlnorm(n, 2.5, 1)
#' quants <- stats::quantile(x, probs = c(0.25, 0.5, 0.75))
#'
#' ## Estimate sampling variance of the median
#' qe.study.level(q1.g1 = quants[1], med.g1 = quants[2], q3.g1 = quants[3],
#'                n.g1 = n)
#'
#' @export


qe.study.level <- function(min.g1, q1.g1, med.g1, q3.g1, max.g1, n.g1, mean.g1,
                           sd.g1, min.g2, q1.g2, med.g2, q3.g2, max.g2, n.g2,
                           mean.g2, sd.g2, single.family = FALSE,
                           loc.shift = FALSE, qe.fit.control.g1 = list(),
                           qe.fit.control.g2 = list()) {
  if ((missing(min.g2) || is.na(min.g2)) & (missing(q1.g2) || is.na(q1.g2)) &
      (missing(med.g2) || is.na(med.g2)) & (missing(q3.g2) || is.na(q3.g2)) &
      (missing(max.g2) || is.na(max.g2)) & (missing(mean.g2) || is.na(mean.g2))
      & (missing(sd.g2) || is.na(sd.g2))) {
    one.sample <- TRUE
  } else {
    one.sample <- FALSE
  }
  scenario.g1 <- get.scenario(min.g1, q1.g1, med.g1, q3.g1, max.g1, mean.g1,
                              sd.g1)
  if (missing(n.g1) || is.na(n.g1)) {
    stop("Need to specify n.g1")
  }
  if (!one.sample) {
    scenario.g2 <- get.scenario(min.g2, q1.g2, med.g2, q3.g2, max.g2, mean.g2,
                                sd.g2)
    if (missing(n.g2) || is.na(n.g2)) {
      stop("Need to specify n.g2")
    }
  }

  if (scenario.g1 == "S4") {
    effect.size.g1 <- mean.g1
    asymptvar.g1 <- sd.g1^2 / n.g1
    selected.dist.g1 <- "normal"
  } else {
    effect.size.g1 <- med.g1
    fit.g1 <- estmeansd::qe.fit(min.val = min.g1, q1.val = q1.g1,
                                med.val = med.g1, q3.val = q3.g1,
                                max.val = max.g1, n = n.g1,
                                qe.fit.control = qe.fit.control.g1,
                                two.sample.default = TRUE)
    selected.dist.g1 <- names(which.min(fit.g1$values))
    fitted.fm.g1 <- get.fm(fit.g1, selected.dist.g1)
    asymptvar.g1 <- 0.25 * (1 / (fitted.fm.g1^2 * n.g1))
  }
  if (one.sample) {
    effect.size <- effect.size.g1
    asymptvar <- asymptvar.g1
    selected.dist <- selected.dist.g1
  } else {
    if (scenario.g2 == "S4") {
      effect.size.g2 <- mean.g2
      asymptvar.g2 <- sd.g2^2 / n.g2
      selected.dist.g2 <- "normal"
    } else {
      effect.size.g2 <- med.g2
      fit.g2 <- estmeansd::qe.fit(min.val = min.g2, q1.val = q1.g2,
                                  med.val = med.g2, q3.val = q3.g2,
                                  max.val = max.g2, n = n.g2,
                                  qe.fit.control = qe.fit.control.g2,
                                  two.sample.default = TRUE)
      selected.dist.g2 <- names(which.min(fit.g2$values))
      fitted.fm.g2 <- get.fm(fit.g2, selected.dist.g2)
      asymptvar.g2 <- 0.25 * (1 / (fitted.fm.g2^2 * n.g2))
    }
    effect.size <- effect.size.g1 - effect.size.g2
    asymptvar <- asymptvar.g1 + asymptvar.g2
    selected.dist <- c(selected.dist.g1, selected.dist.g2)

    if (scenario.g1 != "S4" & scenario.g2 != "S4" &
        (single.family | loc.shift)){
      if (single.family){
        selected.dist <- names(which.min(fit.g1$values + fit.g2$values))
        fitted.fm.g1 <- get.fm(fit.g1, selected.dist)
        fitted.fm.g2 <- get.fm(fit.g2, selected.dist)
      }
      if (loc.shift) {
        fitted.fm <- stats::weighted.mean(x = c(fitted.fm.g1, fitted.fm.g2),
                                          w = c(n.g1, n.g2))
        asymptvar <- 0.25 / (fitted.fm)^2 * (1 / n.g1 + 1 / n.g2)
      } else {
        asymptvar <- 0.25 * (1 / (fitted.fm.g1^2 * n.g1) +
                               1 / (fitted.fm.g2^2 * n.g2))
      }
    }
  }
  if (one.sample) {
    study.type <- "one-group"
  } else {
    study.type <- "two-group"
  }
  return(list(var = asymptvar, effect.size = effect.size,
              selected.dist = selected.dist, study.type = study.type))
}
