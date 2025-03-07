#' Meta-Analysis of the median survival times
#'
#' This function implements the Wald approximation-based approach described by McGrath et al. (2025) to meta-analyze median survival times. This approach considers that each study reports a Kaplan-Meier estimate of median survival in (each group of) each study along with confidence intervals. This approach performs an inverse-variance weighted meta-analysis of the median survival time (for one group studies) or the difference/ratio of median survival times between groups (for two-group studies). The within-study standard errors are estimated by assuming the confidence intervals around the median survival estimates are Wald-type intervals.
#'
#' @param data data frame containing the study-specific summary data. For one-group studies, this data frame can contain the following columns:
#' \tabular{ll}{
#' \code{med.g1} \tab median. \cr
#' \code{med.ci.lb.g1} \tab lower confidence interval bound around the median. \cr
#' \code{med.ci.ub.g1} \tab upper confidence interval bound around the median. \cr
#' \code{med.ci.level.g1} \tab level of the confidence interval}
#' For two group studies, this data frame can also contain the following columns for the summary data of the second group: \code{med.g2}, \code{med.ci.lb.g2}, \code{med.ci.ub.g2}, and \code{med.ci.level.g2}.
#' @param estimand character string specifying the estimand in the meta-analysis. The options are \code{"median_g1"} (median in group 1), \code{"median_g2"} (median in group 2), \code{"difference_median"} (difference of medians across groups), and \code{"ratio_median"} (ratio of medians across groups).
#' @param pool_studies logical scalar specifying whether to meta-analyze the studies. If this argument is set to \code{FALSE}, function will not meta-analyze the studies and will return a list with components \code{yi} containing the study-specific outcome measure estimates and \code{sei} containing the study-specific within-study standard error estimates. The default is \code{TRUE}.
#' @param ... optional arguments that are passed into the \code{\link[metafor]{rma.uni}} function for pooling. See documentation of \code{\link[metafor]{rma.uni}}.
#'
#' @return an object of class "rma.uni". For additional details, see \code{\link[metafor]{rma.uni}}.
#' @references McGrath S., Kimmelman J., Ozturk O., Steele R., and Benedetti A. (2025). Meta-analysis of median survival times with inverse-variance weighting. arXiv e-prints arXiv:2503.03065.
#'
#' @examples
#' ## Median survival in the comparator group
#' metamedian_survival(data = dat.lung, estimand = "median_g2")
#'
#' ## Difference of median survival
#' metamedian_survival(data = dat.lung, estimand = "difference_median")
#'
#' ## Ratio of median survival (logarithm scale)
#' metamedian_survival(data = dat.lung, estimand = "ratio_median")
#'
#' @export

metamedian_survival <- function(data, estimand, pool_studies = TRUE, ...) {
  df <- check_and_clean_df(df = data, method = 'wald')
  if (!missing(estimand)){
    if (!estimand %in% c('median_g1', 'median_g2', 'difference_median', 'ratio_median')){
      stop("estimand must be set to 'median_g1', 'median_g2', 'difference_median', or 'ratio_median'")
    }
  }

  n_studies <- nrow(df)
  yi <- vi <- rep(NA, length = n_studies)

  for (i in 1:n_studies){
    if (estimand != 'median_g2'){
      se_g1 <- se_median_survival_helper(est = df$med.g1[i],
                                         lb = df$med.ci.lb.g1[i],
                                         ub = df$med.ci.ub.g1[i],
                                         ci_level = df$med.ci.level.g1[i])
    }
    if (estimand != 'median_g1'){
      se_g2 <- se_median_survival_helper(est = df$med.g2[i],
                                         lb = df$med.ci.lb.g2[i],
                                         ub = df$med.ci.ub.g2[i],
                                         ci_level = df$med.ci.level.g2[i])
    }
    if (estimand == 'median_g1'){
      yi[i] <- df$med.g1[i]
      vi[i] <- se_g1^2
    } else if (estimand == 'median_g2'){
      yi[i] <- df$med.g2[i]
      vi[i] <- se_g2^2
    } else if (estimand == 'difference_median'){
      yi[i] <- df$med.g1[i] - df$med.g2[i]
      vi[i] <- se_g1^2 + se_g2^2
    } else if (estimand == 'ratio_median'){
      yi[i] <- log(df$med.g1[i]) - log(df$med.g2[i])
      vi[i] <- se_g1^2 / df$med.g1[i]^2 + se_g2^2 / df$med.g2[i]^2
    }
  }
  if (!pool_studies){
    res <- list(yi = yi, sei = sqrt(vi))
  } else {
    res <- metafor::rma.uni(yi = yi, vi = vi, ...)
  }

  return(res)
}

se_median_survival_helper <- function(est, lb, ub, ci_level){
  alpha <- (1 - ci_level)
  if (!is.na(ub)){
    return((ub - lb) / (2 * stats::qnorm(1 - alpha / 2)))
  } else {
    return((est - lb) / (stats::qnorm(1 - alpha / 2)))
  }
}
