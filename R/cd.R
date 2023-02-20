#' Meta-Analysis via the confidence distribution approach
#'
#' The function applies the confidence distribution (CD) approach of Ozturk and Balakrishnan (2020) to meta-analyze one-group studies where each study reports one of the following summary measures: \itemize{
#' \item C1 (and C2): lower and upper bounds of a confidence interval around the median, and coverage probability
#' \item C3: median, variance estimate of the median, and sample size
#' \item C4: mean, standard deviation, and sample size.
#' \item C5: median, first and third quartiles, and sample size
#'  }
#' The function estimates the pooled median.
#'
#' Letting \eqn{k} denote the number of studies, provide study-specific summary data as vectors of length \eqn{k}. If a study does not report a given summary measure (e.g., the minimum value), give a value of \code{NA} for the position in the relevant vector. If no studies report a given summary measure, a vector of only \code{NA} values need not be provided. See 'Examples' for appropriate use.
#'
#' @param q1 vector of study-specific sample first quartile values. See 'Details'.
#' @param med vector of study-specific sample median values. See 'Details'.
#' @param q3 vector of study-specific sample third quartile values. See 'Details'.
#' @param n vector of study-specific sample sizes. See 'Details'.
#' @param mean vector of study-specific sample mean values. See 'Details'.
#' @param sd vector of study-specific sample standard deviation values. See 'Details'.
#' @param med.var vector of study-specific estimates of the variance of the median. See 'Details'.
#' @param med.ci.lb vector of study-specific lower confidence interval bounds around the medians
#' @param med.ci.ub vector of study-specific upper confidence interval bounds around the medians
#' @param alpha.1 vector of the study-specific \eqn{\alpha_1} values from Ozturk and Balakrishnan (2020)
#' @param alpha.2 vector of the study-specific \eqn{\alpha_2} values from Ozturk and Balakrishnan (2020)
#' @param pooled.median.ci.level optional numeric scalar indicating the desired coverage probability for the pooled median estimate. The default is \code{0.95}.
#' @param method character string specifying whether a fixed effect or random effects model is used. The options are \code{FE} (fixed effect) are \code{RE} (random effects). The default is \code{RE}.
#' @param pool_studies logical scalar specifying whether to meta-analyze the studies. If this argument is set to \code{FALSE}, function will not meta-analyze the studies and will return a list with components \code{yi} containing the study-specific effect size estimates and \code{sei} containing the study-specific within-study standard error estimates. The default is \code{TRUE}.
#'
#' @return A list with components
#' \item{pooled.est}{Pooled estimate of the median}
#' \item{pooled.est.var}{Estimated variance of the pooled median estimator}
#' \item{pooled.est.ci.lb}{Lower bound of confidence interval for the pooled median}
#' \item{pooled.est.ci.ub}{Upper bound of confidence interval for the pooled median}
#' \item{tausq.est}{Estimate of between-study variance (applicable only when \code{method} is set to \code{RE})}
#' \item{yi}{Study-specific point estimates}
#' \item{vi}{Study-specific sampling variances}
#'
#' @references Ozturk, O. and Balakrishnan N. (2020). Meta‐analysis of quantile intervals from different studies with an application to a pulmonary tuberculosis data. \emph{Statistics in Medicine}, \strong{39}, 4519-4537.
#'
#' @examples
#' ## Example 1:
#' med.vals <- c(6.1, 5.2, 3.1, 2.8, 4.5)
#' q1.vals <- c(2.0, 1.6, 2.6, 0.9, 3.2)
#' q3.vals <- c(10.2, 13.0, 8.3, 8.2, 9.9)
#' n.vals <- c(100, 92, 221, 81, 42)
#'
#' ## Meta-analyze studies via CD method
#' cd(q1 = q1.vals, med = med.vals, q3 = q3.vals, n = n.vals)
#'
#'
#' @export

cd <- function(q1, med, q3, n, mean, sd, med.var, med.ci.lb, med.ci.ub,
               alpha.1, alpha.2, pooled.median.ci.level = 0.95, method = 'RE',
               pool_studies = FALSE) {
  all.data.args.names <- c("q1", "med", "q3", "n", "mean",
                           "sd", "med.var", "med.ci.lb", "med.ci.ub", "alpha.1",
                           "alpha.2")

  all.args <- as.list(environment())
  data.args <- all.args[names(all.args) %in% all.data.args.names]
  data.args.spec <- data.args[sapply(data.args,
                                     function(x) class(x) == "numeric" |
                                       class(x) == "integer")]
  data.args.unspec.names <- all.data.args.names[!(all.data.args.names
                                                  %in% names(data.args.spec))]

  if (length(unique(lengths(data.args.spec))) != 1) {
    stop("All vectors of summary data must have same length")
  }

  df <- as.data.frame(data.args.spec)
  df[, data.args.unspec.names] <- rep(NA, nrow(df))
  na.row.indicator <- apply(df, 1, function(x) all(is.na(x)))
  if (any(na.row.indicator)) {
    df <- df[!na.row.indicator, ]
  }

  alpha <- 1 - pooled.median.ci.level
  n_studies <- length(med)
  yi <- vi <- rep(NA, length = n_studies)

  for (i in 1:n_studies){
    scenario <- get.scenario.cd(q1 = df$q1[i], med = df$med[i], q3 = df$q3[i], mean = df$mean[i], sd = df$sd[i],
                                med.var = df$med.var[i], med.ci.lb = df$med.ci.lb[i], med.ci.ub = df$med.ci.ub[i],
                                alpha.1 = df$alpha.1[i], alpha.2 = df$alpha.2[i])

    if (scenario == 'C1'){
      zl <- stats::qnorm(df$alpha.1[i])
      zu <- stats::qnorm(1 - df$alpha.2[i])
      vi[i] <- (df$med.ci.ub[i] - df$med.ci.lb[i])^2 / (zu - zl)^2
      yi[i] <- (df$med.ci.lb[i] + df$med.ci.ub[i]) / 2 - (zl + zu) * sqrt(vi[i]) / 2
    } else if (scenario == 'C3'){
      yi[i] <- df$med[i]
      vi[i] <- df$med.var[i]
    } else if (scenario == 'C4'){
      yi[i] <- df$mean[i]
      vi[i] <- df$sd[i]^2 / df$n[i]
    } else if (scenario == 'C5'){
      yi[i] <- df$med[i]
      rr <- round(df$n[i] * 0.25, digits = 0)
      ss <- round(df$n[i] * 0.75, digits = 0)
      zl <- (rr - df$n[i] * 0.5 - 0.5) / sqrt(df$n[i] * 0.5^2)
      zu <- (ss - df$n[i] * 0.5 - 0.5) / sqrt(df$n[i] * 0.5^2)
      vi[i] <- (df$q3[i] - df$q1[i])^2 / (zu - zl)^2
    }
  }

  if (pool_studies){
    pooled_est_FE <- stats::weighted.mean(x = yi, w = 1 / vi)
    if (method == 'FE'){
      pooled_est <- pooled_est_FE
      pooled_est_var <- Jacknife(yi, vi, pooled_est)
      tausq_est <- NULL
    } else if (method == 'RE'){
      Q <- sum((yi - pooled_est_FE)^2 / vi)  # computation of Q^2 in section  3 of the paper
      A1 <- sum(1 / vi)
      C <- A1 - sum((1 / vi)^2) / A1
      tausq_est <- max((Q - (n_studies - 1)) / C, 0)
      pooled_est <- stats::weighted.mean(x = yi, w = 1 / (vi + tausq_est))
      pooled_est_var <- Jacknife(yi, vi + tausq_est, pooled_est)
    }

    ci.lb <- stats::qt(alpha / 2, n_studies - 1) * sqrt(pooled_est_var) + pooled_est
    ci.ub <- stats::qt(1 - alpha / 2, n_studies - 1) * sqrt(pooled_est_var) + pooled_est
    output <- list(pooled.est = pooled_est, pooled.est.var = pooled_est_var,
                   ci.lb = ci.lb, ci.ub = ci.ub, tausq.est = tausq_est,
                   yi = yi, vi = vi)
    return(output)
  } else {
    return(list(yi = yi, sei = sqrt(vi)))
  }

}

Jacknife <- function(yi, vi, pooled_est){
  n_studies <- length(yi)
  pooled_est_minusi <- rep(NA, n_studies)
  for (i in (1:n_studies)){
    pooled_est_minusi[i] <- stats::weighted.mean(yi[-i], 1 / vi[-i])
  }
  pi <- n_studies * pooled_est - (n_studies - 1) * pooled_est_minusi
  JVE <- sum((pi - mean(pi))^2) / ((n_studies - 1) * n_studies)
  return(JVE)
}

get.scenario.cd <- function(q1, med, q3, mean, sd, med.var, med.ci.lb, med.ci.ub,
                            alpha.1, alpha.2){
  if (!is.na(med.ci.lb) & !is.na(med.ci.ub) & !is.na(alpha.1) & !is.na(alpha.2)){
    return('C1')
  } else if (!is.na(med) & !is.na(med.var)){
    return('C3')
  } else if (!is.na(mean) & !is.na(sd)){
    return('C4')
  } else if (!is.na(med) & !is.na(q1) & !is.na(q3)){
    return('C5')
  } else {
    stop('Summary measures not in appropriate form. See documentation for
         appropriate forms for the CD approach.')
  }
}
