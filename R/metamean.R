#' Meta-Analysis of the (difference of) means
#'
#' The function meta-analyzes one-group or two-group studies where each study reports one of the following summary measures: \itemize{
#' \item S1: median, minimum and maximum values, and sample size
#' \item S2: median, first and third quartiles, and sample size
#' \item S3: median, minimum and maximum values, first and third quartiles, and sample size
#' \item S4: mean, standard deivation, and sample size.
#'  }
#' This function estimates the study-specific means and their standard errors from the S1, S2, S3, or S4 summary data. When studies report S1, S2, or S3 summary data, a number of approaches can be applied to estimate the study-specific means and their standard errors. Then, this function estimates the pooled mean (for one-group studies) or the pooled difference of means (for two-group studies) based on the standard inverse variance method via the \code{\link[metafor]{rma.uni}} function. The convention used for calculating differences of means in two-group studies is: mean in group 1 minus mean in group 2.
#'
#' @param data data frame containing the study-specific summary data. For one-group studies, this data frame can contain the following columns:
#' \tabular{ll}{
#' \code{min.g1} \tab minimum value. \cr
#' \code{q1.g1} \tab first quartile. \cr
#' \code{med.g1} \tab median. \cr
#' \code{q3.g1} \tab third quartile. \cr
#' \code{max.g1} \tab maximum value. \cr
#' \code{n.g1} \tab sample size. \cr
#' \code{mean.g1} \tab sample mean. \cr
#' \code{sd.g1} \tab sample standard deviation. \cr}
#' For two group studies, this data frame can also contain the following columns for the summary data of the second group: \code{min.g2}, \code{q1.g2}, \code{med.g2}, \code{q3.g2}, \code{max.g2}, \code{n.g2}, \code{mean.g2}, and \code{sd.g2}.
#' @param mean_method character string specifying the approach used to estimate the study-specific means. The options are the following:
#' \tabular{ll}{
#' \code{'hozo/wan/bland'} \tab Method recommended by Wan et al. (2014), i.e., the method of Hozo et al. (2005) in scenario S1, method of Wan et al. (2014) in scenario S2, and method of Bland (2015) in scenario S3. \cr
#' \code{'luo'} \tab Method of Luo et al. (2018). \cr
#' \code{'shi_lognormal'} \tab Method of Shi et al. (2020b). \cr
#' \code{'qe'} \tab Quantile Matching Estimation method (McGrath et al. 20220). \cr
#' \code{'bc'} \tab Box-Cox method (McGrath et al. 2020). \cr
#' \code{'mln'} \tab Method for Unknown Non-Normal Distributions (Cai et al. 2021). \cr
#' \code{'yang'} \tab Method of Yang et al. (2022) under the assumption of normality. \cr}
#' @param se_method character string specifying the approach used to estimate the standard errors of the study-specific means estimators in scenarios S1, S2, and S3. The options are the following:
#' \tabular{ll}{
#' \code{'naive'} \tab Uses the estimated standard deviation divided by the square root of the sample size as the estimate of the standard error of the mean estimator. This is the default option when \code{mean_method} is set to \code{'wan'}, \code{'luo'}, or \code{'shi_lognormal'}. The approach used to estimate the standard deviation is specified by the \code{sd_method} argument. \cr
#' \code{'bootstrap'} \tab Parametric bootstrap approach described by McGrath et al. (2023). This option is only available (and is the default option) when \code{mean_method} is set to \code{'qe'}, \code{'bc'}, or \code{'mln'}. \cr
#' \code{'plugin'} \tab Uses the analytically derived standard error of the mean estimator, and plugs in the estimated standard deviation in place of the distributional standard deviation. This option is only available (and is the default option) when \code{mean_method} is set to \code{'yang'}. \cr}
#' @param sd_method character string specifying the approach used to estimate the study-specific standard deviations when applying the naive standard error estimator (if applicable). The options are the following:
#' \tabular{ll}{
#' \code{'wan'} \tab Method of Wan et al. (2014). This is the default option when \code{mean_method} is set to \code{'hozo/wan/bland/hozo'}.\cr
#' \code{'wan/shi_normal'} \tab Method recommended by Shi et al. (2020a), i.e., the method of Wan et al. (2014) in scenarios S1 and S2 and the method of Shi et al. (2020a) in scenario S3. This is the default option when \code{mean_method} is set to \code{'luo'}. \cr
#' \code{'shi_lognormal'} \tab Method of Shi et al. (2020b). This is the default option when \code{mean_method} is set to \code{'shi_lognormal'}. \cr
#' \code{'qe'} \tab Quantile Matching Estimation method (McGrath et al. 20220). This is the default option when \code{mean_method} is set to \code{'qe'}. \cr
#' \code{'bc'} \tab Box-Cox method (McGrath et al. 2020). This is the default option when \code{mean_method} is set to \code{'bc'}. \cr
#' \code{'mln'} \tab Method for Unknown Non-Normal Distributions (Cai et al. 2021). This is the default option when \code{mean_method} is set to \code{'mln'}. \cr
#' \code{'yang'} \tab Method of Yang et al. (2022) under the assumption of normality. This is the default option when \code{mean_method} is set to \code{'yang'}. \cr}
#' @param nboot integer specifying the number of bootstrap samples to use when using parametric bootstrap to estimate the study-specific standard errors in scenarios S1, S2, and S3. The default is \code{1000}.
#' @param pool_studies logical scalar specifying whether to meta-analyze the studies. If this argument is set to \code{FALSE}, function will not meta-analyze the studies and will return a list with components \code{yi} containing the study-specific outcome measure estimates and \code{sei} containing the study-specific within-study standard error estimates. The default is \code{TRUE}.
#' @param ... optional arguments that are passed into the \code{\link[metafor]{rma.uni}} function for pooling. See documentation of \code{\link[metafor]{rma.uni}}.
#'
#' @return an object of class "rma.uni". See documentation of \code{\link[metafor]{rma.uni}}.
#' @references Bland M. (2015). Estimating mean and standard deviation from the sample size, three quartiles, minimum, and maximum. \emph{International Journal of Statistics in Medical Research}. \strong{4}(1):57-64.
#' @references Cai S., Zhou J., and Pan J. (2021). Estimating the sample mean and standard deviation from order statistics and sample size in meta-analysis. \emph{Statistical Methods in Medical Research}. \strong{30}(12):2701-2719.
#' @references Hozo S.P., Djulbegovic B., and Hozo I. (2005). Estimating the mean and variance from the median, range, and the size of a sample. \emph{BMC Medical Research Methodology}. \strong{5}(1):1-10.
#' @references Luo D., Wan X., Liu J., and Tong T. (2016). Optimally estimating the sample mean from the sample size, median, mid-range, and/or mid-quartile range. \emph{Statistical Methods in Medical Research}. \strong{27}(6):1785-805.
#' @references McGrath S., Zhao X., Steele R., Thombs B.D., Benedetti A., and the DEPRESsion Screening Data (DEPRESSD) Collaboration. (2020). Estimating the sample mean and standard deviation from commonly reported quantiles in meta-analysis. \emph{Statistical Methods in Medical Research}. \strong{29}(9):2520-2537.
#' @references McGrath S., Katzenschlager S., Zimmer A.J., Seitel A., Steele R., and Benedetti A. (2023). Standard error estimation in meta-analysis of studies reporting medians. \emph{Statistical Methods in Medical Research}. \strong{32}(2):373-388.
#' @references McGrath S., Zhao X., Ozturk O., Katzenschlager S., Steele R., and Benedetti A. (2024). metamedian: An R package for meta-analyzing studies reporting medians. \emph{Research Synthesis Methods}. \strong{15}(2):332-346.
#' @references Shi J., Luo D., Weng H., Zeng X.T., Lin L., Chu H., and Tong T. (2020a). Optimally estimating the sample standard deviation from the five-number summary. \emph{Research synthesis methods}. \strong{11}(5):641-654.
#' @references Shi J., Tong T., Wang Y., and Genton M.G. (2020b). Estimating the mean and variance from the five-number summary of a log-normal distribution. \emph{Statistics and Its Interface}. \strong{13}(4):519-531.
#' @references Wan X., Wang W., Liu J., and Tong T. (2014). Estimating the sample mean and standard deviation from the sample size, median, range and/or interquartile range. \emph{BMC Medical Research Methodology}. \strong{14}(1):1-13.
#' @references Yang X., Hutson A.D., and Wang D. (2022). A generalized BLUE approach for combining location and scale information in a meta-analysis. \emph{Journal of Applied Statistics}. \strong{49}(15):3846-3867.
#'
#' @examples
#' \donttest{
#'
#' ## Method for Unknown Non-Normal Distributions
#' metamean(data = dat.age, mean_method = "mln", se_method = "bootstrap", nboot = 50)
#'
#' ## Box-Cox method
#' metamean(data = dat.age, mean_method = "bc", se_method = "bootstrap", nboot = 50)
#'
#' ## Quantile Matching Estimation method
#' metamean(data = dat.age, mean_method = "qe", se_method = "bootstrap", nboot = 50)
#'
#' }
#'
#' @export

metamean <- function(data, mean_method = 'mln', se_method,
                     sd_method, nboot = 1e3, pool_studies = TRUE, ...) {

  df <- check_and_clean_df(df = data, method = mean_method)
  n_studies <- nrow(df)

  res <- check_mean_se_methods(mean_method = mean_method, se_method = se_method,
                               sd_method = sd_method, n = n_studies)
  mean_method <- res$mean_method
  se_method <- res$se_method
  sd_method <- res$sd_method

  one_group <- all(is.na(df[, c('min.g2', 'q1.g2', 'med.g2', 'q3.g2', 'max.g2', 'n.g2', 'mean.g2', 'sd.g2')]))
  mean.g1 <- rep(NA, times = n_studies); mean.g2 <- rep(NA, times = n_studies)
  se.g1 <- rep(NA, times = n_studies); se.g2 <- rep(NA, times = n_studies)

  for (i in 1:n_studies){
    # Group 1 Analyses
    scenario.g1 <- get.scenario(df[i, 'min.g1'], df[i, 'q1.g1'], df[i, 'med.g1'], df[i, 'q3.g1'], df[i, 'max.g1'],
                                df[i, 'mean.g1'], df[i, 'sd.g1'])
    if (scenario.g1 == 'S4'){
      mean.g1[i] <- df[i, 'mean.g1']
      se.g1[i] <- df[i, 'sd.g1'] / sqrt(df[i, 'n.g1'])
    } else {
      res <- get_mean_se(df = df[i, ], mean_method = mean_method[i], se_method = se_method[i],
                         sd_method = sd_method[i], scenario = scenario.g1, group = 1, nboot = nboot)
      mean.g1[i] <- res$est.mean
      se.g1[i] <- res$est.se

    }

    # Group 2 Analyses
    if (!one_group){
      scenario.g2 <- get.scenario(df[i, 'min.g2'], df[i, 'q1.g2'], df[i, 'med.g2'], df[i, 'q3.g2'], df[i, 'max.g2'],
                                  df[i, 'mean.g2'], df[i, 'sd.g2'])
      if (scenario.g2 == 'S4'){
        mean.g2[i] <- df[i, 'mean.g2']
        se.g2[i] <- df[i, 'sd.g2'] / sqrt(df[i, 'n.g2'])
      } else {
        res <- get_mean_se(df = df[i, ], mean_method = mean_method[i], se_method = se_method[i],
                           sd_method = sd_method[i], scenario = scenario.g2, group = 2, nboot = nboot)
        mean.g2[i] <- res$est.mean
        se.g2[i] <- res$est.se
      }
    }
  }
  if (one_group){
    yi <- mean.g1
    sei <- se.g1
  } else {
    yi <- mean.g1 - mean.g2
    sei <- sqrt(se.g1^2 + se.g2^2)
  }
  if (pool_studies){
    return(metafor::rma.uni(yi = yi, sei = sei, ...))
  } else {
    return(list(yi = yi, sei = sei))
  }
}

check_and_clean_df <- function(df, method){
  if (!is.data.frame(df)){
    stop("'data' must be of class data.frame")
  } else {
    if ("tbl_df" %in% class(df)) {
      df <- as.data.frame(df)
    }
  }
  if (length(method) == 1 && method == 'cd'){
    all_possible_colnames <- c('q1.g1', 'med.g1', 'q3.g1', 'n.g1', 'mean.g1', 'sd.g1', 'med.var.g1',
                               'med.ci.lb.g1', 'med.ci.ub.g1', 'alpha.1.g1', 'alpha.2.g1')
    if (any(c('min.g2', 'q1.g2', 'med.g2', 'q3.g2', 'max.g2', 'n.g2', 'mean.g2', 'sd.g2') %in% colnames(df))){
      warning('A data set with two groups was provided, but the CD method is only applicable for one-group studies. The CD method is only meta-analyzing the group 1 data.')
    }
  } else if (length(method) == 1 && method == 'wald') {
    all_possible_colnames <- c('med.g1', 'med.ci.lb.g1', 'med.ci.ub.g1', 'med.ci.level.g1',
                               'med.g2', 'med.ci.lb.g2', 'med.ci.ub.g2', 'med.ci.level.g2')
  } else {
    all_possible_colnames <- c('min.g1', 'q1.g1', 'med.g1', 'q3.g1', 'max.g1', 'n.g1', 'mean.g1', 'sd.g1',
                               'min.g2', 'q1.g2', 'med.g2', 'q3.g2', 'max.g2', 'n.g2', 'mean.g2', 'sd.g2')
  }

  # Filling missing columns
  missing_cols <- all_possible_colnames[which(!all_possible_colnames %in% colnames(df))]
  df[, missing_cols] <- NA

  # Checking class of summary data
  for (col in all_possible_colnames){
    if (!class(df[, col]) %in% c('numeric', 'integer')){
      if (class(df[, col]) %in% 'logical' | all(is.na(df[, col]))){
        next()
      }
      stop('Summary data must be of class numeric or integer. Column ', col, ' is not of one of these classes')
    }
  }

  # Removing any empty rows
  temp <- df[, all_possible_colnames]
  na.row.indicator <- apply(temp, 1, function(x) all(is.na(x)))
  if (any(na.row.indicator)) {
    if (all(na.row.indicator)){
      stop(paste0('All rows in the input data set have all summary data set to NA. This may be due to the column names of the input data set being set incorrectly.'))
    } else {
      stop(paste0('The following rows in the input data set have all summary data set to NA: ',
                  paste(which(na.row.indicator), collapse = ' ')))
    }
  }

  return(df)
}

check_mean_se_methods <- function(mean_method, se_method, sd_method, n){
  if (length(mean_method) == 1){
    mean_method <- rep(mean_method, times = n)
  } else {
    if (length(mean_method) != n){
      stop("The length of 'mean_method' must equal 1 or the number of rows in 'data'.")
    }
  }
  if (missing(se_method)){
    se_method <- rep(NA, times = n)
    for (i in 1:n){
      if (mean_method[i] %in% c('qe', 'bc', 'mln')){
        se_method[i] <- 'bootstrap'
      } else if (mean_method[i] == 'yang'){
        se_method[i] <- 'plugin'
      } else {
        se_method[i] <- 'naive'
      }
    }
  } else if (length(se_method) == 1){
    se_method <- rep(se_method, times = n)
  } else if (length(se_method) != n){
    stop("The length of 'se_method' must equal 1 or the number of rows in 'data'.")
  }

  if (missing(sd_method)){
    sd_method <- rep(NA, times = n)
    for (i in 1:n){
      if (se_method[i] == 'naive'){
        if (mean_method[i] %in% c('shi_lognormal', 'qe', 'bc', 'mln', 'yang')){
          sd_method[i] <- mean_method[i]
        } else if (mean_method[i] == 'luo'){
          sd_method[i] <- 'wan/shi_normal'
        } else if (mean_method[i] == 'hozo/wan/bland'){
          sd_method[i] <- 'wan'
        }
      }
    }
  } else if (length(sd_method) == 1){
    sd_method <- rep(sd_method, times = n)
  } else if (length(sd_method) != n){
    stop("The length of 'sd_method' must equal 1 or the number of rows in 'data'.")
  }

  # Checking that mean_method, se_method, and sd_method are set to valid options
  all_mean_methods <- c('qe', 'bc', 'mln', 'hozo/wan/bland', 'luo', 'yang', 'shi_lognormal')
  if (!all(mean_method %in% all_mean_methods)){
    if (any(mean_method == 'wan')){
      stop("The 'wan' option for mean_method has been renamed to 'hozo/wan/bland'. Please use the 'hozo/wan/bland' option instead.")
    } else if (any(mean_method == 'shi_normal')){
      stop("The 'shi_normal' option for mean_method has been removed because it was redundant with the 'luo' option. Please use the 'luo' option instead.")
    }
    stop("mean_method must be set to 'qe', 'bc', 'mln', 'hozo/wan/bland', 'luo', 'yang', or 'shi_lognormal'")
  }
  all_se_methods <- c('bootstrap', 'plugin', 'naive')
  if (!all(se_method %in% all_se_methods)){
    stop("se_method must be set to 'bootstrap', 'plugin', or 'naive'")
  }
  all_sd_methods <- c('qe', 'bc', 'mln', 'wan', 'yang', 'wan/shi_normal', 'shi_lognormal')
  if (!all(is.na(sd_method) | sd_method %in% all_sd_methods)){
    if (any(!is.na(sd_method) & sd_method == 'shi_normal')){
      stop("The 'shi_normal' option for sd_method has been renamed to 'wan/shi_normal'. Please use the 'wan/shi_normal' option instead.")
    }
    stop("sd_method must be set to 'qe', 'bc', 'mln', 'wan', 'yang', 'wan/shi_normal', or 'shi_lognormal'")
  }

  # Checking consistency between mean_method, sd_method, and se_method
  for (i in 1:n){
    if (se_method[i] == 'bootstrap'){
      if (!mean_method[i] %in% c('qe', 'bc', 'mln')){
        stop("se_method can only be set to 'bootstrap' when mean_method is set to 'qe', 'bc', or 'mln'")
      }
    } else if (se_method[i] == 'plugin'){
      if (mean_method[i] != 'yang'){
        stop("se_method can only be set to 'plugin' when mean_method is set to 'yang'")
      }
    } else if (se_method[i] == 'naive'){
      if (is.na(sd_method[i])){
        stop("sd_method must be specified when se_method is set to 'naive'")
      }
    }
  }

  return(list(mean_method = mean_method, se_method = se_method, sd_method = sd_method))
}

get_mean_se <- function(df, mean_method, se_method, sd_method, scenario, group, nboot){
  min.val <- df[, paste0('min.g', group)]
  q1.val <- df[, paste0('q1.g', group)]
  med.val <- df[, paste0('med.g', group)]
  q3.val <- df[, paste0('q3.g', group)]
  max.val <- df[, paste0('max.g', group)]
  n <- df[, paste0('n.g', group)]

  if (mean_method %in% c('luo', 'yang', 'shi_lognormal') |
      (!is.na(sd_method) & sd_method %in% c('wan', 'yang', 'wan/shi_normal', 'shi_lognormal'))){
    if (scenario == 'S1'){
      quants <- c(min.val, med.val, max.val)
    } else if (scenario == 'S2'){
      quants <- c(q1.val, med.val, q3.val)
    } else if (scenario == 'S3'){
      quants <- c(min.val, q1.val, med.val, q3.val, max.val)
    }
    if (min(quants) <= 0){
      if (mean_method == 'shi_lognormal'){
        stop('The mean method ', mean_method, ' can only be used for positive data')
      }
      if (sd_method == 'shi_lognormal'){
        stop('The standard deviation method ', sd_method, ' can only be used for positive data')
      }
    }
  }

  if (mean_method == 'mln' | (!is.na(sd_method) & sd_method == 'mln')){
    if (check_negative(min.val = min.val, q1.val = q1.val, med.val = med.val, q3.val = q3.val, max.val = max.val, scenario = scenario)){
      stop('The mln method can only be used for positive data')
    }
    fit_mln <- estmeansd::mln.mean.sd(min.val = min.val, q1.val = q1.val, med.val = med.val, q3.val = q3.val, max.val = max.val, n = n)
    if (mean_method == 'mln'){
      est.mean <- fit_mln$est.mean
    }
  }
  if (mean_method == 'bc' | (!is.na(sd_method) & sd_method == 'bc')){
    if (check_negative(min.val = min.val, q1.val = q1.val, med.val = med.val, q3.val = q3.val, max.val = max.val, scenario = scenario)){
      stop('The bc method can only be used for positive data')
    }
    fit_bc <- estmeansd::bc.mean.sd(min.val = min.val, q1.val = q1.val, med.val = med.val, q3.val = q3.val, max.val = max.val, n = n)
    if (mean_method == 'bc'){
      est.mean <- fit_bc$est.mean
    }
  }
  if (mean_method == 'qe' | (!is.na(sd_method) & sd_method == 'qe')){
    fit_qe <- estmeansd::qe.mean.sd(min.val = min.val, q1.val = q1.val, med.val = med.val, q3.val = q3.val, max.val = max.val, n = n)
    if (mean_method == 'qe'){
      est.mean <- fit_qe$est.mean
    }
  }
  if (mean_method == 'yang' | (!is.na(sd_method) & sd_method == 'yang')){
    fit_yang <- metaBLUE::BLUE_s(X = quants, n = n, type = scenario)
    if (mean_method == 'yang'){
      est.mean <- fit_yang$muhat
    }
  }

  if (mean_method == 'luo'){
    est.mean <- metaBLUE::Luo.mean(X = quants, n = n, type = scenario)$muhat
  } else if (mean_method == 'hozo/wan/bland'){
    if (scenario == 'S1'){
      est.mean <- (min.val + 2 * med.val + max.val) / 4
    } else if (scenario == 'S2'){
      est.mean <- (q1.val + med.val + q3.val) / 3
    } else if (scenario == 'S3'){
      est.mean <- (min.val + 2 * q1.val + 2 * med.val + 2 * q3.val + max.val) / 8
    }
  } else if (mean_method == 'shi_lognormal'){
    muhat <- metaBLUE::Luo.mean(X = log(quants), n = n, type = scenario)$muhat
    if (scenario == 'S1'){
      xi <- 2 * stats::qnorm((n - 0.375) / (n + 0.25))
      sigma2hat1 <- ((log(max.val) - log(min.val)) / xi)^2 / (1.01 + 0.25 / (log(n))^2)
      sigma4hat1 <- ((log(max.val) - log(min.val)) / xi)^4 / (1 + 2.23 * (log(n))^(-2))
      est.mean <- exp(muhat + sigma2hat1 / 2) / (1 + 0.565 * sigma2hat1 / n + 0.37 * sigma4hat1 / n)
    } else if (scenario == 'S2'){
      eta <- 2 * stats::qnorm((0.75 * n - 0.125) / (n + 0.25))
      sigma2hat2 <- ((log(q3.val) - log(q1.val)) / eta)^2 / (1 + 1.58 / n)
      sigma4hat2 <- ((log(q3.val) - log(q1.val)) / eta)^4 / (1 + 19.2 / n^1.2)
      est.mean <- exp(muhat + sigma2hat2 / 2) / (1 + 0.57 * sigma2hat2 / n + 0.75 * sigma4hat2 / n)
    } else if (scenario == 'S3'){
      xi <- 2 * stats::qnorm((n - 0.375) / (n + 0.25))
      eta <- 2 * stats::qnorm((0.75 * n - 0.125) / (n + 0.25))
      w3 <- 1 / (1 + 0.07 * n^0.6)
      sigma2hat3 <- (w3 * ((log(max.val) - log(min.val)) / xi) + (1 - w3) * ((log(q3.val) - log(q1.val)) / eta))^2 /
        (1 + 0.28 / (log(n))^2)
      sigma4hat3 <- (w3 * ((log(max.val) - log(min.val)) / xi) + (1 - w3) * ((log(q3.val) - log(q1.val)) / eta))^4 /
        (1 + 3.93 / n)
      est.mean <- exp(muhat + sigma2hat3 / 2) / (1 + 0.405 * sigma2hat3 / n + 0.315 * sigma4hat3 / n)
    }
  }

  if (se_method == 'bootstrap'){
    if (mean_method == 'mln'){
      est.se <- estmeansd::get_SE(fit_mln, nboot = nboot)$est.se
    } else if (mean_method == 'bc'){
      est.se <- estmeansd::get_SE(fit_bc, nboot = nboot)$est.se
    } else if (mean_method == 'qe'){
      est.se <- estmeansd::get_SE(fit_qe, nboot = nboot)$est.se
    }
  } else if (se_method == 'plugin'){
    est.se <- sqrt(fit_yang$Var_mu)
  } else if (se_method == 'naive'){
    if (sd_method == 'mln'){
      est.se <- fit_mln$est.sd / sqrt(n)
    } else if (sd_method == 'bc'){
      est.se <- fit_bc$est.sd / sqrt(n)
    } else if (sd_method == 'qe'){
      est.se <- fit_qe$est.sd / sqrt(n)
    } else if (sd_method == 'yang'){
      est.se <- fit_yang$sigmahat / sqrt(n)
    }
    else if (sd_method == 'wan/shi_normal' & scenario == 'S3'){
      theta1 <- (2 + 0.14 * n^0.6) * stats::qnorm((n - 0.375) / (n + 0.25))
      theta2 <- (2 + 2 / (0.07 * n^0.6)) * stats::qnorm((0.75 * n - 0.125) / (n + 0.25))
      est.se <- ((max.val - min.val) / theta1 + (q3.val - q1.val) / theta2) / sqrt(n)
    } else if (sd_method %in% c('wan', 'wan/shi_normal')){
      est.se <- metaBLUE::Wan.std(X = quants, n = n, type = scenario)$sigmahat / sqrt(n)
    } else if (sd_method == 'shi_lognormal'){
      muhat <- metaBLUE::Luo.mean(X = log(quants), n = n, type = scenario)$muhat
      if (scenario == 'S1'){
        xi <- 2 * stats::qnorm((n - 0.375) / (n + 0.25))
        sigma2hat1 <- ((log(max.val) - log(min.val)) / xi)^2 / (1.01 + 0.25 / (log(n))^2)
        sigma4hat1 <- ((log(max.val) - log(min.val)) / xi)^4 / (1 + 2.23 * (log(n))^(-2))
        sigma2hat <- exp(2 * muhat + 2 * sigma2hat1) / (1 + 2.26 * sigma2hat1 / n + 5.92 * sigma4hat1 / n) -
          exp(2 * muhat + sigma2hat1) / (1 + 2.26 * sigma2hat1 / n + 1.48 * sigma4hat1 / n)
      } else if (scenario == 'S2'){
        eta <- 2 * stats::qnorm((0.75 * n - 0.125) / (n + 0.25))
        sigma2hat2 <- ((log(q3.val) - log(q1.val)) / eta)^2 / (1 + 1.58 / n)
        sigma4hat2 <- ((log(q3.val) - log(q1.val)) / eta)^4 / (1 + 19.2 / n^1.2)
        sigma2hat <- exp(2 * muhat + 2 * sigma2hat2) / (1 + 2.28 * sigma2hat2 / n + 12 * sigma4hat2 / n) -
          exp(2 * muhat + sigma2hat2) / (1 + 2.28 * sigma2hat2 / n + 3 * sigma4hat2 / n)
      } else if (scenario == 'S3'){
        xi <- 2 * stats::qnorm((n - 0.375) / (n + 0.25))
        eta <- 2 * stats::qnorm((0.75 * n - 0.125) / (n + 0.25))
        w3 <- 1 / (1 + 0.07 * n^0.6)
        sigma2hat3 <- (w3 * ((log(max.val) - log(min.val)) / xi) + (1 - w3) * ((log(q3.val) - log(q1.val)) / eta))^2 /
          (1 + 0.28 / (log(n))^2)
        sigma4hat3 <- (w3 * ((log(max.val) - log(min.val)) / xi) + (1 - w3) * ((log(q3.val) - log(q1.val)) / eta))^4 /
          (1 + 3.93 / n)
        sigma2hat <- exp(2 * muhat + 2 * sigma2hat3) / (1 + 1.62 * sigma2hat3 / n + 5.04 * sigma4hat3 / n) -
          exp(2 * muhat + sigma2hat3) / (1 + 1.62 * sigma2hat3 / n + 1.26 * sigma4hat3 / n)
      }
      est.se <- sqrt(sigma2hat / n)
    }
  }

  return(list(est.mean = est.mean, est.se = est.se))
}

check_negative <- function(min.val = NA, q1.val = NA, med.val = NA,
                           q3.val = NA, max.val = NA, scenario){
  if (scenario %in% c('S1', 'S3')){
    return(min.val <= 0)
  } else if (scenario == 'S2'){
    return(q1.val <= 0)
  }
}
