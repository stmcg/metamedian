#' Meta-Analysis of the (difference of) means
#'
#' The function meta-analyzes one-group or two-group studies where each study reports one of the following summary measures: \itemize{
#' \item S1: median, minimum and maximum values, and sample size
#' \item S2: median, first and third quartiles, and sample size
#' \item S3: median, minimum and maximum values, first and third quartiles, and sample size
#' \item S4: mean, standard deivation, and sample size.
#'  }
#' This function estimates the study-specific means and their standard errors from the S1, S2, S3, or S4 summary data. Then, this function estimates the pooled mean (for one-group studies) or the pooled difference of means (for two-group studies) based on the standard inverse variance method via the \code{\link[metafor]{rma.uni}} function. The convention used for calculating differences of means in two-group studies is: mean in group 1 minus mean in group 2.
#'
#' When studies report S1, S2, or S3 summary data, the approaches described by McGrath et al. (2020) and Cai et al. (2021) can be applied to estimate the study-specific means. Their standard errors are estimated by parametric bootstrap (McGrath et al. 2022).
#'
#' @param df data frame containing the study-specific summary data. For one-group studies, this data frame can contain the following columns:
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
#' \code{qe} \tab Quantile Matching Estimation (McGrath et al. 20220). \cr
#' \code{bc} \tab Box-Cox (McGrath et al. 2020). \cr
#' \code{mln} \tab Method for Unknown Non-Normal Distributions (Cai et al. 2021). This is the default option. \cr}
#' @param nboot integer specifying the number of bootstrap samples to use when estimating the study-specific standard errors. The default is \code{1000}.
#' @param ... optional arguments that are passed into the \code{\link[metafor]{rma.uni}} function for pooling. See documentation of \code{\link[metafor]{rma.uni}}.
#'
#' @return an object of class "rma.uni". See documentation of \code{\link[metafor]{rma.uni}}.
#' @references McGrath S., Zhao X., Steele R., Thombs B.D., Benedetti A., and the DEPRESsion Screening Data (DEPRESSD) Collaboration. (2020). Estimating the sample mean and standard deviation from commonly reported quantiles in meta-analysis. \emph{Statistical Methods in Medical Research}. \strong{29}(9):2520-2537.
#' @references Cai S., Zhou J., and Pan J. (2021). Estimating the sample mean and standard deviation from order statistics and sample size in meta-analysis. \emph{Statistical Methods in Medical Research}. \strong{30}(12):2701-2719.
#' @references McGrath S., Katzenschlager S., Zimmer A.J., Seitel A., Steele R., Benedetti A. (2022). Standard error estimation in meta-analysis of studies reporting medians. \emph{arXiv e-prints}.
#' @export

metamean <- function(df, mean_method = 'mln', nboot = 1e3, ...) {
  df <- check_and_clean_df(df = df, method = mean_method)
  if (!mean_method %in% c('qe', 'bc', 'mln')){
    stop("mean_method must be set to 'qe', 'bc', or 'mln'")
  }
  one_group <- all(is.na(df[, c('min.g2', 'q1.g2', 'med.g2', 'q3.g2', 'max.g2', 'n.g2', 'mean.g2', 'sd.g2')]))

  n_studies <- nrow(df)
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
      if (mean_method == 'mln'){
        res <- estmeansd::mln.mean.sd(min.val = df[i, 'min.g1'], q1.val = df[i, 'q1.g1'], med.val = df[i, 'med.g1'],
                                      q3.val = df[i, 'q3.g1'], max.val = df[i, 'max.g1'], n = df[i, 'n.g1'])
      } else if (mean_method == 'bc'){
        res <- estmeansd::bc.mean.sd(min.val = df[i, 'min.g1'], q1.val = df[i, 'q1.g1'], med.val = df[i, 'med.g1'],
                                     q3.val = df[i, 'q3.g1'], max.val = df[i, 'max.g1'], n = df[i, 'n.g1'])
      } else if (mean_method == 'qe'){
        res <- estmeansd::qe.mean.sd(min.val = df[i, 'min.g1'], q1.val = df[i, 'q1.g1'], med.val = df[i, 'med.g1'],
                                     q3.val = df[i, 'q3.g1'], max.val = df[i, 'max.g1'], n = df[i, 'n.g1'])
      }
      mean.g1[i] <- res$est.mean
      se.g1[i] <- estmeansd::get_SE(res, nboot = nboot)$est.se
    }

    # Group 2 Analyses
    if (!one_group){
      scenario.g2 <- get.scenario(df[i, 'min.g2'], df[i, 'q1.g2'], df[i, 'med.g2'], df[i, 'q3.g2'], df[i, 'max.g2'],
                                  df[i, 'mean.g2'], df[i, 'sd.g2'])
      if (scenario.g2 == 'S4'){
        mean.g2[i] <- df[i, 'mean.g2']
        se.g2[i] <- df[i, 'sd.g2'] / sqrt(df[i, 'n.g2'])
      } else {
        if (mean_method == 'mln'){
          res <- estmeansd::mln.mean.sd(min.val = df[i, 'min.g2'], q1.val = df[i, 'q1.g2'], med.val = df[i, 'med.g2'],
                                        q3.val = df[i, 'q3.g2'], max.val = df[i, 'max.g2'], n = df[i, 'n.g2'])
        } else if (mean_method == 'bc'){
          res <- estmeansd::bc.mean.sd(min.val = df[i, 'min.g2'], q1.val = df[i, 'q1.g2'], med.val = df[i, 'med.g2'],
                                       q3.val = df[i, 'q3.g2'], max.val = df[i, 'max.g2'], n = df[i, 'n.g2'])
        } else if (mean_method == 'qe'){
          res <- estmeansd::qe.mean.sd(min.val = df[i, 'min.g2'], q1.val = df[i, 'q1.g2'], med.val = df[i, 'med.g2'],
                                       q3.val = df[i, 'q3.g2'], max.val = df[i, 'max.g2'], n = df[i, 'n.g2'])
        }
        mean.g2[i] <- res$est.mean
        se.g2[i] <- estmeansd::get_SE(res, nboot = nboot)$est.se
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

  return(metafor::rma.uni(yi = yi, sei = sei, ...))
}

check_and_clean_df <- function(df, method){
  if (method != 'ob'){
    all_possible_colnames <- c('min.g1', 'q1.g1', 'med.g1', 'q3.g1', 'max.g1', 'n.g1', 'mean.g1', 'sd.g1',
                               'min.g2', 'q1.g2', 'med.g2', 'q3.g2', 'max.g2', 'n.g2', 'mean.g2', 'sd.g2')
  } else {
    all_possible_colnames <- c('q1.g1', 'med.g1', 'q3.g1', 'n.g1', 'mean.g1', 'sd.g1', 'med.var.g1',
                               'med.ci.lb.g1', 'med.ci.ub.g1', 'alpha.1.g1', 'alpha.2.g1')
  }



  # Filling missing columns
  missing_cols <- all_possible_colnames[which(!all_possible_colnames %in% colnames(df))]
  df[, missing_cols] <- NA

  # Checking class of summary data
  for (col in all_possible_colnames){
    if (!class(df[, col]) %in% c('numeric', 'integer')){
      if (class(df[, col]) == 'logical' | all(is.na(df[, col]))){
        next()
      }
      stop('Summary data must be of class numeric or integer. Column ', col, 'is not of one of these classes')
    }
  }

  # Removing any empty rows
  na.row.indicator <- apply(df, 1, function(x) all(is.na(x)))
  if (any(na.row.indicator)) {
    df <- df[!na.row.indicator, ]
  }

  return(df)
}
