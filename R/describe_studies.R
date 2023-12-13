#' Descriptive statistics for meta-analyzing studies reporting medians
#'
#' This function performs some descriptive analyses. Specifically, this function describes: (i) the number of studies reporting various summary statistics, (ii) the Bowley skewness (Bowley, 1901) in the primary studies, and (iii) the results of a skewness test (Shi et al., 2023) applied to the summary statistics reported in the primary studies.
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
#' \code{sd.g1} \tab sample standard deviation. \cr
#' \code{med.var.g1} \tab sampling variance of the median (only applicable when \code{median_method="cd"}). \cr
#' \code{med.ci.lb.g1} \tab lower confidence interval bound around the median (only applicable when \code{median_method="cd"}). \cr
#' \code{med.ci.ub.g1} \tab upper confidence interval bound around the median (only applicable when \code{median_method="cd"}). \cr
#' \code{alpha.1.g1} \tab \eqn{\alpha_1} values from Ozturk and Balakrishnan (2020) (only applicable when \code{median_method="cd"}). \cr
#' \code{alpha.2.g1} \tab \eqn{\alpha_2} values from Ozturk and Balakrishnan (2020) (only applicable when \code{median_method="cd"}). \cr}
#' For two group studies, this data frame can also contain the following columns for the summary data of the second group: \code{min.g2}, \code{q1.g2}, \code{med.g2}, \code{q3.g2}, \code{max.g2}, \code{n.g2}, \code{mean.g2}, and \code{sd.g2}.
#' @param method character string specifying the sets of summary statistics to consider. If this argument is set to \code{"cd"}, the summary statistics for the CD method (see C1/C2, C3, C4, C5 in \code{\link{cd}}) are considered. Otherwise, the S1, S2, S3, and S4 summary statistics described in \code{\link{qe}} are considered.
#' @param group_labels vector of character strings specifying the names corresponding to groups 1 and 2, respectively. This argument is only applicable when the meta-analysis consists of two-group studies. By default, this argument is set to \code{c('Group 1', 'Group 2')}.
#' @return an object of class \code{"describe_studies"}. The object is a list with the following components:
#' \item{description}{data frame containing the results of the descriptive analyses.}
#' \item{bowley_g1}{vector containing the study-specific Bowley skewness values in group 1.}
#' \item{bowley_g2}{vector containing the study-specific Bowley skewness values in group 2.}
#' \item{skew_test_g1}{data frame containing the results of the skewness test of Shi et al. (2023) based on the group 1 data. The data frame contains the test statistic values, critical values at the 0.05 level, and indicators of statistical significance at the 0.05 level.}
#' \item{skew_test_g2}{data frame containing the results of the skewness test of Shi et al. (2023) based on the group 2 data. The data frame contains the test statistic values, critical values at the 0.05 level, and indicators of statistical significance at the 0.05 level.}
#' The results are printed with the \code{\link{print.describe_studies}} function.
#'
#' @references Bowley, A.L. (1901). Elements of Statistics. London: P.S. King & Son.
#' @references McGrath S., Zhao X., Ozturk O., Katzenschlager S., Steele R., and Benedetti A. (in press). metamedian: An R package for meta-analyzing studies reporting medians. \emph{Research Synthesis Methods}.
#' @references Shi J., Luo D., Wan X., Yue L., Liu J., Bian Z., Tong T. (2023). Detecting the skewness of data from the five-number summary and its application in meta-analysis. \emph{Statistical Methods in Medical Research}. \strong{0}(0).
#'
#' @examples
#' describe_studies(data = dat.age, group_labels = c("Nonsurvivors", "Survivors"))
#'
#' @export
describe_studies <- function(data, method = 'qe',
                             group_labels = c('Group 1', 'Group 2')){
  df <- check_and_clean_df(df = data, method = method)
  n_studies <- nrow(df)

  if (length(method) != 1){
    stop("Argument 'method' must be of length 1")
  }
  if (method == 'cd'){
    one_group <- TRUE
  } else {
    g2_names <- c('min.g2', 'q1.g2', 'med.g2', 'q3.g2', 'max.g2', 'n.g2', 'mean.g2', 'sd.g2')
    one_group <- all(is.na(df[, g2_names]))
  }
  if (!one_group & length(group_labels) != 2){
    stop("Argument 'group_labels' must be of length 2")
  }

  scenario_g1 <- scenario_g2 <- rep(NA, times = n_studies)
  for (i in 1:n_studies){
    if (method != 'cd'){
      scenario_g1[i] <- tryCatch(get.scenario(df[i, 'min.g1'], df[i, 'q1.g1'], df[i, 'med.g1'], df[i, 'q3.g1'],
                                              df[i, 'max.g1'], df[i, 'mean.g1'], df[i, 'sd.g1']),
                                 error = function(e) NA)
    } else {
      scenario_g1[i] <- tryCatch(get.scenario.cd(df[i, 'q1.g1'], df[i, 'med.g1'], df[i, 'q3.g1'],
                                                 df[i, 'mean.g1'], df[i, 'sd.g1'], df[i, 'med.var.g1'],
                                                 df[i, 'med.ci.lb.g1'], df[i, 'med.ci.ub.g1'], df[i, 'alpha.1.g1'],
                                                 df[i, 'alpha.2.g1']),
                                 error = function(e) NA)
    }

    if (!one_group){
      scenario_g2[i] <- tryCatch(get.scenario(df[i, 'min.g2'], df[i, 'q1.g2'], df[i, 'med.g2'], df[i, 'q3.g2'],
                                              df[i, 'max.g2'], df[i, 'mean.g2'], df[i, 'sd.g2']),
                                 error = function (e) NA)
    }
  }
  df_g1_for_bowley <- df[!is.na(scenario_g1) & (scenario_g1 %in% c('S2', 'S3') | scenario_g1 == 'C5'), ]
  bowley_g1 <- (df_g1_for_bowley$q1.g1 - 2 * df_g1_for_bowley$med.g1 + df_g1_for_bowley$q3.g1) /
    (df_g1_for_bowley$q3.g1 - df_g1_for_bowley$q1.g1)
  skew_test_g1 <- perform_skew_test(df = df, group = 1, scenario = scenario_g1, n_studies = n_studies)

  if (!one_group){
    df_g2_for_bowley <- df[!is.na(scenario_g2) & (scenario_g2 %in% c('S2', 'S3') | scenario_g2 == 'C5'), ]
    bowley_g2 <- (df_g2_for_bowley$q1.g2 - 2 * df_g2_for_bowley$med.g2 + df_g2_for_bowley$q3.g2) /
      (df_g2_for_bowley$q3.g2 - df_g2_for_bowley$q1.g2)
    skew_test_g2 <- perform_skew_test(df = df, group = 1, scenario = scenario_g1, n_studies = n_studies)
  } else {
    bowley_g2 <- skew_test_g2 <-  NULL
  }

  if (one_group){
    ncols <- 1
  } else {
    ncols <- 2
  }

  # Bowley skewness descriptions
  description_bowley <- matrix(NA, nrow = 7, ncol = ncols)
  rownames(description_bowley) <- c('Bowley skewness', '  Minimum:', '  First quartile:',
                                    '  Median:', '  Mean:', '  Third quartile:', '  Maximum:')
  description_bowley['Bowley skewness', 1] <- ''
  description_bowley['  Minimum:', 1] <- myround(min(bowley_g1))
  description_bowley['  First quartile:', 1] <- myround(stats::quantile(bowley_g1, probs = 0.25))
  description_bowley['  Median:', 1] <- myround(stats::median(bowley_g1))
  description_bowley['  Mean:', 1] <- myround(mean(bowley_g1))
  description_bowley['  Third quartile:', 1] <- myround(stats::quantile(bowley_g1, probs = 0.75))
  description_bowley['  Maximum:', 1] <- myround(max(bowley_g1))
  if (!one_group){
    description_bowley['Bowley skewness', 2] <- ''
    description_bowley['  Minimum:', 2] <- myround(min(bowley_g2))
    description_bowley['  First quartile:', 2] <- myround(stats::quantile(bowley_g2, probs = 0.25))
    description_bowley['  Median:', 2] <- myround(stats::median(bowley_g2))
    description_bowley['  Mean:', 2] <- myround(mean(bowley_g2))
    description_bowley['  Third quartile:', 2] <- myround(stats::quantile(bowley_g2, probs = 0.75))
    description_bowley['  Maximum:', 2] <- myround(max(bowley_g2))
  }

  # Number of studies descriptions
  if (method == 'cd'){
    description <- matrix(NA, nrow = 7, ncol = ncols)
    rownames(description) <- c('N. studies:',
                               'N. studies reporting the median:',
                               '  N. studies reporting C1/C2 (med, CI bounds, coverage):',
                               '  N. studies reporting C3 (med, sampling variance):',
                               '  N. studies reporting C5 (q1, med, q3, n):',
                               'N. studies reporting the mean:',
                               '  N. studies reporting C4 (mean, sd, n):')
    description['N. studies:', 1] <- n_studies
    description['N. studies reporting the median:', 1] <- sum(!is.na(df$med.g1))
    description['  N. studies reporting C1/C2 (med, CI bounds, coverage):', 1] <- sum(scenario_g1 == 'C1', na.rm = TRUE)
    description['  N. studies reporting C3 (med, sampling variance):', 1] <- sum(scenario_g1 == 'C3', na.rm = TRUE)
    description['  N. studies reporting C5 (q1, med, q3, n):', 1] <- sum(scenario_g1 == 'C5', na.rm = TRUE)
    description['N. studies reporting the mean:', 1] <- sum(!is.na(df$mean.g1))
    description['  N. studies reporting C4 (mean, sd, n):', 1] <- sum(scenario_g1 == 'C4', na.rm = TRUE)

  } else {
    description <- matrix(NA, nrow = 7, ncol = ncols)
    rownames(description) <- c('N. studies:',
                               'N. studies reporting the median:',
                               '  N. studies reporting S1 (min, med, max, n):',
                               '  N. studies reporting S2 (q1, med, q3, n):',
                               '  N. studies reporting S3 (min, q1, med, q3, max, n):',
                               'N. studies reporting the mean:',
                               '  N. studies reporting the mean, sd, and n:')
    description['N. studies:', 1] <- n_studies
    description['N. studies reporting the median:', 1] <- sum(!is.na(df$med.g1))
    description['  N. studies reporting S1 (min, med, max, n):', 1] <- sum(scenario_g1 == 'S1', na.rm = TRUE)
    description['  N. studies reporting S2 (q1, med, q3, n):', 1] <- sum(scenario_g1 == 'S2', na.rm = TRUE)
    description['  N. studies reporting S3 (min, q1, med, q3, max, n):', 1] <- sum(scenario_g1 == 'S3', na.rm = TRUE)
    description['N. studies reporting the mean:', 1] <- sum(!is.na(df$mean.g1))
    description['  N. studies reporting the mean, sd, and n:', 1] <- sum(scenario_g1 == 'S4', na.rm = TRUE)
    if (!one_group){
      description['N. studies:', 2] <- n_studies
      description['N. studies reporting the median:', 2] <- sum(!is.na(df$med.g2))
      description['  N. studies reporting S1 (min, med, max, n):', 2] <- sum(scenario_g2 == 'S1', na.rm = TRUE)
      description['  N. studies reporting S2 (q1, med, q3, n):', 2] <- sum(scenario_g2 == 'S2', na.rm = TRUE)
      description['  N. studies reporting S3 (min, q1, med, q3, max, n):', 2] <- sum(scenario_g2 == 'S3', na.rm = TRUE)
      description['N. studies reporting the mean:', 2] <- sum(!is.na(df$mean.g2))
      description['  N. studies reporting the mean, sd, and n:', 2] <- sum(scenario_g2 == 'S4', na.rm = TRUE)
    }
  }

  description <- rbind(description, description_bowley)
  if (one_group){
    colnames(description) <- ' '
  } else {
    colnames(description) <- group_labels
  }

  output <- list(description = data.frame(description, check.names = FALSE),
                 bowley_g1 = bowley_g1, bowley_g2 = bowley_g2,
                 skew_test_g1 = skew_test_g1, skew_test_g2 = skew_test_g2)
  class(output) <- 'describe_studies'

  return(output)
}

perform_skew_test <- function(df, group, scenario, n_studies){
  res <- data.frame(
    test_stat = rep(NA, n_studies),
    critical_value = rep(NA, n_studies),
    reject = rep(NA, n_studies)
  )
  for (i in 1:n_studies){
    if (!is.na(scenario[i])){
      if (group == 1){
        n <- df[i, 'n.g1']
        min_val <- df[i, 'min.g1']
        q1_val <- df[i, 'q1.g1']
        med_val <- df[i, 'med.g1']
        q3_val <- df[i, 'q3.g1']
        max_val <- df[i, 'max.g1']
      } else if (group == 2){
        n <- df[i, 'n.g2']
        min_val <- df[i, 'min.g2']
        q1_val <- df[i, 'q1.g2']
        med_val <- df[i, 'med.g2']
        q3_val <- df[i, 'q3.g2']
        max_val <- df[i, 'max.g2']
      }

      if (scenario[i] == 'S1'){
        res[i, 'test_stat'] <- (min_val + max_val - 2 * med_val) / (max_val - min_val)
        res[i, 'critical_value'] <- 1 / log(n + 9) + 2.5 / (n + 1)
        res[i, 'reject'] <- abs(res[i, 'test_stat']) > res[i, 'critical_value']
      } else if (scenario[i] == 'S2'){
        res[i, 'test_stat'] <- (q1_val + q3_val - 2 * med_val) / (q3_val - q1_val)
        res[i, 'critical_value'] <- 2.65 / sqrt(n) - 6 / n^2
        res[i, 'reject'] <- abs(res[i, 'test_stat']) > res[i, 'critical_value']
      } else if (scenario[i] == 'S3'){
        T1 <- (min_val + max_val - 2 * med_val) / (max_val - min_val)
        T2 <- (q1_val + q3_val - 2 * med_val) / (q3_val - q1_val)
        res[i, 'test_stat'] <- max((2.65 * log(0.6 * n) / sqrt(n)) * abs(T1), T2)
        res[i, 'critical_value'] <- 3 / sqrt(n) - 40 / n^3
        res[i, 'reject'] <- abs(res[i, 'test_stat']) > res[i, 'critical_value']
      }
    }
  }
  return(res)
}

#' Print method for "describe_studies" objects
#'
#' Print method for objects of class "describe_studies"
#'
#' @param x object of class "describe_studies".
#' @param ... other arguments.
#' @return No value is returned.
#'
#' @seealso \code{\link{describe_studies}}
#'
#' @export
print.describe_studies <- function(x, ...){
  cat('DESCRIPTION OF PRIMARY STUDIES\n')
  print(x$description)
}

myround <- function(x){
  return(format(round(unname(x), 4), nsmall = 4))
}
