#' Meta-Analysis of the (difference of) medians
#'
#' This function is a wrapper function for the \code{\link{qe}}, \code{\link{cd}}, and \code{\link{pool.med}} functions. The function implements the methods of McGrath et al. (2019), McGrath et al. (2020), and Ozturk and Balakrishnan (2020) to estimate the pooled (difference of) medians in a meta-analysis. Specifically, the function implements the (weighted) median of medians method, the Ozturk and Balakrishnan (2020) method, and the quantile matching estimation method to meta-analyze one-group studies; the function implements the (weighted) median of the difference of medians method and quantile matching estimation method to meta-analyze two-group studies.
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
#' @param median_method character string specifying the approach used to estimate the study-specific means and their standard errors. The options are
#' \tabular{ll}{
#' \code{"mm"} \tab Median of Medians (McGrath et al. 2019) for one-group studies and Median of the Difference of Medians (McGrath et al. 2020) for two group studies. \cr
#' \code{"wm"} \tab Weighted Median of Medians (McGrath et al. 2019) for one-group studies and Weighted Median of the Difference of Medians (McGrath et al. 2020) for two group studies. \cr
#' \code{"qe"} \tab Quantile Matching Estimation (McGrath et al. 2020). This approach is applicable for one-group studies or two-group studies. This is the default option. \cr
#' \code{"cd"} \tab Confidence Distribution (Ozturk and Balakrishnan 2020). This approach is applicable for one-group studies. \cr}
#' @param single.family (only applicable when \code{median_method} is set to \code{"qe"}) logical scalar indicating that for two-group studies, the parametric family of distributions is assumed to be the same across both groups. The default is \code{FALSE}. See 'Details' of \code{\link{qe.study.level}}.
#' @param loc.shift (only applicable when \code{median_method} is set to \code{"qe"}) logical scalar indicating that for two-group studies, distributions are assumed to only differ by a location shift. The default is \code{FALSE}. See 'Details' of \code{\link{qe.study.level}}.
#' @param norm.approx (only applicable when \code{median_method} is set to \code{"mm"} or \code{"wm"}) logical scalar indicating whether normality approximation of the binomial should be used to construct an approximate confidence interval. The default is \code{TRUE}.
#' @param coverage.prob (only applicable when \code{median_method} is set to \code{"mm"}, \code{"wm"}, or \code{"cd"}) numeric scalar indicating the desired coverage probability for the pooled (difference of medians) estimate. The default is \code{0.95}.
#' @param method_cd (only applicable when \code{median_method} is set to \code{"cd"}) character string specifying whether a fixed effect or random effects model is used. The options are \code{FE} (fixed effect) are \code{RE} (random effects). The default is \code{RE}.
#' @param pool_studies logical scalar specifying whether to meta-analyze the studies. If this argument is set to \code{FALSE}, function will not meta-analyze the studies and will return a list with components \code{yi} containing the study-specific outcome measure estimates and \code{sei} containing the study-specific within-study standard error estimates. The default is \code{TRUE}.
#' @param ... (only applicable when \code{median_method} is set to \code{"qe"}) optional arguments that are passed into the \code{\link[metafor]{rma.uni}} function for pooling. See documentation of \code{\link[metafor]{rma.uni}}.
#'
#' @return an object of class "rma.uni" (when \code{median_method} is set to \code{"qe"}) or a list (when \code{median_method} is set to \code{"mm"}, \code{"wm"}, or \code{"cd"}). For additional details, see \code{\link[metafor]{rma.uni}} (when \code{median_method} is set to \code{"qe"}), \code{\link{pool.med}} (when \code{median_method} is set to \code{"mm"} or \code{"wm"}), and \code{\link{cd}} (when \code{median_method} is set to \code{"cd"}).
#' @references McGrath S., Zhao X., Qin Z.Z., Steele R., and Benedetti A. (2019). One-sample aggregate data meta-analysis of medians. \emph{Statistics in Medicine}, \strong{38}, 969-984.
#' @references McGrath S., Sohn H., Steele R., and Benedetti A. (2020). Meta-analysis of the difference of medians. \emph{Biometrical Journal}, \strong{62}, 69-98.
#' @references Ozturk, O. and Balakrishnan N. (2020). Meta‐analysis of quantile intervals from different studies with an application to a pulmonary tuberculosis data. \emph{Statistics in Medicine}, \strong{39}, 4519-4537.
#'
#' @examples
#' ## Quantile Matching Estimation method
#' metamedian(data = dat.age, median_method = "qe")
#'
#' ## Median of the Difference of Medians method
#' metamedian(data = dat.age, median_method = "mm")
#'
#' ## Weighted Median of the Difference of Medians method
#' metamedian(data = dat.age, median_method = "wm")
#'
#' @export

metamedian <- function(data, median_method = 'qe', single.family = FALSE,
                       loc.shift = FALSE, norm.approx = TRUE, coverage.prob = 0.95,
                       method_cd = 'RE', pool_studies = TRUE, ...) {
  df <- check_and_clean_df(df = data, method = median_method)
  if (!median_method %in% c('qe', 'mm', 'wm', 'cd')){
    stop("median_method must be set to 'mm', 'wm', 'qe', or 'cd'")
  }

  if (median_method == 'qe'){
    # QE method
    res <- qe(min.g1 = df$min.g1, q1.g1 = df$q1.g1, med.g1 = df$med.g1, q3.g1 = df$q3.g1, max.g1 = df$max.g1, n.g1 = df$n.g1, mean.g1 = df$mean.g1, sd.g1 = df$sd.g1,
              min.g2 = df$min.g2, q1.g2 = df$q1.g2, med.g2 = df$med.g2, q3.g2 = df$q3.g2, max.g2 = df$max.g2, n.g2 = df$n.g2, mean.g2 = df$mean.g2, sd.g2 = df$sd.g2,
              single.family = single.family, loc.shift = loc.shift, pool_studies = pool_studies, ...)
  } else if (median_method == 'cd') {
    res <- cd(q1 = df$q1.g1, med = df$med.g1, q3 = df$q3.g1, n = df$n.g1, mean = df$mean.g1, sd = df$sd.g1,
              med.var = df$med.var.g1, med.ci.lb = df$med.ci.lb.g1, med.ci.ub = df$med.ci.ub.g1,
              alpha.1 = df$alpha.1.g1, alpha.2 = df$alpha.2.g1, pooled.median.ci.level = coverage.prob,
              method = method_cd, pool_studies = pool_studies)
  } else {
    # MM and WM methods
    one_group <- all(is.na(df[, c('min.g2', 'q1.g2', 'med.g2', 'q3.g2', 'max.g2', 'n.g2', 'mean.g2', 'sd.g2')]))
    if (one_group){
      yi <- ifelse(!is.na(df$med.g1), df$med.g1, df$mean.g1)
      if (!pool_studies){
        return(list(yi = yi, sei = NULL))
      }
      if (median_method == 'wm'){
        wi <- df$n.g1
      }
    } else {
      yi <- ifelse(!is.na(df$med.g1), df$med.g1, df$mean.g1) -
        ifelse(!is.na(df$med.g2), df$med.g2, df$mean.g2)
      if (!pool_studies){
        return(list(yi = yi, sei = NULL))
      }
      if (median_method == 'wm'){
        wi <- df$n.g1 + df$n.g2
      }
    }
    if (median_method == 'mm'){
      res <- pool.med(yi = yi, norm.approx = norm.approx, coverage.prob = coverage.prob)
    } else if (median_method == 'wm'){
      res <- pool.med(yi = yi, wi = wi, norm.approx = norm.approx, coverage.prob = coverage.prob)
    }
  }
  return(res)
}
