#' Meta-Analysis via quantile estimation method
#'
#' The function applies the quantile estimation (QE) method (McGrath et al., 2018) to meta-analyze one-group or two-group studies where each study reports one of the following summary measures: \itemize{
#' \item S1: median, minimum and maximum values, and sample size
#' \item S2: median, first and third quartiles, and sample size
#' \item S3: median, minimum and maximum values, first and third quartiles, and sample size
#' \item S4: mean, standard deivation, and sample size.
#'  }
#' For one-group studies, the function estimates the pooled median. For two-group studies, the function estimates the pooled raw difference of medians across groups. The convention used for calculating differences in two-group studies is: value in group 1 minus value in group 2.
#'
#' Letting \eqn{k} denote the number of studies, provide study-specific summary data as vectors of length \eqn{k}. If a study does not report a given summary measure (e.g., the minimum value), give a value of \code{NA} for the position in the relevant vector. If no studies report a given summary measure, a vector of only \code{NA} values need not be provided. See 'Examples' for appropriate use.
#'
#' The sampling variance of the effect size for each study is estimated via the QE method. The default starting values and box constraints of the parameters in the minimization algorithm (\code{\link{qe.fit}}) are used. After estimating the sampling variances for all studies, studies are meta-analyzed using the \code{\link[metafor]{rma.uni}} function.
#'
#' @param min.g1 vector of study-specific sample minimum values (first group for two-group studies). See 'Details'.
#' @param q1.g1 vector of study-specific sample first quartile values (first group for two-group studies). See 'Details'.
#' @param med.g1 vector of study-specific sample median values (first group for two-group studies). See 'Details'.
#' @param q3.g1 vector of study-specific sample third quartile values (first group for two-group studies). See 'Details'.
#' @param max.g1 vector of study-specific sample maximum values (first group for two-group studies). See 'Details'.
#' @param n.g1 vector of study-specific sample sizes (first group for two-group studies). See 'Details'.
#' @param mean.g1 vector of study-specific sample mean values (first group for two-group studies). See 'Details'.
#' @param sd.g1 vector of study-specific sample standard deviation values (first group for two-group studies). See 'Details'.
#' @param min.g2 vector of study-specific sample minimum values of the second group for two-group studies. See 'Details'.
#' @param q1.g2 vector of study-specific sample first quartile values of the second group for two-group studies. See 'Details'.
#' @param med.g2 vector of study-specific sample median values of the second group for two-group studies. See 'Details'.
#' @param q3.g2 vector of study-specific sample third quartile values of the second group for two-group studies. See 'Details'.
#' @param max.g2 vector of study-specific sample maximum values of the second group for two-group studies. See 'Details'.
#' @param n.g2 vector of study-specific sample sizes of the second group for two-group studies. See 'Details'.
#' @param mean.g2 vector of study-specific sample mean values of the second group for two-group studies. See 'Details'.
#' @param sd.g2 vector of study-specific sample standard deviation values of the second group for two-group studies. See 'Details'.
#' @param single.family logical scalar indicating that for two-group studies, the parametric family of distributions is assumed to be the same across both groups (the default is \code{FALSE}). See 'Details' of \code{\link{qe.study.level}}.
#' @param loc.shift logical scalar indicating that for two-group studies, distributions are assumed to only differ by a location shift (the default is \code{FALSE}). See 'Details' of \code{\link{qe.study.level}}.
#' @param ... optional arguments for pooling. See documentation of \code{\link[metafor]{rma.uni}}.

#' @return An object of class "rma.uni". See documentation of \code{\link[metafor]{rma.uni}}.
#' @references McGrath S., Sohn H., Steele R., and Benedetti A. (2018). Two-sample aggregate data meta-analysis of medians. \emph{ArXiv e-prints}. \url{https://arxiv.org/abs/1809.01278}.
#'
#' @examples
#' ## Example 1: Meta-analysis of one-group studies
#'
#' ## Storing data
#' ## Note: All 6 studies report S2
#' med.vals <- c(6.1, 5.2, 3.1, 2.8, 4.5)
#' q1.vals <- c(2.0, 1.6, 2.6, 0.9, 3.2)
#' q3.vals <- c(10.2, 13.0, 8.3, 8.2, 9.9)
#' n.vals <- c(100, 92, 221, 81, 42)
#'
#' ## Meta-analyze studies via QE method
#' qe(q1.g1 = q1.vals, med.g1 = med.vals, q3.g1 = q3.vals, n.g1 = n.vals)
#'
#'
#' ## Example 2: Meta-analysis of one-group studies
#'
#' ## Storing data
#' ## Note: Studies 1, 2, 3, and 4 report S1, S2, S3, and S4, respectively
#' min.vals <- c(0.7, NA, 1.1, NA)
#' q1.vals <- c(NA, 5.2, 5.3, NA)
#' med.vals <- c(8.7, 10.7, 11.0, NA)
#' q3.vals <- c(NA, 5.2, 5.3, NA)
#' max.vals <- c(22.2, NA, 24.7, NA)
#' n.vals <- c(52, 34, 57, 90)
#' sd.vals <- c(NA, NA, NA, 4.2)
#' mean.vals <- c(NA, NA, NA, 12.2)
#'
#' ## Meta-analyze studies via QE method
#' qe(min.g1 = min.vals, q1.g1 = q1.vals, med.g1 = med.vals, q3.g1 = q3.vals,
#'    max.g1 = max.vals, n.g1 = n.vals, mean.g1 = mean.vals, sd.g1 = sd.vals)
#'
#'
#' ## Example 3: Meta-analysis of two-group studies
#'
#' ## Storing data
#' ## Note: All 4 studies report S3
#' min.g1 <- c(2.3, 3.2, 1.9, 1.7)
#' q1.g1 <- c(6.0, 7.1, 3.5, 3.8)
#' med.g1 <- c(8.7, 9.5, 5.9, 6.0)
#' q3.g1 <- c(11.3, 13.1, 10.8, 11.0)
#' max.g1 <- c(20.6, 25.3, 17.0, 18.6)
#' n.g1 <- c(53, 49, 66, 75)
#' min.g2 <- c(0.4, 0.9, 0.5, 0.3)
#' q1.g2 <- c(2.5, 3.1, 2.7, 2.3)
#' med.g2 <- c(5.1, 6.2, 4.9, 4.7)
#' q3.g2 <- c(9.6, 10.1, 8.8, 9.2)
#' max.g2 <- c(20.2, 21.4, 18.8, 19.2)
#' n.g2 <- c(50, 45, 60, 73)
#'
#' ## Meta-analyze studies via QE method
#' qe(min.g1 = min.g1, q1.g1 = q1.g1, med.g1 = med.g1, q3.g1 = q3.g1,
#'    max.g1 = max.g1, n.g1 = n.g1, min.g2 = min.g2, q1.g2 = q1.g2,
#'    med.g2 = med.g2, q3.g2 = q3.g2, max.g2 = max.g2, n.g2 = n.g2)
#'
#' @export

qe <- function(min.g1, q1.g1, med.g1, q3.g1, max.g1, n.g1, mean.g1, sd.g1,
               min.g2, q1.g2, med.g2, q3.g2, max.g2, n.g2, mean.g2, sd.g2,
               single.family = FALSE, loc.shift = FALSE, ...) {
  all.data.args.names <- c("min.g1", "q1.g1", "med.g1", "q3.g1", "max.g1",
                           "n.g1", "mean.g1", "sd.g1", "min.g2", "q1.g2",
                           "med.g2", "q3.g2", "max.g2", "n.g2", "mean.g2",
                           "sd.g2")

  all.args <- as.list(environment())
  data.args <- all.args[names(all.args) %in% all.data.args.names]
  data.args.spec <- data.args[sapply(data.args,
                                     function(x) class(x) == "numeric")]
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

  res <- mapply(qe.study.level, df$min.g1, df$q1.g1, df$med.g1, df$q3.g1,
                df$max.g1, df$n.g1, df$mean.g1, df$sd.g1, df$min.g2, df$q1.g2,
                df$med.g2, df$q3.g2, df$max.g2, df$n.g2, df$mean.g2, df$sd.g2,
                single.family, loc.shift)

  study.types <- unlist(res["study.type", ])

  if (length(unique(study.types)) != 1) {
    stop("All studies must have the same study type (i.e.,
         all studies must be one-group or all studies must be two-group)")
  }
  output <- metafor::rma.uni(yi = unlist(res["effect.size", ]),
                             vi = unlist(res["var", ]), ...)
  return(output)
}
