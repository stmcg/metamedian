% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metamedian_survival.R
\name{metamedian_survival}
\alias{metamedian_survival}
\title{Meta-Analysis of the median survival times}
\usage{
metamedian_survival(data, estimand, pool_studies = TRUE, ...)
}
\arguments{
\item{data}{data frame containing the study-specific summary data. For one-group studies, this data frame can contain the following columns:
\tabular{ll}{
\code{med.g1} \tab median. \cr
\code{med.ci.lb.g1} \tab lower confidence interval bound around the median. \cr
\code{med.ci.ub.g1} \tab upper confidence interval bound around the median. \cr
\code{med.ci.level.g1} \tab level of the confidence interval}
For two group studies, this data frame can also contain the following columns for the summary data of the second group: \code{med.g2}, \code{med.ci.lb.g2}, \code{med.ci.ub.g2}, and \code{med.ci.level.g2}.}

\item{estimand}{character string specifying the estimand in the meta-analysis. The options are \code{"median_g1"} (median in group 1), \code{"median_g2"} (median in group 2), \code{"difference_median"} (difference of medians across groups), and \code{"ratio_median"} (ratio of medians across groups).}

\item{pool_studies}{logical scalar specifying whether to meta-analyze the studies. If this argument is set to \code{FALSE}, function will not meta-analyze the studies and will return a list with components \code{yi} containing the study-specific outcome measure estimates and \code{sei} containing the study-specific within-study standard error estimates. The default is \code{TRUE}.}

\item{...}{optional arguments that are passed into the \code{\link[metafor]{rma.uni}} function for pooling. See documentation of \code{\link[metafor]{rma.uni}}.}
}
\value{
an object of class "rma.uni". For additional details, see \code{\link[metafor]{rma.uni}}.
}
\description{
This function implements the Wald approximation-based approach described by McGrath et al. (2025) to meta-analyze median survival times. This approach considers that each study reports a Kaplan-Meier estimate of median survival in (each group of) each study along with confidence intervals. This approach performs an inverse-variance weighted meta-analysis of the median survival time (for one group studies) or the difference/ratio of median survival times between groups (for two-group studies). The within-study standard errors are estimated by assuming the confidence intervals around the median survival estimates are Wald-type intervals.
}
\examples{
## Median survival in the comparator group
metamedian_survival(data = dat.lung, estimand = "median_g2")

## Difference of median survival
metamedian_survival(data = dat.lung, estimand = "difference_median")

## Ratio of median survival (logarithm scale)
metamedian_survival(data = dat.lung, estimand = "ratio_median")

}
\references{
McGrath S., Kimmelman J., Ozturk O., Steele R., and Benedetti A. (2025). Meta-analysis of median survival times with inverse-variance weighting. arXiv e-prints arXiv:2503.03065.
}
