#' Classify scenario based on summary measures
#'
#' The function indicates whether S1, S2, S3, or S4 summary measures are provided.  See documentation of \code{\link{qe}} for definitions of S1, S2, S3, and S4 summary measures. Note that the sample size is not included in this check.
#'
#' @param min.val numeric value giving the sample minimum.
#' @param q1.val numeric value giving the sample first quartile.
#' @param med.val numeric value giving the sample median.
#' @param q3.val numeric value giving the sample third quartile.
#' @param max.val numeric value giving the sample maximum.
#' @param mean.val numeric value giving the sample mean.
#' @param sd.val numeric value giving the sample standard deviation.
#'
#' @return A character string of one of the following: \code{"S1"}, \code{"S2"}, \code{"S3"}, or \code{"S4"}.
#'
#' @examples
#' scenario <- get.scenario(min.val=1, med.val=5, max.val=20)

#' @export


get.scenario <- function(min.val, q1.val, med.val, q3.val, max.val, mean.val,
                         sd.val) {
  if (!(missing(min.val) | missing(q1.val) | missing(med.val) | missing(q3.val)
        | missing(max.val))) {
    if (!any(is.na(c(min.val, q1.val, med.val, q3.val, max.val)))) {
      return("S3")
    }
  }
  if (!(missing(q1.val) | missing(med.val) | missing(q3.val))) {
    if (!any(is.na(c(q1.val, med.val, q3.val)))) {
      return("S2")
    }
  }
  if (!(missing(min.val) | missing(med.val) | missing(max.val))) {
    if (!any(is.na(c(min.val, med.val, max.val)))) {
      return("S1")
    }
  }
  if (!(missing(mean.val) | missing(sd.val))) {
    if (!any(is.na(c(mean.val, sd.val)))) {
      return("S4")
    }
  }
  stop("Summary measures not in appropriate form. See documentation for
       appropriate forms.")
}
