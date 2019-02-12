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
