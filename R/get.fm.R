get.fm <- function(x, family) {
  if (!(family %in% c("normal", "log-normal", "gamma", "weibull"))) {
    stop("family must be either normal, log-normal, gamma, or Weibull.")
  }
  if (family == "normal") {
    par <- x$norm.par
    fm <- stats::dnorm(stats::qnorm(0.5, par[1], par[2]), par[1], par[2])
  }
  else if (family == "log-normal") {
    par <- x$lnorm.par
    fm <- stats::dlnorm(stats::qlnorm(0.5, par[1], par[2]), par[1], par[2])
  }
  else if (family == "gamma") {
    par <- x$gamma.par
    fm <- stats::dgamma(stats::qgamma(0.5, par[1], par[2]), par[1], par[2])
  }
  else if (family == "weibull") {
    par <- x$weibull.par
    fm <- stats::dweibull(stats::qweibull(0.5, par[1], par[2]), par[1], par[2])
  }
  return(fm)
}

