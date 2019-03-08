
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metamedian: Meta-Analysis of Medians

[![Build\_Status](https://travis-ci.org/stmcg/metamedian.svg?branch=master)](https://travis-ci.org/stmcg/metamedian)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/metamedian)](https://cran.r-project.org/package=metamedian)
[![CRAN\_Download\_Badge](https://cranlogs.r-pkg.org/badges/metamedian)](https://www.r-pkg.org/pkg/metamedian)
[![CRAN\_Download\_Badge\_All](https://cranlogs.r-pkg.org/badges/grand-total/metamedian)](https://www.r-pkg.org/pkg/metamedian)

The `metamedian` package implements several methods to meta-analyze
studies that report the sample median of the outcome. When the primary
studies are one-group studies, the methods of [McGrath et
al. (2019)](https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.8013?af=R)
can be applied to estimate the pooled median. In the two-group context,
the methods of [McGrath et al. (2018)](https://arxiv.org/abs/1809.01278)
can be applied to estimate the pooled raw difference of medians across
groups.

## Installation

You can install the released version of `metamedian` from CRAN with:

``` r
install.packages("metamedian")
```

After installing the `devtools` package (i.e., calling
`install.packages(devtools)`), the development version of `metamedian`
can be installed from GitHub with:

``` r
devtools::install_github("stmcg/metamedian")
```
