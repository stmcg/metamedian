
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metamedian: Meta-Analysis of Medians

[![Build_Status](https://travis-ci.org/stmcg/metamedian.svg?branch=master)](https://travis-ci.org/stmcg/metamedian)
[![CRAN_Status_Badge](https://badges.cranchecks.info/worst/metamedian.svg)](https://cran.r-project.org/package=metamedian)
[![CRAN_Download_Badge](https://cranlogs.r-pkg.org/badges/metamedian)](https://www.r-pkg.org/pkg/metamedian)
[![CRAN_Download_Badge_All](https://cranlogs.r-pkg.org/badges/grand-total/metamedian)](https://www.r-pkg.org/pkg/metamedian)

The `metamedian` package implements several methods to meta-analyze
studies that report the sample median of the outcome. When the primary
studies are one-group studies, the methods of [McGrath et
al. (2019)](https://doi.org/10.1002/sim.8013) and [Ozturk and
Balakrishnan (2020)](https://doi.org/10.1002/sim.8738) can be applied to
estimate the pooled median. In the two-group context, the methods of
[McGrath et al. (2020a)](https://doi.org/10.1002/bimj.201900036) can be
applied to estimate the pooled difference of medians across groups.

Additionally, this package implements a number of methods to estimate
the study-specific means and their standard errors from studies
reporting sample medians in order to estimate the pooled (difference of)
means. Specifically, one can apply the following methods in this
context:

- [Hozo et
  al. (2005)](https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-5-13)
- [Wan et
  al. (2014)](https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-14-135)
- [Bland
  (2015)](https://lifescienceglobal.com/pms/index.php/ijsmr/article/view/2688)
- [Luo et al. (2018)](https://doi.org/10.1177/0962280216669183)
- [Shi et al. (2020a)](https://doi.org/10.1002/jrsm.1429)
- [Shi et
  al. (2020b)](https://www.intlpress.com/site/pub/pages/journals/items/sii/content/vols/0013/0004/a009/)
- [McGrath et al. (2020b)](https://doi.org/10.1177/0962280219889080)
- [Cai et al. (2021)](https://doi.org/10.1177/09622802211047348)
- [Yang et
  al. (2022)](https://www.tandfonline.com/doi/full/10.1080/02664763.2021.1967890)
- [McGrath et al. (2023)](https://doi.org/10.1177/09622802221139233)

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
