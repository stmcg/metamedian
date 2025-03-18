
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metamedian: Meta-Analysis of Medians

[![CRAN_Status_Badge](https://badges.cranchecks.info/worst/metamedian.svg)](https://cran.r-project.org/package=metamedian)
[![R-CMD-check](https://github.com/stmcg/metamedian/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stmcg/metamedian/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/stmcg/metamedian/graph/badge.svg)](https://app.codecov.io/gh/stmcg/metamedian)
[![CRAN_Download_Badge](https://cranlogs.r-pkg.org/badges/metamedian)](https://www.r-pkg.org/pkg/metamedian)
[![CRAN_Download_Badge_All](https://cranlogs.r-pkg.org/badges/grand-total/metamedian)](https://www.r-pkg.org/pkg/metamedian)

The `metamedian` package implements methods to meta-analyze studies that
report estimates of the median of the outcome of interest.

## Methods Included

### Median-Based Methods

This package implements several methods to directly meta-analyze studies
reporting sample medians. When the primary studies are one-group
studies, the methods of [McGrath et
al. (2019)](https://doi.org/10.1002/sim.8013) and [Ozturk and
Balakrishnan (2020)](https://doi.org/10.1002/sim.8738) can be applied to
estimate the pooled median. In the two-group context, the methods of
[McGrath et al. (2020a)](https://doi.org/10.1002/bimj.201900036) can be
applied to estimate the pooled difference of medians between groups.

The package also implements methods to meta-analyze median survival
times. The package can apply the methods described by [McGrath et
al. (2025)](https://doi.org/10.48550/arXiv.2503.03065) to meta-analyze
the median survival time, difference of median survival times between
groups, and the ratio of median survival times between groups.

### Mean-Based Methods

These methods estimate the study-specific means and their standard
errors from studies reporting sample medians in order to estimate the
pooled (difference of) means. Specifically, one can apply the following
methods:

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
- [Yang et al. (2022)](https://doi.org/10.1080/02664763.2021.1967890)
- [McGrath et al. (2023)](https://doi.org/10.1177/09622802221139233)
- [Shi et al. (2023)](https://doi.org/10.1177/09622802231172043)

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

## Usage

See [McGrath et al. (2024)](https://doi.org/10.1002/jrsm.1686) for a
detailed guide on using the `metamedian` package. Below, we include the
code used in the main examples in the paper. The examples are based on
the dataset `dat.age` included in the package.

Loading the package:

``` r
library(metamedian)
```

Setting a random number seed for reproducibility:

``` r
set.seed(1234)
```

### Descriptive Analyses

``` r
describe_studies(data = dat.age, group_labels = c ("Nonsurvivors", "Survivors"))
#> DESCRIPTION OF PRIMARY STUDIES
#>                                                       Nonsurvivors Survivors
#> N. studies:                                                     51        51
#> N. studies reporting the median:                                29        29
#>   N. studies reporting S1 (min, med, max, n):                    0         0
#>   N. studies reporting S2 (q1, med, q3, n):                     29        29
#>   N. studies reporting S3 (min, q1, med, q3, max, n):            0         0
#> N. studies reporting the mean:                                  22        22
#>   N. studies reporting the mean, sd, and n:                     22        22
#> Bowley skewness                                                             
#>   Minimum:                                                 -0.4000   -0.6000
#>   First quartile:                                          -0.0818   -0.1304
#>   Median:                                                   0.0000   -0.0526
#>   Mean:                                                    -0.0087   -0.0250
#>   Third quartile:                                           0.0909    0.1458
#>   Maximum:                                                  0.3846    0.4167
```

### Mean-Based Methods

The analyses below may take 5-10 minutes to run on a standard laptop.
For faster runtime, consider using fewer bootstrap replicates for the
QE, BC, and MLN methods.

``` r
res_wan <- metamean(dat.age, mean_method = "hozo/wan/bland")
res_luo <- metamean(dat.age, mean_method = "luo")
res_shi <- metamean(dat.age, mean_method = "shi_lognormal")
res_qe_mean <- metamean(dat.age, mean_method = "qe")
res_bc <- metamean(dat.age, mean_method = "bc")
res_mln <- metamean(dat.age, mean_method = "mln")
res_yang <- metamean(dat.age, mean_method = "yang")
```

The output of `res_mln` is illustrated below:

``` r
res_mln
#> 
#> Random-Effects Model (k = 51; tau^2 estimator: REML)
#> 
#> tau^2 (estimated amount of total heterogeneity): 28.0379 (SE = 7.0239)
#> tau (square root of estimated tau^2 value):      5.2951
#> I^2 (total heterogeneity / total variability):   86.72%
#> H^2 (total variability / sampling variability):  7.53
#> 
#> Test for Heterogeneity:
#> Q(df = 50) = 357.0967, p-val < .0001
#> 
#> Model Results:
#> 
#> estimate      se     zval    pval    ci.lb    ci.ub      
#>  12.8650  0.8342  15.4222  <.0001  11.2300  14.4999  *** 
#> 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

### Median-Based Methods

``` r
res_mm <- metamedian(dat.age, median_method = "mm")
res_wm <- metamedian(dat.age, median_method = "wm")
res_qe_median <- metamedian(dat.age, median_method = "qe")
```

The output of `res_qe_median` is illustrated below:

``` r
res_qe_median
#> 
#> Random-Effects Model (k = 51; tau^2 estimator: REML)
#> 
#> tau^2 (estimated amount of total heterogeneity): 33.5585 (SE = 8.3883)
#> tau (square root of estimated tau^2 value):      5.7930
#> I^2 (total heterogeneity / total variability):   86.95%
#> H^2 (total variability / sampling variability):  7.66
#> 
#> Test for Heterogeneity:
#> Q(df = 50) = 373.6841, p-val < .0001
#> 
#> Model Results:
#> 
#> estimate      se     zval    pval    ci.lb    ci.ub      
#>  13.2238  0.9121  14.4980  <.0001  11.4361  15.0115  *** 
#> 
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Reference

To cite this package, use:

> McGrath S, Zhao X, Ozturk O, Katzenschlager S, Steele R, Benedetti A.
> metamedian: An R package for meta-analyzing studies reporting medians.
> *Research Synthesis Methods*. 2024; 15(2):332-346.
> <doi:10.1002/jrsm.1686>

For a BibTeX citation, use:

    @article{mcgrath2024metamedian,
      title={metamedian: An R package for meta-analyzing studies reporting medians},
      author={McGrath, Sean and Zhao, XiaoFei and Ozturk, Omer and Katzenschlager, Stephan and Steele, Russell and Benedetti, Andrea},
      journal={Research Synthesis Methods},
      volume={15},
      number={2},
      pages={332--346},
      year={2024},
      publisher={Wiley Online Library}
    }
