## Package Updates

### Changes in Version 1.1.1 (2023-12-13)

-   Renamed the argument `method_cd` to `cd_method` in the
    `metamedian()` function
-   Updated references

### Changes in Version 1.1.0 (2023-09-16)

-   Added the skewness test of Shi et al. (2023) to the
    `describe_studies()` function
-   Added the `dat.phq9_raw` and `dat.phq9` data sets
-   Changed the default values for the `se_method` and `sd_method`
    arguments in the `metamean()` function. They are now set based on
    the value set for the `mean_method` argument.
-   Renamed the `'wan'` option for the `mean_method` argument to
    `'hozo/wan/bland'`. Similarly, renamed the `'shi_normal'` option for
    the `'sd_method'` argument to `'wan/shi_normal'`. the `metamean()`
    function.
-   Expanded the error checking
-   Removed Travis CI
-   Updates to the documentation

### Changes in Version 1.0.0 (2023-02-27)

-   Added the `metamean()` function, which can apply several methods to
    estimate study-specific means and their standard errors in order to
    estimate the pooled (difference of) means
-   Added the `cd()` function, which applies the Confidence Distribution
    method of Ozturk and Balakrishnan (2020) to estimate the pooled
    median
-   Added the `metamedian()` function, which is a wrapper for the
    `qe()`, `cd()`, and `pool.med()` functions.
-   Added the `describe_studies()` function for performing descriptive
    analyses
-   Added the `dat.age_raw`, `dat.age`, `dat.asat_raw`, `dat.asat`,
    `dat.ck_raw`, and `dat.ck` data sets and corresponding example
    applications in the documentation
-   Added unit tests for the `metamean()`, `metamedian()`, and
    `describe_studies()` functions

### Changes in Version 0.1.6 (2022-06-18)

-   Fixed an error in Example 2 in the documentation for the `qe()`
    function

### Changes in Version 0.1.5 (2020-01-27)

-   Revised the `pool.med()` function to allow users to specify the
    desired coverage probability
-   Updated references

### Changes in Version 0.1.4 (2019-05-19)

-   Fixed an error in the `qe()` function when calculating the sampling
    variance for studies reporting means. Consequently, removed the need
    for the internal `qe.group.level()` function
-   Fixed an error in the `qe()` function when data provided are of
    class “integer”
-   Made `qe.fit()` and `print.qe.fit()` defunct. These functions were
    moved to the ‘estmeansd’ package.

### Changes in Version 0.1.3 (2019-03-08)

-   Begun using Git as a version-control system and hosting repository
    on GitHub (<https://github.com/stmcg/metamedian>) for the
    development version of the package
-   Fixed an issue with `qe()` throwing errors
-   Added URL and BugReport webpage to the DESCRIPTION file
-   Added README.md file
-   Removed export of `get.scenario()`, as we do not anticipate users
    applying this function

### Changes in Version 0.1.2 (2019-02-10)

-   Deprecated `qe.fit()` and `print.qe.fit()` functions. These
    functions were moved to the ‘estmeansd’ package. As a result,
    ‘estmeansd’ was imported.

### Changes in Version 0.1.1 (2018-11-23)

-   Updated DESCRIPTION file
-   Updated references
-   Added NEWS.md file to track changes

### Changes in Version 0.1.0 (2018-10-06)

-   First version released on CRAN
