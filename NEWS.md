## Package Updates

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
