# demUtils

<!-- badges: start -->
[![R build status](https://github.com/ihmeuw/demUtils/workflows/R-CMD-check/badge.svg)](https://github.com/ihmeuw/demUtils/actions)
[![Codecov test coverage](https://codecov.io/gh/ihmeuw/demUtils/branch/master/graph/badge.svg)](https://codecov.io/gh/ihmeuw/demUtils?branch=master)
<!-- badges: end -->

Demographics data is often hierarchically structured. Categorical and numeric
interval variables like location, sex, age, race, ethnicity, education, marital
status, etc. have different levels of detail. The demUtils package provides
functions for formatting, aggregating, scaling, and other related tasks useful
for hierarchical data.

## Installation

This package has not yet been published on CRAN, but can be installed from
GitHub using the [remotes](https://remotes.r-lib.org/) package.

```r
remotes::install_github("ihmeuw/demUtils")
```

## Getting help

If you encounter a clear bug, please file a minimal reproducible example on [github](https://github.com/ihmeuw/demUtils/issues).
