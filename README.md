
<!-- README.md is generated from README.Rmd. Please edit that file -->

# iNZightRegression

<!-- badges: start -->

![R-CMD-check](https://github.com/iNZightVIT/iNZightRegression/workflows/R-CMD-check/badge.svg)
[![Coverage
status](https://codecov.io/gh/iNZightVIT/iNZightRegression/branch/master/graph/badge.svg)](https://codecov.io/github/iNZightVIT/iNZightRegression?branch=master)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

An R package which provides summary information and plots which have
been altered from those provided by base R.

It now handles `glm` object, as well as `svyglm` objects from the
`survey` package.

## Installation

You can install the released version of iNZightRegression from
[CRAN](https://CRAN.R-project.org) with:

``` r
# Not yet on CRAN - please use devtools below
# install.packages("iNZightRegression")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("iNZightVIT/iNZightRegression")
```

## Example

Plots and summaries of model objects:

``` r
library(iNZightRegression)
#> Registered S3 methods overwritten by 'iNZightRegression':
#>   method                from     
#>   plot.moecalc          iNZightMR
#>   print.moecalc         iNZightMR
#>   print.summary.moecalc iNZightMR
#>   summary.moecalc       iNZightMR
iris.lm <- lm(Sepal.Width ~ Sepal.Length, data = iris)
inzplot(iris.lm)
```

<img src="man/figures/README-example-1.png" width="100%" />
