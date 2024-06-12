# nichetools <img src="man/figures/hex_sticker.png" align="right" width="120" />

<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN status](https://www.r-pkg.org/badges/version/nichetools)](https://CRAN.R-project.org/package=nichetools)
[![nichetools status badge](https://benjaminhlina.r-universe.dev/badges/nichetools)](https://benjaminhlina.r-universe.dev/nichetools)
[![R-CMD-check](https://github.com/benjaminhlina/nichetool/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/benjaminhlina/nichetools/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/benjaminhlina/nichetools/graph/badge.svg?token=mk3RjaD0hb)](https://app.codecov.io/gh/benjaminhlina/nichetools)
<!-- badges: end -->

[{nichetools}](https://benjaminhlina.github.io/nichetools/) is a complementary package to [{nicheROVER}](https://cran.r-project.org/package=nicheROVER) and 
[{SIBER}](https://cran.r-project.org/package=SIBER) that allows the user to extract Bayesian estimates from data objects created by [{nicheROVER}](https://cran.r-project.org/package=nicheROVER) or 
[{SIBER}](https://cran.r-project.org/package=SIBER)(e.g., niche size and similarities).

## Installation

You can install the development version of {nichetools} using the following:
``` r
install.packages("devtools")
devtools::install_github("benjaminhlina/nichetools")
```


You can install the r-universe version of {nichetools} using the following:
``` r
install.packages("nichetools", 
repos = c("https://benjaminhlina.r-universe.dev", 
"https://cran.r-project.org"))
```

You can install the CRAN version of {nichetools} using the following:

``` r
install.packages("nichetools")
```

## Vignette 

Once you have loaded {nichetools} and {nicheROVER} or {SIBER} you can access 
the vignettes using the following code: 

``` r
vignette("using-nichetools-with-the-package-nicheROVER")
```
The vignettes are also available online under the [articles section](https://benjaminhlina.github.io/nichetools/articles/using-nichetools-with-the-package-nicheROVER.html) of the package
and on my [blog](https://blog.benjaminhlina.com/posts/post-with-code/nichetools/). 

I highly suggest going through the vignettes as they will walk you through 
how to use {nichetools} in tandem with {nicheROVER} or {SIBER}.

## Citation 

To cite this package please cite the following publications 

-   Swanson, H.K., Lysy, M., Power, M., Stasko, A.D., Johnson, J.D., and Reist, J.D. 2015. A new probabilistic method for quantifying n-dimensional ecological niches and niche overlap. Ecology 96(2): 318–324. [doi:10.1890/14-0235.1](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/14-0235.1)

-   Jackson, A.L., Inger, R., Parnell, A.C., and Bearhop, S. 2011. Comparing isotopic niche widths among and within communities: SIBER – Stable Isotope Bayesian Ellipses in R. Journal of Animal Ecology 80(3): 595–602. [doi:10.1111/j.1365-2656.2011.01806.x.](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2656.2011.01806.x)


-   Hlina B.L. 2024. nichetools: Complementary package to nicheROVER and SIBER. R package version 0.2.0. https://benjaminhlina.github.io/nichetools/
