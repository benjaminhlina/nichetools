# nichetools <img src="man/figures/hex_sticker.png" align="right" width="120" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![nichetools status badge](https://benjaminhlina.r-universe.dev/badges/nichetools)](https://benjaminhlina.r-universe.dev/nichetools)
[![R-CMD-check](https://github.com/benjaminhlina/nichetool/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/benjaminhlina/nichetool/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/benjaminhlina/nichetools/graph/badge.svg?token=mk3RjaD0hb)](https://codecov.io/gh/benjaminhlina/nichetools)
<!-- badges: end -->

{nichetools} is a complementary package to [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html) that allows the user to extract Bayesian estimates from data objects created by [{nicheROVER}](https://cran.r-project.org/web/packages/nicheROVER/index.html) (e.g., niche size and similarities).

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


## Progress
-   [X] create `extract_mu()`
    -   [X] build tests for `extract_mu()`
    
-   [X] create `extract_sigma()`
    -   [X] build tests for `extract_sigma()`
    
-   [X] create `niche_ellipse()`
    -   [X] build tests for `niche_ellipse()`
    
-   [x] create `extract_overlap()`
    -   [X] build tests for `extract_overlap()`
    
-   [X] create `extract_niche_size()` 
    -   [X] build tests for `extract_niche_size()` 
    
-   [X] Create vignette/modify blog post vignette

## Citation 

To cite this package please cite the following publications 

-   Swanson, H.K., Lysy, M., Power, M., Stasko, A.D., Johnson, J.D., and Reist, J.D. 2015. A new probabilistic method for quantifying n-dimensional ecological niches and niche overlap. Ecology 96(2): 318–324. [doi:10.1890/14-0235.1](https://esajournals.onlinelibrary.wiley.com/doi/full/10.1890/14-0235.1)

-   Hlina BL (2024). nichetools: Complementary package to nicheROVER. R package version 0.1.0. https://benjaminhlina.github.io/nichetools/
