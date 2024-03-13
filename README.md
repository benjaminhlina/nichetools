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


## Progress
-   [X] create `mu_extract()`
    -   [X] build tests for `mu_extract()`
    
-   [X] create `sigma_extract()`
    -   [X] build tests for `sigma_extract()`
    
-   [X] create `niche_ellipse()`
    -   [X] build tests for `niche_ellipse()`
    
-   [x] create `overlap_extract()`
    -   [X] build tests for `overlap_extract()`
    
-   [X] create `niche_size_extract()` 
    -   [X] build tests for `niche_size_extract()` 
    
-   [ ] Create vignette/modify blog post vignette
