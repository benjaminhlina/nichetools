# A `list` of the posterior estimates of \\\mu\\ and \\\Sigma\\ from `{nicheROVER}`

Posterior estimates of \\\mu\\ and \\\Sigma\\ using the `fish` data set
from [nicheROVER](https://cran.r-project.org/package=nicheROVER), using
Normal-Inverse-Wishart (NIW) priors. This `list` is produced using the
function `niw.post()` from
[nicheROVER](https://cran.r-project.org/package=nicheROVER).

## Usage

``` r
niw_fish_post
```

## Format

A `list` with elements \\\mu\\ and \\\Sigma\\ of sizes c(nsamples,
length(lambda)) and c(dim(Psi)).
