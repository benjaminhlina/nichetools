# Create ellipses based on Bayesian estimates of \\\mu\\ and \\\Sigma\\

This function allows the user to supply Bayesian estimates of \\\mu\\
and \\\Sigma\\ to create estimated Bayesian ellipse for niche region.

## Usage

``` r
niche_ellipse(
  dat_mu,
  dat_sigma,
  isotope_a = NULL,
  isotope_b = NULL,
  p_ell = NULL,
  random = NULL,
  set_seed = NULL,
  n = NULL,
  message = TRUE,
  ...
)
```

## Arguments

- dat_mu:

  a `data.frame` containing \\\mu\\ Bayesian estimates. This
  `data.frame` needs to be in long format with each \\\mu\\ estimate for
  each isotope stacked on top of each other. This can be produced using
  [`extract_mu()`](https://benjaminhlina.github.io/nichetools/reference/extract_mu.md).

- dat_sigma:

  a `data.frame` containing \\\Sigma\\ Bayesian estimates. This
  `data.frame` needs be in wide format, that is \\\Sigma\\ (covariance)
  matrices stacked on top of each other. See example of how to convert
  to wide format. This can be produced using
  [`extract_sigma()`](https://benjaminhlina.github.io/nichetools/reference/extract_sigma.md).

- isotope_a:

  character string that is the column name of the first isotope used in
  `dat_sigma`. Defaults to `"d13c"`.

- isotope_b:

  character string that is the column name of the second isotope used in
  `dat_sigma`. Defaults to `"d15n"`.

- p_ell:

  is the confidence interval of each ellipse estimate. Default is 0.95
  (i.e., 95% confidence interval). This value is bound by 0 and 1 and
  has to be a `numeric`.

- random:

  logical value indicating whether or not to randomly sample posterior
  distributions for \\\mu\\ and \\\Sigma\\ to create a sub-sample of
  ellipse. Default is `TRUE`.

- set_seed:

  numerical value to set seed for random sampling. Default is a random
  value. To consistently sample the same subsample, please supply a
  numerical value (e.g., `4`). It is highly suggested to use set_seed to
  make the function results when randomly sampling reproducible.

- n:

  numerical value that controls the number of random samples. Default is
  `10`.

- message:

  control whether the time processing is displayed after the end of the
  function. Default is `TRUE`.

- ...:

  additional paramaters to pass to
  [`ellipse::ellipse()`](https://dmurdoch.github.io/ellipse/reference/ellipse.html).
  See that function for more detail.

## Value

A `tibble` containing, `sample_name`, `sample_number`, and the isotopes
that were used in the estimation of ellipse (i.e., and `d13c` and
`d15n`).

## See also

[`nicheROVER::niw.post()`](https://rdrr.io/pkg/nicheROVER/man/niw.post.html),
[`SIBER::siberMVN()`](https://rdrr.io/pkg/SIBER/man/siberMVN.html),
[`extract_mu()`](https://benjaminhlina.github.io/nichetools/reference/extract_mu.md),
and
[`extract_sigma()`](https://benjaminhlina.github.io/nichetools/reference/extract_sigma.md)

## Examples

``` r
niche_ellipse(dat_mu = mu_est_long,
              dat_sigma = sigma_est_wide)
#> → Total time processing was 0.06 secs
#> # A tibble: 3,600 × 4
#>    sample_name sample_number  d13c  d15n
#>    <chr>               <dbl> <dbl> <dbl>
#>  1 ARCS                  126 -21.9  14.1
#>  2 ARCS                  126 -22.0  14.1
#>  3 ARCS                  126 -22.1  14.2
#>  4 ARCS                  126 -22.2  14.2
#>  5 ARCS                  126 -22.3  14.3
#>  6 ARCS                  126 -22.4  14.3
#>  7 ARCS                  126 -22.6  14.3
#>  8 ARCS                  126 -22.7  14.3
#>  9 ARCS                  126 -22.8  14.4
#> 10 ARCS                  126 -23.0  14.4
#> # ℹ 3,590 more rows
```
